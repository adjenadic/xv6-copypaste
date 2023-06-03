// Console input and output.
// Input is from the keyboard or serial port.
// Output is written to the screen and serial port.

#include "types.h"
#include "defs.h"
#include "param.h"
#include "traps.h"
#include "spinlock.h"
#include "sleeplock.h"
#include "fs.h"
#include "file.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "x86.h"

static void consputc(int);

static int panicked = 0;

static struct {
  struct spinlock lock;
  int locking;
} cons;

static void printint(int xx, int base, int sign) {
  static char digits[] = "0123456789abcdef";
  char buf[16];
  int i;
  uint x;

  if (sign && (sign = xx < 0))
    x = -xx;
  else
    x = xx;

  i = 0;
  do {
    buf[i++] = digits[x % base];
  } while ((x /= base) != 0);

  if (sign)
    buf[i++] = '-';

  while (--i >= 0)
    consputc(buf[i]);
}

// Print to the console. only understands %d, %x, %p, %s.
void cprintf(char *fmt, ...) {
  int i, c, locking;
  uint *argp;
  char *s;

  locking = cons.locking;
  if (locking)
    acquire(&cons.lock);

  if (fmt == 0)
    panic("null fmt");

  argp = (uint *)(void *)(&fmt + 1);
  for (i = 0;
    (c = fmt[i] &0xff) != 0; i++) {
    if (c != '%') {
      consputc(c);
      continue;
    }
    c = fmt[++i] &0xff;
    if (c == 0)
      break;
    switch (c) {
    case 'd':
      printint(*argp++, 10, 1);
      break;
    case 'x':
    case 'p':
      printint(*argp++, 16, 0);
      break;
    case 's':
      if ((s = (char *) *argp++) == 0)
        s = "(null)";
      for (;* s; s++)
        consputc(*s);
      break;
    case '%':
      consputc('%');
      break;
    default:
      // Print unknown % sequence to draw attention.
      consputc('%');
      consputc(c);
      break;
    }
  }

  if (locking)
    release(&cons.lock);
}

void panic(char *s) {
  int i;
  uint pcs[10];

  cli();
  cons.locking = 0;
  // use lapiccpunum so that we can call panic from mycpu()
  cprintf("lapicid %d: panic: ", lapicid());
  cprintf(s);
  cprintf("\n");
  getcallerpcs(&s, pcs);
  for (i = 0; i < 10; i++)
    cprintf(" %p", pcs[i]);
  panicked = 1; // freeze other CPU
  for (;;)
  ;
}

#define BACKSPACE 0x100
#define CRTPORT 0x3d4
static ushort *crt = (ushort *) P2V(0xb8000); // CGA memory

static ushort special_mode, spm_switch;
static int spm_orig_pos = 0;

static ushort selection_mode, sem_anchor, sem_old_pos;
static char copied_string[128];

void cgaputc_fill_string(int pos1, int pos2) {
  int j = 0;
  for (int i = pos1; i < pos2; i++) {
    crt[i] = (crt[i] & 0xff) | 0x0700;
    if (j < 128) {
      copied_string[j++] = crt[i];
    }
  }
  copied_string[j] = 0;
}

static void cgaputc(int c) {
  int pos;

  // Cursor position: col + 80*row.
  outb(CRTPORT, 14);
  pos = inb(CRTPORT + 1) << 8;
  outb(CRTPORT, 15);
  pos |= inb(CRTPORT + 1);

	if (spm_switch) {
		crt[pos] = (crt[pos] & 0xff) | 0x0700;
		pos = spm_orig_pos;

		spm_orig_pos = 0;
		spm_switch = 0;
	}

	if (special_mode) {
		if (spm_orig_pos == -1) {
			spm_orig_pos = pos;
    }

    switch (c) {
      case 'w':
        sem_old_pos = pos;
        pos -= 80;
        if (pos < 0 || pos > 25 * 80 - 1) {
          pos = sem_old_pos;
          break;
        }

        crt[pos] = (crt[pos] & 0xff) | 0x7000;
        crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x0700;

        if (selection_mode) {
          crt[pos] = (crt[pos] & 0xff) | 0x0700;
          if (sem_old_pos > sem_anchor && pos < sem_anchor) { // Cross-anchor
            for (int i = sem_old_pos; i >= sem_anchor; i--) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            for (int i = sem_anchor - 1; i > pos; i--) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }

          if (sem_old_pos <= sem_anchor && pos < sem_anchor) { // Left side
            if (sem_old_pos == sem_anchor) {
              sem_old_pos--;
            }
            for (int i = sem_old_pos; i > pos; i--) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }

          if (sem_old_pos >= sem_anchor && pos >= sem_anchor) { // Right side
            for (int i = sem_old_pos; i > pos; i--) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            break;
          }
        }
        break;
	    case 'a':
        sem_old_pos = pos;
	      pos -= 1;
        if (pos < 0 || pos > 25 * 80 - 1) {
          pos = sem_old_pos;
          break;
        }

        crt[pos] = (crt[pos] & 0xff) | 0x7000;
        crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x0700;

        if (selection_mode) {
          crt[pos] = (crt[pos] & 0xff) | 0x0700;
          if (sem_old_pos > sem_anchor && pos < sem_anchor) { // Cross-anchor
            for (int i = sem_old_pos; i > sem_anchor; i--) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            for (int i = sem_anchor - 1; i > pos; i--) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }

          if (sem_old_pos < sem_anchor && pos < sem_anchor) { // Left side
            crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x7000;
            break;
          }

          if (sem_old_pos >= sem_anchor && pos > sem_anchor) { // Right side
            crt[pos] = (crt[pos] & 0xff) | 0x0700;
            break;
          }
        }
	      break;
	    case 's':
        sem_old_pos = pos;
        pos += 80;
        if (pos < 0 || pos > 25 * 80 - 1) {
          pos = sem_old_pos;
          break;
        }

        crt[pos] = (crt[pos] & 0xff) | 0x7000;
        crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x0700;

        if (selection_mode) {
          crt[pos] = (crt[pos] & 0xff) | 0x0700;
          if (sem_old_pos < sem_anchor && pos > sem_anchor) { // Cross-anchor
            for (int i = sem_old_pos; i < sem_anchor; i++) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            for (int i = sem_anchor; i < pos; i++) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }

          if (sem_old_pos <= sem_anchor && pos <= sem_anchor) { // Left side
            for (int i = sem_old_pos; i < pos; i++) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            break;
          }

          if (sem_old_pos >= sem_anchor && pos > sem_anchor) { // Right side
            for (int i = sem_old_pos; i < pos; i++) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }
        }
        break;
	    case 'd':
        sem_old_pos = pos;
        pos += 1;
        if (pos < 0 || pos > 25 * 80 - 1) {
          pos = sem_old_pos;
          break;
        }

        crt[pos] = (crt[pos] & 0xff) | 0x7000;
        crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x0700;

        if (selection_mode) {
          crt[pos] = (crt[pos] & 0xff) | 0x0700;
          if (sem_old_pos < sem_anchor && pos > sem_anchor) { // Cross-anchor
            for (int i = sem_old_pos; i < sem_anchor; i++) {
              crt[i] = (crt[i] & 0xff) | 0x0700;
            }
            for (int i = sem_anchor; i < pos; i++) {
              crt[i] = (crt[i] & 0xff) | 0x7000;
            }
            break;
          }

          if (sem_old_pos <= sem_anchor && pos < sem_anchor) { // Left side
            crt[pos] = (crt[pos] & 0xff) | 0x0700;
            break;
          }

          if (sem_old_pos >= sem_anchor && pos > sem_anchor) { // Right side
            crt[sem_old_pos] = (crt[sem_old_pos] & 0xff) | 0x7000;
            break;
          }
        }
        break;

	    case 'q':
        if (!selection_mode) {
          selection_mode = 1;
          sem_anchor = pos;
          crt[pos] = (crt[pos] & 0xff) | 0x0700;
        }
	      break;
	    case 'e':
        if (selection_mode) {
          selection_mode = 0;
          if (pos > sem_anchor) { // Anchor : Pos
            cgaputc_fill_string(sem_anchor, pos);
          } else if (pos < sem_anchor) { // Pos : Anchor
            cgaputc_fill_string(pos + 1, sem_anchor);
          }
          pos = sem_anchor;
        }
	      break;
	    default:
	      break;
    }
  }

  if (!special_mode) {
    if (c == '\n')
      pos += 80 - pos % 80;
    else if (c == BACKSPACE) {
      if (pos > 0)
        --pos;
    } else
      crt[pos++] = (c & 0xff) | 0x0700; // white on black
  }

  if ((pos < 0 || pos > 25 * 80) && !special_mode)
    panic("pos under/overflow");

  if ((pos / 80) >= 24 && !special_mode) { // Scroll up.
    memmove(crt, crt + 80, sizeof(crt[0]) * 23 * 80);
    pos -= 80;
    memset(crt + pos, 0, sizeof(crt[0]) * (24 * 80 - pos));
  }

  outb(CRTPORT, 14);
  outb(CRTPORT + 1, pos >> 8);
  outb(CRTPORT, 15);
  outb(CRTPORT + 1, pos);
	if (!special_mode) {
  	crt[pos] = ' ' | 0x0700;
	}
}

void consputc(int c) {
  if (panicked) {
    cli();
    for (;;)
    ;
  }

  if (c == BACKSPACE) {
    uartputc('\b');
    uartputc(' ');
    uartputc('\b');
  } else
    uartputc(c);
  cgaputc(c);
}

#define INPUT_BUF 128
struct {
  char buf[INPUT_BUF];
  uint r; // Read index
  uint w; // Write index
  uint e; // Edit index
} input;

#define C(x)((x) - '@') // Control-x
#define SPECIAL_CODE(x)(x - '%') // Shift-Alt-x

void consoleintr(int(*getc)(void)) {
  int c, doprocdump = 0;

  acquire(&cons.lock);
  while ((c = getc()) >= 0) {
    switch (c) {
    case C('P'): // Process listing.
      // procdump() locks cons.lock indirectly; invoke later
      doprocdump = 1;
      break;
    case C('U'): // Kill line.
      while (input.e != input.w &&
        input.buf[(input.e - 1) % INPUT_BUF] != '\n') {
        input.e--;
        consputc(BACKSPACE);
      }
      break;
    case C('H'):
    case '\x7f': // Backspace
      if (input.e != input.w) {
        input.e--;
        consputc(BACKSPACE);
      }
      break;

    case SPECIAL_CODE('C'):
      if (selection_mode) {
        break;
      }
      if (!special_mode) {
        special_mode = 1;
        spm_orig_pos = -1;
      } else if (special_mode && spm_orig_pos == -1) {
        special_mode = 0;
        spm_orig_pos |= inb(CRTPORT + 1);
      } else {
        special_mode = 0;
        spm_switch = 1;
      }
      break;
    case SPECIAL_CODE('P'):
      if (!special_mode) {
        for (int i = 0; copied_string[i] != 0 && (input.e % INPUT_BUF) != INPUT_BUF - 1; i++) {
          input.buf[input.e++ % INPUT_BUF] = copied_string[i];
          consputc(copied_string[i]);
        }
      }
      break;

    default:
      if (c != 0 && input.e - input.r < INPUT_BUF) {
        c = (c == '\r') ? '\n' : c;
        if (!special_mode) {
          input.buf[input.e++ % INPUT_BUF] = c;
        }
        consputc(c);
        if (c == '\n' || c == C('D') || input.e == input.r + INPUT_BUF) {
          input.w = input.e;
          wakeup(&input.r);
        }
      }
      break;
    }
  }
  release(&cons.lock);
  if (doprocdump) {
    procdump(); // now call procdump() wo. cons.lock held
  }
}

int consoleread(struct inode *ip, char *dst, int n) {
  uint target;
  int c;

  iunlock(ip);
  target = n;
  acquire(&cons.lock);
  while (n > 0) {
    while (input.r == input.w) {
      if (myproc() -> killed) {
        release(&cons.lock);
        ilock(ip);
        return -1;
      }
      sleep(&input.r, &cons.lock);
    }
    c = input.buf[input.r++ % INPUT_BUF];
    if (c == C('D')) { // EOF
      if (n < target) {
        // Save ^D for next time, to make sure
        // caller gets a 0-byte result.
        input.r--;
      }
      break;
    }
    *dst++ = c;
    --n;
    if (c == '\n')
      break;
  }
  release(&cons.lock);
  ilock(ip);

  return target - n;
}

int consolewrite(struct inode *ip, char *buf, int n) {
  int i;

  iunlock(ip);
  acquire(&cons.lock);
  for (i = 0; i < n; i++)
    consputc(buf[i] &0xff);
  release(&cons.lock);
  ilock(ip);

  return n;
}

void consoleinit(void) {
  initlock(&cons.lock, "console");

  devsw[CONSOLE].write = consolewrite;
  devsw[CONSOLE].read = consoleread;
  cons.locking = 1;

  ioapicenable(IRQ_KBD, 0);
}
