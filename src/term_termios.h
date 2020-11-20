#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include "build.h"
#include "character_check.h"
#include "character_queue.h"
#include "constant.h"
#include "encode.h"
#include "memory.h"
#include "symbol.h"
#include "term.h"

enum term_unicode {
	term_unicode_error,
	term_unicode_code,
	term_unicode_up,
	term_unicode_down,
	term_unicode_left,
	term_unicode_right,
	term_unicode_function,
	term_unicode_size
};
struct keyboard_input {
	enum term_unicode type;
	unicode c;
};


/*
 *  initialize
 */
static int term_initialize = 0;
static struct termios term_rollback;
static struct termios term_initial;
static byte term_buffer[8];
static size_t term_buffer_size;

static void term_signal(int sig, siginfo_t *info, void *voidp)
{
}

static int begin_term_signal(void)
{
	struct sigaction act;

	memset(&act, 0, sizeof(act));
	act.sa_sigaction = term_signal;
	act.sa_flags = SA_SIGINFO;
	if (sigaction(SIGWINCH, &act, NULL) == -1) {
		Debug("sigaction error.");
		return 1;
	}

	return 0;
}

static int begin_term_rawmode(void)
{
	int fd;
	struct termios v;

	fd = STDIN_FILENO;
	v = term_rollback;
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	if (tcsetattr(fd, TCSANOW, &v) == -1)
		return 1;
	term_initial = v;

	return 0;
}

int begin_term(int ignore)
{
	term_enable = 1;

	/* termios */
	if (tcgetattr(STDIN_FILENO, &term_rollback)) {
		Debug("begin_term tcgetattr error");
		return 1;
	}

	/* signal */
	if (begin_term_signal()) {
		Debug("begin_term_signal error.");
		return 1;
	}

	/* raw-mode */
	if (begin_term_rawmode()) {
		(void)tcsetattr(STDIN_FILENO, TCSAFLUSH, &term_rollback);
		Debug("begin_term_rawmode error.");
		return 1;
	}

	/* begin */
	term_initialize = 1;
	return 0;
}

int end_term(void)
{
	if (term_initialize == 0)
		return 0;

	/* termios */
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &term_rollback) == -1) {
		Debug("end_term tcsetattr error");
		return 1;
	}

	/* end */
	term_initialize = 0;
	return 0;
}


/*
 *  getc
 */
static int term_read_byte(byte *ret)
{
	ssize_t size;

	/* ungetc */
	if (term_buffer_size) {
		*ret = term_buffer[0];
		term_buffer_size--;
		if (term_buffer_size)
			memmove(term_buffer, term_buffer + 1, term_buffer_size);
		return 0;
	}

	/* read */
	size = read(STDIN_FILENO, ret, 1);
	if (size == 1)
		return 0;

	/* error */
	return 1;
}

static int term_input_unicode(unicode *ret)
{
	byte c;
	unicode value;

	if (term_read_byte(&c))
		goto error;
	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	goto error;

sequence1:
	*ret = (unicode)c;
	return 0;

sequence2:
	value = (0x1F & c) << 6;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= 0x3F & c;
	if (value < 0x80)
		goto error;
	*ret = value;
	return 0;

sequence3:
	value = (0x0F & c) << 12;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= (0x3F & c) << 6;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto error;
	if (UTF16range(value))
		goto error;
	*ret = value;
	return 0;

sequence4:
	value = (0x07 & c) << 18;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= (0x3F & c) << 12;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= (0x3F & c) << 6;
	if (term_read_byte(&c))
		goto error;
	if (c < 0x80 || 0xBF < c)
		goto error;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto error;
	if (UnicodeCount <= value)
		goto error;
	*ret = value;
	return 0;

error:
	*ret = 0;
	return 1;
}

/*
 *  Up       ^[OA
 *  Down     ^[OB
 *  Right    ^[OC
 *  Left     ^[OD
 *  PF1      ^[OP
 *  PF2      ^[OQ
 *  PF3      ^[OR
 *  PF4      ^[OS
 *  F1       ^[[11~
 *  F2       ^[[12~
 *  F3       ^[[13~
 *  F4       ^[[14~
 *  F5       ^[[15~
 *  F6       ^[[17~
 *  F7       ^[[18~
 *  F8       ^[[19~
 *  F9       ^[[20~
 *  F10      ^[[21~
 *  F11      ^[[23~
 *  F12      ^[[24~
 */
static void term_input_escape(struct keyboard_input *ret)
{
	byte c;

	if (term_read_byte(&c))
		goto error;

	if (c == 0x4F)
		goto third_4F;
	if (c == 0x5B)
		goto third_5B;
	goto error;

third_4F:
	if (term_read_byte(&c))
		goto error;
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto error;

third_5B:
	if (term_read_byte(&c))
		goto error;
	if (c == 0x31)
		goto forth_31;
	if (c == 0x43)
		goto escape_right;
	if (c == 0x44)
		goto escape_left;
	goto error;

forth_31:
	if (term_read_byte(&c))
		goto error;
	if (0x31 <= c && c <= 0x39) { /* F1 - F9 */
		if (term_read_byte(&c))
			goto error;
		if (c == 0x7E) /* \E[[11~: F1 */
			goto function1;
	}
	goto error;

escape_right: /* 0x1B 0x5B 0x43 */
	ret->type = term_unicode_right;
	return;

escape_left: /* 0x1B 0x5B 0x44 */
	ret->type = term_unicode_left;
	return;

program:
	ret->type = term_unicode_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	return;

function1:
	ret->type = term_unicode_function;
	ret->c = (c - 0x31) + 1; /* F1 -> 1 */
	return;

error:
	ret->type = term_unicode_error;
	return;
}

static void term_getc(struct keyboard_input *ret)
{
	unicode c;

	/* error */
	if (term_input_unicode(&c)) {
		ret->type = term_unicode_error;
		return;
	}

	/* escape sequence */
	if (c == 0x1B) {
		term_input_escape(ret);
		return;
	}

	ret->type = term_unicode_code;
	ret->c = c;
}

void term_clear_input(void)
{
	term_buffer_size = 0;
}


/*
 *  prompt
 */
int prompt_term_(Execute ptr, addr pos)
{
	LocalRoot local;
	const char *body;
	size_t size;

	local = ptr->local;
	Return(UTF8_buffer_clang_(local, &pos, pos));
	posbody(pos, (addr *)&body);
	size = strlen(body);
	(void)write(STDOUT_FILENO, (const void *)body, size);

	return 0;
}

int readline_term_(Execute ptr, addr *ret)
{
	addr queue;
	struct keyboard_input input;
	unicode c;

	GetConst(SYSTEM_TERM_QUEUE, &queue);
	Return(getspecialcheck_local_(ptr, queue, &queue));

	for (;;) {
		term_getc(&input);
		if (input.type != term_unicode_code)
			continue;
		c = input.c;
		Return(push_charqueue_heap_(queue, c));
		if (c == 0x0A)
			break;
	}
	make_charqueue_heap(queue, ret);
	clear_charqueue(queue);

	return 0;
}

