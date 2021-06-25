#include <stdio.h>
#include <unistd.h>
#include <sys/select.h>
#include "character_check.h"
#include "terme_input.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE	3
#else
#define TERME_INPUT_SIZE	4096
#endif

#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_UNREAD		8

/* buffer */
static byte terme_input_buffer[TERME_INPUT_SIZE];
static size_t terme_input_size;
static size_t terme_input_now;
/* unicode */
static byte terme_input_unbyte[TERME_INPUT_UNBYTE];
static int terme_input_unbyte_size;
static int terme_input_unbyte_base;
static int terme_input_unbyte_now;
/* unread */
static unicode terme_input_unread[TERME_INPUT_UNREAD];
static int terme_input_unread_size;
static int terme_input_unread_base;
static int terme_input_unread_now;

void terme_input_init(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
}

static int terme_input_select(int *ret)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = 0;
	tm.tv_usec = 0;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti < 0) {
		*ret = 0;
		return 1; /* error */
	}
	if (reti == 0) {
		/* empty */
		*ret = 0;
		return 0;
	}
	else {
		/* can read */
		*ret = 1;
		return 0;
	}
}

#define TERME_CLEAR_INPUT_STDIN		4096
static int terme_clear_input_stdin(void)
{
	byte data[TERME_CLEAR_INPUT_STDIN];
	int check;
	ssize_t rets;

	for (;;) {
		if (terme_input_select(&check))
			return 1;
		if (! check)
			break;
		rets = read(STDIN_FILENO, data, TERME_CLEAR_INPUT_STDIN);
		if (rets < 0)
			return 1;
	}

	return 0;
}

int terme_clear_input(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
	return terme_clear_input_stdin();
}


/*
 *  unbyte
 */
static int terme_unbyte_push(byte c)
{
	if (TERME_INPUT_UNBYTE <= terme_input_unbyte_size)
		return 1; /* error */

	terme_input_unbyte_size++;
	terme_input_unbyte[terme_input_unbyte_base] = c;
	terme_input_unbyte_base++;
	terme_input_unbyte_base %= TERME_INPUT_UNBYTE;

	return 0;
}

static void terme_unbyte_pop(byte *value, int *ret)
{
	if (terme_input_unbyte_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unbyte[terme_input_unbyte_now];
	terme_input_unbyte_size--;
	if (terme_input_unbyte_size) {
		terme_input_unbyte_now++;
		terme_input_unbyte_now %= TERME_INPUT_UNBYTE;
	}
	else {
		terme_input_unbyte_base = 0;
		terme_input_unbyte_now = 0;
	}
	*ret = 1;
}


/*
 *  getc
 */
static int terme_input_wait(void)
{
	int fd, reti;
	fd_set fdset;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	reti = select(fd + 1, &fdset, NULL, NULL, NULL);
	return reti < 0;
}

static int terme_getc_buffering(void)
{
	ssize_t size;

	terme_input_size = 0; /* for error */
	terme_input_now = 0;
	size = read(STDIN_FILENO, terme_input_buffer, TERME_INPUT_SIZE);
	if (size < 0)
		return 1;
	terme_input_size = (size_t)size;

	return 0;
}

static int terme_getc_hang(byte *value, int *ret)
{
	byte c;
	int check;

	/* unbyte */
	terme_unbyte_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* input buffer */
	if (terme_input_size <= terme_input_now) {
		if (terme_input_select(&check))
			goto error;
		if (! check) { /* empty */
			*value = 0;
			*ret = 0;
			return 0;
		}
		if (terme_getc_buffering())
			goto error;
	}
	*ret = 1;
	*value = terme_input_buffer[terme_input_now];
	terme_input_now++;
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}

static int terme_getc(byte *ret)
{
	byte c;
	int check;

	for (;;) {
		if (terme_getc_hang(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
		return 0;
	}
	*ret = c;
	return 0;

error:
	*ret = 0;
	return 1;
}


/*
 *  unread-char
 */
int terme_unread_char(unicode c)
{
	if (TERME_INPUT_UNREAD <= terme_input_unread_size)
		return 1; /* error */

	terme_input_unread_size++;
	terme_input_unread[terme_input_unread_base] = c;
	terme_input_unread_base++;
	terme_input_unread_base %= TERME_INPUT_UNREAD;

	return 0;
}

static void terme_unread_pop(unicode *value, int *ret)
{
	if (terme_input_unread_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unread[terme_input_unread_now];
	terme_input_unread_size--;
	if (terme_input_unread_size) {
		terme_input_unread_now++;
		terme_input_unread_now %= TERME_INPUT_UNREAD;
	}
	else {
		terme_input_unread_base = 0;
		terme_input_unread_now = 0;
	}
	*ret = 1;
}


/*
 *  read-char
 */
static int terme_listen_rollback(byte *data, int size)
{
	while (size) {
		size--;
		if (terme_unbyte_push(data[size]))
			return 1;
	}

	return 0;
}

#define terme_listen_utf16(x) (0xD800 <= (x) && (x) < 0xE000)

#define terme_listen_getc() { \
	if (terme_getc_hang(&c, &check)) goto error; \
	if (! check) goto hang; \
	if (c == 0x1B) goto escape; \
	data[i++] = c; \
}
static int terme_listen_unicode(void)
{
	byte data[8], c;
	int i, check;
	unicode value;

	i = 0;
	terme_listen_getc();

	/* encode */
	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	goto invalid;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x80)
		goto invalid;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto invalid;
	if (terme_listen_utf16(value))
		goto invalid;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto invalid;
	if (UnicodeCount <= value)
		goto invalid;
	goto normal;

normal:
	if (terme_unread_char(value))
		goto error;
	return 0;

invalid:
	return 1;

error:
	(void)terme_listen_rollback(data, i);
	return 1;

hang:
	return terme_listen_rollback(data, i);

escape:
	if (terme_listen_rollback(data, i))
		return 1;
	if (terme_unread_char(0x1B))
		goto error;
	return 0;
}

int terme_listen(int *ret)
{
	/* unread */
	if (terme_input_unread_size) {
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	*ret = (terme_input_unread_size != 0);
	return 0;
}

int terme_hang_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	/* unread */
	terme_unread_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*value = 0;
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	terme_unread_pop(value, ret);
	return 0;
}

int terme_read_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	for (;;) {
		if (terme_hang_char(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
	}
	*value = c;
	*ret = 0; /* normal */
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}


/*
 *  read-keyboard
 *
 *  Up       ^[OA
 *  Down     ^[OB
 *  Right    ^[OC
 *  Left     ^[OD
 *  PF1      ^[OP
 *  PF2      ^[OQ
 *  PF3      ^[OR
 *  PF4      ^[OS
 *  F1       ^[[11~  ^[OP
 *  F2       ^[[12~  ^[OQ
 *  F3       ^[[13~  ^[OR
 *  F4       ^[[14~  ^[OS
 *  F5       ^[[15~  ^[Ot
 *  F6       ^[[17~  ^[Ou
 *  F7       ^[[18~  ^[Ov
 *  F8       ^[[19~  ^[Ol
 *  F9       ^[[20~  ^[Ow
 *  F10      ^[[21~  ^[Ox
 *  F11      ^[[23~
 *  F12      ^[[24~
 */
static void terme_read_escape(TermeKeyboard *ret)
{
	byte c;

	if (terme_getc(&c))
		goto error;

	if (c == 0x4F)
		goto third_4F;
	if (c == 0x5B)
		goto third_5B;
	goto error;

third_4F:
	if (terme_getc(&c))
		goto error;
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto error;

third_5B:
	if (terme_getc(&c))
		goto error;
	if (c == 0x31)
		goto forth_31;
	if (c == 0x43)
		goto escape_right;
	if (c == 0x44)
		goto escape_left;
	goto error;

forth_31:
	if (terme_getc(&c))
		goto error;
	if (0x31 <= c && c <= 0x39) { /* F1 - F9 */
		if (terme_getc(&c))
			goto error;
		if (c == 0x7E) /* \E[[11~: F1 */
			goto function1;
	}
	goto error;

escape_right: /* 0x1B 0x5B 0x43 */
	ret->type = terme_escape_right;
	return;

escape_left: /* 0x1B 0x5B 0x44 */
	ret->type = terme_escape_left;
	return;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	return;

function1:
	ret->type = terme_escape_function;
	ret->c = (c - 0x31) + 1; /* F1 -> 1 */
	return;

error:
	ret->type = terme_escape_error;
	return;
}

int terme_read_keyboard(TermeKeyboard *ret)
{
	int check;
	unicode c;

	if (terme_read_char(&c, &check))
		return 1;
	if (c != 0x1B) {
		ret->type = terme_escape_code;
		ret->c = c;
		return 0;
	}

	/* escape sequence */
	terme_read_escape(ret);
	return 0;
}

