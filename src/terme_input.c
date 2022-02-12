#include "character_check.h"
#include "terme_arch.h"
#include "terme_input.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE		3
#else
#define TERME_INPUT_SIZE		4096
#endif

#define TERME_INPUT_ENCODE		64
#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_UNREAD		8

/* buffer */
static byte terme_input_buffer[TERME_INPUT_SIZE];
static size_t terme_input_size;
static size_t terme_input_now;
/* sequence */
static byte terme_input_unbyte[TERME_INPUT_UNBYTE];
static int terme_input_unbyte_size;
static int terme_input_unbyte_now;

void terme_input_init(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
}

#define TERME_CLEAR_INPUT_STDIN		4096
static int terme_input_clear_stdin(void)
{
	byte data[TERME_CLEAR_INPUT_STDIN];
	int check;
	size_t ignore;

	for (;;) {
		check = terme_arch_select(&check);
		if (check < 0)
			continue;
		if (! check)
			break;

		check = terme_arch_read(data, TERME_CLEAR_INPUT_STDIN, &ignore);
		if (check < 0)
			continue;
		if (check)
			return 1;
	}

	return 0;
}

int terme_input_clear(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
	return terme_input_clear_stdin();
}


/*
 *  unbyte
 */
static void terme_unbyte_set(byte *data, int size)
{
	Check(TERME_INPUT_UNBYTE <= size, "size error");
	memcpy(terme_input_unbyte, data, (size_t)size);
	terme_input_unbyte_size = size;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_clear(void)
{
	terme_input_unbyte_size = 0;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_value(byte c)
{
	terme_input_unbyte[0] = c;
	terme_input_unbyte_size = 1;
	terme_input_unbyte_now = 0;
}

static void terme_unbyte_pop(byte *value, int *ret)
{
	if (terme_input_unbyte_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unbyte[terme_input_unbyte_now++];
	if (terme_input_unbyte_size <= terme_input_unbyte_now) {
		terme_input_unbyte_size = 0;
		terme_input_unbyte_now = 0;
	}
	*ret = 1;
}

static int terme_getc_buffering(void)
{
	int check;
	size_t size;

	terme_input_now = 0;
	check = terme_arch_read(terme_input_buffer, TERME_INPUT_SIZE, &size);
	if (check < 0) {
		terme_input_size = 0;
		return -1;
	}
	if (check) {
		terme_input_size = 0;
		return 1;
	}
	else {
		terme_input_size = size;
		return 0;
	}
}

static int terme_getc_blocking(int blocking, byte *value, int *ret)
{
	int check, readp;

	/* unbyte */
	terme_unbyte_pop(value, &check);
	if (check) {
		*ret = 1;
		return 0;
	}

	/* read */
retry:
	if (terme_input_now < terme_input_size) {
		*ret = 1;
		*value = terme_input_buffer[terme_input_now];
		terme_input_now++;
		return 0;
	}

	/* non-blocking */
	if (! blocking) {
		check = terme_arch_select(&readp);
		if (check < 0)
			return -1;
		if (readp == 0) {
			*ret = 0;
			return 0;
		}
	}

	/* buffering */
	check = terme_getc_buffering();
	if (! check)
		goto retry;

	return check;
}


/*
 *  table
 */
#define terme_size_escape	64
#define terme_size_utf8		8
#define terme_table_utf16(x) (0xD800 <= (x) && (x) < 0xE000)

#define terme_table_getc(size_array) { \
	check = terme_getc_blocking(blocking, &c, &hang); \
	if (check < 0) goto signal; \
	if (check) goto error; \
	if (hang == 0) goto hang; \
	Check(size_array <= i, "size error"); \
	data[i++] = c; \
}


/*  Up       ^[OA
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
static void terme_table_escape(int blocking, TermeKeyboard *ret)
{
	byte data[terme_size_escape], c;
	int i, check, hang;

	i = 0;
	c = 0;
	hang = 0;

	terme_table_getc(terme_size_escape);
	if (c == 0x4F)
		goto third_4F;
	if (c == 0x5B)
		goto third_5B;
	goto invalid;

third_4F:
	terme_table_getc(terme_size_escape);
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto invalid;

third_5B:
	terme_table_getc(terme_size_escape);
	if (c == 0x31)
		goto forth_31;
	if (c == 0x41)
		goto escape_up;
	if (c == 0x42)
		goto escape_down;
	if (c == 0x43)
		goto escape_right;
	if (c == 0x44)
		goto escape_left;
	goto invalid;

forth_31:
	terme_table_getc(terme_size_escape);
	if (0x31 <= c && c <= 0x39) { /* F1 - F9 */
		terme_table_getc(terme_size_escape);
		if (c == 0x7E) /* \E[[11~: F1 */
			goto function1;
	}
	goto invalid;

escape_up: /* 0x1B 0x5B 0x41 */
	ret->type = terme_escape_up;
	goto finish;

escape_down: /* 0x1B 0x5B 0x42 */
	ret->type = terme_escape_down;
	goto finish;

escape_right: /* 0x1B 0x5B 0x43 */
	ret->type = terme_escape_right;
	goto finish;

escape_left: /* 0x1B 0x5B 0x44 */
	ret->type = terme_escape_left;
	goto finish;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	goto finish;

function1:
	ret->type = terme_escape_function;
	ret->c = (c - 0x31) + 1; /* F1 -> 1 */
	goto finish;

invalid:
	terme_unbyte_value(c);
	return;

finish:
	terme_unbyte_clear();
	return;

signal:
	terme_unbyte_set(data, i);
	ret->type = terme_escape_signal;
	return;

error:
	terme_unbyte_set(data, i);
	ret->type = terme_escape_error;
	return;

hang:
	terme_unbyte_set(data, i);
	ret->type = terme_escape_hang;
	return;
}

static int terme_table_utf8(int blocking, unicode *value, int *ret)
{
	byte data[terme_size_utf8], c;
	int i, check, hang;
	unicode merge;

	i = 0;
	c = 0;
	hang = 0;
	terme_table_getc(terme_size_utf8);

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
	merge = (unicode)c;
	goto normal;

sequence2:
	merge = (0x1F & c) << 6;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x80)
		goto invalid;
	goto normal;

sequence3:
	merge = (0x0F & c) << 12;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x0800)
		goto invalid;
	if (terme_table_utf16(merge))
		goto invalid;
	goto normal;

sequence4:
	merge = (0x07 & c) << 18;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 12;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc(terme_size_utf8);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x010000)
		goto invalid;
	if (UnicodeCount <= merge)
		goto size_error;
	goto normal;

normal:
	terme_unbyte_clear();
	*value = merge;
	*ret = 1;
	return 0;

invalid:
	terme_unbyte_value(c);
	return 1;

size_error:
	terme_unbyte_clear();
	return 1;

signal:
	terme_unbyte_set(data, i);
	return -1;

hang:
	terme_unbyte_set(data, i);
	*ret = 0;
	return 0;

error:
	terme_unbyte_set(data, i);
	return 1;
}

void terme_input_event(int blocking, TermeKeyboard *ret)
{
	int check, readp;
	unicode c;

	terme_input_unbyte_now = 0;
	check = terme_table_utf8(blocking, &c, &readp);
	if (check < 0) {
		ret->type = terme_escape_signal;
		ret->c = 0;
		return;
	}
	if (check) {
		ret->type = terme_escape_error;
		ret->c = 0;
		return;
	}
	if (! readp) {
		ret->type = terme_escape_hang;
		ret->c = 0;
		return;
	}
	if (c != 0x1B) {
		ret->type = terme_escape_code;
		ret->c = c;
		return;
	}

	/* escape sequence */
	terme_input_unbyte_now = 0;
	terme_table_escape(blocking, ret);
}

