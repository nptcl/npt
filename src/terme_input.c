#include "constant.h"
#include "character_check.h"
#include "terme_arch.h"
#include "terme_input.h"
#include "typedef.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE		3
#else
#define TERME_INPUT_SIZE		4096
#endif
#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_WAIT		0.01

enum terme_blocking_type {
	terme_blocking_infinite,
	terme_blocking_integer,
	terme_blocking_float
};

union terme_blocking_value {
	int integer_value;
	double float_value;
};

struct terme_blocking {
	enum terme_blocking_type type;
	union terme_blocking_value wait;
};

typedef struct terme_blocking TermeBlocking;

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

static int terme_getc_wait(TermeBlocking *blocking, int *ret)
{
	switch (blocking->type) {
		case terme_blocking_integer:
			return terme_arch_wait_integer(ret, blocking->wait.integer_value);

		case terme_blocking_float:
			return terme_arch_wait_float(ret, blocking->wait.float_value);

		case terme_blocking_infinite:
		default:
			*ret = 1;
			return 0;
	}
}

static int terme_getc_blocking(TermeBlocking *blocking, byte *value, int *ret)
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

	/* blocking */
	check = terme_getc_wait(blocking, &readp);
	if (check < 0)
		return -1;
	if (readp == 0) {
		*ret = 0;
		return 0;
	}

	/* buffering */
	check = terme_getc_buffering();
	if (! check)
		goto retry;

	return check;
}

static int terme_getc_escape(byte *value, int *ret)
{
	TermeBlocking wait;

	wait.type = terme_blocking_float;
	wait.wait.float_value = TERME_INPUT_WAIT;
	return terme_getc_blocking(&wait, value, ret);
}


/*
 *  table
 */
#define TERME_INPUT_BUFFER	8
#define TERME_INPUT_ESCAPE	32

struct terme_input_data {
	unsigned error : 1;
	unsigned signal : 1;
	unsigned readp : 1;
	int16_t escape[TERME_INPUT_ESCAPE];
	byte data[TERME_INPUT_BUFFER];
	byte c;
	unsigned index, escape_size;
};
typedef struct terme_input_data TermeInputData;

/*  0x4F 'O'
 *    Up         ^[OA
 *    Down       ^[OB
 *    Right      ^[OC
 *    Left       ^[OD
 *    PF1        ^[OP
 *    PF2        ^[OQ
 *    PF3        ^[OR
 *    PF4        ^[OS
 *  0x5B '[':  \[[value1;value2;value3...X
 *    F1         ^[[11~  ^[OP
 *    F2         ^[[12~  ^[OQ
 *    F3         ^[[13~  ^[OR
 *    F4         ^[[14~  ^[OS
 *    F5         ^[[15~  ^[Ot
 *    F6         ^[[17~  ^[Ou
 *    F7         ^[[18~  ^[Ov
 *    F8         ^[[19~  ^[Ol
 *    F9         ^[[20~  ^[Ow
 *    F10        ^[[21~  ^[Ox
 *    F11        ^[[23~
 *    F12        ^[[24~
 *    Home       ^[[1~
 *    Insert     ^[[2~
 *    End        ^[[4~
 *    Page Up    ^[[5~
 *    Page Down  ^[[6~
 */
static void terme_input_getc2_escape(TermeInputData *str)
{
	byte c;
	int check, readp;

	check = terme_getc_escape(&c, &readp);
	if (check < 0) {
		str->signal = 1;
		return;
	}
	if (check) {
		str->error = 1;
		return;
	}
	if (readp == 0) {
		str->readp = 1;
		return;
	}
	str->c = c;
}

static void terme_input_getc2(TermeBlocking *blocking, TermeInputData *str)
{
	byte c;
	int check, readp;

	check = terme_getc_blocking(blocking, &c, &readp);
	if (check < 0) {
		str->signal = 1;
		return;
	}
	if (check) {
		str->error = 1;
		return;
	}
	if (readp == 0) {
		str->readp = 1;
		return;
	}
	Check(TERME_INPUT_BUFFER <= str->index, "size error");
	str->data[str->index++] = c;
	str->c = c;
}

#define terme_table_getc2(blocking, str, c) { \
	terme_input_getc2(blocking, &str); \
	if (str.signal) goto signal; \
	if (str.error) goto error; \
	if (str.readp) goto hang; \
	c = str.c; \
}

static void terme_table_setbyte1(TermeKeyboard *ret,
		TermeInputData *str,
		TermeEscape type)
{
	int16_t value;

	value = str->escape[0];
	if (value < 0)
		value = 1;
	ret->c = (unicode)value;
	ret->type = type;
}

static void terme_table_escape(TermeBlocking *blocking, TermeKeyboard *ret)
{
	byte c;
	char data[16];
	int16_t type;
	int check, datai;
	TermeInputData str;

	str.signal = 0;
	str.error = 0;
	str.readp = 0;
	str.index = 0;
	str.escape_size = 0;
	datai = 0;

	terme_arch_escape_begin();
	terme_input_getc2_escape(&str);
	if (str.signal)
		goto signal;
	if (str.error)
		goto error;
	if (str.readp) {
		terme_arch_escape_end(&check);
		if (check)
			goto escape0;
		terme_table_getc2(blocking, str, c);
	}
	else {
		c = str.c;
		terme_arch_escape_end(&check);
		if (check)
			goto escape1;
		Check(TERME_INPUT_BUFFER <= str.index, "size error");
		str.data[str.index++] = c;
	}
	if (c == 0x4F) /* O */
		goto third_4F;
	if (c == 0x5B) /* [ */
		goto third_5B;
	goto escape1;

third_4F: /* O */
	terme_table_getc2(blocking, str, c);
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto invalid;

third_5B: /* [ */
	if (8 <= datai)
		goto parse_error;
	if (TERME_INPUT_ESCAPE <= str.escape_size)
		goto parse_error;

	/* read char */
	terme_table_getc2(blocking, str, c);

	/* digit */
	if (isdigit(c)) {
		data[datai++] = c;
		goto third_5B;
	}

	/* not digit */
	if (datai == 0) {
		check = -1;
	}
	else {
		data[datai++] = 0;
		check = atoi(data);
		datai = 0;
		if (check < 0 || 0xFF <= check)
			goto parse_error;
	}
	str.escape[str.escape_size++] = (int16_t)check;

	/* separator */
	if (c == ';' || c == ':')
		goto third_5B;

	/* operator */
	if (c == 0x7E) /* ~ */
		goto forth_7E;
	if (c == 0x41) /* A */
		goto escape_up;
	if (c == 0x42) /* B */
		goto escape_down;
	if (c == 0x43) /* C */
		goto escape_right;
	if (c == 0x44) /* D */
		goto escape_left;
	goto invalid;

forth_7E:
	type = str.escape[0];
	if (type < 0)
		type = 0;
	if (type == 1)
		goto escape_home;
	if (type == 2)
		goto escape_insert;
	if (type == 4)
		goto escape_end;
	if (type == 5)
		goto escape_page_up;
	if (type == 6)
		goto escape_page_down;
	if (11 <= type && type <= 19)
		goto function1;
	goto invalid;

escape_up:
	terme_table_setbyte1(ret, &str, terme_escape_up);
	goto finish;

escape_down:
	terme_table_setbyte1(ret, &str, terme_escape_down);
	goto finish;

escape_right:
	terme_table_setbyte1(ret, &str, terme_escape_right);
	goto finish;

escape_left:
	terme_table_setbyte1(ret, &str, terme_escape_left);
	goto finish;

escape_home:
	ret->type = terme_escape_home;
	goto finish;

escape_insert:
	ret->type = terme_escape_insert;
	goto finish;

escape_end:
	ret->type = terme_escape_end;
	goto finish;

escape_page_up:
	ret->type = terme_escape_page_up;
	goto finish;

escape_page_down:
	ret->type = terme_escape_page_down;
	goto finish;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	goto finish;

function1:
	ret->type = terme_escape_function;
	ret->c = (type - 10); /* F1 -> 1 */
	goto finish;

escape0:
	ret->type = terme_escape_escape;
	terme_unbyte_clear();
	return;

escape1:
	ret->type = terme_escape_escape;
	terme_unbyte_clear();
	terme_unbyte_value(c);
	return;

invalid:
	terme_unbyte_clear();
	ret->type = terme_escape_error;
	return;

finish:
	terme_unbyte_clear();
	return;

signal:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_signal;
	return;

error:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_error;
	return;

hang:
	terme_unbyte_set(str.data, str.index);
	ret->type = terme_escape_hang;
	return;

parse_error:
	terme_unbyte_clear();
	return;
}


/*
 *  UTF-8 table
 */
static void terme_input_getc1(TermeBlocking *blocking, TermeInputData *str)
{
	byte c;
	int check, readp;

	check = terme_getc_blocking(blocking, &c, &readp);
	if (check < 0) {
		str->signal = 1;
		return;
	}
	if (check) {
		str->error = 1;
		return;
	}
	if (readp == 0) {
		str->readp = 1;
		return;
	}
	Check(TERME_INPUT_BUFFER <= str->index, "size error");
	str->data[str->index++] = c;
	str->c = c;
}

#define terme_table_getc1(blocking, str, c) { \
	terme_input_getc1(blocking, &str); \
	if (str.signal) goto signal; \
	if (str.error) goto error; \
	if (str.readp) goto hang; \
	c = str.c; \
}

static int terme_table_utf8(TermeBlocking *blocking, unicode *value, int *ret)
{
	byte c;
	unicode merge;
	TermeInputData str;

	str.signal = 0;
	str.error = 0;
	str.readp = 0;
	str.index = 0;

	terme_table_getc1(blocking, str, c);
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
	terme_table_getc1(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x80)
		goto invalid;
	goto normal;

sequence3:
	merge = (0x0F & c) << 12;
	terme_table_getc1(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc1(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= 0x3F & c;
	if (merge < 0x0800)
		goto invalid;
	if (isSurrogatePair(merge))
		goto invalid;
	goto normal;

sequence4:
	merge = (0x07 & c) << 18;
	terme_table_getc1(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 12;
	terme_table_getc1(blocking, str, c);
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	merge |= (0x3F & c) << 6;
	terme_table_getc1(blocking, str, c);
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
	terme_unbyte_set(str.data, str.index);
	return -1;

hang:
	terme_unbyte_set(str.data, str.index);
	*ret = 0;
	return 0;

error:
	terme_unbyte_set(str.data, str.index);
	return 1;
}

static void terme_table_wait(TermeBlocking *blocking, TermeKeyboard *ret)
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

void terme_table_infinite(TermeKeyboard *ret)
{
	TermeBlocking blocking;
	blocking.type = terme_blocking_infinite;
	terme_table_wait(&blocking, ret);
}


/*
 *  values
 */
static void terme_input_value(TermeBlocking *blocking, addr *rtype, addr *rvalue)
{
	TermeKeyboard v;

	terme_table_wait(blocking, &v);
	*rvalue = Nil;
	switch (v.type) {
		case terme_escape_hang:
			GetConst(SYSTEM_TERME_HANG, rtype);
			break;

		case terme_escape_code:
			GetConst(SYSTEM_TERME_CODE, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_up:
			GetConst(SYSTEM_TERME_UP, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_down:
			GetConst(SYSTEM_TERME_DOWN, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_left:
			GetConst(SYSTEM_TERME_LEFT, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_right:
			GetConst(SYSTEM_TERME_RIGHT, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_page_up:
			GetConst(SYSTEM_TERME_PAGE_UP, rtype);
			break;

		case terme_escape_page_down:
			GetConst(SYSTEM_TERME_PAGE_DOWN, rtype);
			break;

		case terme_escape_home:
			GetConst(SYSTEM_TERME_HOME, rtype);
			break;

		case terme_escape_end:
			GetConst(SYSTEM_TERME_END, rtype);
			break;

		case terme_escape_insert:
			GetConst(SYSTEM_TERME_INSERT, rtype);
			break;

		case terme_escape_function:
			GetConst(SYSTEM_TERME_FUNCTION, rtype);
			fixnum_heap(rvalue, (fixnum)v.c);
			break;

		case terme_escape_signal:
			GetConst(SYSTEM_TERME_SIGNAL, rtype);
			break;

		case terme_escape_escape:
			GetConst(SYSTEM_TERME_ESCAPE, rtype);
			break;

		default:
			*rtype = Nil;
			break;
	}
}

void terme_input_infinite(addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;
	blocking.type = terme_blocking_infinite;
	terme_input_value(&blocking, rtype, rvalue);
}

void terme_input_integer(int wait, addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;

	if (wait < 0)
		wait = 0;
	blocking.type = terme_blocking_integer;
	blocking.wait.integer_value = wait;
	terme_input_value(&blocking, rtype, rvalue);
}

void terme_input_float(double wait, addr *rtype, addr *rvalue)
{
	TermeBlocking blocking;

	if (wait < 0.0)
		wait = 0;
	blocking.type = terme_blocking_float;
	blocking.wait.float_value = wait;
	terme_input_value(&blocking, rtype, rvalue);
}

