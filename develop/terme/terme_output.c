#include <stdio.h>
#include <unistd.h>
#include "character_check.h"
#include "eastasian_unicode.h"
#include "terme_output.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define TERME_OUTPUT_SIZE	3
#else
#define TERME_OUTPUT_SIZE	4096
#endif

static byte terme_output_buffer[TERME_OUTPUT_SIZE];
size_t terme_output_size;
size_t terme_output_x;

void terme_output_init(void)
{
	terme_output_size = 0;
	terme_output_x = 0;
}

int terme_finish_output(void)
{
	byte *data;
	size_t size, retu;
	ssize_t rets;

	size = terme_output_size;
	data = terme_output_buffer;
	while (size) {
		rets = write(STDOUT_FILENO, data, size);
		if (rets < 0)
			return 1; /* error */
		retu = (size_t)rets;
		if (size <= retu)
			break;
		size -= retu;
		data += retu;
	}
	terme_output_size = 0;

	return 0;
}

int terme_write_byte(byte c)
{
	if (TERME_OUTPUT_SIZE <= terme_output_size) {
		if (terme_finish_output())
			return 1;
	}

	terme_output_buffer[terme_output_size] = c;
	terme_output_size++;

	return 0;
}

static int terme_write_memory(const byte *data, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		if (terme_write_byte(data[i]))
			return 1;
	}

	return 0;
}

static int terme_write_utf8(unicode u, byte *dst, size_t *ret)
{
	size_t w;

	w = 0;
	/* 1 byte */
	if (u < 0x80) {
		dst[w++] = u;
		goto normal;
	}
	/* 2 byte */
	if (u < 0x0800) {
		dst[w++] = 0xC2 | (u >> 6);
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 3 byte */
	if (u < 0xD800) {
		goto sequence3;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		goto error;
	}
	/* 3 byte */
	if (u < 0x010000) {
sequence3:
		dst[w++] = 0xE0 | (u >> 12);
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 4 byte */
	if (u < UnicodeCount) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* error */

error:
	return 1;

normal:
	*ret = w;
	return 0;
}

int terme_write_char(unicode c, int *ret)
{
	byte data[8];
	size_t size;

	if (terme_write_utf8(c, data, &size)) {
		/* encode error */
		if (ret)
			*ret = 1;
		return 0;
	}

	/* eastasian width */
	if (c == 0x0D)
		terme_output_x = 0;
	else
		terme_output_x += (size_t)eastasian_width(c);

	/* output */
	if (ret)
		*ret = 0;
	return terme_write_memory(data, size);
}

int terme_write_ascii(const char *str)
{
	int i;
	unicode c;

	for (i = 0; ; i++) {
		c = (unicode)str[i];
		if (c == 0)
			break;
		if (0x80 <= c)
			continue;
		if (terme_write_char(c, NULL))
			return 1;
	}

	return 0;
}

int terme_write_unicode(const unicode *str)
{
	int i;
	unicode c;

	for (i = 0; ; i++) {
		c = str[i];
		if (c == 0)
			break;
		if (terme_write_char(c, NULL))
			return 1;
	}

	return 0;
}


/*
 *  call
 */
int terme_terpri(void)
{
	return terme_write_byte(0x0D) || terme_write_byte(0x0A);
}

int terme_fresh_line(void)
{
	return terme_output_x? terme_terpri(): 0;
}

