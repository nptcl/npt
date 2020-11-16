#ifndef __ENCODE_UNICODE_HEADER__
#define __ENCODE_UNICODE_HEADER__

#include "file_memory.h"
#include "typedef.h"

#define read_utf8_normal _n(read_utf8_normal)
#define read_utf8_nonblock _n(read_utf8_nonblock)
#define read_utf8_buffer _n(read_utf8_buffer)
#define read_utf16_normal _n(read_utf16_normal)
#define read_utf16_nonblock _n(read_utf16_nonblock)
#define read_utf32_normal _n(read_utf32_normal)
#define read_utf32_nonblock _n(read_utf32_nonblock)
#define encode_utf8 _n(encode_utf8)
#define encode_utf16a _n(encode_utf16a)
#define encode_utf16b _n(encode_utf16b)
#define encode_utf16 _n(encode_utf16)
#define encode_utf32check _n(encode_utf32check)
#define encode_utf32 _n(encode_utf32)

int read_utf8_normal(filestream fm, unicode *ret);
int read_utf8_nonblock(filestream fm, unicode *ret, int *hang);
int read_utf8_buffer(unicode *dst, const byte *src, size_t size, size_t *ret);

int read_utf16_normal(filestream fm, unicode *ret, int be);
int read_utf16_nonblock(filestream fm, unicode *ret, int *hang, int be);

int read_utf32_normal(filestream fm, unicode *ret, int be);
int read_utf32_nonblock(filestream fm, unicode *ret, int *hang, int be);

int encode_utf8(unicode u, byte *dst, size_t *ret);
int encode_utf16a(unicode u, byte16 *surrogate, byte16 *code);
int encode_utf16b(unicode u, byte16 *dst, size_t *ret);
int encode_utf16(unicode u, int big_endian_p, byte *dst, size_t *ret);
int encode_utf32check(unicode u);
int encode_utf32(unicode u, int big_endian_p, byte *dst, size_t *ret);

#endif

