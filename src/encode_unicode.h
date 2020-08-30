#ifndef __ENCODE_UNICODE_HEADER__
#define __ENCODE_UNICODE_HEADER__

#include "file_memory.h"
#include "typedef.h"

#define read_utf8_normal _n(read_utf8_normal)
#define read_utf8_nonblocking _n(read_utf8_nonblocking)
#define read_utf8_buffer _n(read_utf8_buffer)
#define read_utf16_normal _n(read_utf16_normal)
#define read_utf16_nonblocking _n(read_utf16_nonblocking)
#define read_utf32_normal _n(read_utf32_normal)
#define read_utf32_nonblocking _n(read_utf32_nonblocking)
#define encode_utf8 _n(encode_utf8)
#define encode_utf16a _n(encode_utf16a)
#define encode_utf16b _n(encode_utf16b)
#define encode_utf16 _n(encode_utf16)
#define encode_utf32check _n(encode_utf32check)
#define encode_utf32 _n(encode_utf32)

_g int read_utf8_normal(struct filememory *fm, unicode *ret);
_g int read_utf8_nonblocking(struct filememory *fm, unicode *ret, int *hang);
_g int read_utf8_buffer(unicode *dst, const byte *src, size_t size, size_t *ret);

_g int read_utf16_normal(struct filememory *fm, unicode *ret, int be);
_g int read_utf16_nonblocking(struct filememory *fm, unicode *ret, int *hang, int be);

_g int read_utf32_normal(struct filememory *fm, unicode *ret, int be);
_g int read_utf32_nonblocking(struct filememory *fm, unicode *ret, int *hang, int be);

_g int encode_utf8(unicode u, byte *dst, size_t *ret);
_g int encode_utf16a(unicode u, byte16 *surrogate, byte16 *code);
_g int encode_utf16b(unicode u, byte16 *dst, size_t *ret);
_g int encode_utf16(unicode u, int big_endian_p, byte *dst, size_t *ret);
_g int encode_utf32check(unicode u);
_g int encode_utf32(unicode u, int big_endian_p, byte *dst, size_t *ret);

#endif

