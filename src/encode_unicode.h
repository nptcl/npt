#ifndef __ENCODE_UNICODE_HEADER__
#define __ENCODE_UNICODE_HEADER__

#include "file_memory.h"
#include "typedef.h"

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

