#ifndef __ENCODE_HEADER__
#define __ENCODE_HEADER__

#include "file_memory.h"
#include "local.h"
#include "typedef.h"

#define LISP_UTF8_SEQ5CHECK
#define UTF16range(x) (0xD800 <= (x) && (x) < 0xE000)
#define UTF16high(x) (0xD800 <= (x) && (x) < 0xDC00)
#define UTF16low(x) (0xDC00 <= (x) && (x) < 0xE000)
#define UTF16unicode(a, b) \
	((((((a)>>6UL)&0x0FUL)+1UL)<<16UL) | (((a)&0x3FUL)<<10UL) | ((b)&0x03FFUL))

/* Byte Order Mark */
_g int readbom8_encode(struct filememory *fm);
_g int readbom16_encode(struct filememory *fm); /* 1:le, 2:be */
_g int readbom32_encode(struct filememory *fm); /* 1:le, 2:be */
_g int writebom8_encode(struct filememory *fm);
_g int writebom16_encode(struct filememory *fm, int bigp);
_g int writebom32_encode(struct filememory *fm, int bigp);

_g int read_char_encode(struct filememory *fm, unicode *c);
_g int read_hang_encode(struct filememory *fm, unicode *c, int *hang);
_g int write_char_encode(struct filememory *fm, unicode c);
_g int length_char_encode(struct filememory *fm, unicode c);
_g int length_string_encode(struct filememory *fm, addr pos, size_t *ret);

/* unicode buffer */
_g int UTF8_buffer_clang(LocalRoot local, addr *ret, addr string);
_g int UTF16_buffer_clang(LocalRoot local, addr *ret, addr string);

/* unicode string */
_g int UTF8_null_strlen(const byte *src, size_t *ret);
_g int UTF8_size_strlen(const byte *src, size_t size, size_t *ret);
_g int UTF8_null_makeunicode(unicode *dst, const byte *src);
_g int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size);
_g int UTF16_null_strlen(const byte16 *src, size_t *ret);
_g int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret);
_g int UTF16_null_makeunicode(unicode *dst, const byte16 *src);
_g int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size);

_g int UTF32_length_utf8(const unicode *ptr, size_t size, size_t *ret);
_g int UTF32_length_utf16(const unicode *ptr, size_t size, size_t *ret);
_g int UTF32_make_utf8(byte *dst, const unicode *src, size_t size);
_g int UTF32_make_utf16(byte16 *dst, const unicode *src, size_t size);

/* initialize */
_g void init_encode(void);

#endif

