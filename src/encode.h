#ifndef __ENCODE_HEADER__
#define __ENCODE_HEADER__

#include "file_memory.h"
#include "local.h"
#include "typedef.h"

#define readbom8_encode _n(readbom8_encode)
#define readbom16_encode _n(readbom16_encode)
#define readbom32_encode _n(readbom32_encode)
#define writebom_encode_ _n(writebom_encode_)
#define read_char_encode_ _n(read_char_encode_)
#define read_hang_encode_ _n(read_hang_encode_)
#define write_char_encode_ _n(write_char_encode_)
#define length_char_encode _n(length_char_encode)
#define length_string_encode_ _n(length_string_encode_)
#define UTF8_buffer_clang_ _n(UTF8_buffer_clang_)
#define UTF16_buffer_clang_ _n(UTF16_buffer_clang_)
#define UTF8_null_strlen _n(UTF8_null_strlen)
#define UTF8_size_strlen _n(UTF8_size_strlen)
#define UTF8_null_makeunicode _n(UTF8_null_makeunicode)
#define UTF8_size_makeunicode _n(UTF8_size_makeunicode)
#define UTF16_null_strlen _n(UTF16_null_strlen)
#define UTF16_size_strlen _n(UTF16_size_strlen)
#define UTF16_null_makeunicode _n(UTF16_null_makeunicode)
#define UTF16_size_makeunicode _n(UTF16_size_makeunicode)
#define UTF32_null_strlen _n(UTF32_null_strlen)
#define UTF32_size_strlen _n(UTF32_size_strlen)
#define UTF32_null_makeunicode _n(UTF32_null_makeunicode)
#define UTF32_size_makeunicode _n(UTF32_size_makeunicode)
#define UTF32_length_utf8 _n(UTF32_length_utf8)
#define UTF32_length_utf16 _n(UTF32_length_utf16)
#define UTF32_make_utf8 _n(UTF32_make_utf8)
#define UTF32_make_utf16 _n(UTF32_make_utf16)
#define init_encode _n(init_encode)

#define LISP_UTF8_SEQ5CHECK
#define UTF16range(x) (0xD800 <= (x) && (x) < 0xE000)
#define UTF16high(x) (0xD800 <= (x) && (x) < 0xDC00)
#define UTF16low(x) (0xDC00 <= (x) && (x) < 0xE000)
#define UTF16unicode(a, b) \
	((((((a)>>6UL)&0x0FUL)+1UL)<<16UL) | (((a)&0x3FUL)<<10UL) | ((b)&0x03FFUL))

/* Byte Order Mark */
_g int readbom8_encode(addr stream);
_g int readbom16_encode(addr stream); /* 1:le, 2:be */
_g int readbom32_encode(addr stream); /* 1:le, 2:be */
_g int writebom_encode_(addr stream);

_g int read_char_encode_(filestream fm, unicode *c, int *ret);
_g int read_hang_encode_(filestream fm, unicode *c, int *hang, int *ret);
_g int write_char_encode_(filestream fm, unicode c);
_g int length_char_encode(filestream fm, unicode c);
_g int length_string_encode_(filestream fm, addr pos, size_t *rsize, int *ret);

/* unicode buffer */
_g int UTF8_buffer_clang_(LocalRoot local, addr *ret, addr string);
_g int UTF16_buffer_clang_(LocalRoot local, addr *ret, addr string);

/* unicode string */
_g int UTF8_null_strlen(const byte *src, size_t *ret);
_g int UTF8_size_strlen(const byte *src, size_t size, size_t *ret);
_g int UTF8_null_makeunicode(unicode *dst, const byte *src);
_g int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size);
_g int UTF16_null_strlen(const byte16 *src, size_t *ret);
_g int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret);
_g int UTF16_null_makeunicode(unicode *dst, const byte16 *src);
_g int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size);
_g int UTF32_null_strlen(const unicode *src, size_t *ret);
_g int UTF32_size_strlen(const unicode *src, size_t size, size_t *ret);
_g int UTF32_null_makeunicode(unicode *dst, const unicode *src);
_g int UTF32_size_makeunicode(unicode *dst, const unicode *src, size_t size);

_g int UTF32_length_utf8(const unicode *ptr, size_t size, size_t *ret);
_g int UTF32_length_utf16(const unicode *ptr, size_t size, size_t *ret);
_g int UTF32_make_utf8(byte *dst, const unicode *src, size_t size);
_g int UTF32_make_utf16(byte16 *dst, const unicode *src, size_t size);

/* initialize */
_g void init_encode(void);

#endif

