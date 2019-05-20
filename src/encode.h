#ifndef __ENCODE_HEADER__
#define __ENCODE_HEADER__

#include "file_memory.h"
#include "local.h"
#include "typedef.h"

/* Byte Order Mark */
int readbom8_encode(struct filememory *fm);
int readbom16_encode(struct filememory *fm); /* 1:le, 2:be */
int readbom32_encode(struct filememory *fm); /* 1:le, 2:be */
int writebom8_encode(struct filememory *fm);
int writebom16_encode(struct filememory *fm, int bigp);
int writebom32_encode(struct filememory *fm, int bigp);

int read_char_encode(struct filememory *fm, unicode *c);
int read_hang_encode(struct filememory *fm, unicode *c, int *hang);
int write_char_encode(struct filememory *fm, unicode c);
int length_char_encode(struct filememory *fm, unicode c);
int length_string_encode(struct filememory *fm, addr pos, size_t *ret);

/* unicode buffer */
int UTF8_buffer_clang(LocalRoot local, addr *ret, addr string);
int UTF16_buffer_clang(LocalRoot local, addr *ret, addr string);

/* unicode string */
int UTF8_null_strlen(const byte *src, size_t *ret);
int UTF8_size_strlen(const byte *src, size_t size, size_t *ret);
int UTF8_null_makeunicode(unicode *dst, const byte *src);
int UTF8_size_makeunicode(unicode *dst, const byte *src, size_t size);
int UTF16_null_strlen(const byte16 *src, size_t *ret);
int UTF16_size_strlen(const byte16 *src, size_t size, size_t *ret);
int UTF16_null_makeunicode(unicode *dst, const byte16 *src);
int UTF16_size_makeunicode(unicode *dst, const byte16 *src, size_t size);

/* initialize */
void init_encode(void);

#endif

