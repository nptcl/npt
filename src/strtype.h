#ifndef __STRTYPE_HEADER__
#define __STRTYPE_HEADER__

#include "typedef.h"

/* string check */
int array_stringp(addr pos);
int strarrayp(addr pos);
int stringp(addr pos);
int string_designer_p(addr pos);

/* strarray */
addr strarray_allocr(LocalRoot local, size_t len);
addr strarray_localr(LocalRoot local, size_t len);
addr strarray_heapr(size_t len);
void strarray_alloc(LocalRoot local, addr *ret, size_t len);
void strarray_local(LocalRoot local, addr *ret, size_t len);
void strarray_heap(addr *ret, size_t len);

addr strarray_char_allocr(LocalRoot local, const char *arg);
addr strarray_char_localr(LocalRoot local, const char *arg);
addr strarray_char_heapr(const char *arg);
void strarray_char_alloc(LocalRoot local, addr *ret, const char *arg);
void strarray_char_local(LocalRoot local, addr *ret, const char *arg);
void strarray_char_heap(addr *ret, const char *arg);

addr strarray_size1_allocr(LocalRoot, const char *, size_t);
addr strarray_size1_localr(LocalRoot, const char *, size_t);
addr strarray_size1_heapr(const char *, size_t);
void strarray_size1_alloc(LocalRoot, addr *, const char *, size_t);
void strarray_size1_local(LocalRoot, addr *, const char *, size_t);
void strarray_size1_heap(addr *, const char *, size_t);

addr strarray_sizeu_allocr(LocalRoot, const unicode *, size_t);
addr strarray_sizeu_localr(LocalRoot, const unicode *, size_t);
addr strarray_sizeu_heapr(const unicode *, size_t);
void strarray_sizeu_alloc(LocalRoot, addr *, const unicode *, size_t);
void strarray_sizeu_local(LocalRoot, addr *, const unicode *, size_t);
void strarray_sizeu_heap(addr *, const unicode *, size_t);

void strarray_length(addr pos, size_t *ret);
void strarray_length_buffer(addr pos, size_t *ret);
void strarray_posbodylen(addr pos, const unicode **body, size_t *len);
unicode strarray_refc(addr pos, size_t index);
void strarray_getc(addr pos, size_t index, unicode *u);
void strarray_setc(addr pos, size_t index, unicode u);

int strarray_equal_binary(addr left, const unicode *right, size_t len);
int strarray_equalp_binary(addr left, const unicode *right, size_t len);
int strarray_equal_char(addr left, const char *right);
int strarray_equalp_char(addr left, const char *right);
int strarray_equal(addr left, addr right);
int strarray_equalp(addr left, addr right);
int strarray_character_equal(addr left, addr right);
int strarray_character_equalp(addr left, addr right);

int strarray_compare_binary(addr left, const unicode *right, size_t len);
int strarray_comparep_binary(addr left, const unicode *right, size_t len);
int strarray_compare_char(addr left, const char *right);
int strarray_comparep_char(addr left, const char *right);
int strarray_compare(addr left, addr right);
int strarray_comparep(addr left, addr right);

/* string */
addr string_allocr(LocalRoot local, addr pos);
addr string_localr(LocalRoot local, addr pos);
addr string_heapr(addr pos);
void string_alloc(LocalRoot local, addr *ret, addr pos);
void string_local(LocalRoot local, addr *ret, addr pos);
void string_heap(addr *ret, addr pos);

void string_length(addr pos, size_t *ret);
void string_posbodylen(addr pos, const unicode **body, size_t *len);
unicode string_refc(addr pos, size_t index);
void string_getc(addr pos, size_t index, unicode *c);
void string_setc(addr pos, size_t index, unicode c);

int string_equal_binary(addr left, const unicode *right, size_t len);
int string_equalp_binary(addr left, const unicode *right, size_t len);
int string_equal_char(addr left, const char *right);
int string_equalp_char(addr left, const char *right);
int string_equal(addr left, addr right);
int string_equalp(addr left, addr right);
int string_character_equal(addr left, addr right);
int string_character_equalp(addr left, addr right);

int string_compare_binary(addr left, const unicode *right, size_t len);
int string_comparep_binary(addr left, const unicode *right, size_t len);
int string_compare_char(addr left, const char *right);
int string_comparep_char(addr left, const char *right);
int string_compare(addr left, addr right);
int string_comparep(addr left, addr right);

int string_designer_equal(addr left, addr right);
int string_designer_alloc(LocalRoot local, addr *ret, addr pos);
int string_designer_local(LocalRoot local, addr *ret, addr pos);
int string_designer_heap(addr *ret, addr pos);
int string_designer_string(addr *ret, addr pos);

#endif

