#ifndef __STRTYPE_HEADER__
#define __STRTYPE_HEADER__

#include "typedef.h"

/* string check */
_g int array_stringp(addr pos);
_g int strarrayp(addr pos);
_g int stringp(addr pos);
_g int string_designer_p(addr pos);

_g int string_base_p(addr pos);
_g int string_simple_p(addr pos);
_g int strarray_base_p(addr pos);
_g int strarray_simple_p(addr pos);
_g void strarray_update_character_type(addr pos);

/* strarray */
_g void strarray_alloc(LocalRoot local, addr *ret, size_t len);
_g void strarray_local(LocalRoot local, addr *ret, size_t len);
_g void strarray_heap(addr *ret, size_t len);

_g void strarray_char_alloc(LocalRoot local, addr *ret, const char *arg);
_g void strarray_char_local(LocalRoot local, addr *ret, const char *arg);
_g void strarray_char_heap(addr *ret, const char *arg);

_g void strarray_size1_alloc(LocalRoot, addr *, const char *, size_t);
_g void strarray_size1_local(LocalRoot, addr *, const char *, size_t);
_g void strarray_size1_heap(addr *, const char *, size_t);

_g void strarray_sizeu_alloc(LocalRoot, addr *, const unicode *, size_t);
_g void strarray_sizeu_local(LocalRoot, addr *, const unicode *, size_t);
_g void strarray_sizeu_heap(addr *, const unicode *, size_t);

_g void strarray_length(addr pos, size_t *ret);
_g void strarray_length_buffer(addr pos, size_t *ret);
_g unicode strarray_refc(addr pos, size_t index);
_g void strarray_getc(addr pos, size_t index, unicode *u);
_g void strarray_setc(addr pos, size_t index, unicode u);

_g int strarray_equal_binary(addr left, const unicode *right, size_t len);
_g int strarray_equalp_binary(addr left, const unicode *right, size_t len);
_g int strarray_equal_char(addr left, const char *right);
_g int strarray_equalp_char(addr left, const char *right);
_g int strarray_equal(addr left, addr right);
_g int strarray_equalp(addr left, addr right);
_g int strarray_character_equal(addr left, addr right);
_g int strarray_character_equalp(addr left, addr right);

_g int strarray_compare_binary(addr left, const unicode *right, size_t len);
_g int strarray_comparep_binary(addr left, const unicode *right, size_t len);
_g int strarray_compare_char(addr left, const char *right);
_g int strarray_comparep_char(addr left, const char *right);
_g int strarray_compare(addr left, addr right);
_g int strarray_comparep(addr left, addr right);

/* string */
_g void string_alloc(LocalRoot local, addr *ret, addr pos);
_g void string_local(LocalRoot local, addr *ret, addr pos);
_g void string_heap(addr *ret, addr pos);

_g void string_length(addr pos, size_t *ret);
_g unicode string_refc(addr pos, size_t index);
_g void string_getc(addr pos, size_t index, unicode *c);
_g void string_setc(addr pos, size_t index, unicode c);

_g int string_equal_binary(addr left, const unicode *right, size_t len);
_g int string_equalp_binary(addr left, const unicode *right, size_t len);
_g int string_equal_char(addr left, const char *right);
_g int string_equalp_char(addr left, const char *right);
_g int stringp_equal_char(addr left, const char *right);
_g int stringp_equalp_char(addr left, const char *right);
_g int string_equal(addr left, addr right);
_g int string_equalp(addr left, addr right);
_g int string_character_equal(addr left, addr right);
_g int string_character_equalp(addr left, addr right);

_g int string_compare_binary(addr left, const unicode *right, size_t len);
_g int string_comparep_binary(addr left, const unicode *right, size_t len);
_g int string_compare_char(addr left, const char *right);
_g int string_comparep_char(addr left, const char *right);
_g int string_compare(addr left, addr right);
_g int string_comparep(addr left, addr right);

_g int string_designer_equal(addr left, addr right);
_g int string_designer_equal_char(addr left, const char *right);
_g int string_designer_alloc(LocalRoot local, addr *ret, addr pos);
_g int string_designer_local(LocalRoot local, addr *ret, addr pos);
_g int string_designer_heap(addr *ret, addr pos);
_g int string_designer_string(addr *ret, addr pos);

_g void string_concat_heap(addr *ret, addr a, addr b);
_g void string_concat_hyphen_heap(addr *ret, addr a, addr b);
_g void string_concat_char1_heap(addr *ret, const char *str, addr b);
_g void string_concat_char2_heap(addr *ret, addr a, const char *str);

#endif

