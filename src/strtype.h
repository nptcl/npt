#ifndef __STRTYPE_HEADER__
#define __STRTYPE_HEADER__

#include "typedef.h"

/* string check */
_g int array_stringp(addr pos);
_g int strarrayp(addr pos);
_g int stringp(addr pos);
_g int string_designer_p(addr pos);

_g int string_base_p_(addr pos, int *ret);
_g int string_simple_p(addr pos);
_g int strarray_base_p_(addr pos, int *ret);
_g int strarray_simple_p(addr pos);
_g int strarray_update_character_type_(addr pos);

/* strarray */
_g int strarray_alloc_(LocalRoot local, addr *ret, size_t len);
_g int strarray_local_(LocalRoot local, addr *ret, size_t len);
_g int strarray_heap_(addr *ret, size_t len);

_g int strarray_char_alloc_(LocalRoot local, addr *ret, const char *arg);
_g int strarray_char_local_(LocalRoot local, addr *ret, const char *arg);
_g int strarray_char_heap_(addr *ret, const char *arg);

_g int strarray_size1_alloc_(LocalRoot, addr *, const char *, size_t);
_g int strarray_size1_local_(LocalRoot, addr *, const char *, size_t);
_g int strarray_size1_heap_(addr *, const char *, size_t);

_g int strarray_sizeu_alloc_(LocalRoot, addr *, const unicode *, size_t);
_g int strarray_sizeu_local_(LocalRoot, addr *, const unicode *, size_t);
_g int strarray_sizeu_heap_(addr *, const unicode *, size_t);

_g void strarray_length(addr pos, size_t *ret);
_g void strarray_length_buffer(addr pos, size_t *ret);
_g int strarray_getc_(addr pos, size_t index, unicode *u);
_g int strarray_setc_(addr pos, size_t index, unicode u);

_g int strarray_equal_binary_(addr left, const unicode *right, size_t size, int *ret);
_g int strarray_equalp_binary_(addr left, const unicode *right, size_t size, int *ret);
_g int strarray_equal_char_(addr left, const char *right, int *ret);
_g int strarray_equalp_char_(addr left, const char *right, int *ret);
_g int strarray_equal_(addr left, addr right, int *ret);
_g int strarray_equalp_(addr left, addr right, int *ret);
_g int strarray_character_equal_(addr left, addr right, int *ret);
_g int strarray_character_equalp_(addr left, addr right, int *ret);

_g int strarray_compare_binary_(addr left,
		const unicode *right, size_t size2, int *ret);
_g int strarray_comparep_binary_(addr left,
		const unicode *right, size_t size2, int *ret);
_g int strarray_compare_char_(addr left, const char *right, int *ret);
_g int strarray_comparep_char_(addr left, const char *right, int *ret);
_g int strarray_compare_(addr left, addr right, int *ret);
_g int strarray_comparep_(addr left, addr right, int *ret);

/* string */
_g int string_alloc_(LocalRoot local, addr *ret, addr pos);
_g int string_local_(LocalRoot local, addr *ret, addr pos);
_g int string_heap_(addr *ret, addr pos);

_g int strvect_value_heap_(addr *ret, addr pos);

_g void string_length(addr pos, size_t *ret);
_g int string_getc_(addr pos, size_t index, unicode *u);
_g int string_setc_(addr pos, size_t index, unicode u);

_g int string_equal_binary_(addr left, const unicode *right, size_t len, int *ret);
_g int string_equalp_binary_(addr left, const unicode *right, size_t len, int *ret);
_g int string_equal_char_(addr left, const char *right, int *ret);
_g int string_equalp_char_(addr left, const char *right, int *ret);
_g int stringp_equal_char_(addr left, const char *right, int *ret);
_g int stringp_equalp_char_(addr left, const char *right, int *ret);
_g int string_equalp_char_va_(addr pos, int *ret, ...);
_g int string_equal_(addr left, addr right, int *ret);
_g int string_equalp_(addr left, addr right, int *ret);
_g int string_character_equal_(addr left, addr right, int *ret);
_g int string_character_equalp_(addr left, addr right, int *ret);

_g int string_compare_binary_(addr left, const unicode *right, size_t size2, int *ret);
_g int string_comparep_binary_(addr left, const unicode *right, size_t size2, int *ret);
_g int string_compare_char_(addr left, const char *right, int *ret);
_g int string_comparep_char_(addr left, const char *right, int *ret);
_g int string_compare_(addr left, addr right, int *ret);
_g int string_comparep_(addr left, addr right, int *ret);

_g int string_designer_equal_(addr left, addr right, int *ret);
_g int string_designer_equal_char_(addr left, const char *right, int *ret);
_g int string_designer_equalp_(addr left, addr right, int *ret);
_g int string_designer_equalp_char_(addr left, const char *right, int *ret);
_g int string_designer_alloc_(LocalRoot local, addr *value, addr pos, int *ret);
_g int string_designer_local_(LocalRoot local, addr *value, addr pos, int *ret);
_g int string_designer_heap_(addr *value, addr pos, int *ret);
_g int string_designer_string(addr *value, addr pos);

_g int string_concat_heap_(addr *ret, addr a, addr b);
_g int string_concat_hyphen_heap_(addr *ret, addr a, addr b);
_g int string_concat_char1_heap_(addr *ret, const char *str, addr b);
_g int string_concat_char2_heap_(addr *ret, addr a, const char *str);

/* debug */
_g int string_equal_char_debug(addr left, const char *right);
_g int string_equalp_char_debug(addr left, const char *right);
_g int string_equal_debug(addr left, addr right);

#endif

