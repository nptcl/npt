#ifndef __STRTYPE_HEADER__
#define __STRTYPE_HEADER__

#include "local.h"
#include "typedef.h"

#define array_stringp _n(array_stringp)
#define strarrayp _n(strarrayp)
#define stringp _n(stringp)
#define string_designator_p _n(string_designator_p)
#define string_base_p_ _n(string_base_p_)
#define string_simple_p _n(string_simple_p)
#define string_character_type_ _n(string_character_type_)
#define strarray_base_p_ _n(strarray_base_p_)
#define strarray_simple_p _n(strarray_simple_p)
#define strarray_character_type_ _n(strarray_character_type_)
#define strarray_alloc_ _n(strarray_alloc_)
#define strarray_local_ _n(strarray_local_)
#define strarray_heap_ _n(strarray_heap_)
#define strarray_char_alloc_ _n(strarray_char_alloc_)
#define strarray_char_local_ _n(strarray_char_local_)
#define strarray_char_heap_ _n(strarray_char_heap_)
#define strarray_size1_alloc_ _n(strarray_size1_alloc_)
#define strarray_size1_local_ _n(strarray_size1_local_)
#define strarray_size1_heap_ _n(strarray_size1_heap_)
#define strarray_sizeu_alloc_ _n(strarray_sizeu_alloc_)
#define strarray_sizeu_local_ _n(strarray_sizeu_local_)
#define strarray_sizeu_heap_ _n(strarray_sizeu_heap_)
#define strarray_length _n(strarray_length)
#define strarray_length_buffer _n(strarray_length_buffer)
#define strarray_getc_ _n(strarray_getc_)
#define strarray_setc_ _n(strarray_setc_)
#define strarray_equal_binary_ _n(strarray_equal_binary_)
#define strarray_equalp_binary_ _n(strarray_equalp_binary_)
#define strarray_equal_char_ _n(strarray_equal_char_)
#define strarray_equalp_char_ _n(strarray_equalp_char_)
#define strarray_equal_ _n(strarray_equal_)
#define strarray_equalp_ _n(strarray_equalp_)
#define strarray_character_equal_ _n(strarray_character_equal_)
#define strarray_character_equalp_ _n(strarray_character_equalp_)
#define strarray_compare_binary_ _n(strarray_compare_binary_)
#define strarray_comparep_binary_ _n(strarray_comparep_binary_)
#define strarray_compare_char_ _n(strarray_compare_char_)
#define strarray_comparep_char_ _n(strarray_comparep_char_)
#define strarray_compare_ _n(strarray_compare_)
#define strarray_comparep_ _n(strarray_comparep_)
#define string_alloc_ _n(string_alloc_)
#define string_local_ _n(string_local_)
#define string_heap_ _n(string_heap_)
#define strvect_value_heap_ _n(strvect_value_heap_)
#define string_length _n(string_length)
#define string_getc_ _n(string_getc_)
#define string_setc_ _n(string_setc_)
#define string_equal_binary_ _n(string_equal_binary_)
#define string_equalp_binary_ _n(string_equalp_binary_)
#define string_equal_char_ _n(string_equal_char_)
#define string_equalp_char_ _n(string_equalp_char_)
#define stringp_equal_char_ _n(stringp_equal_char_)
#define stringp_equalp_char_ _n(stringp_equalp_char_)
#define string_equalp_char_va_ _n(string_equalp_char_va_)
#define string_equal_ _n(string_equal_)
#define string_equalp_ _n(string_equalp_)
#define string_character_equal_ _n(string_character_equal_)
#define string_character_equalp_ _n(string_character_equalp_)
#define string_compare_binary_ _n(string_compare_binary_)
#define string_comparep_binary_ _n(string_comparep_binary_)
#define string_compare_char_ _n(string_compare_char_)
#define string_comparep_char_ _n(string_comparep_char_)
#define string_compare_ _n(string_compare_)
#define string_comparep_ _n(string_comparep_)
#define string_designator_equal_ _n(string_designator_equal_)
#define string_designator_equal_char_ _n(string_designator_equal_char_)
#define string_designator_equalp_ _n(string_designator_equalp_)
#define string_designator_equalp_char_ _n(string_designator_equalp_char_)
#define string_designator_alloc_ _n(string_designator_alloc_)
#define string_designator_local_ _n(string_designator_local_)
#define string_designator_heap_ _n(string_designator_heap_)
#define string_designator_string _n(string_designator_string)
#define string_concat_heap_ _n(string_concat_heap_)
#define string_concat_hyphen_heap_ _n(string_concat_hyphen_heap_)
#define string_concat_char1_heap_ _n(string_concat_char1_heap_)
#define string_concat_char2_heap_ _n(string_concat_char2_heap_)
#define string_upper_p_ _n(string_upper_p_)
#define string_lower_p_ _n(string_lower_p_)
#define string_upper_alloc_ _n(string_upper_alloc_)
#define string_upper_local_ _n(string_upper_local_)
#define string_upper_heap_ _n(string_upper_heap_)
#define string_lower_alloc_ _n(string_lower_alloc_)
#define string_lower_local_ _n(string_lower_local_)
#define string_lower_heap_ _n(string_lower_heap_)

#define string_equal_char_debug _n(string_equal_char_debug)
#define string_equalp_char_debug _n(string_equalp_char_debug)
#define string_equal_debug _n(string_equal_debug)

/* string check */
int array_stringp(addr pos);
int strarrayp(addr pos);
int stringp(addr pos);
int string_designator_p(addr pos);

int string_base_p_(addr pos, int *ret);
int string_simple_p(addr pos);
int string_character_type_(addr pos, enum CHARACTER_TYPE *ret);
int strarray_base_p_(addr pos, int *ret);
int strarray_simple_p(addr pos);
int strarray_character_type_(addr pos, enum CHARACTER_TYPE *ret);

/* strarray */
int strarray_alloc_(LocalRoot local, addr *ret, size_t len);
int strarray_local_(LocalRoot local, addr *ret, size_t len);
int strarray_heap_(addr *ret, size_t len);

int strarray_char_alloc_(LocalRoot local, addr *ret, const char *arg);
int strarray_char_local_(LocalRoot local, addr *ret, const char *arg);
int strarray_char_heap_(addr *ret, const char *arg);

int strarray_size1_alloc_(LocalRoot, addr *, const char *, size_t);
int strarray_size1_local_(LocalRoot, addr *, const char *, size_t);
int strarray_size1_heap_(addr *, const char *, size_t);

int strarray_sizeu_alloc_(LocalRoot, addr *, const unicode *, size_t);
int strarray_sizeu_local_(LocalRoot, addr *, const unicode *, size_t);
int strarray_sizeu_heap_(addr *, const unicode *, size_t);

void strarray_length(addr pos, size_t *ret);
void strarray_length_buffer(addr pos, size_t *ret);
int strarray_getc_(addr pos, size_t index, unicode *u);
int strarray_setc_(addr pos, size_t index, unicode u);

int strarray_equal_binary_(addr left, const unicode *right, size_t size, int *ret);
int strarray_equalp_binary_(addr left, const unicode *right, size_t size, int *ret);
int strarray_equal_char_(addr left, const char *right, int *ret);
int strarray_equalp_char_(addr left, const char *right, int *ret);
int strarray_equal_(addr left, addr right, int *ret);
int strarray_equalp_(addr left, addr right, int *ret);
int strarray_character_equal_(addr left, addr right, int *ret);
int strarray_character_equalp_(addr left, addr right, int *ret);

int strarray_compare_binary_(addr left,
		const unicode *right, size_t size2, int *ret);
int strarray_comparep_binary_(addr left,
		const unicode *right, size_t size2, int *ret);
int strarray_compare_char_(addr left, const char *right, int *ret);
int strarray_comparep_char_(addr left, const char *right, int *ret);
int strarray_compare_(addr left, addr right, int *ret);
int strarray_comparep_(addr left, addr right, int *ret);

/* string */
int string_alloc_(LocalRoot local, addr *ret, addr pos);
int string_local_(LocalRoot local, addr *ret, addr pos);
int string_heap_(addr *ret, addr pos);

int strvect_value_heap_(addr *ret, addr pos);

void string_length(addr pos, size_t *ret);
int string_getc_(addr pos, size_t index, unicode *u);
int string_setc_(addr pos, size_t index, unicode u);

int string_equal_binary_(addr left, const unicode *right, size_t len, int *ret);
int string_equalp_binary_(addr left, const unicode *right, size_t len, int *ret);
int string_equal_char_(addr left, const char *right, int *ret);
int string_equalp_char_(addr left, const char *right, int *ret);
int stringp_equal_char_(addr left, const char *right, int *ret);
int stringp_equalp_char_(addr left, const char *right, int *ret);
int string_equalp_char_va_(addr pos, int *ret, ...);
int string_equal_(addr left, addr right, int *ret);
int string_equalp_(addr left, addr right, int *ret);
int string_character_equal_(addr left, addr right, int *ret);
int string_character_equalp_(addr left, addr right, int *ret);

int string_compare_binary_(addr left, const unicode *right, size_t size2, int *ret);
int string_comparep_binary_(addr left, const unicode *right, size_t size2, int *ret);
int string_compare_char_(addr left, const char *right, int *ret);
int string_comparep_char_(addr left, const char *right, int *ret);
int string_compare_(addr left, addr right, int *ret);
int string_comparep_(addr left, addr right, int *ret);

int string_designator_equal_(addr left, addr right, int *ret);
int string_designator_equal_char_(addr left, const char *right, int *ret);
int string_designator_equalp_(addr left, addr right, int *ret);
int string_designator_equalp_char_(addr left, const char *right, int *ret);
int string_designator_alloc_(LocalRoot local, addr *value, addr pos, int *ret);
int string_designator_local_(LocalRoot local, addr *value, addr pos, int *ret);
int string_designator_heap_(addr *value, addr pos, int *ret);
int string_designator_string(addr *value, addr pos);

int string_concat_heap_(addr *ret, addr a, addr b);
int string_concat_hyphen_heap_(addr *ret, addr a, addr b);
int string_concat_char1_heap_(addr *ret, const char *str, addr b);
int string_concat_char2_heap_(addr *ret, addr a, const char *str);

int string_upper_p_(addr pos, int *ret);
int string_lower_p_(addr pos, int *ret);
int string_upper_alloc_(LocalRoot local, addr pos, addr *ret);
int string_upper_local_(LocalRoot local, addr pos, addr *ret);
int string_upper_heap_(addr pos, addr *ret);
int string_lower_alloc_(LocalRoot local, addr pos, addr *ret);
int string_lower_local_(LocalRoot local, addr pos, addr *ret);
int string_lower_heap_(addr pos, addr *ret);

/* debug */
int string_equal_char_debug(addr left, const char *right);
int string_equalp_char_debug(addr left, const char *right);
int string_equal_debug(addr left, addr right);

#endif

