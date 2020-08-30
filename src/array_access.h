#ifndef __ARRAY_ACCESS_HEADER__
#define __ARRAY_ACCESS_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

#define arraymemory_get_ _n(arraymemory_get_)
#define array_arefindex_ _n(array_arefindex_)
#define array_get_t_ _n(array_get_t_)
#define array_get_bit_ _n(array_get_bit_)
#define array_get_unicode_ _n(array_get_unicode_)
#define array_get_ _n(array_get_)
#define array_set_bit_ _n(array_set_bit_)
#define array_set_character_ _n(array_set_character_)
#define array_set_signed8_ _n(array_set_signed8_)
#define array_set_signed16_ _n(array_set_signed16_)
#define array_set_signed32_ _n(array_set_signed32_)
#define array_set_unsigned8_ _n(array_set_unsigned8_)
#define array_set_unsigned16_ _n(array_set_unsigned16_)
#define array_set_unsigned32_ _n(array_set_unsigned32_)
#define array_set_signed64_ _n(array_set_signed64_)
#define array_set_unsigned64_ _n(array_set_unsigned64_)
#define array_set_single_ _n(array_set_single_)
#define array_set_double_ _n(array_set_double_)
#define array_set_long_ _n(array_set_long_)
#define array_set_ _n(array_set_)
#define array_setget_ _n(array_setget_)
#define array_aref_ _n(array_aref_)
#define array_setf_aref_ _n(array_setf_aref_)
#define array_aref_bit_ _n(array_aref_bit_)
#define array_setf_aref_bit_ _n(array_setf_aref_bit_)
#define array_equal_type _n(array_equal_type)
#define array_equal_dimension _n(array_equal_dimension)
#define array_get_element_type_ _n(array_get_element_type_)
#define array_get_vector_length _n(array_get_vector_length)
#define array_get_rowlength _n(array_get_rowlength)

_g int arraymemory_get_(addr pos, size_t index, addr *retp, size_t *rets, int *ret);
_g int array_arefindex_(addr pos, addr args, size_t *ret);

_g int array_get_t_(addr pos, size_t index, addr *ret);
_g int array_get_bit_(addr pos, size_t index, int *ret);
_g int array_get_unicode_(addr pos, size_t index, unicode *ret);
_g int array_get_(LocalRoot local, addr pos, size_t index, addr *ret);

_g int array_set_bit_(addr pos, size_t index, int value);
_g int array_set_character_(addr pos, size_t index, unicode value);
_g int array_set_signed8_(addr pos, size_t index, int8_t value);
_g int array_set_signed16_(addr pos, size_t index, int16_t value);
_g int array_set_signed32_(addr pos, size_t index, int32_t value);
_g int array_set_unsigned8_(addr pos, size_t index, uint8_t value);
_g int array_set_unsigned16_(addr pos, size_t index, uint16_t value);
_g int array_set_unsigned32_(addr pos, size_t index, uint32_t value);
#ifdef LISP_64BIT
_g int array_set_signed64_(addr pos, size_t index, int64_t value);
_g int array_set_unsigned64_(addr pos, size_t index, uint64_t value);
#endif
_g int array_set_single_(addr pos, size_t index, single_float value);
_g int array_set_double_(addr pos, size_t index, double_float value);
_g int array_set_long_(addr pos, size_t index, long_float value);

_g int array_set_(addr pos, size_t index, addr value);
_g int array_setget_(addr p1, size_t s1, addr p2, size_t s2);
_g int array_aref_(LocalRoot local, addr pos, addr args, addr *ret);
_g int array_setf_aref_(addr pos, addr args, addr value);
_g int array_aref_bit_(LocalRoot local, addr pos, addr args, addr *ret);
_g int array_setf_aref_bit_(addr pos, addr args, addr value);

_g int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size);
_g int array_equal_dimension(addr left, addr right);
_g int array_get_element_type_(addr pos, addr *ret);
_g size_t array_get_vector_length(addr pos, int fill);
_g void array_get_rowlength(addr pos, size_t *ret);

#endif

