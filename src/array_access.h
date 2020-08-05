#ifndef __ARRAY_ACCESS_HEADER__
#define __ARRAY_ACCESS_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

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

