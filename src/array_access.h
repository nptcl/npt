#ifndef __ARRAY_ACCESS_HEADER__
#define __ARRAY_ACCESS_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g int arraymemory_get(addr pos, size_t index, addr *retp, size_t *rets);
_g size_t array_arefindex(addr pos, addr args);

_g void array_get_t(addr pos, size_t index, addr *ret);
_g void array_get_bit(addr pos, size_t index, int *ret);
_g void array_get_unicode(addr pos, size_t index, unicode *ret);
_g void array_get(LocalRoot local, addr pos, size_t index, addr *ret);

_g void array_set_bit(addr pos, size_t index, int value);
_g void array_set_character(addr pos, size_t index, unicode value);
_g void array_set_signed8(addr pos, size_t index, int8_t value);
_g void array_set_signed16(addr pos, size_t index, int16_t value);
_g void array_set_signed32(addr pos, size_t index, int32_t value);
_g void array_set_unsigned8(addr pos, size_t index, uint8_t value);
_g void array_set_unsigned16(addr pos, size_t index, uint16_t value);
_g void array_set_unsigned32(addr pos, size_t index, uint32_t value);
#ifdef LISP_64BIT
_g void array_set_signed64(addr pos, size_t index, int64_t value);
_g void array_set_unsigned64(addr pos, size_t index, uint64_t value);
#endif
_g void array_set_single(addr pos, size_t index, single_float value);
_g void array_set_double(addr pos, size_t index, double_float value);
_g void array_set_long(addr pos, size_t index, long_float value);

_g void array_set(addr pos, size_t index, addr value);
_g void array_setget(addr p1, size_t s1, addr p2, size_t s2);
_g void array_aref(LocalRoot local, addr pos, addr args, addr *ret);
_g void array_setf_aref(addr pos, addr args, addr value);
_g void array_aref_bit(LocalRoot local, addr pos, addr args, addr *ret);
_g void array_setf_aref_bit(addr pos, addr args, addr value);

_g int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size);
_g int array_equal_dimension(addr left, addr right);
_g void array_get_element_type(addr pos, addr *ret);
_g size_t array_get_vector_length(addr pos, int fill);
_g void array_get_rowlength(addr pos, size_t *ret);

#endif

