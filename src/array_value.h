#ifndef __ARRAY_VALUE_HEADER__
#define __ARRAY_VALUE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g void arrayvalue_get_character(struct array_value *str, addr x);
_g void arrayvalue_get_bit(struct array_value *str, addr x);
_g void arrayvalue_get_signed8(struct array_value *str, addr x);
_g void arrayvalue_get_signed16(struct array_value *str, addr x);
_g void arrayvalue_get_signed32(struct array_value *str, addr x);
_g void arrayvalue_get_signed(struct array_value *str, addr x, unsigned size);
_g void arrayvalue_get_unsigned8(struct array_value *str, addr x);
_g void arrayvalue_get_unsigned16(struct array_value *str, addr x);
_g void arrayvalue_get_unsigned32(struct array_value *str, addr x);
_g void arrayvalue_get_unsigned(struct array_value *str, addr x, unsigned size);
_g void arrayvalue_get_single(struct array_value *str, addr x);
_g void arrayvalue_get_double(struct array_value *str, addr x);
_g void arrayvalue_get_long(struct array_value *str, addr x);
#ifdef LISP_64BIT
_g void arrayvalue_get_signed64(struct array_value *str, addr x);
_g void arrayvalue_get_unsigned64(struct array_value *str, addr x);
#endif

_g void arrayvalue_alloc(LocalRoot local, addr *ret, const struct array_value *str);
_g void arrayvalue_local(LocalRoot local, addr *ret, const struct array_value *str);
_g void arrayvalue_heap(addr *ret, const struct array_value *str);

#endif

