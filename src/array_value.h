#ifndef __ARRAY_VALUE_HEADER__
#define __ARRAY_VALUE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g int arrayvalue_get_character_(struct array_value *str, addr x);
_g int arrayvalue_get_bit_(struct array_value *str, addr x);
_g int arrayvalue_get_signed8_(struct array_value *str, addr x);
_g int arrayvalue_get_signed16_(struct array_value *str, addr x);
_g int arrayvalue_get_signed32_(struct array_value *str, addr x);
#ifdef LISP_64BIT
_g int arrayvalue_get_signed64_(struct array_value *str, addr x);
#endif
_g int arrayvalue_get_signed_(struct array_value *str, addr x, unsigned size);
_g int arrayvalue_get_unsigned8_(struct array_value *str, addr x);
_g int arrayvalue_get_unsigned16_(struct array_value *str, addr x);
_g int arrayvalue_get_unsigned32_(struct array_value *str, addr x);
#ifdef LISP_64BIT
_g int arrayvalue_get_unsigned64_(struct array_value *str, addr x);
#endif
_g int arrayvalue_get_unsigned_(struct array_value *str, addr x, unsigned size);
_g int arrayvalue_get_single_(struct array_value *str, addr x);
_g int arrayvalue_get_double_(struct array_value *str, addr x);
_g int arrayvalue_get_long_(struct array_value *str, addr x);

_g int arrayvalue_alloc_(LocalRoot local, addr *ret, const struct array_value *str);
_g int arrayvalue_local_(LocalRoot local, addr *ret, const struct array_value *str);
_g int arrayvalue_heap_(addr *ret, const struct array_value *str);

#endif

