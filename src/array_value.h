#ifndef __ARRAY_VALUE_HEADER__
#define __ARRAY_VALUE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

#define arrayvalue_get_character_ _n(arrayvalue_get_character_)
#define arrayvalue_get_bit_ _n(arrayvalue_get_bit_)
#define arrayvalue_get_signed8_ _n(arrayvalue_get_signed8_)
#define arrayvalue_get_signed16_ _n(arrayvalue_get_signed16_)
#define arrayvalue_get_signed32_ _n(arrayvalue_get_signed32_)
#define arrayvalue_get_signed64_ _n(arrayvalue_get_signed64_)
#define arrayvalue_get_signed_ _n(arrayvalue_get_signed_)
#define arrayvalue_get_unsigned8_ _n(arrayvalue_get_unsigned8_)
#define arrayvalue_get_unsigned16_ _n(arrayvalue_get_unsigned16_)
#define arrayvalue_get_unsigned32_ _n(arrayvalue_get_unsigned32_)
#define arrayvalue_get_unsigned64_ _n(arrayvalue_get_unsigned64_)
#define arrayvalue_get_unsigned_ _n(arrayvalue_get_unsigned_)
#define arrayvalue_get_single_ _n(arrayvalue_get_single_)
#define arrayvalue_get_double_ _n(arrayvalue_get_double_)
#define arrayvalue_get_long_ _n(arrayvalue_get_long_)
#define arrayvalue_alloc_ _n(arrayvalue_alloc_)
#define arrayvalue_local_ _n(arrayvalue_local_)
#define arrayvalue_heap_ _n(arrayvalue_heap_)

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

