#ifndef __ARRAY_COERCE_HEADER__
#define __ARRAY_COERCE_HEADER__

#include "typedef.h"

#define array_coerce_t_heap_ _n(array_coerce_t_heap_)
#define array_coerce_bit_heap_ _n(array_coerce_bit_heap_)
#define array_coerce_character_heap_ _n(array_coerce_character_heap_)
#define array_coerce_signed8_heap_ _n(array_coerce_signed8_heap_)
#define array_coerce_signed16_heap_ _n(array_coerce_signed16_heap_)
#define array_coerce_signed32_heap_ _n(array_coerce_signed32_heap_)
#define array_coerce_signed64_heap_ _n(array_coerce_signed64_heap_)
#define array_coerce_unsigned8_heap_ _n(array_coerce_unsigned8_heap_)
#define array_coerce_unsigned16_heap_ _n(array_coerce_unsigned16_heap_)
#define array_coerce_unsigned32_heap_ _n(array_coerce_unsigned32_heap_)
#define array_coerce_unsigned64_heap_ _n(array_coerce_unsigned64_heap_)
#define array_coerce_single_heap_ _n(array_coerce_single_heap_)
#define array_coerce_double_heap_ _n(array_coerce_double_heap_)
#define array_coerce_long_heap_ _n(array_coerce_long_heap_)
#define vector_coerce_signed8_heap_ _n(vector_coerce_signed8_heap_)
#define vector_coerce_signed16_heap_ _n(vector_coerce_signed16_heap_)
#define vector_coerce_signed32_heap_ _n(vector_coerce_signed32_heap_)
#define vector_coerce_signed64_heap_ _n(vector_coerce_signed64_heap_)
#define vector_coerce_unsigned8_heap_ _n(vector_coerce_unsigned8_heap_)
#define vector_coerce_unsigned16_heap_ _n(vector_coerce_unsigned16_heap_)
#define vector_coerce_unsigned32_heap_ _n(vector_coerce_unsigned32_heap_)
#define vector_coerce_unsigned64_heap_ _n(vector_coerce_unsigned64_heap_)
#define vector_coerce_single_heap_ _n(vector_coerce_single_heap_)
#define vector_coerce_double_heap_ _n(vector_coerce_double_heap_)
#define vector_coerce_long_heap_ _n(vector_coerce_long_heap_)
#define array_coerce_bit_t_ _n(array_coerce_bit_t_)
#define array_coerce_character_t_ _n(array_coerce_character_t_)
#define array_coerce_signed8_t_ _n(array_coerce_signed8_t_)
#define array_coerce_signed16_t_ _n(array_coerce_signed16_t_)
#define array_coerce_signed32_t_ _n(array_coerce_signed32_t_)
#define array_coerce_signed64_t_ _n(array_coerce_signed64_t_)
#define array_coerce_unsigned8_t_ _n(array_coerce_unsigned8_t_)
#define array_coerce_unsigned16_t_ _n(array_coerce_unsigned16_t_)
#define array_coerce_unsigned32_t_ _n(array_coerce_unsigned32_t_)
#define array_coerce_unsigned64_t_ _n(array_coerce_unsigned64_t_)
#define array_coerce_single_t_ _n(array_coerce_single_t_)
#define array_coerce_double_t_ _n(array_coerce_double_t_)
#define array_coerce_long_t_ _n(array_coerce_long_t_)
#define array_coerce_bit_ _n(array_coerce_bit_)
#define array_coerce_character_ _n(array_coerce_character_)
#define array_coerce_signed8_ _n(array_coerce_signed8_)
#define array_coerce_signed16_ _n(array_coerce_signed16_)
#define array_coerce_signed32_ _n(array_coerce_signed32_)
#define array_coerce_signed64_ _n(array_coerce_signed64_)
#define array_coerce_unsigned8_ _n(array_coerce_unsigned8_)
#define array_coerce_unsigned16_ _n(array_coerce_unsigned16_)
#define array_coerce_unsigned32_ _n(array_coerce_unsigned32_)
#define array_coerce_unsigned64_ _n(array_coerce_unsigned64_)
#define array_coerce_single_ _n(array_coerce_single_)
#define array_coerce_double_ _n(array_coerce_double_)
#define array_coerce_long_ _n(array_coerce_long_)
#define vector_coerce_bit_ _n(vector_coerce_bit_)
#define vector_coerce_character_ _n(vector_coerce_character_)
#define vector_coerce_signed8_ _n(vector_coerce_signed8_)
#define vector_coerce_signed16_ _n(vector_coerce_signed16_)
#define vector_coerce_signed32_ _n(vector_coerce_signed32_)
#define vector_coerce_signed64_ _n(vector_coerce_signed64_)
#define vector_coerce_unsigned8_ _n(vector_coerce_unsigned8_)
#define vector_coerce_unsigned16_ _n(vector_coerce_unsigned16_)
#define vector_coerce_unsigned32_ _n(vector_coerce_unsigned32_)
#define vector_coerce_unsigned64_ _n(vector_coerce_unsigned64_)
#define vector_coerce_single_ _n(vector_coerce_single_)
#define vector_coerce_double_ _n(vector_coerce_double_)
#define vector_coerce_long_ _n(vector_coerce_long_)

/* make */
_g int array_coerce_t_heap_(addr *ret, addr array);
_g int array_coerce_bit_heap_(addr *ret, addr array);
_g int array_coerce_character_heap_(addr *ret, addr array);
_g int array_coerce_signed8_heap_(addr *ret, addr array);
_g int array_coerce_signed16_heap_(addr *ret, addr array);
_g int array_coerce_signed32_heap_(addr *ret, addr array);
#ifdef LISP_64BIT
_g int array_coerce_signed64_heap_(addr *ret, addr array);
#endif
_g int array_coerce_unsigned8_heap_(addr *ret, addr array);
_g int array_coerce_unsigned16_heap_(addr *ret, addr array);
_g int array_coerce_unsigned32_heap_(addr *ret, addr array);
#ifdef LISP_64BIT
_g int array_coerce_unsigned64_heap_(addr *ret, addr array);
#endif
_g int array_coerce_single_heap_(addr *ret, addr array);
_g int array_coerce_double_heap_(addr *ret, addr array);
_g int array_coerce_long_heap_(addr *ret, addr array);

_g int vector_coerce_signed8_heap_(addr *ret, size_t size);
_g int vector_coerce_signed16_heap_(addr *ret, size_t size);
_g int vector_coerce_signed32_heap_(addr *ret, size_t size);
#ifdef LISP_64BIT
_g int vector_coerce_signed64_heap_(addr *ret, size_t size);
#endif
_g int vector_coerce_unsigned8_heap_(addr *ret, size_t size);
_g int vector_coerce_unsigned16_heap_(addr *ret, size_t size);
_g int vector_coerce_unsigned32_heap_(addr *ret, size_t size);
#ifdef LISP_64BIT
_g int vector_coerce_unsigned64_heap_(addr *ret, size_t size);
#endif
_g int vector_coerce_single_heap_(addr *ret, size_t size);
_g int vector_coerce_double_heap_(addr *ret, size_t size);
_g int vector_coerce_long_heap_(addr *ret, size_t size);

/* coerce */
_g int array_coerce_bit_t_(addr pos, int *rv, int *ret);
_g int array_coerce_character_t_(addr pos, unicode *rv, int *ret);
_g int array_coerce_signed8_t_(addr pos, int8_t *rv, int *ret);
_g int array_coerce_signed16_t_(addr pos, int16_t *rv, int *ret);
_g int array_coerce_signed32_t_(addr pos, int32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int array_coerce_signed64_t_(addr pos, int64_t *rv, int *ret);
#endif
_g int array_coerce_unsigned8_t_(addr pos, uint8_t *rv, int *ret);
_g int array_coerce_unsigned16_t_(addr pos, uint16_t *rv, int *ret);
_g int array_coerce_unsigned32_t_(addr pos, uint32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int array_coerce_unsigned64_t_(addr pos, uint64_t *rv, int *ret);
#endif
_g int array_coerce_single_t_(addr value, single_float *rv, int *ret);
_g int array_coerce_double_t_(addr value, double_float *rv, int *ret);
_g int array_coerce_long_t_(addr value, long_float *rv, int *ret);

_g int array_coerce_bit_(addr pos, size_t i, int *rv, int *ret);
_g int array_coerce_character_(addr pos, size_t i, unicode *rv, int *ret);
_g int array_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret);
_g int array_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret);
_g int array_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int array_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret);
#endif
_g int array_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret);
_g int array_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret);
_g int array_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int array_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret);
#endif
_g int array_coerce_single_(addr pos, size_t i, single_float *rv, int *ret);
_g int array_coerce_double_(addr pos, size_t i, double_float *rv, int *ret);
_g int array_coerce_long_(addr pos, size_t i, long_float *rv, int *ret);

_g int vector_coerce_bit_(addr pos, size_t i, int *rv, int *ret);
_g int vector_coerce_character_(addr pos, size_t i, unicode *rv, int *ret);
_g int vector_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret);
_g int vector_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret);
_g int vector_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int vector_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret);
#endif
_g int vector_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret);
_g int vector_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret);
_g int vector_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret);
#ifdef LISP_64BIT
_g int vector_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret);
#endif
_g int vector_coerce_single_(addr pos, size_t i, single_float *rv, int *ret);
_g int vector_coerce_double_(addr pos, size_t i, double_float *rv, int *ret);
_g int vector_coerce_long_(addr pos, size_t i, long_float *rv, int *ret);

#endif

