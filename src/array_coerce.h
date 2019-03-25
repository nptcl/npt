#ifndef __ARRAY_COERCE_HEADER__
#define __ARRAY_COERCE_HEADER__

#include "typedef.h"

/* make */
void array_coerce_t_heap(addr *ret, addr array);
void array_coerce_bit_heap(addr *ret, addr array);
void array_coerce_character_heap(addr *ret, addr array);
void array_coerce_signed8_heap(addr *ret, addr array);
void array_coerce_signed16_heap(addr *ret, addr array);
void array_coerce_signed32_heap(addr *ret, addr array);
#ifdef LISP_64BIT
void array_coerce_signed64_heap(addr *ret, addr array);
#endif
void array_coerce_unsigned8_heap(addr *ret, addr array);
void array_coerce_unsigned16_heap(addr *ret, addr array);
void array_coerce_unsigned32_heap(addr *ret, addr array);
#ifdef LISP_64BIT
void array_coerce_unsigned64_heap(addr *ret, addr array);
#endif
void array_coerce_single_heap(addr *ret, addr array);
void array_coerce_double_heap(addr *ret, addr array);
void array_coerce_long_heap(addr *ret, addr array);

void vector_coerce_signed8_heap(addr *ret, size_t size);
void vector_coerce_signed16_heap(addr *ret, size_t size);
void vector_coerce_signed32_heap(addr *ret, size_t size);
#ifdef LISP_64BIT
void vector_coerce_signed64_heap(addr *ret, size_t size);
#endif
void vector_coerce_unsigned8_heap(addr *ret, size_t size);
void vector_coerce_unsigned16_heap(addr *ret, size_t size);
void vector_coerce_unsigned32_heap(addr *ret, size_t size);
#ifdef LISP_64BIT
void vector_coerce_unsigned64_heap(addr *ret, size_t size);
#endif
void vector_coerce_single_heap(addr *ret, size_t size);
void vector_coerce_double_heap(addr *ret, size_t size);
void vector_coerce_long_heap(addr *ret, size_t size);

/* coerce */
int array_coerce_bit_t(addr pos, int *ret);
int array_coerce_character_t(addr pos, unicode *ret);
int array_coerce_signed8_t(addr pos, int8_t *ret);
int array_coerce_signed16_t(addr pos, int16_t *ret);
int array_coerce_signed32_t(addr pos, int32_t *ret);
#ifdef LISP_64BIT
int array_coerce_signed64_t(addr pos, int64_t *ret);
#endif
int array_coerce_unsigned8_t(addr pos, uint8_t *ret);
int array_coerce_unsigned16_t(addr pos, uint16_t *ret);
int array_coerce_unsigned32_t(addr pos, uint32_t *ret);
#ifdef LISP_64BIT
int array_coerce_unsigned64_t(addr pos, uint64_t *ret);
#endif
int array_coerce_single_t(addr pos, single_float *ret);
int array_coerce_double_t(addr pos, double_float *ret);
int array_coerce_long_t(addr pos, long_float *ret);

int array_coerce_bit(addr pos, size_t i, int *ret);
int array_coerce_character(addr pos, size_t i, unicode *ret);
int array_coerce_signed8(addr pos, size_t i, int8_t *ret);
int array_coerce_signed16(addr pos, size_t i, int16_t *ret);
int array_coerce_signed32(addr pos, size_t i, int32_t *ret);
#ifdef LISP_64BIT
int array_coerce_signed64(addr pos, size_t i, int64_t *ret);
#endif
int array_coerce_unsigned8(addr pos, size_t i, uint8_t *ret);
int array_coerce_unsigned16(addr pos, size_t i, uint16_t *ret);
int array_coerce_unsigned32(addr pos, size_t i, uint32_t *ret);
#ifdef LISP_64BIT
int array_coerce_unsigned64(addr pos, size_t i, uint64_t *ret);
#endif
int array_coerce_single(addr pos, size_t i, single_float *ret);
int array_coerce_double(addr pos, size_t i, double_float *ret);
int array_coerce_long(addr pos, size_t i, long_float *ret);

int vector_coerce_bit(addr pos, size_t i, int *ret);
int vector_coerce_character(addr pos, size_t i, unicode *ret);
int vector_coerce_signed8(addr pos, size_t i, int8_t *ret);
int vector_coerce_signed16(addr pos, size_t i, int16_t *ret);
int vector_coerce_signed32(addr pos, size_t i, int32_t *ret);
#ifdef LISP_64BIT
int vector_coerce_signed64(addr pos, size_t i, int64_t *ret);
#endif
int vector_coerce_unsigned8(addr pos, size_t i, uint8_t *ret);
int vector_coerce_unsigned16(addr pos, size_t i, uint16_t *ret);
int vector_coerce_unsigned32(addr pos, size_t i, uint32_t *ret);
#ifdef LISP_64BIT
int vector_coerce_unsigned64(addr pos, size_t i, uint64_t *ret);
#endif
int vector_coerce_single(addr pos, size_t i, single_float *ret);
int vector_coerce_double(addr pos, size_t i, double_float *ret);
int vector_coerce_long(addr pos, size_t i, long_float *ret);

#endif

