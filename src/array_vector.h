#ifndef __ARRAY_VECTOR_HEADER__
#define __ARRAY_VECTOR_HEADER__

#include "local.h"
#include "typedef.h"

void vector_pop_common(addr pos, addr *ret);
void vector_push_common(addr value, addr pos, addr *ret);
void vector_push_extend_common(addr value, addr pos, addr extension, addr *ret);

void vector_get(addr pos, size_t index, addr *ret);
void vector_set(addr pos, size_t index, addr value);
void vector_aref(addr pos, addr args, addr *ret);
void vector_setf_aref(addr pos, addr args, addr value);
void vector_array_dimension(addr pos, addr arg, size_t size, addr *ret);
void vector_array_dimensions(size_t size, addr *ret);
int vector_array_in_bounds_p(addr rest, size_t size);
void vector_array_row_major_index(addr rest, size_t size, addr *ret);

void vector_signed_uninit(addr *ret, size_t size, enum ARRAY_TYPE type, int bs);
void vector_float_uninit(addr *ret, size_t size, enum ARRAY_TYPE type);
void vector_signed(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value);
void vector_float(addr *ret, size_t size, enum ARRAY_TYPE type, addr value);

void vector_setelt(addr pos, size_t index, addr value);
void vector_adjust(addr *ret, addr array, size_t size, addr value, addr check);
void vector_reverse(LocalRoot local, addr *ret, addr pos);
void vector_nreverse(addr *ret, addr pos);

#endif

