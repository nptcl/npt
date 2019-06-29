#ifndef __ARRAY_VECTOR_HEADER__
#define __ARRAY_VECTOR_HEADER__

#include "local.h"
#include "typedef.h"

_g void vector_pop_common(addr pos, addr *ret);
_g void vector_push_common(addr value, addr pos, addr *ret);
_g void vector_push_extend_common(addr value, addr pos, addr extension, addr *ret);

_g void vector_get(addr pos, size_t index, addr *ret);
_g void vector_set(addr pos, size_t index, addr value);
_g void vector_aref(addr pos, addr args, addr *ret);
_g void vector_setf_aref(addr pos, addr args, addr value);
_g void vector_array_dimension(addr pos, addr arg, size_t size, addr *ret);
_g void vector_array_dimensions(size_t size, addr *ret);
_g int vector_array_in_bounds_p(addr rest, size_t size);
_g void vector_array_row_major_index(addr rest, size_t size, addr *ret);

_g void vector_signed_uninit(addr *ret, size_t size, enum ARRAY_TYPE type, int bs);
_g void vector_float_uninit(addr *ret, size_t size, enum ARRAY_TYPE type);
_g void vector_signed(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value);
_g void vector_float(addr *ret, size_t size, enum ARRAY_TYPE type, addr value);

_g void vector_setelt(addr pos, size_t index, addr value);
_g void vector_adjust(addr *ret, addr array, size_t size, addr value, addr check);
_g void vector_reverse(LocalRoot local, addr *ret, addr pos);
_g void vector_nreverse(addr *ret, addr pos);

#endif

