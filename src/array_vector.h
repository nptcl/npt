#ifndef __ARRAY_VECTOR_HEADER__
#define __ARRAY_VECTOR_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

_g int vector_type_p(addr pos);
_g int vector_pop_common_(Execute ptr, addr pos, addr *ret);
_g int vector_push_common_(Execute ptr, addr value, addr pos, addr *ret);
_g int vector_push_extend_common_(Execute ptr,
		addr value, addr pos, addr extension, addr *ret);

_g int vector_get_(addr pos, size_t index, addr *ret);
_g int vector_set_(addr pos, size_t index, addr value);
_g int vector_aref_(addr pos, addr args, addr *ret);
_g int vector_setf_aref_(addr pos, addr args, addr value);
_g int vector_array_dimension_(addr pos, addr arg, size_t size, addr *ret);
_g void vector_array_dimensions(size_t size, addr *ret);
_g int vector_array_in_bounds_p_(addr rest, size_t size, int *ret);
_g int vector_array_row_major_index_(addr rest, size_t size, addr *ret);

_g int vector_signed_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs);
_g int vector_float_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type);
_g int vector_signed_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value);
_g int vector_float_(addr *ret, size_t size, enum ARRAY_TYPE type, addr value);

_g int vector_setelt_(addr pos, size_t index, addr value);
_g void vector_reverse(LocalRoot local, addr *ret, addr pos);
_g void vector_nreverse(addr *ret, addr pos);

#endif

