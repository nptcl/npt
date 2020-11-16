#ifndef __ARRAY_VECTOR_HEADER__
#define __ARRAY_VECTOR_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define vector_type_p _n(vector_type_p)
#define vector_pop_common_ _n(vector_pop_common_)
#define vector_push_common_ _n(vector_push_common_)
#define vector_push_extend_common_ _n(vector_push_extend_common_)
#define vector_get_ _n(vector_get_)
#define vector_set_ _n(vector_set_)
#define vector_aref_ _n(vector_aref_)
#define vector_setf_aref_ _n(vector_setf_aref_)
#define vector_array_dimension_ _n(vector_array_dimension_)
#define vector_array_dimensions _n(vector_array_dimensions)
#define vector_array_in_bounds_p_ _n(vector_array_in_bounds_p_)
#define vector_array_row_major_index_ _n(vector_array_row_major_index_)
#define vector_signed_uninit_ _n(vector_signed_uninit_)
#define vector_float_uninit_ _n(vector_float_uninit_)
#define vector_signed_ _n(vector_signed_)
#define vector_float_ _n(vector_float_)
#define vector_setelt_ _n(vector_setelt_)
#define vector_reverse _n(vector_reverse)
#define vector_nreverse _n(vector_nreverse)

int vector_type_p(addr pos);
int vector_pop_common_(Execute ptr, addr pos, addr *ret);
int vector_push_common_(Execute ptr, addr value, addr pos, addr *ret);
int vector_push_extend_common_(Execute ptr,
		addr value, addr pos, addr extension, addr *ret);

int vector_get_(addr pos, size_t index, addr *ret);
int vector_set_(addr pos, size_t index, addr value);
int vector_aref_(addr pos, addr args, addr *ret);
int vector_setf_aref_(addr pos, addr args, addr value);
int vector_array_dimension_(addr pos, addr arg, size_t size, addr *ret);
void vector_array_dimensions(size_t size, addr *ret);
int vector_array_in_bounds_p_(addr rest, size_t size, int *ret);
int vector_array_row_major_index_(addr rest, size_t size, addr *ret);

int vector_signed_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs);
int vector_float_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type);
int vector_signed_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value);
int vector_float_(addr *ret, size_t size, enum ARRAY_TYPE type, addr value);

int vector_setelt_(addr pos, size_t index, addr value);
void vector_reverse(LocalRoot local, addr *ret, addr pos);
void vector_nreverse(addr *ret, addr pos);

#endif

