#ifndef __ARRAY_COMMON_HEADER__
#define __ARRAY_COMMON_HEADER__

#include "local.h"
#include "execute.h"
#include "typedef.h"

_g void make_array_common(Execute ptr, addr var, addr rest, addr *ret);
_g void adjust_array_common(Execute ptr, addr pos, addr dim, addr rest, addr *ret);
_g int adjustable_array_p_common(addr var);
_g void aref_common(addr var, addr rest, addr *ret);
_g void setf_aref_common(addr value, addr var, addr rest);
_g void array_dimension_common(addr var, addr axis, addr *ret);
_g void array_dimensions_common(addr var, addr *ret);
_g void array_element_type_common(addr var, addr *ret);
_g int array_has_fill_pointer_p_common(addr var);
_g void array_displacement_common(addr pos, addr *ret, addr *offset);
_g int array_in_bounds_p_common(addr array, addr rest);
_g void array_rank_common(addr pos, addr *ret);
_g void array_row_major_index_common(addr array, addr rest, addr *ret);
_g void array_total_size_common(addr array, addr *ret);
_g int arrayp_common(addr var);
_g void fill_pointer_common(addr array, addr *ret);
_g void setf_fill_pointer_common(addr value, addr array);
_g void row_major_aref_common(addr array, addr index, addr *ret);
_g void setf_row_major_aref_common(addr value, addr array, addr index);
_g int simple_vector_p_common(addr var);
_g void svref_common(addr pos, addr index, addr *ret);
_g void setf_svref_common(addr value, addr pos, addr index);
_g int vectorp_common(addr var);
_g void bit_common(addr pos, addr rest, addr *ret);
_g void setf_bit_common(addr value, addr pos, addr rest);
_g int bit_vector_p_common(addr var);
_g int simple_bit_vector_p_common(addr var);

#endif

