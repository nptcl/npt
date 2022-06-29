#ifndef __CALL_ARRAYS_HEADER__
#define __CALL_ARRAYS_HEADER__

#include "local.h"
#include "execute.h"
#include "typedef.h"

#define make_array_common_ _n(make_array_common_)
#define adjust_array_common_ _n(adjust_array_common_)
#define adjustable_array_p_common_ _n(adjustable_array_p_common_)
#define aref_common_ _n(aref_common_)
#define setf_aref_common_ _n(setf_aref_common_)
#define array_dimension_common_ _n(array_dimension_common_)
#define array_dimensions_common_ _n(array_dimensions_common_)
#define array_element_type_common_ _n(array_element_type_common_)
#define array_has_fill_pointer_p_common_ _n(array_has_fill_pointer_p_common_)
#define array_displacement_common_ _n(array_displacement_common_)
#define array_in_bounds_p_common_ _n(array_in_bounds_p_common_)
#define array_rank_common_ _n(array_rank_common_)
#define array_row_major_index_common_ _n(array_row_major_index_common_)
#define array_total_size_common_ _n(array_total_size_common_)
#define arrayp_common _n(arrayp_common)
#define fill_pointer_common_ _n(fill_pointer_common_)
#define setf_fill_pointer_common_ _n(setf_fill_pointer_common_)
#define row_major_aref_common_ _n(row_major_aref_common_)
#define setf_row_major_aref_common_ _n(setf_row_major_aref_common_)
#define simple_vector_p_common _n(simple_vector_p_common)
#define svref_common_ _n(svref_common_)
#define setf_svref_common_ _n(setf_svref_common_)
#define vectorp_common _n(vectorp_common)
#define bit_common_ _n(bit_common_)
#define setf_bit_common_ _n(setf_bit_common_)
#define bit_vector_p_common _n(bit_vector_p_common)
#define simple_bit_vector_p_common _n(simple_bit_vector_p_common)
#define bit_and_common_ _n(bit_and_common_)
#define bit_andc1_common_ _n(bit_andc1_common_)
#define bit_andc2_common_ _n(bit_andc2_common_)
#define bit_eqv_common_ _n(bit_eqv_common_)
#define bit_ior_common_ _n(bit_ior_common_)
#define bit_nand_common_ _n(bit_nand_common_)
#define bit_nor_common_ _n(bit_nor_common_)
#define bit_orc1_common_ _n(bit_orc1_common_)
#define bit_orc2_common_ _n(bit_orc2_common_)
#define bit_xor_common_ _n(bit_xor_common_)
#define bit_not_common_ _n(bit_not_common_)

int make_array_common_(Execute ptr, addr var, addr rest, addr *ret);
int adjust_array_common_(Execute ptr, addr pos, addr dim, addr rest, addr *ret);
int adjustable_array_p_common_(addr var, int *ret);
int aref_common_(addr var, addr rest, addr *ret);
int setf_aref_common_(addr value, addr var, addr rest);
int array_dimension_common_(addr var, addr axis, addr *ret);
int array_dimensions_common_(addr var, addr *ret);
int array_element_type_common_(Execute ptr, addr var, addr *ret);
int array_has_fill_pointer_p_common_(addr var, int *ret);
int array_displacement_common_(addr pos, addr *ret, addr *offset);
int array_in_bounds_p_common_(addr array, addr rest, int *ret);
int array_rank_common_(addr pos, addr *ret);
int array_row_major_index_common_(addr array, addr rest, addr *ret);
int array_total_size_common_(addr array, addr *ret);
int arrayp_common(addr var);
int fill_pointer_common_(Execute ptr, addr array, addr *ret);
int setf_fill_pointer_common_(Execute ptr, addr value, addr array);
int row_major_aref_common_(addr array, addr index, addr *ret);
int setf_row_major_aref_common_(addr value, addr array, addr index);
int simple_vector_p_common(addr var);
int svref_common_(addr pos, addr index, addr *ret);
int setf_svref_common_(addr value, addr pos, addr index);
int vectorp_common(addr var);
int bit_common_(addr pos, addr rest, addr *ret);
int setf_bit_common_(addr value, addr pos, addr rest);
int bit_vector_p_common(addr var);
int simple_bit_vector_p_common(addr var);
int bit_and_common_(addr x, addr y, addr opt, addr *ret);
int bit_andc1_common_(addr x, addr y, addr opt, addr *ret);
int bit_andc2_common_(addr x, addr y, addr opt, addr *ret);
int bit_eqv_common_(addr x, addr y, addr opt, addr *ret);
int bit_ior_common_(addr x, addr y, addr opt, addr *ret);
int bit_nand_common_(addr x, addr y, addr opt, addr *ret);
int bit_nor_common_(addr x, addr y, addr opt, addr *ret);
int bit_orc1_common_(addr x, addr y, addr opt, addr *ret);
int bit_orc2_common_(addr x, addr y, addr opt, addr *ret);
int bit_xor_common_(addr x, addr y, addr opt, addr *ret);
int bit_not_common_(addr x, addr opt, addr *ret);

#endif

