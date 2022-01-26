#include "array_access.h"
#include "array_adjust.h"
#include "array_make.h"
#include "array_sequence.h"
#include "array_vector.h"
#include "bit.h"
#include "call_arrays.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "function.h"
#include "integer.h"
#include "strvect.h"
#include "type_parse.h"

/*
 *  make-array
 */
int make_array_common_(Execute ptr, addr var, addr rest, addr *ret)
{
	addr type, ielem, icont, adj, fill, dto, off;

	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &type))
		type = T;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &ielem))
		ielem = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_CONTENTS, &icont))
		icont = Unbound;
	if (GetKeyArgs(rest, KEYWORD_ADJUSTABLE, &adj))
		adj = Nil;
	if (GetKeyArgs(rest, KEYWORD_FILL_POINTER, &fill))
		fill = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_TO, &dto))
		dto = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off))
		fixnum_heap(&off, 0);
	Return(parse_type_(ptr, &type, type, Nil));
	return array_make_array_(ret, var, type, ielem, icont, adj, fill, dto, off);
}


/*
 *  adjust-array
 */
int adjust_array_common_(Execute ptr, addr pos, addr dim, addr rest, addr *ret)
{
	addr type, ielem, icont, fill, dto, off;

	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &type))
		type = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &ielem))
		ielem = Unbound;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_CONTENTS, &icont))
		icont = Unbound;
	if (GetKeyArgs(rest, KEYWORD_FILL_POINTER, &fill))
		fill = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_TO, &dto))
		dto = Nil;
	if (GetKeyArgs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off))
		fixnum_heap(&off, 0);
	if (type != Unbound) {
		Return(parse_type_(ptr, &type, type, Nil));
	}

	return array_adjust_array_(ret, pos, dim, type, ielem, icont, fill, dto, off);
}


/*
 *  adjustable-array-p
 */
int adjustable_array_p_common_(addr var, int *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return Result(ret, array_adjustable_p(var));

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  aref
 */
int aref_common_(addr var, addr rest, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_aref_(NULL, var, rest, ret);

		case LISPTYPE_STRING:
			return strvect_aref_(NULL, var, rest, ret);

		case LISPTYPE_VECTOR:
			return vector_aref_(var, rest, ret);

		case LISPTYPE_BITVECTOR:
			return bitmemory_aref_(NULL, var, rest, ret);

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  setf-aref
 */
int setf_aref_common_(addr value, addr var, addr rest)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_setf_aref_(var, rest, value);

		case LISPTYPE_STRING:
			return strvect_setf_aref_(var, rest, value);

		case LISPTYPE_VECTOR:
			return vector_setf_aref_(var, rest, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_setf_aref_(var, rest, value);

		default:
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array_dimension
 */
static int array_array_dimension_common_(addr array, addr axis, addr *ret)
{
	int check;
	struct array_struct *str;
	size_t index, dimension;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	dimension = str->dimension;
	if (dimension == 0)
		return fmte_("The array have no dimension.", NULL);

	if (GetIndex_integer(axis, &index)) {
		Return(minusp_integer_(axis, &check));
		if (check)
			return fmte_("Index ~A must be a non-negative integer.", axis, NULL);
		else
			return fmte_("Index ~A is too large.", axis, NULL);
	}

	if (dimension <= index) {
		return fmte_("The dimension index ~A must be less than "
				"the array-dimension limit ~A.", axis, intsizeh(dimension), NULL);
	}
	index = (array_ptrsize(array))[index];
	make_index_integer_heap(ret, index);
	return 0;
}

int array_dimension_common_(addr var, addr axis, addr *ret)
{
	size_t size;

	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_array_dimension_common_(var, axis, ret);

		case LISPTYPE_VECTOR:
			lenarray(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		case LISPTYPE_STRING:
			strvect_length(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(var, &size);
			return vector_array_dimension_(var, axis, size, ret);

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array-dimensions
 */
static void array_array_dimensions_common(addr array, addr *ret)
{
	struct array_struct *str;
	const size_t *data;
	size_t size, i, n;
	addr root, pos;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	root = Nil;
	for (i = 0; i < size; i++) {
		n = size - i - 1;
		make_index_integer_heap(&pos, data[n]);
		cons_heap(&root, pos, root);
	}
	*ret = root;
}

int array_dimensions_common_(addr var, addr *ret)
{
	size_t size;

	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_array_dimensions_common(var, ret);
			break;

		case LISPTYPE_VECTOR:
			lenarray(var, &size);
			vector_array_dimensions(size, ret);
			break;

		case LISPTYPE_STRING:
			strvect_length(var, &size);
			vector_array_dimensions(size, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(var, &size);
			vector_array_dimensions(size, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}

	return 0;
}


/*
 *  array-element-type
 */
int array_element_type_common_(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_get_element_type_(var, ret);

		case LISPTYPE_VECTOR:
			*ret = T;
			break;

		case LISPTYPE_STRING:
			GetConst(COMMON_CHARACTER, ret);
			break;

		case LISPTYPE_BITVECTOR:
			GetConst(COMMON_BIT, ret);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, ARRAY);
	}

	return 0;
}


/*
 *  array-has-fill-pointer-p
 */
int array_has_fill_pointer_p_common_(addr var, int *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return Result(ret, array_fillpointer_p(var));

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(var, ARRAY);
	}
}


/*
 *  array-displacement
 */
static void array_array_displacement_common(addr array, addr *displaced, addr *offset)
{
	struct array_struct *str;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	if (str->displaced) {
		GetArrayInfo(array, ARRAY_INDEX_DISPLACED, displaced);
		make_index_integer_heap(offset, str->offset);
	}
	else {
		*displaced = Nil;
		fixnum_heap(offset, 0);
	}
}

int array_displacement_common_(addr pos, addr *ret, addr *offset)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			array_array_displacement_common(pos, ret, offset);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			*ret = Nil;
			fixnum_heap(offset, 0);
			break;

		default:
			*ret = *offset = Nil;
			return TypeError_(pos, ARRAY);
	}

	return 0;
}


/*
 *  array-in-bounds-p
 */
static int array_array_in_bounds_p_common_(addr array, addr rest, int *ret)
{
	int value;
	struct array_struct *str;
	addr list, pos;
	size_t size, i, check;
	const size_t *data;

	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	value = 1;
	list = rest;
	for (i = 0; i < size; i++) {
		if (list == Nil)
			return fmte_("The subscripts ~S is too few arguments.", rest, NULL);
		if (! consp_getcons(list, &pos, &list))
			return fmte_("Invalid subscripts arguments ~S.", rest, NULL);
		if (! integerp(pos))
			return fmte_("The subscript ~S must be integer type.", pos, NULL);
		if (GetIndex_integer(pos, &check)) {
			/* minus or large value */
			value = 0;
			continue;
		}
		if (data[i] <= check) {
			/* out of range */
			value = 0;
			continue;
		}
	}
	if (list != Nil)
		return fmte_("The subscripts ~S is too many arguments.", rest, NULL);

	return Result(ret, value);
}

int array_in_bounds_p_common_(addr array, addr rest, int *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			*ret = 0;
			return array_array_in_bounds_p_common_(array, rest, ret);

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			return vector_array_in_bounds_p_(rest, size, ret);

		default:
			*ret = 0;
			return TypeError_(array, ARRAY);
	}
}


/*
 *  array-rank
 */
int array_rank_common_(addr pos, addr *ret)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			size = array_dimension_size(pos);
			make_index_integer_heap(ret, size);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			fixnum_heap(ret, 1);
			break;

		default:
			*ret = Nil;
			return TypeError_(pos, ARRAY);
	}

	return 0;
}


/*
 *  array-row-major-index
 */
static int array_array_row_major_index_common_(addr array, addr rest, addr *ret)
{
	size_t size;

	Return(array_arefindex_(array, rest, &size));
	make_index_integer_heap(ret, size);

	return 0;
}

int array_row_major_index_common_(addr array, addr rest, addr *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_array_row_major_index_common_(array, rest, ret);

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			return vector_array_row_major_index_(rest, size, ret);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}


/*
 *  array-total-size
 */
int array_total_size_common_(addr array, addr *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			size = array_total_size(array);
			break;

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			break;

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			break;

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
	make_index_integer_heap(ret, size);
	return 0;
}


/*
 *  arrayp
 */
int arrayp_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  fill-pointer
 */
int fill_pointer_common_(Execute ptr, addr array, addr *ret)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			if (array_fill_pointer(array, ret))
				return call_type_error_fill_pointer_(ptr, array);
			return 0;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, array);

		default:
			*ret = Nil;
			return TypeError_(array, VECTOR);
	}
}

int setf_fill_pointer_common_(Execute ptr, addr value, addr array)
{
	int check;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			Return(array_setf_fill_pointer_(array, value, &check));
			if (check)
				return call_type_error_fill_pointer_(ptr, array);
			return 0;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			return call_type_error_fill_pointer_(ptr, array);

		default:
			return TypeError_(array, VECTOR);
	}
}


/*
 *  row-major-aref
 */
int row_major_aref_common_(addr array, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_get_(NULL, array, size, ret);

		case LISPTYPE_VECTOR:
			return vector_get_(array, size, ret);

		case LISPTYPE_STRING:
			return strvect_get_(NULL, array, size, ret);

		case LISPTYPE_BITVECTOR:
			return bitmemory_get_(NULL, array, size, ret);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}

int setf_row_major_aref_common_(addr value, addr array, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_set_(array, size, value);

		case LISPTYPE_VECTOR:
			return vector_set_(array, size, value);

		case LISPTYPE_STRING:
			return strvect_set_(array, size, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_set_(array, size, value);

		default:
			return TypeError_(array, ARRAY);
	}
}


/*
 *  simple-vector-p
 */
int simple_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_simple_vector_p(var);

		case LISPTYPE_VECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  svref
 */
int svref_common_(addr pos, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return array_get_(NULL, pos, size, ret);

		case LISPTYPE_VECTOR:
			return vector_get_(pos, size, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, SIMPLE_VECTOR);
	}
}

int setf_svref_common_(addr value, addr pos, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return array_set_(pos, size, value);

		case LISPTYPE_VECTOR:
			return vector_set_(pos, size, value);

		default:
			return TypeError_(pos, SIMPLE_VECTOR);
	}
}


/*
 *  vectorp
 */
int vectorp_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_vector_p(var);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  bit
 */
int bit_common_(addr pos, addr rest, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_aref_(NULL, pos, rest, ret);

		case LISPTYPE_ARRAY:
			return array_aref_bit_(NULL, pos, rest, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, ARRAY);
	}
}

int setf_bit_common_(addr value, addr pos, addr rest)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return bitmemory_setf_aref_(pos, rest, value);

		case LISPTYPE_ARRAY:
			return array_setf_aref_bit_(pos, rest, value);

		default:
			return TypeError_(pos, ARRAY);
	}
}


/*
 *  bit-vector-p
 */
int bit_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_bvarrayp(var);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  simple-bit-vector-p
 */
int simple_bit_vector_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return simple_array_bvarrayp(var);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}


/*
 *  bit-and
 */
static fixed bitcalc_and_common(fixed a, fixed b)
{
	return a & b;
}

int bit_and_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_and_common);
}


/*
 *  bit-andc1
 */
static fixed bitcalc_andc1_common(fixed a, fixed b)
{
	return (~a) & b;
}

int bit_andc1_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_andc1_common);
}


/*
 *  bit-andc2
 */
static fixed bitcalc_andc2_common(fixed a, fixed b)
{
	return a & (~b);
}

int bit_andc2_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_andc2_common);
}


/*
 *  bit-eqv
 */
static fixed bitcalc_eqv_common(fixed a, fixed b)
{
	return ~(a ^ b);
}

int bit_eqv_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_eqv_common);
}


/*
 *  bit-ior
 */
static fixed bitcalc_ior_common(fixed a, fixed b)
{
	return a | b;
}

int bit_ior_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_ior_common);
}


/*
 *  bit-nand
 */
static fixed bitcalc_nand_common(fixed a, fixed b)
{
	return ~(a & b);
}

int bit_nand_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_nand_common);
}


/*
 *  bit-nor
 */
static fixed bitcalc_nor_common(fixed a, fixed b)
{
	return ~(a | b);
}

int bit_nor_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_nor_common);
}


/*
 *  bit-orc1
 */
static fixed bitcalc_orc1_common(fixed a, fixed b)
{
	return (~a) | b;
}

int bit_orc1_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_orc1_common);
}


/*
 *  bit-orc2
 */
static fixed bitcalc_orc2_common(fixed a, fixed b)
{
	return a | (~b);
}

int bit_orc2_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_orc2_common);
}


/*
 *  bit-xor
 */
static fixed bitcalc_xor_common(fixed a, fixed b)
{
	return a ^ b;
}

int bit_xor_common_(addr x, addr y, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitcalc_(ret, x, y, opt, bitcalc_xor_common);
}


/*
 *  bit-not
 */
int bit_not_common_(addr x, addr opt, addr *ret)
{
	if (opt == Unbound)
		opt = Nil;
	return array_bitnot_(ret, x, opt);
}

