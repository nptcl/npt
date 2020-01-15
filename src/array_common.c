#include "array_access.h"
#include "array_adjust.h"
#include "array_make.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "function.h"
#include "integer.h"
#include "type_parse.h"

/*
 *  make-array
 */
_g void make_array_common(Execute ptr, addr var, addr rest, addr *ret)
{
	addr type, ielem, icont, adj, fill, dto, off;

	if (getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &type)) type = T;
	if (getkeyargs(rest, KEYWORD_INITIAL_ELEMENT, &ielem)) ielem = Unbound;
	if (getkeyargs(rest, KEYWORD_INITIAL_CONTENTS, &icont)) icont = Unbound;
	if (getkeyargs(rest, KEYWORD_ADJUSTABLE, &adj)) adj = Nil;
	if (getkeyargs(rest, KEYWORD_FILL_POINTER, &fill)) fill = Nil;
	if (getkeyargs(rest, KEYWORD_DISPLACED_TO, &dto)) dto = Nil;
	if (getkeyargs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off)) off = fixnumh(0);
	if (parse_type(ptr, &type, type, Nil)) return;
	array_make_array(ret, var, type, ielem, icont, adj, fill, dto, off);
}


/*
 *  adjust-array
 */
_g void adjust_array_common(Execute ptr, addr pos, addr dim, addr rest, addr *ret)
{
	addr type, ielem, icont, fill, dto, off;

	if (getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &type)) type = Unbound;
	if (getkeyargs(rest, KEYWORD_INITIAL_ELEMENT, &ielem)) ielem = Unbound;
	if (getkeyargs(rest, KEYWORD_INITIAL_CONTENTS, &icont)) icont = Unbound;
	if (getkeyargs(rest, KEYWORD_FILL_POINTER, &fill)) fill = Nil;
	if (getkeyargs(rest, KEYWORD_DISPLACED_TO, &dto)) dto = Nil;
	if (getkeyargs(rest, KEYWORD_DISPLACED_INDEX_OFFSET, &off)) off = fixnumh(0);
	if (type != Unbound) {
		if (parse_type(ptr, &type, type, Nil)) return;
	}
	array_adjust_array(ret, pos, dim, type, ielem, icont, fill, dto, off);
}


/*
 *  adjustable-array-p
 */
_g int adjustable_array_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_adjustable_p(var);

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			return 0;

		default:
			TypeError(var, ARRAY);
			return 0;
	}
}


/*
 *  aref
 */
_g void aref_common(addr var, addr rest, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_aref(NULL, var, rest, ret);
			break;

		case LISPTYPE_STRING:
			strvect_aref(NULL, var, rest, ret);
			break;

		case LISPTYPE_VECTOR:
			vector_aref(var, rest, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_aref(NULL, var, rest, ret);
			break;

		default:
			TypeError(var, ARRAY);
			*ret = Nil;
			break;
	}
}


/*
 *  setf-aref
 */
_g void setf_aref_common(addr value, addr var, addr rest)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_setf_aref(var, rest, value);
			break;

		case LISPTYPE_STRING:
			strvect_setf_aref(var, rest, value);
			break;

		case LISPTYPE_VECTOR:
			vector_setf_aref(var, rest, value);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_setf_aref(var, rest, value);
			break;

		default:
			TypeError(var, ARRAY);
			break;
	}
}


/*
 *  array_dimension
 */
static void array_array_dimension_common(addr array, addr axis, addr *ret)
{
	struct array_struct *str;
	size_t index, dimension;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	dimension = str->dimension;
	if (dimension == 0) {
		fmte("The array have no dimension.", NULL);
	}
	if (GetIndex_integer(axis, &index)) {
		if (minusp_integer(axis))
			fmte("Index ~A must be a non-negative integer.", axis, NULL);
		else
			fmte("Index ~A is too large.", axis, NULL);
	}
	if (dimension <= index) {
		fmte("The dimension index ~A must be less than "
				"the array-dimension limit ~A.", axis, intsizeh(dimension), NULL);
	}
	index = (array_ptrsize(array))[index];
	make_index_integer_alloc(NULL, ret, index);
}

_g void array_dimension_common(addr var, addr axis, addr *ret)
{
	size_t size;

	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_array_dimension_common(var, axis, ret);
			break;

		case LISPTYPE_VECTOR:
			lenarray(var, &size);
			vector_array_dimension(var, axis, size, ret);
			break;

		case LISPTYPE_STRING:
			strvect_length(var, &size);
			vector_array_dimension(var, axis, size, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(var, &size);
			vector_array_dimension(var, axis, size, ret);
			break;

		default:
			TypeError(var, ARRAY);
			*ret = Nil;
			break;
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
		make_index_integer_alloc(NULL, &pos, data[n]);
		cons_heap(&root, pos, root);
	}
	*ret = root;
}

_g void array_dimensions_common(addr var, addr *ret)
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
			TypeError(var, ARRAY);
			*ret = Nil;
			break;
	}
}


/*
 *  array-element-type
 */
_g void array_element_type_common(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			array_get_element_type(var, ret);
			break;

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
			TypeError(var, ARRAY);
			*ret = Nil;
			break;
	}
}


/*
 *  array-has-fill-pointer-p
 */
_g int array_has_fill_pointer_p_common(addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_ARRAY:
			return array_fillpointer_p(var);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 0;

		default:
			TypeError(var, ARRAY);
			return 0;
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
		make_index_integer_alloc(NULL, offset, str->offset);
	}
	else {
		*displaced = Nil;
		*offset = fixnumh(0);
	}
}

_g void array_displacement_common(addr pos, addr *ret, addr *offset)
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
			TypeError(pos, ARRAY);
			*ret = *offset = Nil;
			break;
	}
}


/*
 *  array-in-bounds-p
 */
static int array_array_in_bounds_p_common(addr array, addr rest)
{
	int result;
	struct array_struct *str;
	addr list, pos;
	size_t size, i, check;
	const size_t *data;

	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	result = 1;
	list = rest;
	for (i = 0; i < size; i++) {
		if (list == Nil)
			fmte("The subscripts ~S is too few arguments.", rest, NULL);
		if (! consp(list))
			fmte("Invalid subscripts arguments ~S.", rest, NULL);
		GetCons(list, &pos, &list);
		if (! integerp(pos))
			fmte("The subscript ~S must be integer type.", pos, NULL);
		if (GetIndex_integer(pos, &check)) {
			/* minus or large value */
			result = 0;
			continue;
		}
		if (data[i] <= check) {
			/* out of range */
			result = 0;
			continue;
		}
	}
	if (list != Nil)
		fmte("The subscripts ~S is too many arguments.", rest, NULL);

	return result;
}

_g int array_in_bounds_p_common(addr array, addr rest)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_array_in_bounds_p_common(array, rest);

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			return vector_array_in_bounds_p(rest, size);

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			return vector_array_in_bounds_p(rest, size);

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			return vector_array_in_bounds_p(rest, size);

		default:
			TypeError(array, ARRAY);
			return 0;
	}
}


/*
 *  array-rank
 */
_g void array_rank_common(addr pos, addr *ret)
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
			TypeError(pos, ARRAY);
			*ret = Nil;
			break;
	}
}


/*
 *  array-row-major-index
 */
static void array_array_row_major_index_common(addr array, addr rest, addr *ret)
{
	size_t value = array_arefindex(array, rest);
	*ret = intsizeh(value);
}

_g void array_row_major_index_common(addr array, addr rest, addr *ret)
{
	size_t size;

	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_array_row_major_index_common(array, rest, ret);
			break;

		case LISPTYPE_VECTOR:
			lenarray(array, &size);
			vector_array_row_major_index(rest, size, ret);
			break;

		case LISPTYPE_STRING:
			strvect_length(array, &size);
			vector_array_row_major_index(rest, size, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_length(array, &size);
			vector_array_row_major_index(rest, size, ret);
			break;

		default:
			TypeError(array, ARRAY);
			*ret = Nil;
			break;
	}
}


/*
 *  array-total-size
 */
_g void array_total_size_common(addr array, addr *ret)
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
			TypeError(array, ARRAY);
			size = 0;
			break;
	}
	make_index_integer_heap(ret, size);
}


/*
 *  arrayp
 */
_g int arrayp_common(addr var)
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
_g void fill_pointer_common(addr array, addr *ret)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			if (array_fill_pointer(array, ret))
				type_error_fill_pointer(array);
			break;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			type_error_fill_pointer(array);
			*ret = Nil;
			break;

		default:
			TypeError(array, VECTOR);
			*ret = Nil;
			break;
	}
}

_g void setf_fill_pointer_common(addr value, addr array)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			if (array_setf_fill_pointer(array, value))
				type_error_fill_pointer(array);
			break;

		case LISPTYPE_STRING:
		case LISPTYPE_VECTOR:
		case LISPTYPE_BITVECTOR:
			type_error_fill_pointer(array);
			break;

		default:
			TypeError(array, VECTOR);
			break;
	}
}


/*
 *  row-major-aref
 */
_g void row_major_aref_common(addr array, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		fmte("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_get(NULL, array, size, ret);
			break;

		case LISPTYPE_VECTOR:
			vector_get(array, size, ret);
			break;

		case LISPTYPE_STRING:
			strvect_get(NULL, array, size, ret);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_get(NULL, array, size, ret);
			break;

		default:
			TypeError(array, ARRAY);
			*ret = Nil;
			break;
	}
}

_g void setf_row_major_aref_common(addr value, addr array, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		fmte("Index ~A is too large.", index, NULL);
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_set(array, size, value);
			break;

		case LISPTYPE_VECTOR:
			vector_set(array, size, value);
			break;

		case LISPTYPE_STRING:
			strvect_set(array, size, value);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_set(array, size, value);
			break;

		default:
			TypeError(array, ARRAY);
			break;
	}
}


/*
 *  simple-vector-p
 */
_g int simple_vector_p_common(addr var)
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
_g void svref_common(addr pos, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		fmte("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			array_get(NULL, pos, size, ret);
			break;

		case LISPTYPE_VECTOR:
			vector_get(pos, size, ret);
			break;

		default:
			TypeError(pos, SIMPLE_VECTOR);
			*ret = Nil;
			break;
	}
}

_g void setf_svref_common(addr value, addr pos, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		fmte("Index ~A is too large.", index, NULL);
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			array_set(pos, size, value);
			break;

		case LISPTYPE_VECTOR:
			vector_set(pos, size, value);
			break;

		default:
			TypeError(pos, SIMPLE_VECTOR);
			break;
	}
}


/*
 *  vectorp
 */
_g int vectorp_common(addr var)
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
_g void bit_common(addr pos, addr rest, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			bitmemory_aref(NULL, pos, rest, ret);
			break;

		case LISPTYPE_ARRAY:
			array_aref_bit(NULL, pos, rest, ret);
			break;

		default:
			TypeError(pos, ARRAY);
			*ret = Nil;
			break;
	}
}

_g void setf_bit_common(addr value, addr pos, addr rest)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			bitmemory_setf_aref(pos, rest, value);
			break;

		case LISPTYPE_ARRAY:
			array_setf_aref_bit(pos, rest, value);
			break;

		default:
			TypeError(pos, ARRAY);
			break;
	}
}


/*
 *  bit-vector-p
 */
_g int bit_vector_p_common(addr var)
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
_g int simple_bit_vector_p_common(addr var)
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

