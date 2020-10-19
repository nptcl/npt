#include <math.h>
#include "arch.h"
#include "array.h"
#include "array_access.h"
#include "array_adjust.h"
#include "array_make.h"
#include "array_vector.h"
#include "bit.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "integer.h"
#include "local.h"
#include "sequence.h"
#include "strtype.h"
#include "strvect.h"
#include "type_table.h"
#include "type_upgraded.h"

/*
 *  adjust-array
 */
static int array_adjust_move_default_(addr pos, size_t index, addr value)
{
	struct array_struct *str;

	/* :initial-element */
	if (value != Unbound)
		return array_set_(pos, index, value);

	/* default value */
	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			return array_set_character_(pos, index, 0);

#ifdef LISP_DEBUG
		case ARRAY_TYPE_BIT:
			return array_set_bit_(pos, index, 1);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			fixnum_heap(&value, 88);
			return array_set_(pos, index, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return array_set_single_(pos, index, nanf(""));

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return array_set_double_(pos, index, nan(""));

		case ARRAY_TYPE_LONG_FLOAT:
			return array_set_long_(pos, index, nanl(""));
#endif

		default:
			return 0;
	}
}

static int array_index_dimension_(const size_t *data,
		const size_t *dimension, size_t depth, size_t *ret)
{
	size_t i, value;

	/*  [1] n1
	 *  [2] s2*... + n2
	 *  [3] s3*... + n3
	 *  [4] s4*... + n4
	 */
	if (depth == 0)
		return Result(ret, 1);
	if (depth == 1)
		return Result(ret, data[0]);

	value = data[0];
	for (i = 1; i < depth; i++) {
		if (multisafe_size(value, dimension[i], &value)) {
			*ret = 0;
			return fmte_("Too large index value.", NULL);
		}
		if (plussafe_size(value, data[i], &value)) {
			*ret = 0;
			return fmte_("Too large index value.", NULL);
		}
	}

	return Result(ret, value);
}

static int array_adjust_move_p(addr array, const size_t *data)
{
	struct array_struct *str;
	const size_t *bound;
	size_t size, i;

	str = ArrayInfoStruct(array);
	bound = array_ptrsize(array);
	size = str->dimension;
	for (i = 0; i < size; i++) {
		if (bound[i] <= data[i])
			return 0;
	}

	return 1;
}

static int array_adjust_move_element_(
		addr pos, addr array, addr initial, const size_t *data)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t index1, index2, depth;

	str1 = ArrayInfoStruct(pos);
	data1 = array_ptrsize(pos);
	depth = str1->dimension;
	Return(array_index_dimension_(data, data1, depth, &index1));
	if (! array_adjust_move_p(array, data))
		return array_adjust_move_default_(pos, index1, initial);
	data2 = array_ptrsize(array);
	Return(array_index_dimension_(data, data2, depth, &index2));
	return array_setget_(pos, index1, array, index2);
}

static int array_adjust_move_depth_(
		addr pos, addr array, addr initial, size_t *data, size_t depth)
{
	struct array_struct *str;
	const size_t *data1;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->dimension <= depth)
		return array_adjust_move_element_(pos, array, initial, data);

	data1 = array_ptrsize(pos);
	size = data1[depth];
	for (i = 0; i < size; i++) {
		data[depth] = i;
		Return(array_adjust_move_depth_(pos, array, initial, data, depth + 1));
	}

	return 0;
}

static int array_adjust_move_(addr pos, addr array, addr initial)
{
	struct array_struct *str;
	size_t size, *data;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	str = ArrayInfoStruct(pos);
	size = str->dimension;
	push_local(local, &stack);
	data = (size_t *)lowlevel_local(local, IdxSize * size);
	memset(data, 0, IdxSize * size);
	Return(array_adjust_move_depth_(pos, array, initial, data, 0));
	rollback_local(local, stack);

	return 0;
}

static int array_adjust_notnot_(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	if (contents == Unbound)
		return array_adjust_move_(pos, array, initial);
	else
		return array_make_initial_(pos, initial, contents);
}

static int array_adjust_make_(addr pos, addr array, addr initial, addr contents)
{
	int check1, check2;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	check1 = str1->displaced;
	check2 = str2->displaced;
	if (check1 && check2) {
		/* displaced -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else if (check1) {
		/* not -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else if (check2) {
		/* displaced -> not */
		return array_adjust_notnot_(pos, array, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		return array_adjust_notnot_(pos, array, initial, contents);
	}
}

static int array_adjust_element_type_(addr pos, addr type, addr array)
{
	enum ARRAY_TYPE value;
	int size;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (type == Unbound) {
		str1->type = str2->type;
		str1->bytesize = str2->bytesize;
	}
	else {
		Return(upgraded_array_value_(type, &value, &size));
		if (! array_equal_type(str2, value, size))
			return fmte_(":element-type ~S must be equal to base array.", type, NULL);
		str1->type = value;
		str1->bytesize = size;
	}
	array_set_type(pos);

	return 0;
}

static void array_adjust_adjustable(addr pos, addr array)
{
	ArrayInfoStruct(pos)->adjustable = ArrayInfoStruct(array)->adjustable;
}

static int array_adjust_fillpointer_(addr pos, addr array, addr fill)
{
	int check;
	struct array_struct *str1;
	struct array_struct *str2;
	addr type;
	size_t size;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	/* nil */
	if (fill == Nil) {
		if (str2->fillpointer == 0)
			return 0;
		str1->fillpointer = str2->fillpointer;
		str1->front = str2->front;
		goto fill_check;
	}
	/* t */
	if (fill == T) {
		str1->fillpointer = 1;
		str1->front = str1->size;
		goto fill_check;
	}
	/* integer */
	if (integerp(fill)) {
		Return(minusp_integer_(fill, &check));
		if (check)
			return fmte_("fill-pointer ~A must be a non-negative integer.", fill, NULL);
		if (GetIndex_integer(fill, &size))
			return fmte_("fill-pointer ~A is too large.", fill, NULL);
		str1->fillpointer = 1;
		str1->front = size;
		goto fill_check;
	}
	/* type error */
	return fmte_("Invalid fill-pointer value ~S.", fill, NULL);

fill_check:
	if (str2->fillpointer == 0) {
		GetTypeTable(&type, Array);
		return call_type_error_va_(NULL, array, type,
				"The argument ~S must be a fill-pointer array.", array, NULL);
	}
	if (str1->dimension != 1)
		return fmte_("fill-pointer array must be a 1 dimensional.", NULL);
	if (str1->size < str1->front) {
		return fmte_("fill-pointer ~A must be less than array size ~A.",
				intsizeh(str1->front), intsizeh(str1->size), NULL);
	}

	return 0;
}

static int array_adjust_dimension_(addr pos, addr array)
{
	struct array_struct *str1;
	struct array_struct *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (str1->dimension != str2->dimension) {
		return fmte_("Array rank ~S must be equal to base array ~S.",
				intsizeh(str1->dimension),
				intsizeh(str2->dimension), NULL);
	}

	return 0;
}

static void array_adjust_replace(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	int i;
	addr temp;

	str1 = ArrayInfoStruct(array);
	str2 = ArrayInfoStruct(pos);
	*str1 = *str2;

	for (i = 0; i < ARRAY_INDEX_SIZE; i++) {
		GetArrayInfo(pos, i, &temp);
		SetArrayInfo(array, i, temp);
	}
}

static void array_adjust_result(addr pos, addr array, addr *ret)
{
	if (array_adjustable_p(array)) {
		array_adjust_replace(pos, array);
		*ret = array;
	}
	else {
		*ret = pos;
	}
}

static int array_adjust_arraytype_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;

	CheckType(array, LISPTYPE_ARRAY);
	array_empty_heap(&pos);
	Return(array_adjust_element_type_(pos, type, array));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	array_adjust_adjustable(pos, array);
	Return(array_adjust_fillpointer_(pos, array, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	Return(array_adjust_dimension_(pos, array));
	Return(array_adjust_make_(pos, array, initial, contents));
	array_adjust_result(pos, array, ret);

	return 0;
}


/*
 *  array_adjust_simple
 */
static int array_adjust_simple_contents_list_(addr pos, size_t size, addr list)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return_getcons(list, &value, &list);
		Return(setelt_sequence_(pos, i, value));
	}

	return 0;
}

static int array_adjust_simple_contents_sequence_(addr pos, size_t size, addr x)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, x, i, &value));
		Return(setelt_sequence_(pos, i, value));
	}

	return 0;
}

static int array_adjust_simple_contents_(addr pos, size_t size, addr contents)
{
	size_t len;

	Return(length_sequence_(contents, 1, &len));
	if (size != len)
		return fmte_("Mismatch :displaced-to ~S length.", pos, NULL);
	if (listp(contents))
		return array_adjust_simple_contents_list_(pos, size, contents);
	else
		return array_adjust_simple_contents_sequence_(pos, size, contents);
}

static int array_adjust_simple_default_(addr pos, size_t index, addr initial)
{
	/* :initial-element */
	if (initial != Unbound)
		return setelt_sequence_(pos, index, initial);

	/* default value */
	switch (GetType(pos)) {
		case LISPTYPE_STRING:
			return strvect_setc_(pos, index, 0);

#ifdef LISP_DEBUG
		case LISPTYPE_BITVECTOR:
			return bitmemory_setint_(pos, index, 1);

		case LISPTYPE_VECTOR:
			setarray(pos, index, T);
			break;
#endif

		default:
			break;
	}

	return 0;
}

static int array_adjust_simple_move_(addr pos, addr array, size_t size, addr initial)
{
	addr value;
	size_t check, i;

	Return(length_sequence_(array, 1, &check));
	for (i = 0; i < size; i++) {
		if (i < check) {
			Return(getelt_sequence_(NULL, array, i, &value));
			Return(setelt_sequence_(pos, i, value));
		}
		else {
			Return(array_adjust_simple_default_(pos, i, initial));
		}
	}

	return 0;
}

static int array_adjust_simple_(
		addr pos, addr array, size_t size, addr initial, addr contents)
{
	if (contents != Unbound)
		return array_adjust_simple_contents_(pos, size, contents);
	else
		return array_adjust_simple_move_(pos, array, size, initial);
}


/*
 *  array_adjust_sequence
 */
static int array_adjust_vector_type_(addr pos, addr type, enum ARRAY_TYPE check)
{
	int size;
	enum ARRAY_TYPE value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type == Unbound) {
		str->type = check;
		str->bytesize = 0;
	}
	else {
		Return(upgraded_array_value_(type, &value, &size));
		if (check != value)
			return fmte_(":element-type ~S must be equal to base array.", type, NULL);
		str->type = value;
		str->bytesize = 0;
	}
	array_set_type(pos);

	return 0;
}

static int array_adjust_vector_move_(addr pos, addr array, addr initial)
{
	struct array_struct *str;
	addr value;
	size_t size1, size2, i;

	str = ArrayInfoStruct(pos);
	size1 = str->size;
	Return(length_sequence_(array, 0, &size2));
	for (i = 0; i < size1; i++) {
		if (i < size2) {
			Return(getelt_sequence_(NULL, array, i, &value));
			Return(setelt_sequence_(pos, i, value));
		}
		else {
			Return(array_adjust_move_default_(pos, i, initial));
		}
	}

	return 0;
}

static int array_adjust_vector_not_(
		addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	if (contents == Unbound)
		return array_adjust_vector_move_(pos, array, initial);
	else
		return array_make_initial_(pos, initial, contents);
}

static int array_adjust_vector_make_(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced) {
		/* not -> displaced */
		return array_make_initial_(pos, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		return array_adjust_vector_not_(pos, array, initial, contents);
	}
}

static void array_adjust_vector_adjustable(addr pos)
{
	ArrayInfoStruct(pos)->adjustable = 0;
}

static int array_adjust_vector_fillpointer_(addr pos, addr array, addr fill)
{
	addr type;

	if (fill != Nil) {
		GetTypeTable(&type, Array);
		return call_type_error_va_(NULL, array, type,
				"The argument ~S must be a fill-pointer array.", array, NULL);
	}
	ArrayInfoStruct(pos)->fillpointer = 0;
	return 0;
}

static int array_adjust_sequence_(addr *ret, addr array, addr dimension,
		addr type, enum ARRAY_TYPE type_value,
		addr initial, addr contents, addr fill, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	Return(array_adjust_vector_type_(pos, type, type_value));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	array_adjust_vector_adjustable(pos);
	Return(array_adjust_vector_fillpointer_(pos, array, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	Return(array_adjust_vector_make_(pos, array, initial, contents));

	return Result(ret, pos);
}


/*
 *  array_adjust_array
 */
static int array_adjust_vector_check_(addr pos, size_t *ret)
{
	if (pos == Nil) {
		*ret = 0;
		return fmte_("Array rank must be a 1, but 0.", NULL);
	}
	if (singlep(pos))
		GetCar(pos, &pos);
	if (integerp(pos)) {
		if (GetIndex_integer(pos, ret)) {
			*ret = 0;
			return fmte_("Dimension ~A is too large.", pos, NULL);
		}
		return 0;
	}

	/* error */
	*ret = 0;
	if (consp(pos))
		return fmte_("Array rank must be a 1.", NULL);
	else
		return fmte_("Invalid pos type ~S.", pos, NULL);
}

static int array_adjust_simple_check(addr pos, addr fill, addr displaced)
{
	if (fill != Nil || displaced != Nil)
		return 0;
	if (! arrayp(pos))
		return 1;
	return ArrayInfoStruct(pos)->adjustable == 0;
}

static int array_adjust_vector_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (array_adjust_simple_check(array, fill, displaced)) {
		vector_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_T, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

static int array_adjust_string_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (initial != Unbound && characterp(initial) == 0) {
		*ret = Nil;
		return fmte_(":initial-element ~S must be a character type.", initial, NULL);
	}
	if (array_adjust_simple_check(array, fill, displaced)) {
		strvect_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_CHARACTER, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

static int array_adjust_bitvector_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	Return(array_adjust_vector_check_(dimension, &size));
	if (initial != Unbound && bitp(initial) == 0) {
		*ret = Nil;
		return fmte_(":initial-element ~S must be a bit type (0 or 1).", initial, NULL);
	}
	if (array_adjust_simple_check(array, fill, displaced)) {
		bitmemory_heap(&pos, size);
		Return(array_adjust_simple_(pos, array, size, initial, contents));
	}
	else {
		Return(array_adjust_sequence_(&pos, array, dimension, type,
					ARRAY_TYPE_BIT, initial, contents, fill, displaced, offset));
	}

	return Result(ret, pos);
}

_g int array_adjust_array_(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			return array_adjust_arraytype_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_VECTOR:
			return array_adjust_vector_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_STRING:
			return array_adjust_string_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		case LISPTYPE_BITVECTOR:
			return array_adjust_bitvector_(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);

		default:
			*ret = Nil;
			return TypeError_(array, ARRAY);
	}
}

