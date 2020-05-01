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
#include "type_upgraded.h"

/*
 *  adjust-array
 */
static void array_adjust_move_default(addr pos, size_t index, addr value)
{
	struct array_struct *str;

	/* :initial-element */
	if (value != Unbound) {
		array_set(pos, index, value);
		return;
	}

	/* default value */
	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			array_set_character(pos, index, 0);
			break;

#ifdef LISP_DEBUG
		case ARRAY_TYPE_BIT:
			array_set_bit(pos, index, 1);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			fixnum_heap(&value, 88);
			array_set(pos, index, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			array_set_single(pos, index, nanf(""));
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			array_set_double(pos, index, nan(""));
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			array_set_long(pos, index, nanl(""));
			break;
#endif

		default:
			break;
	}
}

static size_t array_index_dimension(const size_t *data,
		const size_t *dimension, size_t depth)
{
	size_t i, value;

	/*  [1] n1
	 *  [2] s2*... + n2
	 *  [3] s3*... + n3
	 *  [4] s4*... + n4
	 */
	if (depth == 0)
		return 1;
	if (depth == 1)
		return data[0];

	value = data[0];
	for (i = 1; i < depth; i++) {
		if (multisafe_size(value, dimension[i], &value))
			fmte("Too large index value.", NULL);
		if (plussafe_size(value, data[i], &value))
			fmte("Too large index value.", NULL);
	}

	return value;
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

static void array_adjust_move_element(
		addr pos, addr array, addr initial, const size_t *data)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t index1, index2, depth;

	str1 = ArrayInfoStruct(pos);
	data1 = array_ptrsize(pos);
	depth = str1->dimension;
	index1 = array_index_dimension(data, data1, depth);
	if (! array_adjust_move_p(array, data)) {
		array_adjust_move_default(pos, index1, initial);
		return;
	}
	data2 = array_ptrsize(array);
	index2 = array_index_dimension(data, data2, depth);
	array_setget(pos, index1, array, index2);
}

static void array_adjust_move_depth(
		addr pos, addr array, addr initial, size_t *data, size_t depth)
{
	struct array_struct *str;
	const size_t *data1;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->dimension <= depth) {
		array_adjust_move_element(pos, array, initial, data);
		return;
	}

	data1 = array_ptrsize(pos);
	size = data1[depth];
	for (i = 0; i < size; i++) {
		data[depth] = i;
		array_adjust_move_depth(pos, array, initial, data, depth + 1);
	}
}

static void array_adjust_move(addr pos, addr array, addr initial)
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
	array_adjust_move_depth(pos, array, initial, data, 0);
	rollback_local(local, stack);
}

static void array_adjust_notnot(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_allocate(NULL, pos, str);
	if (contents == Unbound)
		array_adjust_move(pos, array, initial);
	else
		array_make_initial(pos, initial, contents);
}

static void array_adjust_make(addr pos, addr array, addr initial, addr contents)
{
	int check1, check2;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	check1 = str1->displaced;
	check2 = str2->displaced;
	if (check1 && check2) {
		/* displaced -> displaced */
		array_make_initial(pos, initial, contents);
	}
	else if (check1) {
		/* not -> displaced */
		array_make_initial(pos, initial, contents);
	}
	else if (check2) {
		/* displaced -> not */
		array_adjust_notnot(pos, array, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		array_adjust_notnot(pos, array, initial, contents);
	}
}

static void array_adjust_element_type(addr pos, addr type, addr array)
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
		upgraded_array_value(type, &value, &size);
		if (! array_equal_type(str2, value, size))
			fmte(":element-type ~S must be equal to base array.", type, NULL);
		str1->type = value;
		str1->bytesize = size;
	}
	array_set_type(pos);
}

static void array_adjust_adjustable(addr pos, addr array)
{
	ArrayInfoStruct(pos)->adjustable = ArrayInfoStruct(array)->adjustable;
}

static void array_adjust_fillpointer(addr pos, addr array, addr fill)
{
	struct array_struct *str1;
	struct array_struct *str2;
	size_t size;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	/* nil */
	if (fill == Nil) {
		if (str2->fillpointer == 0)
			return;
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
		if (minusp_integer(fill))
			fmte("fill-pointer ~A must be a non-negative integer.", fill, NULL);
		if (GetIndex_integer(fill, &size))
			fmte("fill-pointer ~A is too large.", fill, NULL);
		str1->fillpointer = 1;
		str1->front = size;
		goto fill_check;
	}
	/* type error */
	fmte("Invalid fill-pointer value ~S.", fill, NULL);
	return;

fill_check:
	if (str2->fillpointer == 0)
		fmte("The argument ~S must be a fill-pointer array.", array, NULL);
	if (str1->dimension != 1)
		fmte("fill-pointer array must be a 1 dimensional.", NULL);
	if (str1->size < str1->front) {
		fmte("fill-pointer ~A must be less than array size ~A.",
				intsizeh(str1->front), intsizeh(str1->size), NULL);
	}
}

static void array_adjust_dimension(addr pos, addr array)
{
	struct array_struct *str1;
	struct array_struct *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (str1->dimension != str2->dimension) {
		fmte("Array rank ~S must be equal to base array ~S.",
				intsizeh(str1->dimension),
				intsizeh(str2->dimension), NULL);
	}
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

static void array_adjust_arraytype(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;

	CheckType(array, LISPTYPE_ARRAY);
	array_empty_heap(&pos);
	array_adjust_element_type(pos, type, array);
	array_set_element_size(pos);
	array_set_dimension(pos, dimension);
	array_adjust_adjustable(pos, array);
	array_adjust_fillpointer(pos, array, fill);
	array_set_displaced(pos, displaced, offset);
	array_set_simple(pos);
	array_adjust_dimension(pos, array);
	array_adjust_make(pos, array, initial, contents);
	array_adjust_result(pos, array, ret);
}


/*
 *  array_adjust_simple
 */
static void array_adjust_simple_contents_list(addr pos, size_t size, addr list)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		getcons(list, &value, &list);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_simple_contents_sequence(addr pos, size_t size, addr x)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		getelt_sequence(NULL, x, i, &value);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_simple_contents(addr pos, size_t size, addr contents)
{
	if (size != length_sequence(contents, 1))
		fmte("Mismatch :displaced-to ~S length.", pos, NULL);
	if (listp(contents))
		array_adjust_simple_contents_list(pos, size, contents);
	else
		array_adjust_simple_contents_sequence(pos, size, contents);
}

static void array_adjust_simple_default(addr pos, size_t index, addr initial)
{
	/* :initial-element */
	if (initial != Unbound) {
		setelt_sequence(pos, index, initial);
		return;
	}

	/* default value */
	switch (GetType(pos)) {
		case LISPTYPE_STRING:
			strvect_setc(pos, index, 0);
			break;

#ifdef LISP_DEBUG
		case LISPTYPE_BITVECTOR:
			bitmemory_setint(pos, index, 1);
			break;

		case LISPTYPE_VECTOR:
			setarray(pos, index, T);
			break;
#endif

		default:
			break;
	}
}

static void array_adjust_simple_move(addr pos, addr array, size_t size, addr initial)
{
	addr value;
	size_t check, i;

	check = length_sequence(array, 1);
	for (i = 0; i < size; i++) {
		if (i < check) {
			getelt_sequence(NULL, array, i, &value);
			setelt_sequence(pos, i, value);
		}
		else {
			array_adjust_simple_default(pos, i, initial);
		}
	}
}

static void array_adjust_simple(
		addr pos, addr array, size_t size, addr initial, addr contents)
{
	if (contents != Unbound)
		array_adjust_simple_contents(pos, size, contents);
	else
		array_adjust_simple_move(pos, array, size, initial);
}


/*
 *  array_adjust_sequence
 */
static void array_adjust_vector_type(addr pos, addr type, enum ARRAY_TYPE check)
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
		upgraded_array_value(type, &value, &size);
		if (check != value)
			fmte(":element-type ~S must be equal to base array.", type, NULL);
		str->type = value;
		str->bytesize = 0;
	}
	array_set_type(pos);
}

static void array_adjust_vector_move(addr pos, addr array, addr initial)
{
	struct array_struct *str;
	addr value;
	size_t size1, size2, i;

	str = ArrayInfoStruct(pos);
	size1 = str->size;
	size2 = length_sequence(array, 0);
	for (i = 0; i < size1; i++) {
		if (i < size2) {
			getelt_sequence(NULL, array, i, &value);
			setelt_sequence(pos, i, value);
		}
		else {
			array_adjust_move_default(pos, i, initial);
		}
	}
}

static void array_adjust_vector_not(
		addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_allocate(NULL, pos, str);
	if (contents == Unbound)
		array_adjust_vector_move(pos, array, initial);
	else
		array_make_initial(pos, initial, contents);
}

static void array_adjust_vector_make(addr pos, addr array, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced) {
		/* not -> displaced */
		array_make_initial(pos, initial, contents);
	}
	else {
		/* not-displaced -> not-displaced */
		array_adjust_vector_not(pos, array, initial, contents);
	}
}

static void array_adjust_vector_adjustable(addr pos)
{
	ArrayInfoStruct(pos)->adjustable = 0;
}

static void array_adjust_vector_fillpointer(addr pos, addr array, addr fill)
{
	if (fill != Nil)
		fmte("The argument ~S must be a fill-pointer array.", array, NULL);
	ArrayInfoStruct(pos)->fillpointer = 0;
}

static void array_adjust_sequence(addr *ret, addr array, addr dimension,
		addr type, enum ARRAY_TYPE type_value,
		addr initial, addr contents, addr fill, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	array_adjust_vector_type(pos, type, type_value);
	array_set_element_size(pos);
	array_set_dimension(pos, dimension);
	array_adjust_vector_adjustable(pos);
	array_adjust_vector_fillpointer(pos, array, fill);
	array_set_displaced(pos, displaced, offset);
	array_set_simple(pos);
	array_adjust_vector_make(pos, array, initial, contents);
	*ret = pos;
}


/*
 *  array_adjust_array
 */
static void array_adjust_vector_check(addr pos, size_t *ret)
{
	if (pos == Nil)
		fmte("Array rank must be a 1, but 0.", NULL);
	if (singlep(pos))
		GetCar(pos, &pos);
	if (integerp(pos)) {
		if (GetIndex_integer(pos, ret))
			fmte("Dimension ~A is too large.", pos, NULL);
		return;
	}
	if (consp(pos))
		fmte("Array rank must be a 1.", NULL);
	else
		fmte("Invalid pos type ~S.", pos, NULL);
}

static int array_adjust_simple_check(addr pos, addr fill, addr displaced)
{
	if (fill != Nil || displaced != Nil)
		return 0;
	if (! arrayp(pos))
		return 1;
	return ArrayInfoStruct(pos)->adjustable == 0;
}

static void array_adjust_vector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vector_check(dimension, &size);
	if (array_adjust_simple_check(array, fill, displaced)) {
		vector_heap(&pos, size);
		array_adjust_simple(pos, array, size, initial, contents);
	}
	else {
		array_adjust_sequence(&pos, array, dimension, type, ARRAY_TYPE_T,
				initial, contents, fill, displaced, offset);
	}
	*ret = pos;
}

static void array_adjust_string(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vector_check(dimension, &size);
	if (initial != Unbound && characterp(initial) == 0)
		fmte(":initial-element ~S must be a character type.", initial, NULL);
	if (array_adjust_simple_check(array, fill, displaced)) {
		strvect_heap(&pos, size);
		array_adjust_simple(pos, array, size, initial, contents);
	}
	else {
		array_adjust_sequence(&pos, array, dimension, type, ARRAY_TYPE_CHARACTER,
				initial, contents, fill, displaced, offset);
	}
	*ret = pos;
}

static void array_adjust_bitvector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vector_check(dimension, &size);
	if (initial != Unbound && bitp(initial) == 0)
		fmte(":initial-element ~S must be a bit type (0 or 1).", initial, NULL);
	if (array_adjust_simple_check(array, fill, displaced)) {
		bitmemory_heap(&pos, size);
		array_adjust_simple(pos, array, size, initial, contents);
	}
	else {
		array_adjust_sequence(&pos, array, dimension, type, ARRAY_TYPE_BIT,
				initial, contents, fill, displaced, offset);
	}
	*ret = pos;
}

_g void array_adjust_array(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fill, addr displaced, addr offset)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_adjust_arraytype(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);
			break;

		case LISPTYPE_VECTOR:
			array_adjust_vector(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);
			break;

		case LISPTYPE_STRING:
			array_adjust_string(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);
			break;

		case LISPTYPE_BITVECTOR:
			array_adjust_bitvector(ret, array, dimension,
					type, initial, contents, fill, displaced, offset);
			break;

		default:
			TypeError(array, ARRAY);
			break;
	}
}

