#include "arch.h"
#include "array.h"
#include "array_adjust.h"
#include "array_object.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "cons.h"
#include "integer.h"
#include "local.h"
#include "sequence.h"
#include "type_upgraded.h"

/*
 *  adjust-array
 */
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

static void array_adjust_copyelement(addr pos, addr array, const size_t *data)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t index1, index2, depth;

	str1 = ArrayInfoStruct(pos);
	data1 = array_ptrsize(pos);
	data2 = array_ptrsize(array);
	depth = str1->dimension;
	index1 = array_index_dimension(data, data1, depth);
	index2 = array_index_dimension(data, data2, depth);
	array_setget(pos, index1, array, index2);
}

static void array_adjust_copydepth(addr pos, addr array, size_t *data, size_t depth)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t size1, size2, i;

	str1 = ArrayInfoStruct(pos);
	if (depth < str1->dimension) {
		data1 = array_ptrsize(pos);
		data2 = array_ptrsize(array);
		size1 = data1[depth];
		size2 = data2[depth];
		for (i = 0; (i < size1) && (i < size2); i++) {
			data[depth] = i;
			array_adjust_copydepth(pos, array, data, depth + 1);
		}
	}
	else {
		array_adjust_copyelement(pos, array, data);
	}
}

static void array_adjust_copydata(addr pos, addr array)
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
	array_adjust_copydepth(pos, array, data, 0);
	rollback_local(local, stack);
}

static void array_adjust_copy(addr pos, addr array,
		addr initial, addr contents, addr fillpointer,
		addr displaced, addr offset)
{
	addr adjustable;

	/* allocate */
	adjustable = ArrayInfoStruct(array)->adjustable? T: Nil;
	array_memory_make(NULL, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	array_initial_make(NULL, pos, initial, contents);
	/* copy array */
	if (array != Nil && contents == Unbound)
		array_adjust_copydata(pos, array);
}

static void array_adjust_make(addr pos, addr array,
		addr initial, addr contents, addr fillpointer, addr displaced, addr offset)
{
	int check1, check2;
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	check1 = (! str->displaced);
	check2 = (displaced == Nil);
	if (check1 && check2) {
		/* not-displaced -> not-displaced */
		array_adjust_copy(pos, array,
				initial, contents, fillpointer, displaced, offset);
	}
	else if (check1) {
		/* not -> displaced */
		array_adjust_copy(pos, Nil,
				initial, contents, fillpointer, displaced, offset);
	}
	else if (check2) {
		/* displaced -> not */
		array_adjust_copy(pos, array,
				initial, contents, fillpointer, displaced, offset);
	}
	else {
		/* displaced -> displaced */
		array_adjust_copy(pos, Nil,
				initial, contents, fillpointer, displaced, offset);
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
	array_settype(pos);
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

	for (i = 0; i < ARRAY_INFO_SIZE; i++) {
		GetArrayInfo(pos, i, &temp);
		SetArrayInfo(pos, i, temp);
	}
}

static void array_adjust_arraytype(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	CheckType(array, LISPTYPE_ARRAY);
	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_adjust_element_type(pos, type, array);
	array_element_size(pos);
	/* dimension */
	array_setsize_heap(pos, dimension);
	/* check */
	array_adjust_dimension(pos, array);
	/* make-adjust */
	array_adjust_make(pos, array, initial, contents, fillpointer, displaced, offset);
	/* result */
	if (ArrayInfoStruct(array)->adjustable) {
		array_adjust_replace(pos, array);
		*ret = array;
	}
	else {
		*ret = pos;
	}
}

static void array_adjust_vectorcheck(addr pos, size_t *ret)
{
	if (pos == Nil)
		fmte("Array rank must be a 1, but 0.", NULL);
	if (singlep(pos))
		GetCar(pos, &pos);
	if (integerp(pos)) {
		if (getindex_integer(pos, ret))
			fmte("Dimension ~A is too large.", pos, NULL);
		return;
	}
	if (consp(pos))
		fmte("Array rank must be a 1.", NULL);
	else
		fmte("Invalid pos type ~S.", pos, NULL);
}

static int array_adjust_simplecheck(addr pos, addr fillpointer, addr displaced)
{
	if (fillpointer != Nil || displaced != Nil) return 0;
	if (! arrayp(pos)) return 1;
	return ! ArrayInfoStruct(pos)->adjustable;
}

static void array_adjust_vector_type(addr pos, addr type, enum ARRAY_TYPE check)
{
	enum ARRAY_TYPE value;
	int size;
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
	array_settype(pos);
}

static void array_adjust_copy_vector(addr pos, addr array)
{
	struct array_struct *str;
	addr temp;
	size_t size1, size2, i;

	str = ArrayInfoStruct(pos);
	size1 = str->size;
	size2 = length_sequence(array, 0);
	for (i = 0; (i < size1) && (i < size2); i++) {
		getelt_sequence(NULL, array, i, &temp);
		setelt_sequence(pos, i, temp);
	}
}

static void array_adjust_array_vector(addr *ret, addr array, addr dimension,
		addr type, enum ARRAY_TYPE check,
		addr initial, addr contents, addr fillpointer, addr displaced, addr offset)
{
	addr pos, adjustable;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_adjust_vector_type(pos, type, check);
	array_element_size(pos);
	/* dimension */
	array_setsize_heap(pos, dimension);
	/* allocate */
	adjustable = ArrayInfoStruct(pos)->adjustable? T: Nil;
	array_memory_make(NULL, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	array_initial_make(NULL, pos, initial, Unbound);
	/* copy-array */
	array_adjust_copy_vector(pos, array);
	/* result */
	*ret = pos;
}

static void array_adjust_vector_contents_list(addr pos, size_t size, addr contents)
{
	addr list, value;
	size_t i;

	list = contents;
	for (i = 0; list != Nil; i++) {
		if (size <= i) {
			fmte("The length of :INITIAL-CONTENTS ~S "
					"must be a new-array size ~S.", contents, intsizeh(size), NULL);
		}
		getcons(list, &value, &list);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_vector_contents_vector(addr pos, size_t size, addr seq)
{
	addr value;
	size_t check, i;

	check = length_sequence(seq, 0);
	if (size <= check) {
		fmte("The length of :INITIAL-CONTENTS ~S "
				"must be a new-array size ~S.", seq, intsizeh(size), NULL);
	}
	for (i = 0; i < size; i++) {
		getelt_sequence(NULL, seq, i, &value);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_vector_contents(addr pos, size_t size, addr contents)
{
	if (contents != Unbound) {
		if (listp(contents))
			array_adjust_vector_contents_list(pos, size, contents);
		else
			array_adjust_vector_contents_vector(pos, size, contents);
	}
}

static void array_adjust_vector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		vector_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_T,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

static void array_adjust_string(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		strvect_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_CHARACTER,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

static void array_adjust_bitvector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		bitmemory_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_BIT,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

_g void array_adjust_array(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_adjust_arraytype(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_VECTOR:
			array_adjust_vector(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_STRING:
			array_adjust_string(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_BITVECTOR:
			array_adjust_bitvector(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		default:
			TypeError(array, ARRAY);
			break;
	}
}

