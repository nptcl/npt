#include "array.h"
#include "array_common.h"
#include "array_copy.h"
#include "array_object.h"
#include "bit.h"
#include "condition.h"
#include "sequence.h"
#include "type_upgraded.h"

static void array_bitcalc_struct(addr pos, addr src)
{
	struct array_struct *str1;
	const struct array_struct *str2;
	addr type;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(src);
	str1->simple = 1;
	str1->adjustable = 0;
	str1->fillpointer = 0;
	str1->displaced = 0;
	str1->type = str2->type;
	str1->element = str2->element;
	str1->bytesize = str2->bytesize;
	str1->size = str2->front;
	str1->front = str2->front;
	str1->dimension = str2->dimension;
	str1->offset = 0;
	str1->refer = str2->front;
	GetArrayInfo(src, ARRAY_INFO_TYPE, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
}

static void array_bitcalc_size(addr pos, addr src)
{
	struct array_struct *str;
	const size_t *data2;
	addr temp;
	size_t size, i, *data1;

	str = ArrayInfoStruct(src);
	data2 = array_ptrsize(src);
	size = str->dimension;
	if (2 <= size) {
		arraysize_alloc(NULL, &temp, size);
		data1 = arraysize_ptr(temp);
		for (i = 0; i < size; i++)
			data1[i] = data2[i];
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
	}
}

static void array_bitcalc_make(addr src, addr *ret)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, src);
	/* dimension */
	array_bitcalc_size(pos, src);
	/* allocate */
	str = ArrayInfoStruct(pos);
	array_allocate_bit(NULL, pos, str);
	/* result */
	*ret = pos;
}

static int array_bitvector_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return 0;

		case LISPTYPE_ARRAY:
			return 1;

		default:
			fmte("Argument ~S must be a bit-array type.", pos, NULL);
			return -1;
	}
}

static int array_bitvector_size_equal(addr pos1, addr pos2)
{
	int check1, check2;

	check1 = array_bitvector_type(pos1);
	check2 = array_bitvector_type(pos2);
	if (check1 && check2) {
		return array_dimension_equal(pos1, pos2);
	}
	if (check1) {
		if (ArrayInfoStruct(pos1)->dimension != 1)
			return 0;
		GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
		return bitmemory_equal_length(pos1, pos2);
	}
	if (check2) {
		if (ArrayInfoStruct(pos2)->dimension != 1)
			return 0;
		GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
		return bitmemory_equal_length(pos1, pos2);
	}
	else {
		return bitmemory_equal_length(pos1, pos2);
	}
}

static void array_bitcalc_aa(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;

	if (! array_bitvector_size_equal(pos1, pos2))
		fmte("Dimension don't match ~S and ~S.", pos1, pos2, NULL);
	if (opt == Nil) {
		array_bitcalc_make(pos1, &opt);
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos1;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! array_bitvector_size_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos1, NULL);
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos2, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
	GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_ab(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;
	size_t size;

	if (! array_bitvector_size_equal(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos2, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = pos1;
		GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! array_bitvector_size_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos1, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_ba(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;
	size_t size;

	if (! array_bitvector_size_equal(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		if (! array_bitvector_size_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos2, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_bb(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	size_t size;

	if (! bitmemory_equal_length(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		if (! array_bitvector_size_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

_g void array_bitcalc(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check1, check2;

	check1 = array_bitvector_type(pos1);
	check2 = array_bitvector_type(pos2);
	if (check1 && check2) {
		array_bitcalc_aa(ret, pos1, pos2, opt, call);
		return;
	}
	if (check1) {
		array_bitcalc_ab(ret, pos1, pos2, opt, call);
		return;
	}
	if (check2) {
		array_bitcalc_ba(ret, pos1, pos2, opt, call);
		return;
	}
	else {
		array_bitcalc_bb(ret, pos1, pos2, opt, call);
		return;
	}
}

_g void array_bitnot_array(addr *ret, addr pos, addr opt)
{
	struct array_struct *str;

	if (opt == Nil) {
		array_bitcalc_make(pos, &opt);
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! array_bitvector_size_equal(pos, opt))
			fmte("Length don't match ~S and optional ~S", pos, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	bitmemory_bitnot(opt, pos);
}

_g void array_bitnot_bitmemory(addr *ret, addr pos, addr opt)
{
	size_t size;

	if (opt == Nil) {
		bitmemory_length(pos, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos;
	}
	else {
		if (! array_bitvector_size_equal(pos, opt))
			fmte("Length don't match ~S and optional ~S", pos, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	bitmemory_bitnot(opt, pos);
}

_g void array_bitnot(addr *ret, addr pos, addr opt)
{
	if (array_bitvector_type(pos))
		array_bitnot_array(ret, pos, opt);
	else
		array_bitnot_bitmemory(ret, pos, opt);
}

_g void array_fill(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	/* argument */
	str = ArrayInfoStruct(pos);
	sequence_start_end(start, end, str->size, &index1, &index2);

	/* fill */
	for (; index1 < index2; index1++)
		array_set(pos, index1, item);
}

static void array_subseq_general(addr *ret, addr pos, size_t index1, size_t index2)
{
	addr root, temp;
	size_t i;

	Check(index2 < index1, "index error");
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		if (array_get_t(pos, index1, &temp)) {
			fmte("Invalid array object.", NULL);
			return;
		}
		setarray(root, i, temp);
	}
	*ret = root;
}

static void array_subseq_specialized_make(addr *ret, addr array, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, array);
	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
	/* allocate */
	Check(str->dimension != 1, "dimension error");
	array_allocate_size(NULL, pos, str);
	/* result */
	*ret = pos;
}

static void array_subseq_specialized(addr *ret,
		addr array, size_t index1, size_t index2)
{
	byte *data1;
	const byte *data2;
	addr pos, mem1, mem2;
	size_t element, diff;

	/* make array */
	Check(index2 < index1, "index error");
	diff = index2 - index1;
	array_subseq_specialized_make(&pos, array, diff);

	/* subseq */
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem1);
	GetArrayInfo(array, ARRAY_INFO_MEMORY, &mem2);
	data1 = (byte *)arrayspec_ptr(mem1);
	data2 = (const byte *)arrayspec_ptr(mem2);
	element = ArrayInfoStruct(pos)->element;
	memcpy(data1, data2 + index1 * element, diff * element);
	*ret = pos;
}

static void array_subseq_type(addr *ret, addr pos, size_t index1, size_t index2)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
			bitmemory_subseq_index(ret, pos, index1, index2);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
			strvect_subseq_index(ret, pos, index1, index2);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			array_subseq_specialized(ret, pos, index1, index2);
			break;

		default:
			array_subseq_general(ret, pos, index1, index2);
			break;
	}
}

_g void array_subseq(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	sequence_start_end(start, end, str->size, &index1, &index2);
	array_subseq_type(ret, pos, index1, index2);
}

_g void array_reverse_t(LocalRoot local, addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	vector_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_t(pos, x, &temp)) {
			fmte("Invalid array ~S.", pos, NULL);
			return;
		}
		setarray(one, y, temp);
	}
	*ret = one;
}

_g void array_reverse_bit(LocalRoot local, addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	bitmemory_unsafe(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_bit(pos, x, &temp))
			fmte("Invalid array ~S.", pos, NULL);
		bitmemory_setint(one, y, temp);
	}
	*ret = one;
}

_g void array_reverse_character(LocalRoot local, addr *ret, addr pos)
{
	unicode temp;
	addr one;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_unicode(pos, x, &temp)) {
			fmte("Invalid array ~S.", pos, NULL);
			return;
		}
		strvect_setc(one, y, temp);
	}
	*ret = one;
}

static void array_type_simple_vector(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	array_settype(pos);
}

static void array_make_simple_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, unsigned bytesize)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	array_type_simple_vector(pos, type, bytesize);
	array_element_size(pos);
	/* dimension */
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = str->refer = size;
	/* allocate */
	array_memory_make(local, pos, Nil, Nil, Nil, Nil);
	/* initial value */
	array_initial_make(local, pos, Unbound, Unbound);
	/* result */
	*ret = pos;
}

_g void array_reverse_size(LocalRoot local, addr *ret, addr pos)
{
	struct array_struct *str;
	addr one;
	size_t size, x, y;
	struct array_value value;

	str = ArrayInfoStruct(pos);
	size = str->size;
	array_make_simple_vector(local, &one, size, str->type, str->bytesize);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		array_get_inplace(pos, x, &value);
		if (array_set_inplace(one, y, &value))
			fmte("Invalid array ~S.", pos, NULL);
	}
	*ret = one;
}

_g void array_reverse(LocalRoot local, addr *ret, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Check(str->dimension != 1, "dimension error");
	switch (str->type) {
		case ARRAY_TYPE_T:
			array_reverse_t(local, ret, pos);
			break;

		case ARRAY_TYPE_BIT:
			array_reverse_bit(local, ret, pos);
			break;

		case ARRAY_TYPE_CHARACTER:
			array_reverse_character(local, ret, pos);
			break;

		default:
			array_reverse_size(local, ret, pos);
			break;
	}
}

_g void array_nreverse(addr *ret, addr pos)
{
	size_t size, x, y;
	struct array_value a, b;

	Check(ArrayInfoStruct(pos)->dimension != 1, "dimension error");
	size = length_sequence(pos, 1);
	if (size <= 1) return;
	x = 0;
	y = size - 1;
	while (x < y) {
		array_get_inplace(pos, x, &a);
		array_get_inplace(pos, y, &b);
		array_set_inplace(pos, x, &b);
		array_set_inplace(pos, y, &a);
		x++;
		y--;
	}
	*ret = pos;
}

