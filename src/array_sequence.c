#include "array.h"
#include "array_access.h"
#include "array_copy.h"
#include "array_inplace.h"
#include "array_make.h"
#include "array_sequence.h"
#include "bit.h"
#include "condition.h"
#include "sequence.h"
#include "strvect.h"
#include "type_upgraded.h"

/*
 *  bitcalc
 */
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
	GetArrayInfo(src, ARRAY_INDEX_TYPE, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
}

static int array_bitcalc_size_(addr pos, addr src)
{
	struct array_struct *str;
	const size_t *data2;
	addr temp;
	size_t size, i, *data1;

	str = ArrayInfoStruct(src);
	data2 = array_ptrsize(src);
	size = str->dimension;
	if (2 <= size) {
		Return(arraysize_heap_(&temp, size));
		data1 = arraysize_ptr(temp);
		for (i = 0; i < size; i++)
			data1[i] = data2[i];
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	}

	return 0;
}

static int array_bitcalc_make_(addr src, addr *ret)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, src);
	/* dimension */
	Return(array_bitcalc_size_(pos, src));
	/* allocate */
	str = ArrayInfoStruct(pos);
	array_allocate_bit(NULL, pos, str);
	/* result */
	return Result(ret, pos);
}

static int array_bitvector_type_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		case LISPTYPE_ARRAY:
			return Result(ret, 1);

		default:
			*ret = 0;
			return fmte_("Argument ~S must be a bit-array type.", pos, NULL);
	}
}

static int array_bitvector_size_equal_(addr pos1, addr pos2, int *ret)
{
	int check1, check2;

	Return(array_bitvector_type_(pos1, &check1));
	Return(array_bitvector_type_(pos2, &check2));
	if (check1 && check2) {
		return Result(ret, array_equal_dimension(pos1, pos2));
	}
	if (check1) {
		if (ArrayInfoStruct(pos1)->dimension != 1)
			return Result(ret, 0);
		GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
	if (check2) {
		if (ArrayInfoStruct(pos2)->dimension != 1)
			return Result(ret, 0);
		GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
	else {
		return Result(ret, bitmemory_equal_length(pos1, pos2));
	}
}

static int array_bitcalc_aa_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Dimension don't match ~S and ~S.", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		Return(array_bitcalc_make_(pos1, &opt));
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos1;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos1, NULL);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos2, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
	GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_ab_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;
	size_t size;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos2, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = pos1;
		GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos1, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos1, ARRAY_INDEX_MEMORY, &pos1);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_ba_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	struct array_struct *str;
	size_t size;

	Return(array_bitvector_size_equal_(pos1, pos2, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos2, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos2, ARRAY_INDEX_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

static int array_bitcalc_bb_(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check;
	size_t size;

	if (! bitmemory_equal_length(pos1, pos2)) {
		*ret = Nil;
		return fmte_("Length don't match ~S and ~S", pos1, pos2, NULL);
	}
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		Return(array_bitvector_size_equal_(pos1, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos1, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	bitmemory_bitcalc(opt, pos1, pos2, call);

	return 0;
}

_g int array_bitcalc_(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check1, check2;

	Return(array_bitvector_type_(pos1, &check1));
	Return(array_bitvector_type_(pos2, &check2));
	if (check1 && check2)
		return array_bitcalc_aa_(ret, pos1, pos2, opt, call);
	else if (check1)
		return array_bitcalc_ab_(ret, pos1, pos2, opt, call);
	else if (check2)
		return array_bitcalc_ba_(ret, pos1, pos2, opt, call);
	else
		return array_bitcalc_bb_(ret, pos1, pos2, opt, call);
}


/*
 *  array_bitnot
 */
static int array_bitnot_array_(addr *ret, addr pos, addr opt)
{
	int check;
	struct array_struct *str;

	if (opt == Nil) {
		Return(array_bitcalc_make_(pos, &opt));
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos;
		GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	else {
		Return(array_bitvector_size_equal_(pos, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_BIT) {
		*ret = Nil;
		return fmte_("Array ~S must be a bit type.", pos, NULL);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	bitmemory_bitnot(opt, pos);

	return 0;
}

static int array_bitnot_bitmemory_(addr *ret, addr pos, addr opt)
{
	int check;
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
		Return(array_bitvector_size_equal_(pos, opt, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("Length don't match ~S and optional ~S", pos, opt, NULL);
		}
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INDEX_MEMORY, &opt);
	}
	if (! bitvectorp(*ret)) {
		return fmte_("Array ~S must be a bit type.", *ret, NULL);
	}
	bitmemory_bitnot(opt, pos);

	return 0;
}

_g int array_bitnot_(addr *ret, addr pos, addr opt)
{
	int check;

	Return(array_bitvector_type_(pos, &check));
	if (check)
		return array_bitnot_array_(ret, pos, opt);
	else
		return array_bitnot_bitmemory_(ret, pos, opt);
}


/*
 *  array_fill
 */
_g int array_fill_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	/* argument */
	str = ArrayInfoStruct(pos);
	Return(size_start_end_sequence_(start, end, str->size, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++) {
		Return(array_set_(pos, index1, item));
	}

	return 0;
}


/*
 *  array_subseq
 */
static int array_subseq_general_(addr *ret, addr pos, size_t index1, size_t index2)
{
	addr root, temp;
	size_t i;

	Check(index2 < index1, "index error");
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		Return(array_get_t_(pos, index1, &temp));
		setarray(root, i, temp);
	}

	return Result(ret, root);
}

static int array_subseq_specialized_make_(addr *ret, addr array, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_bitcalc_struct(pos, array);
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;
	/* allocate */
	Check(str->dimension != 1, "dimension error");
	Return(array_allocate_size_(NULL, pos, str));
	/* result */
	return Result(ret, pos);
}

static int array_subseq_specialized_(addr *ret,
		addr array, size_t index1, size_t index2)
{
	byte *data1;
	const byte *data2;
	addr pos, mem1, mem2;
	size_t element, diff;

	/* make array */
	Check(index2 < index1, "index error");
	diff = index2 - index1;
	Return(array_subseq_specialized_make_(&pos, array, diff));

	/* subseq */
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem1);
	GetArrayInfo(array, ARRAY_INDEX_MEMORY, &mem2);
	data1 = (byte *)arrayspec_ptr(mem1);
	data2 = (const byte *)arrayspec_ptr(mem2);
	element = ArrayInfoStruct(pos)->element;
	memcpy(data1, data2 + index1 * element, diff * element);
	return Result(ret, pos);
}

static int array_subseq_type_(addr *ret, addr pos, size_t index1, size_t index2)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
			return bitmemory_subseq_index_(ret, pos, index1, index2);

		case ARRAY_TYPE_CHARACTER:
			GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
			return strvect_subseq_index_(ret, pos, index1, index2);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return array_subseq_specialized_(ret, pos, index1, index2);

		default:
			return array_subseq_general_(ret, pos, index1, index2);
	}
}

_g int array_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(size_start_end_sequence_(start, end, str->size, &index1, &index2, NULL));
	return array_subseq_type_(ret, pos, index1, index2);
}


/*
 *  array_reverse
 */
static int array_reverse_t_(addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	vector_heap(&one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_t_(pos, x, &temp));
		setarray(one, y, temp);
	}

	return Result(ret, one);
}

static int array_reverse_bit_(addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	bitmemory_unsafe(NULL, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_bit_(pos, x, &temp));
		Return(bitmemory_setint_(one, y, temp));
	}

	return Result(ret, one);
}

static int array_reverse_character_(addr *ret, addr pos)
{
	unicode temp;
	addr one;
	size_t size, x, y;

	Return(length_sequence_(pos, 1, &size));
	strvect_heap(&one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(array_get_unicode_(pos, x, &temp));
		Return(strvect_setc_(one, y, temp));
	}

	return Result(ret, one);
}

static void array_type_simple_vector(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	array_set_type(pos);
}

static int array_make_simple_vector_(addr *ret,
		size_t size, enum ARRAY_TYPE type, unsigned bytesize)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_type_simple_vector(pos, type, bytesize);
	array_set_element_size(pos);
	/* dimension */
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = size;
	/* allocate */
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	/* initial value */
	Return(array_make_initial_(pos, Unbound, Unbound));
	/* result */
	return Result(ret, pos);
}

static int array_reverse_size_(addr *ret, addr pos)
{
	struct array_struct *str;
	addr one;
	size_t size, x, y;
	struct array_value value;

	str = ArrayInfoStruct(pos);
	size = str->size;
	Return(array_make_simple_vector_(&one, size, str->type, str->bytesize));
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(arrayinplace_get_(pos, x, &value));
		Return(arrayinplace_set_(one, y, &value));
	}

	return Result(ret, one);
}

_g int array_reverse_(addr *ret, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Check(str->dimension != 1, "dimension error");
	switch (str->type) {
		case ARRAY_TYPE_T:
			return array_reverse_t_(ret, pos);

		case ARRAY_TYPE_BIT:
			return array_reverse_bit_(ret, pos);

		case ARRAY_TYPE_CHARACTER:
			return array_reverse_character_(ret, pos);

		default:
			return array_reverse_size_(ret, pos);
	}
}


/*
 *  array_nreverse
 */
_g int array_nreverse_(addr *ret, addr pos)
{
	size_t size, x, y;
	struct array_value a, b;

	Check(ArrayInfoStruct(pos)->dimension != 1, "dimension error");
	Return(length_sequence_(pos, 1, &size));
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		Return(arrayinplace_get_(pos, x, &a));
		Return(arrayinplace_get_(pos, y, &b));
		Return(arrayinplace_set_(pos, x, &b));
		Return(arrayinplace_set_(pos, y, &a));
		x++;
		y--;
	}

	return Result(ret, pos);
}

