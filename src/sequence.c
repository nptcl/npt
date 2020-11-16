#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "array_inplace.h"
#include "array_sequence.h"
#include "array_value.h"
#include "array_vector.h"
#include "bit.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "sequence.h"
#include "strvect.h"
#include "type.h"

int sequencep(addr pos)
{
	enum LISPTYPE check;

	check = GetType(pos);
	if (check == LISPTYPE_ARRAY)
		return array_vector_p(pos);
	return
		check == LISPTYPE_NIL ||
		check == LISPTYPE_CONS ||
		check == LISPTYPE_VECTOR ||
		check == LISPTYPE_STRING ||
		check == LISPTYPE_BITVECTOR;
}

int listp_sequence_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return Result(ret, 1);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 0);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int vectorp_sequence_(addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return Result(ret, 0);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return Result(ret, 1);

		case LISPTYPE_ARRAY:
			return Result(ret, array_vector_p(pos));

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int vectorp_sequence_debug(addr pos)
{
	int check;
	check = 0;
	Error(vectorp_sequence_(pos, &check));
	return check;
}


/*
 *  size-check
 */
static int vector_error_sequence_(addr type, addr arg, size_t size)
{
	size_t check;

	if (type_asterisk_p(arg))
		return 0;
	if (! integerp(arg)) {
		return call_type_error_va_(NULL,
				Nil, Nil, "Invalid type-specifier ~S.", type, NULL);
	}
	if (GetIndex_integer(arg, &check))
		return fmte_("Index size ~S is too large.", arg, NULL);
	if (check != size) {
		return call_type_error_va_(NULL, Nil, Nil,
				"The argument size ~S don't match type-spec ~S.",
				intsizeh(size), type, NULL);
	}

	return 0;
}

int vector_check_sequence_(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 1, &arg);
	return vector_error_sequence_(type, arg, size);
}

int simple_vector_check_sequence_(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 0, &arg);
	return vector_error_sequence_(type, arg, size);
}

int array_check_sequence_(addr type, size_t size)
{
	addr arg;
	size_t check;

	GetArrayType(type, 1, &arg);

	/* asterisk */
	if (type_asterisk_p(arg)) {
		return 0;
	}

	/* integer */
	if (integerp(arg)) {
		if (GetIndex_integer(arg, &check))
			return fmte_("Index size ~S is too large.", arg, NULL);
		if (check != 1) {
			return call_type_error_va_(NULL, Nil, Nil,
					"Array ~S dimension must be 1.", type, NULL);
		}
		return 0;
	}

	/* multiple dimension */
	if (GetType(arg) == LISPTYPE_VECTOR) {
		LenArrayA4(arg, &check);
		if (check != 1) {
			return call_type_error_va_(NULL, Nil, Nil,
					"Array ~S dimension must be 1.", type, NULL);
		}
		GetArrayA4(arg, 0, &arg);
		if (GetIndex_integer(arg, &check))
			return fmte_("Index size ~S is too large.", arg, NULL);
		if (check != size) {
			return call_type_error_va_(NULL, Nil, Nil,
					"The argument size ~S don't match type-spec ~S.",
					intsizeh(size), type, NULL);
		}
		return 0;
	}

	/* error */
	return fmte_("Invalid array-type ~S.", type, NULL);
}


/*
 *  make-vector-from-list
 */
int make_vector_from_list_(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The tail of list must be a Nil.", NULL);
		}
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		setarray(array, i, pos);
	}

	return Result(ret, array);
}

int make_vector4_from_list_(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS) {
			*ret = Nil;
			return fmte_("The tail of list must be a Nil.", NULL);
		}
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}

	return Result(ret, array);
}


/*
 *  start-end
 */
int list_start_end_sequence_(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2)
{
	addr temp;
	size_t index1, index2, i, size;

	/* argument */
	if (GetIndex_integer(start, &index1)) {
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A is too large.", start, NULL);
	}
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2)) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A is too large.", end, NULL);
		}
		if (index2 < index1) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":START ~A "
					"must be less than equal to :END ~A.", start, end, NULL);
		}
		length_list_p(*list, &size);
		if (size < index2) {
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A must be less than sequence length.", end, NULL);
		}
	}
	else {
		index2 = 0;
	}

	/* start */
	temp = Nil;
	for (i = 0; i < index1; i++) {
		if (*list == Nil) {
			if (prev)
				*prev = Nil;
			*ret1 = *ret2 = 0;
			return fmte_(":START ~A "
					"must be less than equal to list length.", start, NULL);
		}
		temp = *list;
		Return_getcdr(*list, list);
	}

	if (prev)
		*prev = temp;
	*ret1 = index1;
	*ret2 = index2;
	return 0;
}

static int size_start_end_sequence_call_(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2, int *ret)
{
	size_t index1, index2;

	if (GetIndex_integer(start, &index1)) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A is too large.", start, NULL);
	}
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2)) {
			*ret = 0;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A is too large.", end, NULL);
		}
		if (size < index2) {
			*ret = 0;
			*ret1 = *ret2 = 0;
			return fmte_(":END ~A must be less than sequence length.", end, NULL);
		}
	}
	else {
		index2 = size;
	}
	if (size < index1) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A must be less than sequence length.", start, NULL);
	}
	if (index2 < index1) {
		*ret = 0;
		*ret1 = *ret2 = 0;
		return fmte_(":START ~A must be less than equal to :END ~A.", start, end, NULL);
	}

	*ret1 = index1;
	*ret2 = index2;
	return Result(ret, 0);
}

int size_start_end_sequence_(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2, int *ret)
{
	int ignore;

	if (ret)
		return size_start_end_sequence_call_(start, end, size, ret1, ret2, ret);
	else
		return size_start_end_sequence_call_(start, end, size, ret1, ret2, &ignore);
}


/*
 *  common
 */
int length_sequence_(addr pos, int fill, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, 0);

		case LISPTYPE_CONS:
			return length_list_safe_(pos, ret);

		case LISPTYPE_VECTOR:
			*ret = lenarrayr(pos);
			return 0;

		case LISPTYPE_STRING:
			strvect_length(pos, ret);
			return 0;

		case LISPTYPE_ARRAY:
			return array_get_vector_length_(pos, fill, ret);

		case LISPTYPE_BITVECTOR:
			return bitvector_length_(pos, ret);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}


/*
 *  elt
 */
static int getelt_list_(addr pos, size_t index, addr *ret)
{
	for (;;) {
		if (pos == Nil) {
			*ret = Nil;
			return fmte_("Index ~S is too large.", intsizeh(index), NULL);
		}
		if (! consp(pos)) {
			*ret = Nil;
			return fmte_("The list ~S must be a list type.", pos, NULL);
		}
		if (index == 0)
			break;
		GetCdr(pos, &pos);
		index--;
	}
	GetCar(pos, ret);
	return 0;
}

static int getelt_vector_(addr pos, size_t index, addr *ret)
{
	if (lenarrayr(pos) <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	getarray(pos, index, ret);
	return 0;
}

static int getelt_string_(addr pos, size_t index, unicode *ret)
{
	size_t size;

	strvect_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	strvect_getc(pos, index, ret);
	return 0;
}

static int getelt_bitvector_(addr pos, size_t index, int *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_getint_(pos, index, ret);
}

int getelt_inplace_sequence_(addr pos, size_t index, struct array_value *str)
{
	int bit;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			Return(getelt_list_(pos, index, &(str->value.object)));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_VECTOR:
			Return(getelt_vector_(pos, index, &(str->value.object)));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_STRING:
			Return(getelt_string_(pos, index, &(str->value.character)));
			str->type = ARRAY_TYPE_CHARACTER;
			break;

		case LISPTYPE_ARRAY:
			return arrayinplace_get_(pos, index, str);

		case LISPTYPE_BITVECTOR:
			Return(getelt_bitvector_(pos, index, &bit));
			str->value.bit = bit? 1: 0;
			str->type = ARRAY_TYPE_BIT;
			break;

		default:
			return TypeError_(pos, SEQUENCE);
	}

	return 0;
}

static int setelt_bit_t_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	fixnum check;

	value = str->value.object;
	GetFixnum(value, &check);
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	return fmte_("The bit-vector cannot set an integer ~A.", value, NULL);
}

static int setelt_bit_signed_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	int check;
	int8_t v8;
	int16_t v16;
	int32_t v32;
#ifdef LISP_64BIT
	int64_t v64;
#endif

	/* signed check */
	switch (str->size) {
		case 8:
			v8 = str->value.signed8;
			if (v8 == 0) check = 0;
			else if (v8 == 1) check = 1;
			else check = -1;
			break;

		case 16:
			v16 = str->value.signed16;
			if (v16 == 0) check = 0;
			else if (v16 == 1) check = 1;
			else check = -1;
			break;

		case 32:
			v32 = str->value.signed32;
			if (v32 == 0) check = 0;
			else if (v32 == 1) check = 1;
			else check = -1;
			break;

#ifdef LISP_64BIT
		case 64:
			v64 = str->value.signed64;
			if (v64 == 0) check = 0;
			else if (v64 == 1) check = 1;
			else check = -1;
			break;
#endif

		default:
			return fmte_("Invalid array value.", NULL);
	}

	/* result */
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	Return(arrayvalue_heap_(&pos, str));
	return fmte_("The bit-vector cannot set an integer ~A.", pos, NULL);
}

static int setelt_bit_unsigned_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	int check;
	uint8_t v8;
	uint16_t v16;
	uint32_t v32;
#ifdef LISP_64BIT
	uint64_t v64;
#endif

	/* unsigned check */
	switch (str->size) {
		case 8:
			v8 = str->value.unsigned8;
			if (v8 == 0) check = 0;
			else if (v8 == 1) check = 1;
			else check = -1;
			break;

		case 16:
			v16 = str->value.unsigned16;
			if (v16 == 0) check = 0;
			else if (v16 == 1) check = 1;
			else check = -1;
			break;

		case 32:
			v32 = str->value.unsigned32;
			if (v32 == 0) check = 0;
			else if (v32 == 1) check = 1;
			else check = -1;
			break;

#ifdef LISP_64BIT
		case 64:
			v64 = str->value.unsigned64;
			if (v64 == 0) check = 0;
			else if (v64 == 1) check = 1;
			else check = -1;
			break;
#endif

		default:
			return fmte_("Invalid array value.", NULL);
	}

	/* result */
	if (check == 0)
		return bitmemory_setint_(pos, index, 0);
	if (check == 1)
		return bitmemory_setint_(pos, index, 1);

	/* error */
	Return(arrayvalue_heap_(&pos, str));
	return fmte_("The bit-vector cannot set an integer ~A.", pos, NULL);
}

static int setelt_bit_sequence_(addr pos, size_t index, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_T:
			return setelt_bit_t_sequence_(pos, index, str);

		case ARRAY_TYPE_BIT:
			return bitmemory_setint_(pos, index, (int)str->value.bit);

		case ARRAY_TYPE_SIGNED:
			return setelt_bit_signed_sequence_(pos, index, str);

		case ARRAY_TYPE_UNSIGNED:
			return setelt_bit_unsigned_sequence_(pos, index, str);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int setelt_string_sequence_(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	unicode c;

	/* t */
	if (str->type == ARRAY_TYPE_T) {
		value = str->value.object;
		if (! characterp(value))
			return fmte_("The object ~S must be a character type,", value, NULL);
		GetCharacter(value, &c);
		return strvect_setc_(pos, index, c);
	}

	/* character */
	if (str->type == ARRAY_TYPE_CHARACTER)
		return strvect_setc_(pos, index, str->value.character);

	/* others */
	return fmte_("The element of sequence  ~S must be a character type.", pos, NULL);
}

int setelt_inplace_sequence_(LocalRoot local,
		addr pos, size_t index, const struct array_value *str)
{
	addr value;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			Return(arrayvalue_alloc_(local, &value, str));
			return setnth_(pos, index, value);

		case LISPTYPE_VECTOR:
			Return(arrayvalue_alloc_(local, &value, str));
			Return(vector_setelt_(pos, index, value));
			return 0;

		case LISPTYPE_STRING:
			return setelt_string_sequence_(pos, index, str);

		case LISPTYPE_ARRAY:
			return arrayinplace_set_(pos, index, str);

		case LISPTYPE_BITVECTOR:
			return setelt_bit_sequence_(pos, index, str);

		default:
			return TypeError_(pos, SEQUENCE);
	}
}

static int getelt_string_alloc_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;

	Return(getelt_string_(pos, index, &c));
	character_alloc(local, ret, c);

	return 0;
}

static int getelt_array_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	if (! array_vector_p(pos)) {
		*ret = Nil;
		return TypeError_(pos, SEQUENCE);
	}
	Return(array_get_vector_length_(pos, 1, &size));
	if (size <= index) {
		*ret = Nil;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return array_get_(local, pos, index, ret);
}

static int getelt_bitvector_alloc_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = Nil;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_get_(local, pos, index, ret);
}

int getelt_sequence_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return getelt_list_(pos, index, ret);

		case LISPTYPE_VECTOR:
			return getelt_vector_(pos, index, ret);

		case LISPTYPE_STRING:
			return getelt_string_alloc_(local, pos, index, ret);

		case LISPTYPE_ARRAY:
			return getelt_array_(local, pos, index, ret);

		case LISPTYPE_BITVECTOR:
			return getelt_bitvector_alloc_(local, pos, index, ret);

		default:
			*ret = Nil;
			return TypeError_(pos, SEQUENCE);
	}
}

static int setelt_array_(addr pos, size_t index, addr value)
{
	size_t size;

	if (! array_vector_p(pos))
		return TypeError_(pos, SEQUENCE);
	Return(array_get_vector_length_(pos, 1, &size));
	if (size <= index)
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);

	return array_set_(pos, index, value);
}

int setelt_sequence_(addr pos, size_t index, addr value)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return setnth_(pos, index, value);

		case LISPTYPE_VECTOR:
			return vector_setelt_(pos, index, value);

		case LISPTYPE_STRING:
			return strvect_set_(pos, index, value);

		case LISPTYPE_ARRAY:
			return setelt_array_(pos, index, value);

		case LISPTYPE_BITVECTOR:
			return bitmemory_set_(pos, index, value);

		default:
			return TypeError_(pos, SEQUENCE);
	}
}


/*
 *  reverse / nreverse
 */
int reverse_sequence_heap_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, Nil);

		case LISPTYPE_CONS:
			return reverse_list_heap_safe_(ret, pos);

		case LISPTYPE_VECTOR:
			vector_reverse(NULL, ret, pos);
			return 0;

		case LISPTYPE_STRING:
			return strvect_reverse_(NULL, ret, pos);

		case LISPTYPE_ARRAY:
			return array_reverse_(ret, pos);

		case LISPTYPE_BITVECTOR:
			return bitmemory_reverse_(NULL, ret, pos);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

int nreverse_sequence_(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return Result(ret, Nil);

		case LISPTYPE_CONS:
			return nreverse_list_safe_(ret, pos);

		case LISPTYPE_VECTOR:
			vector_nreverse(ret, pos);
			return 0;

		case LISPTYPE_STRING:
			return strvect_nreverse_(ret, pos);

		case LISPTYPE_ARRAY:
			return array_nreverse_(ret, pos);

		case LISPTYPE_BITVECTOR:
			return bitmemory_nreverse_(ret, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, SEQUENCE);
	}
}

