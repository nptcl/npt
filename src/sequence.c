#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "array_inplace.h"
#include "array_sequence.h"
#include "array_value.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "sequence.h"
#include "type.h"

_g int sequencep(addr pos)
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

_g int listp_sequence(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return 1;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			return 0;

		default:
			TypeError(pos, SEQUENCE);
			return 0;
	}
}

_g int vectorp_sequence(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return 0;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_BITVECTOR:
			return 1;

		case LISPTYPE_ARRAY:
			return array_vector_p(pos);

		default:
			TypeError(pos, SEQUENCE);
			return 0;
	}
}


/*
 *  size-check
 */
static void vector_error_sequence(addr type, addr arg, size_t size)
{
	size_t check;

	if (type_asterisk_p(arg))
		return;
	if (! integerp(arg))
		type_error_stdarg(Nil, Nil, "Invalid type-specifier ~S.", type, NULL);
	if (GetIndex_integer(arg, &check))
		fmte("Index size ~S is too large.", arg, NULL);
	if (check != size) {
		type_error_stdarg(Nil, Nil,
				"The argument size ~S don't match type-spec ~S.",
				intsizeh(size), type, NULL);
	}
}

_g void vector_check_sequence(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 1, &arg);
	vector_error_sequence(type, arg, size);
}

_g void simple_vector_check_sequence(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 0, &arg);
	vector_error_sequence(type, arg, size);
}

_g void array_check_sequence(addr type, size_t size)
{
	addr arg;
	size_t check;

	GetArrayType(type, 1, &arg);

	/* asterisk */
	if (type_asterisk_p(arg)) {
		return;
	}

	/* integer */
	if (integerp(arg)) {
		if (GetIndex_integer(arg, &check))
			fmte("Index size ~S is too large.", arg, NULL);
		if (check != 1)
			type_error_stdarg(Nil, Nil, "Array ~S dimension must be 1.", type, NULL);
		return;
	}

	/* multiple dimension */
	if (GetType(arg) == LISPTYPE_VECTOR) {
		LenArrayA4(arg, &check);
		if (check != 1)
			type_error_stdarg(Nil, Nil, "Array ~S dimension must be 1.", type, NULL);
		GetArrayA4(arg, 0, &arg);
		if (GetIndex_integer(arg, &check))
			fmte("Index size ~S is too large.", arg, NULL);
		if (check != size) {
			type_error_stdarg(Nil, Nil,
					"The argument size ~S don't match type-spec ~S.",
					intsizeh(size), type, NULL);
		}
		return;
	}

	/* error */
	fmte("Invalid array-type ~S.", type, NULL);
}


/*
 *  make-vector-from-list
 */
_g void make_vector_from_list(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS)
			fmte("The tail of list must be a Nil.", NULL);
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		setarray(array, i, pos);
	}
	*ret = array;
}

_g void make_vector4_from_list(addr *ret, addr cons)
{
	addr pos, array;
	size_t i, size;

	/* length */
	pos = cons;
	for (size = 0; pos != Nil; size++) {
		if (GetType(pos) != LISPTYPE_CONS)
			fmte("The tail of list must be a Nil.", NULL);
		GetCdr(pos, &pos);
	}

	/* make vector */
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}


/*
 *  start-end
 */
_g void list_start_end_sequence(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2)
{
	addr temp;
	size_t index1, index2, i;

	/* argument */
	if (GetIndex_integer(start, &index1))
		fmte(":START ~A is too large.", start, NULL);
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2))
			fmte(":END ~A is too large.", end, NULL);
		if (index2 < index1)
			fmte(":START ~A must be less than equal to :END ~A.", start, end, NULL);
	}
	else {
		index2 = 0;
	}

	/* start */
	temp = Nil;
	for (i = 0; i < index1; i++) {
		if (*list == Nil)
			fmte(":START ~A must be less than equal to list length.", start, NULL);
		temp = *list;
		getcdr(*list, list);
	}

	if (prev) *prev = temp;
	*ret1 = index1;
	*ret2 = index2;
}

_g int size_start_end_sequence(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2)
{
	size_t index1, index2;

	if (size == 0) {
		*ret1 = *ret2 = 0;
		return 1;
	}
	if (GetIndex_integer(start, &index1))
		fmte(":START ~A is too large.", start, NULL);
	if (end != Nil && end != Unbound) {
		if (GetIndex_integer(end, &index2))
			fmte(":END ~A is too large.", end, NULL);
		if (size < index2)
			fmte(":END ~A must be less than sequence length.", end, NULL);
	}
	else {
		index2 = size;
	}
	if (size < index1)
		fmte(":START ~A must be less than sequence length.", start, NULL);
	if (index2 < index1)
		fmte(":START ~A must be less than equal to :END ~A.", start, end, NULL);
	*ret1 = index1;
	*ret2 = index2;

	return 0;
}


/*
 *  common
 */
_g size_t length_sequence(addr pos, int fill)
{
	size_t size;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return 0;

		case LISPTYPE_CONS:
			return length_list_safe(pos);

		case LISPTYPE_VECTOR:
			return lenarrayr(pos);

		case LISPTYPE_STRING:
			strvect_length(pos, &size);
			return size;

		case LISPTYPE_ARRAY:
			return array_get_vector_length(pos, fill);

		case LISPTYPE_BITVECTOR:
			bitvector_length(pos, &size);
			return size;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}

	return 0;
}


/*
 *  elt
 */
static void getelt_list(addr pos, size_t index, addr *ret)
{
	for (;;) {
		if (pos == Nil)
			fmte("Index ~S is too large.", intsizeh(index), NULL);
		if (! consp(pos))
			fmte("The list ~S must be a list type.", pos, NULL);
		if (index == 0)
			break;
		GetCdr(pos, &pos);
		index--;
	}
	GetCar(pos, ret);
}

static void getelt_vector(addr pos, size_t index, addr *ret)
{
	if (lenarrayr(pos) <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	getarray(pos, index, ret);
}

static void getelt_string(addr pos, size_t index, unicode *ret)
{
	size_t size;

	strvect_length(pos, &size);
	if (size <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	strvect_getc(pos, index, ret);
}

static void getelt_bitvector(addr pos, size_t index, int *ret)
{
	size_t size;

	bitvector_length(pos, &size);
	if (size <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	bitmemory_getint(pos, index, ret);
}

_g void getelt_inplace_sequence(addr pos, size_t index, struct array_value *str)
{
	int bit;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			getelt_list(pos, index, &(str->value.object));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_VECTOR:
			getelt_vector(pos, index, &(str->value.object));
			str->type = ARRAY_TYPE_T;
			break;

		case LISPTYPE_STRING:
			getelt_string(pos, index, &(str->value.character));
			str->type = ARRAY_TYPE_CHARACTER;
			break;

		case LISPTYPE_ARRAY:
			arrayinplace_get(pos, index, str);
			break;

		case LISPTYPE_BITVECTOR:
			getelt_bitvector(pos, index, &bit);
			str->value.bit = bit? 1: 0;
			str->type = ARRAY_TYPE_BIT;
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

static void setelt_bit_t_sequence(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	fixnum check;

	value = str->value.object;
	GetFixnum(value, &check);
	if (check == 0)
		bitmemory_setint(pos, index, 0);
	else if (check == 1)
		bitmemory_setint(pos, index, 1);
	else
		fmte("The bit-vector cannot set an integer ~A.", value, NULL);
}

static void setelt_bit_signed_sequence(addr pos,
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
			fmte("Invalid array value.", NULL);
			return;
	}

	/* result */
	if (check == 0)
		bitmemory_setint(pos, index, 0);
	else if (check == 1)
		bitmemory_setint(pos, index, 1);
	else {
		arrayvalue_heap(&pos, str);
		fmte("The bit-vector cannot set an integer ~A.", pos, NULL);
	}
}

static void setelt_bit_unsigned_sequence(addr pos,
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
			fmte("Invalid array value.", NULL);
			return;
	}

	/* result */
	if (check == 0)
		bitmemory_setint(pos, index, 0);
	else if (check == 1)
		bitmemory_setint(pos, index, 1);
	else {
		arrayvalue_heap(&pos, str);
		fmte("The bit-vector cannot set an integer ~A.", pos, NULL);
	}
}

static void setelt_bit_sequence(addr pos, size_t index, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_T:
			setelt_bit_t_sequence(pos, index, str);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_setint(pos, index, (int)str->value.bit);
			break;

		case ARRAY_TYPE_SIGNED:
			setelt_bit_signed_sequence(pos, index, str);
			break;

		case ARRAY_TYPE_UNSIGNED:
			setelt_bit_unsigned_sequence(pos, index, str);
			break;

		default:
			fmte("Invalid array type.", NULL);
	}
}

static void setelt_string_sequence(addr pos,
		size_t index, const struct array_value *str)
{
	addr value;
	unicode c;

	/* t */
	if (str->type == ARRAY_TYPE_T) {
		value = str->value.object;
		if (! characterp(value))
			fmte("The object ~S must be a character type,", value, NULL);
		GetCharacter(value, &c);
		strvect_setc(pos, index, c);
		return;
	}

	/* character */
	if (str->type == ARRAY_TYPE_CHARACTER) {
		strvect_setc(pos, index, str->value.character);
		return;
	}

	/* others */
	fmte("The element of sequence  ~S must be a character type.", pos, NULL);
}

_g void setelt_inplace_sequence(LocalRoot local,
		addr pos, size_t index, const struct array_value *str)
{
	addr value;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			arrayvalue_alloc(local, &value, str);
			setnth(pos, index, value);
			break;

		case LISPTYPE_VECTOR:
			arrayvalue_alloc(local, &value, str);
			vector_setelt(pos, index, value);
			break;

		case LISPTYPE_STRING:
			setelt_string_sequence(pos, index, str);
			break;

		case LISPTYPE_ARRAY:
			arrayinplace_set(pos, index, str);
			break;

		case LISPTYPE_BITVECTOR:
			setelt_bit_sequence(pos, index, str);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

static void getelt_string_alloc(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;
	getelt_string(pos, index, &c);
	character_alloc(local, ret, c);
}

static void getelt_array(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	if (! array_vector_p(pos))
		TypeError(pos, SEQUENCE);
	size = array_get_vector_length(pos, 1);
	if (size <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	array_get(local, pos, index, ret);
}

static void getelt_bitvector_alloc(LocalRoot local, addr pos, size_t index, addr *ret)
{
	size_t size;

	bitvector_length(pos, &size);
	if (size <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	bitmemory_get(local, pos, index, ret);
}

_g void getelt_sequence(LocalRoot local, addr pos, size_t index, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			getelt_list(pos, index, ret);
			break;

		case LISPTYPE_VECTOR:
			getelt_vector(pos, index, ret);
			break;

		case LISPTYPE_STRING:
			getelt_string_alloc(local, pos, index, ret);
			break;

		case LISPTYPE_ARRAY:
			getelt_array(local, pos, index, ret);
			break;

		case LISPTYPE_BITVECTOR:
			getelt_bitvector_alloc(local, pos, index, ret);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

static void setelt_array(addr pos, size_t index, addr value)
{
	size_t size;

	if (! array_vector_p(pos))
		TypeError(pos, SEQUENCE);
	size = array_get_vector_length(pos, 1);
	if (size <= index)
		fmte("Index ~S is too large.", intsizeh(index), NULL);
	array_set(pos, index, value);
}

_g void setelt_sequence(addr pos, size_t index, addr value)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			setnth(pos, index, value);
			break;

		case LISPTYPE_VECTOR:
			vector_setelt(pos, index, value);
			break;

		case LISPTYPE_STRING:
			strvect_set(pos, index, value);
			break;

		case LISPTYPE_ARRAY:
			setelt_array(pos, index, value);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_set(pos, index, value);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}


/*
 *  reverse / nreverse
 */
_g void reverse_sequence_heap(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			*ret = Nil;
			break;

		case LISPTYPE_CONS:
			reverse_list_heap_safe(ret, pos);
			break;

		case LISPTYPE_VECTOR:
			vector_reverse(NULL, ret, pos);
			break;

		case LISPTYPE_STRING:
			strvect_reverse(NULL, ret, pos);
			break;

		case LISPTYPE_ARRAY:
			array_reverse(ret, pos);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_reverse(NULL, ret, pos);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

_g void nreverse_sequence(addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			*ret = Nil;
			break;

		case LISPTYPE_CONS:
			nreverse_list_safe(ret, pos);
			break;

		case LISPTYPE_VECTOR:
			vector_nreverse(ret, pos);
			break;

		case LISPTYPE_STRING:
			strvect_nreverse(ret, pos);
			break;

		case LISPTYPE_ARRAY:
			array_nreverse(ret, pos);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_nreverse(ret, pos);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

