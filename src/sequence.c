#include "array.h"
#include "array_common.h"
#include "array_object.h"
#include "array_vector.h"
#include "bit.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "equal.h"
#include "format.h"
#include "integer.h"
#include "number.h"
#include "sequence.h"
#include "strtype.h"
#include "type.h"
#include "type_typep.h"
#include "type_parse.h"
#include "type_upgraded.h"

#define MERGE_SORT_LIMIT 16

/*
 *  unsafe
 */
_g void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	nreverse_list_unsafe(ret, root);
}
_g void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_unsafe(local, ret, cons);
}
_g void copy_list_heap_unsafe(addr *ret, addr cons)
{
	copy_list_alloc_unsafe(NULL, ret, cons);
}

_g void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons)
{
	addr root, last, pos;

	root = last = Nil;
	for (;;) {
		if (GetType(cons) == LISPTYPE_CONS) {
			GetCons(cons, &pos, &cons);
			cons_alloc(local, &root, pos, root);
		}
		else {
			last = cons;
			break;
		}
	}
	nreverse_list_unsafe_dotted(ret, root, last);
}
_g void copy_list_local_safe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_safe(local, ret, cons);
}
_g void copy_list_heap_safe(addr *ret, addr cons)
{
	copy_list_alloc_safe(NULL, ret, cons);
}

_g int list_length_safe(addr list, size_t *ret)
{
	addr fast, slow, one;
	size_t size;

	slow = fast = list;
	size = 0;
	for (;;) {
		if (fast == Nil) {
			break;
		}
		getcdr(fast, &one);
		if (one == Nil) {
			size++;
			break;
		}

		/* circular check */
		if (fast == slow && 0 < size) {
			return 1;
		}

		/* increment */
		size += 2;
		getcdr(one, &fast);
		getcdr(slow, &slow);
	}
	*ret = size;

	return 0;
}

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

_g void nth_unsafe(addr *ret, size_t index, addr cons)
{
	if (cons == Nil) {
		*ret = Nil;
		return;
	}

	for (;;) {
		GetCons(cons, ret, &cons);
		if (index == 0) break;
		if (cons == Nil) {
			*ret = Nil;
			break;
		}
		index--;
	}
}

_g void append_cons_alloc_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2)
{
	addr stack, left;

	if (cons1 == Nil) {
		*ret = cons2;
		return;
	}
	if (cons2 == Nil) {
		*ret = cons1;
		return;
	}

	stack = Nil;
	do {
		GetCons(cons1, &left, &cons1);
		cons_alloc(local, &stack, left, stack);
	} while (cons1 != Nil);

	for (;;) {
		GetCdr(stack, &cons1);
		SetCdr(stack, cons2);
		if (cons1 == Nil) break;
		cons2 = stack;
		stack = cons1;
	}
	*ret = stack;
}

_g void append_cons_heap_unsafe(addr *ret, addr cons1, addr cons2)
{
	append_cons_alloc_unsafe(NULL, ret, cons1, cons2);
}

_g void append_cons_local_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2)
{
	Check(local == NULL, "local error");
	append_cons_alloc_unsafe(local, ret, cons1, cons2);
}

_g int delete_cons_eq_unsafe(addr key, addr cons, addr *ret)
{
	int update;
	addr check, cons1, cons2;

	update = 0;
	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (check == key) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			update++;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return update;
}

_g int delete1_cons_eq_unsafe(addr key, addr cons, addr *ret)
{
	addr check, cons1, cons2;

	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (check == key) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			return 1;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return 0;
}

_g void remove_cons_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	addr result, check;

	for (result = Nil; cons != Nil; ) {
		GetCons(cons, &check, &cons);
		if (check != key)
			cons_alloc(local, &result, check, result);
	}
	nreverse_list_unsafe(ret, result);
}

_g void remove_cons_eq_unsafe_heap(addr key, addr cons, addr *ret)
{
	remove_cons_eq_unsafe_alloc(NULL, key, cons, ret);
}

_g void remove_cons_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	Check(local == NULL, "local error");
	remove_cons_eq_unsafe_alloc(local, key, cons, ret);
}


/*
 *  sequence
 */
_g void reverse_sequence_alloc(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			*ret = Nil;
			break;

		case LISPTYPE_CONS:
			reverse_list_alloc_safe(local, ret, pos);
			break;

		case LISPTYPE_VECTOR:
			vector_reverse(local, ret, pos);
			break;

		case LISPTYPE_STRING:
			strvect_reverse(local, ret, pos);
			break;

		case LISPTYPE_ARRAY:
			array_reverse(local, ret, pos);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_reverse(local, ret, pos);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

_g void reverse_sequence_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	reverse_sequence_alloc(local, ret, pos);
}

_g void reverse_sequence_heap(addr *ret, addr pos)
{
	reverse_sequence_alloc(NULL, ret, pos);
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

_g void list_start_end(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2)
{
	addr temp;
	size_t index1, index2, i;

	/* argument */
	if (getindex_integer(start, &index1))
		fmte(":START ~A is too large.", start, NULL);
	if (end != Nil && end != Unbound) {
		if (getindex_integer(end, &index2))
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

_g int sequence_start_end(addr start, addr end, size_t size, size_t *ret1, size_t *ret2)
{
	size_t index1, index2;

	if (size == 0) {
		*ret1 = *ret2 = 0;
		return 1;
	}
	if (getindex_integer(start, &index1))
		fmte(":START ~A is too large.", start, NULL);
	if (end != Nil && end != Unbound) {
		if (getindex_integer(end, &index2))
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

_g void list_fill_safe(addr list, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	list_start_end(&list, NULL, start, end, &index1, &index2);

	/* fill */
	for (;;) {
		if (end != Nil) {
			if (index2 <= index1)
				break;
			if (list == Nil)
				fmte(":END ~A must be less than equal to list length.", end, NULL);
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list))
			fmte("Don't accept the dotted list ~S.", list, NULL);
		SetCar(list, item);
		GetCdr(list, &list);
		index1++;
	}
}

_g void vector_fill(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	lenarray(pos, &index1);
	sequence_start_end(start, end, index1, &index1, &index2);

	/* fill */
	for (; index1 < index2; index1++)
		setarray(pos, index1, item);
}


/*
 *  sequence control
 */
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
		check == LISPTYPE_BITVECTOR ;
}

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
			return array_vector_length(pos, fill);

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
			array_get_inplace(pos, index, str);
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
	GetFixnum(str->value.object, &check);
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
		array_value_alloc(NULL, &pos, str);
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
		array_value_alloc(NULL, &pos, str);
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

_g void setelt_inplace_sequence(LocalRoot local,
		addr pos, size_t index, const struct array_value *str)
{
	addr value;

	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			array_value_alloc(local, &value, str);
			setnth(pos, index, value);
			break;

		case LISPTYPE_VECTOR:
			array_value_alloc(local, &value, str);
			vector_setelt(pos, index, value);
			break;

		case LISPTYPE_STRING:
			if (str->type != ARRAY_TYPE_CHARACTER)
				fmte("type error", NULL);
			strvect_setc(pos, index, str->value.character);
			break;

		case LISPTYPE_ARRAY:
			array_set_inplace(pos, index, str);
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
	size = array_vector_length(pos, 1);
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
	size = array_vector_length(pos, 1);
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
 *  make-sequence
 */
static int list_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;
	addr root;
	size_t i;

	/* type check */
	if (value == Unbound) value = Nil;
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return 0;

	/* make-sequence */
	root = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&root, value, root);
	*ret = root;

	return 1;
}

static void vector_size_make_sequence(addr type, addr arg, size_t size)
{
	size_t check;

	if (type_asterisk_p(arg))
		return;
	if (! integerp(arg))
		type_error_stdarg(Nil, Nil, "Invalid type-specifier ~S.", type, NULL);
	if (getindex_integer(arg, &check))
		fmte("Index size ~S is too large.", arg, NULL);
	if (check != size) {
		type_error_stdarg(Nil, Nil,
				"The argument size ~S don't match type-spec ~S.",
				intsizeh(size), type, NULL);
	}
}

static void alloc_t_make_sequence(addr *ret, size_t size, addr value)
{
	addr pos;
	size_t i;

	if (value == Unbound) value = Nil;
	vector_heap(&pos, size);
	for (i = 0; i < size; i++)
		setarray(pos, i, value);
	*ret = pos;
}

static void alloc_bitvector_make_sequence(addr *ret, size_t size, addr value)
{
	int bit;

	if (value == Unbound)
		bit = 0;
	else if (bit_getint(value, &bit))
		TypeError(value, BIT);
	bitmemory_unsafe(NULL, &value, size);
	bitmemory_memset(value, bit);
	*ret = value;
}

static void alloc_string_make_sequence(addr *ret, size_t size, addr value)
{
	unicode c;

	if (value == Unbound)
		c = 0;
	else if (characterp(value))
		GetCharacter(value, &c);
	else {
		TypeError(value, CHARACTER);
		return;
	}
	strvect_heap(&value, size);
	strvect_setall(value, c);
	*ret = value;
}

static void vector_upgraded_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	upgraded_array_value(type, &upgraded, &upsize);
	switch (upgraded) {
		case ARRAY_TYPE_BIT:
			alloc_bitvector_make_sequence(ret, size, value);
			break;

		case ARRAY_TYPE_CHARACTER:
			alloc_string_make_sequence(ret, size, value);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			vector_signed(ret, size, upgraded, upsize, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			vector_float(ret, size, upgraded, value);
			break;

		default:
			alloc_t_make_sequence(ret, size, value);
			break;
	}
}

static void vector_size_check(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 1, &arg);
	vector_size_make_sequence(type, arg, size);
}

static void simple_vector_size_check(addr type, size_t size)
{
	addr arg;

	GetArrayType(type, 0, &arg);
	vector_size_make_sequence(type, arg, size);
}

static int vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LispDecl(type) != LISPDECL_VECTOR)
		return 0;
	/* vector size */
	vector_size_check(type, size);
	/* make-sequence */
	vector_upgraded_make_sequence(ret, type, size, value);

	return 1;
}

static int simple_vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return 0;
	simple_vector_size_check(type, size);
	alloc_t_make_sequence(ret, size, value);

	return 1;
}

static int string_decl_p(addr type)
{
	switch (LispDecl(type)) {
		case LISPDECL_STRING:
		case LISPDECL_BASE_STRING:
		case LISPDECL_SIMPLE_STRING:
		case LISPDECL_SIMPLE_BASE_STRING:
			return 1;

		default:
			return 0;
	}
}

static int string_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (! string_decl_p(type))
		return 0;
	simple_vector_size_check(type, size);
	alloc_string_make_sequence(ret, size, value);

	return 1;
}

static void array_size_make_sequence(addr type, size_t size)
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
		if (getindex_integer(arg, &check))
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
		if (getindex_integer(arg, &check))
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

static int array_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return 0;

	/* dimension check */
	array_size_make_sequence(type, size);
	/* make-sequence */
	vector_upgraded_make_sequence(ret, type, size, value);

	return 1;
}

static int bitvector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return 0;

	/* make-sequence */
	simple_vector_size_check(type, size);
	alloc_bitvector_make_sequence(ret, size, value);

	return 1;
}

static void sequence_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	/* list */
	if (list_make_sequence(ret, type, size, value))
		return;

	/* vector */
	if (vector_make_sequence(ret, type, size, value))
		return;

	/* simple-vector */
	if (simple_vector_make_sequence(ret, type, size, value))
		return;

	/* string */
	if (string_make_sequence(ret, type, size, value))
		return;

	/* array */
	if (array_make_sequence(ret, type, size, value))
		return;

	/* bitvector */
	if (bitvector_make_sequence(ret, type, size, value))
		return;

	/* error */
	type_error_stdarg(type, Nil, "Invalid type-specifier ~S.", type, NULL);
}

_g int make_sequence_sequence(Execute ptr, addr *ret, addr type, addr size, addr rest)
{
	addr check, element;
	size_t index;

	if (getkeyargs(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Unbound;
	if (getindex_integer(size, &index))
		fmte("Too large index ~S.", size, NULL);
	if (parse_type(ptr, &check, type, Nil))
		return 1;
	sequence_make_sequence(ret, check, index, element);

	return typep_asterisk_error(*ret, check);
}


/*
 *  seqrange
 */
struct sequence_range {
	unsigned listp : 1;
	unsigned endp : 1;
	addr pos, list, prev;
	size_t start, end, size, index;
	addr save_pos, save_list, save_prev;
	size_t save_index, save_end, save_size;
};

typedef struct sequence_range seqrange;

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

static void save_seqrange(seqrange *ptr)
{
	ptr->save_pos = ptr->pos;
	ptr->save_list = ptr->list;
	ptr->save_prev = ptr->prev;
	ptr->save_size = ptr->size;
	ptr->save_end = ptr->end;
	ptr->save_index = ptr->index;
}

static void load_seqrange(seqrange *ptr)
{
	ptr->pos = ptr->save_pos;
	ptr->list = ptr->save_list;
	ptr->prev = ptr->save_prev;
	ptr->size = ptr->save_size;
	ptr->end = ptr->save_end;
	ptr->index = ptr->save_index;
}

static void build_seqrange(seqrange *ptr, addr pos, addr start, addr end)
{
	unsigned listp;
	addr list, prev;
	size_t index1, index2, size;

	clearpoint(ptr);
	listp = listp_sequence(pos);
	ptr->pos = list = pos;
	ptr->listp = listp;
	if (start == Nil || start == Unbound)
		start = fixnumh(0);
	if (end == Nil)
		end = Unbound;

	if (listp) {
		list_start_end(&list, &prev, start, end, &index1, &index2);
		ptr->list = list;
		ptr->prev = prev;
		ptr->start = index1;
		ptr->endp = (end != Unbound);
		if (ptr->endp) {
			ptr->end = index2;
			ptr->size = index2 - index1;
		}
		else {
			ptr->end = 0;
			ptr->size = 0;
		}
		ptr->index = index1;
	}
	else {
		size = length_sequence(pos, 1);
		sequence_start_end(start, end, size, &index1, &index2);
		ptr->prev = Nil;
		ptr->start = index1;
		ptr->endp = 1;
		ptr->end = index2;
		ptr->size = index2 - index1;
		ptr->index = index1;
	}
	save_seqrange(ptr);
}

static seqrange *seqrange_local(LocalRoot local)
{
	return (seqrange *)lowlevel_local(local, sizeoft(seqrange));
}

static seqrange *make_seqrange(LocalRoot local, addr pos, addr start, addr end)
{
	seqrange *ptr = seqrange_local(local);
	build_seqrange(ptr, pos, start, end);
	return ptr;
}

static size_t list_length_start_end(addr list, size_t index1, size_t index2, addr end)
{
	size_t size;

	for (size = 0; ; size++, index1++) {
		if (end != Unbound || end != Unbound) {
			if (index2 <= index1)
				break;
			if (list == Nil)
				fmte(":END ~A must be less than equal to list length.", end, NULL);
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list))
			fmte("Don't accept the dotted list ~S.", list, NULL);
		GetCdr(list, &list);
	}

	return size;
}

static void build_seqrange_endp(seqrange *ptr, addr list, addr start, addr end)
{
	build_seqrange(ptr, list, start, end);
	if (! ptr->endp) {
		ptr->size = length_list_safe(ptr->list);
		ptr->end = ptr->start + ptr->size;
		ptr->endp = 1;
	}
}

static seqrange *make_seqrange_endp(LocalRoot local, addr list, addr start, addr end)
{
	seqrange *ptr = seqrange_local(local);
	build_seqrange_endp(ptr, list, start, end);
	return ptr;
}

static void build_seqrange_vector_tail(LocalRoot local,
		seqrange *ptr, addr list, addr start, addr end,
		addr *root, addr *tail)
{
	addr pos, value;
	size_t index1, index2, size, i;

	/* vector */
	if (! listp_sequence(list)) {
		build_seqrange(ptr, list, start, end);
		return;
	}

	/* list */
	list_start_end(&list, NULL, start, end, &index1, &index2);
	size = list_length_start_end(list, index1, index2, end);
	if (root) *root = list;
	vector_local(local, &pos, size);
	for (i = 0; i < size; i++) {
		GetCons(list, &value, &list);
		setarray(pos, i, value);
	}
	if (tail) *tail = list;

	build_seqrange(ptr, pos, fixnumh(0), Nil);
}

static void build_seqrange_vector(LocalRoot local,
		seqrange *ptr, addr list, addr start, addr end)
{
	build_seqrange_vector_tail(local, ptr, list, start, end, NULL, NULL);
}

static seqrange *make_seqrange_vector(LocalRoot local, addr list, addr start, addr end)
{
	seqrange *ptr = seqrange_local(local);
	build_seqrange_vector(local, ptr, list, start, end);
	return ptr;
}

static int get_list_seqrange(seqrange *ptr, addr *ret)
{
	if (! ptr->endp) {
		if (ptr->list == Nil)
			return 1;
		else
			goto normal;
	}
	if (ptr->index < ptr->end) {
		if (ptr->list == Nil)
			goto error;
		else
			goto normal;
	}
	return 1;

normal:
	getcar(ptr->list, ret);
	return 0;

error:
	fmte(":END ~A must be less than equal to list length.",
			intsizeh(ptr->end), NULL);
	return 1;
}

static int get_seqrange(seqrange *ptr, addr *ret)
{
	/* list */
	if (ptr->listp)
		return get_list_seqrange(ptr, ret);

	/* vector */
	if (ptr->end <= ptr->index)
		return 1;
	getelt_sequence(NULL, ptr->pos, ptr->index, ret);

	return 0;
}

static int getnext_seqrange(seqrange *ptr, addr *ret)
{
	int check;

	if (ptr->listp) {
		check = get_list_seqrange(ptr, ret);
		if (! check) {
			ptr->prev = ptr->list;
			getcons(ptr->list, ret, &(ptr->list));
			ptr->index++;
		}
		return check;
	}
	else {
		if (ptr->end <= ptr->index)
			return 1;
		getelt_sequence(NULL, ptr->pos, ptr->index++, ret);

		return 0;
	}
}

static int next_seqrange(seqrange *ptr)
{
	addr temp;
	return getnext_seqrange(ptr, &temp);
}

static int endp_seqrange(seqrange *ptr)
{
	if (ptr->endp)
		return ptr->end <= ptr->index;
	else
		return ptr->list == Nil;
}

static void set_seqrange(seqrange *ptr, addr value)
{
	Check(endp_seqrange(ptr), "endp error");
	if (ptr->listp)
		SetCar(ptr->list, value);
	else
		setelt_sequence(ptr->pos, ptr->index, value);
}

static void get_inplace_seqrange(seqrange *ptr, struct array_value *ret)
{
	Check(endp_seqrange(ptr), "endp error");
	if (ptr->listp) {
		ret->type = ARRAY_TYPE_T;
		GetCar(ptr->list, &(ret->value.object));
	}
	else {
		getelt_inplace_sequence(ptr->pos, ptr->index, ret);
	}
}

static void set_inplace_seqrange(LocalRoot local,
		seqrange *ptr, const struct array_value *str)
{
	addr value;

	Check(endp_seqrange(ptr), "endp error");
	if (ptr->listp) {
		array_value_alloc(local, &value, str);
		SetCar(ptr->list, value);
	}
	else {
		setelt_inplace_sequence(local, ptr->pos, ptr->index, str);
	}
}

static void reverse_seqrange(seqrange *ptr)
{
	Check(ptr->listp, "type error");
	ptr->index = ptr->end;
}

static int endp_reverse_seqrange(seqrange *ptr)
{
	Check(ptr->listp, "type error");
	return ptr->index <= ptr->start;
}

static int next_reverse_seqrange(seqrange *ptr)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_seqrange(ptr)) return 1;
	ptr->index--;

	return 0;
}

static int get_reverse_seqrange(seqrange *ptr, addr *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_seqrange(ptr)) return 1;
	getelt_sequence(NULL, ptr->pos, ptr->index - 1, ret);

	return 0;
}

static int getnext_reverse_seqrange(seqrange *ptr, addr *ret)
{
	Check(ptr->listp, "type error");
	if (endp_reverse_seqrange(ptr)) return 1;
	ptr->index--;
	getelt_sequence(NULL, ptr->pos, ptr->index, ret);

	return 0;
}

static void set_reverse_seqrange(seqrange *ptr, addr value)
{
	Check(endp_reverse_seqrange(ptr), "endp error");
	Check(ptr->listp, "type error");
	setelt_sequence(ptr->pos, ptr->index - 1, value);
}

static void remove_seqrange(seqrange *ptr)
{
	Check(! ptr->listp, "type error");
	Check(ptr->list == Nil, "list error");

	if (ptr->endp) {
		if (ptr->end <= ptr->index)
			fmte(":end size error", NULL);
		ptr->end--;
		ptr->size--;
	}
	if (ptr->prev == Nil) {
		getcdr(ptr->list, &(ptr->list));
		ptr->pos = ptr->list;
	}
	else {
		getcdr(ptr->list, &(ptr->list));
		setcdr(ptr->prev, ptr->list);
	}
}


/*
 *  subseq
 */
_g void list_subseq(addr *ret, addr list, addr start, addr end)
{
	addr root, pos;
	seqrange range;

	build_seqrange(&range, list, start, end);
	root = Nil;
	while (! getnext_seqrange(&range, &pos))
		cons_heap(&root, pos, root);
	nreverse_list_unsafe(ret, root);
}

_g void vector_subseq(addr *ret, addr vector, addr start, addr end)
{
	size_t index1, index2, i;
	addr root, item;

	/* argument */
	lenarray(vector, &index1);
	sequence_start_end(start, end, index1, &index1, &index2);

	/* subseq */
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		getarray(vector, index1, &item);
		setarray(root, i, item);
	}
	*ret = root;
}

_g void setf_subseq_sequence(addr root, addr pos, addr start, addr end)
{
	struct array_value value;
	seqrange range1, range2;

	build_seqrange(&range1, root, start, end);
	build_seqrange(&range2, pos, Nil, Nil);
	for (;;) {
		if (endp_seqrange(&range1)) break;
		if (endp_seqrange(&range2)) break;
		get_inplace_seqrange(&range2, &value);
		set_inplace_seqrange(NULL, &range1, &value);
		next_seqrange(&range1);
		next_seqrange(&range2);
	}
}


/*
 *  map
 */
struct iterator_sequence {
	unsigned listp : 1;
	addr pos, root;
	size_t size, index;
};

struct iterator_group {
	struct iterator_sequence **data;
	size_t size, callsize;
	addr list;
};

typedef struct iterator_sequence iterseq;
typedef struct iterator_group itergroup;

static iterseq *make_iterseq_local(LocalRoot local, addr pos, int fill)
{
	unsigned check;
	iterseq *ptr;

	Check(local == NULL, "local error");
	ptr = (iterseq *)lowlevel_local(local, sizeoft(iterseq));
	ptr->pos = pos;
	ptr->root = pos;
	check = listp(pos);
	ptr->listp = check;
	ptr->size = check? 0: length_sequence(pos, fill);
	ptr->index = 0;

	return ptr;
}

static int end_iterseq(iterseq *ptr)
{
	if (ptr->listp)
		return ptr->pos == Nil;
	else
		return ptr->size <= ptr->index;
}

static void length_iterseq(iterseq *ptr, size_t *ret)
{
	if (ptr->listp)
		*ret = length_list_safe(ptr->root);
	else
		*ret = ptr->size;
}

static itergroup *make_itergroup_local(LocalRoot local, addr rest, int fill)
{
	itergroup *ptr;
	iterseq **data;
	addr pos;
	size_t size, i;

	size = length_list_safe(rest);
	ptr = (itergroup *)lowlevel_local(local, sizeoft(itergroup));
	data  = (iterseq **)lowlevel_local(local, sizeoft(iterseq *) * size);

	for (i = 0; i < size; i++) {
		GetCons(rest, &pos, &rest);
		data[i] = make_iterseq_local(local, pos, fill);
	}
	ptr->data = data;
	ptr->size = size;
	ptr->callsize = 0;
	ptr->list = NULL;

	return ptr;
}

static void list_itergroup_local(LocalRoot local, addr *ret, itergroup *group)
{
	addr root;
	size_t size, i;

	size = group->size;
	root = Nil;
	for (i = 0; i < size; i++)
		conscdr_local(local, &root, root);
	group->list = root;
	if (ret) *ret = root;
}

static int object_iterseq(iterseq *iter, addr *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return 0;
		getcons(iter->pos, ret, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return 0;
		getelt_sequence(NULL, iter->pos, iter->index, ret);
	}
	iter->index++;

	return 1;
}

static int next_iterseq(iterseq *iter, addr *ret)
{
	if (iter->listp) {
		if (iter->pos == Nil)
			return 0;
		getcons(iter->pos, ret, &(iter->pos));
	}
	else {
		if (iter->size <= iter->index)
			return 0;
	}
	iter->index++;

	return 1;
}

static int set_iterseq(iterseq *iter, addr value)
{
	if (iter->listp) {
		setcar(iter->pos, value);
		getcdr(iter->pos, &(iter->pos));
		iter->index++;
		return iter->pos == Nil;
	}
	else {
		setelt_sequence(iter->pos, iter->index, value);
		iter->index++;
		return iter->size <= iter->index;
	}
}

static int set_itergroup(itergroup *group, addr list)
{
	iterseq **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		if (! object_iterseq(data[i], &temp))
			return 0;
		Check(! consp(list), "list error");
		SetCar(list, temp);
		GetCdr(list, &list);
	}

	return 1;
}

static void clear_itergroup(itergroup *group)
{
	iterseq **data, *ptr;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		ptr = data[i];
		ptr->pos = ptr->root;
		ptr->index = 0;
	}
}

static int next_itergroup(itergroup *group)
{
	iterseq **data;
	addr temp;
	size_t size, i;

	data = group->data;
	size = group->size;
	for (i = 0; i < size; i++) {
		if (! next_iterseq(data[i], &temp))
			return 0;
	}

	return 1;
}

static void count_itergroup(itergroup *group, size_t *ret)
{
	size_t size;

	for (size = 0; next_itergroup(group); size++)
		continue;
	group->callsize = size;
	if (ret) *ret = size;
}

static int nil_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	LocalRoot local;
	addr list, temp;
	itergroup *group;

	/* type check */
	if (LispDecl(type) != LISPDECL_NIL) {
		*result = 0;
		return 0;
	}

	/* execute */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	list_itergroup_local(local, &list, group);
	while (set_itergroup(group, list)) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
	}
	*result = 1;
	*ret = Nil;
	return 0;
}

static int list_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	LocalRoot local;
	addr list, temp, root;
	itergroup *group;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST) {
		*result = 0;
		return 0;
	}

	/* execute */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	list_itergroup_local(local, &list, group);
	for (root = Nil; set_itergroup(group, list); ) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		cons_heap(&root, temp, root);
	}
	nreverse_list_unsafe(ret, root);

	*result = 1;
	return 0;
}

static int vector_bitvector_map_sequence(Execute ptr, addr *ret,
		addr call, itergroup *group)
{
	addr list, root, value;
	size_t i;

	list = group->list;
	bitmemory_unsafe(NULL, &root, group->callsize);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		bitmemory_set(root, i, value);
	}
	*ret = root;

	return 0;
}

static int vector_string_map_sequence(Execute ptr, addr *ret,
		addr call, itergroup *group)
{
	addr list, root, value;
	size_t i;

	list = group->list;
	strvect_heap(&root, group->callsize);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		strvect_set(root, i, value);
	}
	*ret = root;

	return 0;
}

static int vector_signed_map_sequence(Execute ptr, addr *ret,
		addr call, itergroup *group, enum ARRAY_TYPE type, int bytesize)
{
	addr list, root, value;
	size_t i;

	list = group->list;
	vector_signed_uninit(&root, group->callsize, type, bytesize);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		array_set(root, i, value);
	}
	*ret = root;

	return 0;
}

static int vector_float_map_sequence(Execute ptr, addr *ret,
		addr call, itergroup *group, enum ARRAY_TYPE type)
{
	addr list, root, value;
	size_t i;

	list = group->list;
	vector_float_uninit(&root, group->callsize, type);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		array_set(root, i, value);
	}
	*ret = root;

	return 0;
}

static int vector_general_map_sequence(Execute ptr, addr *ret,
		addr call, itergroup *group)
{
	addr list, root, value;
	size_t i;

	list = group->list;
	vector_heap(&root, group->callsize);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		setarray(root, i, value);
	}
	*ret = root;

	return 0;
}

static int vector_upgraded_map_sequence(Execute ptr,
		addr *ret, addr type, addr call, itergroup *group)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	upgraded_array_value(type, &upgraded, &upsize);
	switch (upgraded) {
		case ARRAY_TYPE_BIT:
			return vector_bitvector_map_sequence(ptr, ret, call, group);

		case ARRAY_TYPE_CHARACTER:
			return vector_string_map_sequence(ptr, ret, call, group);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return vector_signed_map_sequence(ptr, ret, call, group, upgraded, upsize);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_map_sequence(ptr, ret, call, group, upgraded);

		default:
			return vector_general_map_sequence(ptr, ret, call, group);
	}
}

static int vector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	LocalRoot local;
	itergroup *group;
	size_t size;

	/* type check */
	if (LispDecl(type) != LISPDECL_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	count_itergroup(group, &size);
	vector_size_check(type, size);
	clear_itergroup(group);
	list_itergroup_local(local, NULL, group);

	/* map */
	if (vector_upgraded_map_sequence(ptr, ret, type, call, group)) return 1;
	*result = 1;
	return 0;
}

static int simple_vector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	LocalRoot local;
	addr list, temp, root;
	itergroup *group;
	size_t size, i;

	/* type check */
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	count_itergroup(group, &size);
	simple_vector_size_check(type, size);
	clear_itergroup(group);
	list_itergroup_local(local, &list, group);

	/* execute */
	vector_heap(&root, size);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		setarray(root, i, temp);
	}
	*result = 1;
	*ret = root;
	return 0;
}

static int string_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	LocalRoot local;
	addr list, temp, root;
	itergroup *group;
	size_t size, i;

	/* type check */
	if (! string_decl_p(type)) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	count_itergroup(group, &size);
	simple_vector_size_check(type, size);
	clear_itergroup(group);
	list_itergroup_local(local, &list, group);

	/* execute */
	strvect_heap(&root, size);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		strvect_set(root, i, temp);
	}
	*result = 1;
	*ret = root;
	return 0;
}

static int array_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	LocalRoot local;
	itergroup *group;
	size_t size;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	count_itergroup(group, &size);
	array_size_make_sequence(type, size);
	clear_itergroup(group);
	list_itergroup_local(local, NULL, group);

	/* make-sequence */
	if (vector_upgraded_map_sequence(ptr, ret, type, call, group)) return 1;
	*result = 1;
	return 0;
}

static int bitvector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	LocalRoot local;
	addr list, temp, root;
	itergroup *group;
	size_t size, i;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_itergroup_local(ptr->local, rest, 1);
	count_itergroup(group, &size);
	simple_vector_size_check(type, size);
	clear_itergroup(group);
	list_itergroup_local(local, &list, group);

	/* execute */
	bitmemory_unsafe(NULL, &root, size);
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		bitmemory_set(root, i, temp);
	}
	*result = 1;
	*ret = root;
	return 0;
}

static int execute_map_sequence(Execute ptr, addr *ret,
		addr type, addr call, addr rest)
{
	int check;

	/* nil */
	if (nil_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* list */
	if (list_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* vector */
	if (vector_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* simple-vector */
	if (simple_vector_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* string */
	if (string_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* array */
	if (array_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* bitvector */
	if (bitvector_map_sequence(ptr, &check, ret, type, call, rest))
		return 1;
	if (check)
		return 0;

	/* error */
	type_error_stdarg(type, Nil, "Invalid type-specifier ~S.", type, NULL);
	return 1;
}

_g int map_sequence(Execute ptr, addr *ret, addr type, addr call, addr rest)
{
	addr check;

	if (parse_type(ptr, &check, type, Nil))
		return 1;
	if (execute_map_sequence(ptr, ret, check, call, rest))
		return 1;
	if (type == Nil)
		return 0;

	return typep_asterisk_error(*ret, check);
}


/*
 *  map-into
 */
static void fill_map_into_sequence(addr var, size_t size)
{
	struct array_struct *str;

	if (! arrayp(var)) return;
	str = ArrayInfoStruct(var);
	if (! str->fillpointer) return;
	str->front = size;
}

static int execute_map_into_sequence(Execute ptr, addr var, addr call, addr rest)
{
	LocalRoot local;
	iterseq *into;
	itergroup *group;
	addr list, pos;
	size_t i;

	/* argument */
	local = ptr->local;
	into = make_iterseq_local(local, var, 0);
	if (end_iterseq(into))
		return 0;
	group = make_itergroup_local(local, rest, 1);
	list_itergroup_local(local, &list, group);

	/* map-into */
	for (i = 0; set_itergroup(group, list); i++) {
		if (callclang_apply(ptr, &pos, call, list))
			return 1;
		if (set_iterseq(into, pos))
			break;
	}

	/* update fill-pointer */
	fill_map_into_sequence(var, i);

	return 0;
}

_g int map_into_sequence(Execute ptr, addr var, addr call, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (execute_map_into_sequence(ptr, var, call, rest)) return 1;
	rollback_local(local, stack);

	return 0;
}


/*
 *  reduce
 */
struct reduce_struct {
	unsigned valuep : 1;
	unsigned fromp : 1;
	Execute ptr;
	LocalRoot local;
	addr pos, call, key, start, end, from, value;
	seqrange range;
};

static int key_reduce_sequence(struct reduce_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	else {
		*ret = value;
		return 0;
	}
}

static int throw_reduce_sequence(struct reduce_struct *str, int *result, addr *ret)
{
	addr value, pos;
	seqrange *range;

	value = str->value;
	range = &(str->range);
	save_seqrange(range);

	/* empty sequence */
	if (getnext_seqrange(range, &pos)) {
		if (value == Unbound) {
			if (callclang_apply(str->ptr, ret, str->call, Nil))
				return 1;
		}
		else {
			*ret = value;
		}
		*result = 1;
		return 0;
	}

	/* single value */
	if (endp_seqrange(range) && value == Unbound) {
		if (key_reduce_sequence(str, ret, pos))
			return 1;
		*result = 1;
		return 0;
	}

	/* multiple value */
	load_seqrange(range);
	*result = 0;
	return 0;
}

static int value_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	Execute ptr;
	addr pos1, pos2, call;
	seqrange *range;

	ptr = str->ptr;
	range = &(str->range);
	pos1 = str->value;
	call = str->call;

	/* first */
	if (pos1 == Unbound) {
		getnext_seqrange(range, &pos1);
		if (key_reduce_sequence(str, &pos1, pos1)) return 1;
	}

	/* loop */
	while (! getnext_seqrange(range, &pos2)) {
		if (key_reduce_sequence(str, &pos2, pos2)) return 1;
		if (callclang_funcall(ptr, &pos1, call, pos1, pos2, NULL)) return 1;
	}
	*ret = pos1;

	return 0;
}

static int reverse_vector_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	Execute ptr;
	addr pos1, pos2, call;
	seqrange *range;

	ptr = str->ptr;
	range = &(str->range);
	pos2 = str->value;
	call = str->call;

	/* first */
	reverse_seqrange(range);
	if (pos2 == Unbound) {
		getnext_reverse_seqrange(range, &pos2);
		if (key_reduce_sequence(str, &pos2, pos2)) return 1;
	}

	/* loop */
	while (! getnext_reverse_seqrange(range, &pos1)) {
		if (key_reduce_sequence(str, &pos1, pos1)) return 1;
		if (callclang_funcall(ptr, &pos2, call, pos1, pos2, NULL)) return 1;
	}
	*ret = pos2;

	return 0;
}

static int switch_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	int check;

	if (throw_reduce_sequence(str, &check, ret))
		return 1;
	if (check)
		return 0;
	else if (str->range.listp)
		return value_reduce_sequence(str, ret);
	else if (str->fromp)
		return reverse_vector_reduce_sequence(str, ret);
	else
		return value_reduce_sequence(str, ret);
}

static int reverse_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_seqrange_vector(local, range, str->pos, str->start, str->end);
	if (switch_reduce_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int reduce_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr key, start, end, from, value;
	struct reduce_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_INITIAL_VALUE, &value)) value = Unbound;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.valuep = (value != Unbound);
	str.ptr = ptr;
	str.local = ptr->local;
	str.call = call;
	str.pos = pos;
	str.key = key;
	str.start = start;
	str.end = end;
	str.fromp = fromp;
	str.from = from;
	str.value = value;

	if (fromp && listp)
		return reverse_reduce_sequence(&str, ret);
	else {
		build_seqrange(range, pos, start, end);
		return switch_reduce_sequence(&str, ret);
	}
}


/*
 *  count
 */
struct count_struct {
	unsigned delp : 1;
	unsigned fromp : 1;
	unsigned notp : 1;
	unsigned single : 1;
	unsigned test : 2;
	Execute ptr;
	LocalRoot local;
	addr item, second, pos, from, start, end, key, test1, test2, count;
	size_t limit;
	seqrange range;
};

static int boolean_count_sequence(struct count_struct *str, int *result, addr value)
{
	Execute ptr;
	addr test;

	switch (str->test) {
		case 1:
			test = str->test1;
			break;

		case 2:
			test = str->test2;
			break;

		default:
			test = Nil;
			break;
	}

	ptr = str->ptr;
	if (str->key != Nil) {
		if (callclang_funcall(ptr, &value, str->key, value, NULL))
			return 1;
	}
	if (str->single) {
		if (callclang_funcall(ptr, &value, test, value, NULL))
			return 1;
	}
	else if (test == Nil) {
		value = eql_function(str->item, value)? T: Nil;
	}
	else {
		if (callclang_funcall(ptr, &value, test, str->item, value, NULL))
			return 1;
	}
	if (str->notp)
		*result = (value == Nil);
	else
		*result = (value != Nil);

	return 0;
}

static int value_count_sequence(struct count_struct *str, addr *ret)
{
	int check;
	int (*call)(seqrange *, addr *);
	addr value;
	seqrange *range;
	size_t count;

	/* initialize */
	count = 0;
	range = &(str->range);
	if (str->fromp) {
		reverse_seqrange(range);
		call = getnext_reverse_seqrange;
	}
	else {
		call = getnext_seqrange;
	}

	/* loop */
	while (! call(range, &value)) {
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check)
			count++;
	}
	make_index_integer_alloc(NULL, ret, count);

	return 0;
}

static int reverse_count_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_seqrange_vector(local, range, str->pos, str->start, str->end);
	if (value_count_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int count_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("COUNT don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item;
	str.pos = pos;

	if (listp && fromp)
		return reverse_count_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_count_sequence(&str, ret);
}

static int count_if_argument_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.pos = pos;

	if (listp && fromp)
		return reverse_count_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_count_sequence(&str, ret);
}

_g int count_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return count_if_argument_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int count_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return count_if_argument_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  merge
 */
static int key_merge_sequence(Execute ptr, addr *ret, addr key, addr value)
{
	if (key != Nil) {
		return callclang_funcall(ptr, ret, key, value, NULL);
	}
	else {
		*ret = value;
		return 0;
	}
}

static int list_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	enum LISPDECL decl;
	addr root, a1, a2, b1, b2, check;
	iterseq *str1, *str2;
	LocalRoot local;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST) {
		*result = 0;
		return 0;
	}

	/* make list */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	root = Nil;
	if (! object_iterseq(str1, &a1)) {
		goto tail2;
	}
	if (! object_iterseq(str2, &b1)) {
		cons_heap(&root, a1, root);
		goto tail1;
	}
	if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
loop:
	callclang_funcall(ptr, &check, call, a2, b2, NULL);
	if (check != Nil) {
		cons_heap(&root, a1, root);
		if (! object_iterseq(str1, &a1)) {
			cons_heap(&root, b1, root);
			goto tail2;
		}
		if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	}
	else {
		cons_heap(&root, b1, root);
		if (! object_iterseq(str2, &b1)) {
			cons_heap(&root, a1, root);
			goto tail1;
		}
		if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
	}
	goto loop;

tail1:
	while (object_iterseq(str1, &a1))
		cons_heap(&root, a1, root);
	goto result;

tail2:
	while (object_iterseq(str2, &b1))
		cons_heap(&root, b1, root);
	goto result;

result:
	nreverse_list_unsafe(ret, root);
	*result = 1;
	return 0;
}

static int vector_make_merge_sequence(Execute ptr, addr root,
		iterseq *str1, iterseq *str2, addr call, addr key)
{
	addr a1, a2, b1, b2, check;
	size_t i;

	/* make list */
	i = 0;
	if (! object_iterseq(str1, &a1)) {
		goto tail2;
	}
	if (! object_iterseq(str2, &b1)) {
		setelt_sequence(root, i++, a1);
		goto tail1;
	}
	if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
loop:
	callclang_funcall(ptr, &check, call, a2, b2, NULL);
	if (check != Nil) {
		setelt_sequence(root, i++, a1);
		if (! object_iterseq(str1, &a1)) {
			setelt_sequence(root, i++, b1);
			goto tail2;
		}
		if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	}
	else {
		setelt_sequence(root, i++, b1);
		if (! object_iterseq(str2, &b1)) {
			setelt_sequence(root, i++, a1);
			goto tail1;
		}
		if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
	}
	goto loop;

tail1:
	while (object_iterseq(str1, &a1))
		setelt_sequence(root, i++, a1);
	return 0;

tail2:
	while (object_iterseq(str2, &b1))
		setelt_sequence(root, i++, b1);
	return 0;
}

static void make_specialized_sequence(addr *ret,
		enum ARRAY_TYPE type, int bytesize, size_t size)
{
	switch (type) {
		case ARRAY_TYPE_T:
			vector_heap(ret, size);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_unsafe(NULL, ret, size);
			break;

		case ARRAY_TYPE_CHARACTER:
			strvect_heap(ret, size);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			vector_signed_uninit(ret, size, type, bytesize);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			vector_float_uninit(ret, size, type);
			break;

		default:
			fmte("Invalid array type.", NULL);
			return;
	}
}

static void array_upgraded_merge_sequence(addr *ret, addr type, size_t size)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	upgraded_array_value(type, &upgraded, &upsize);
	make_specialized_sequence(ret, upgraded, upsize, size);
}

static int vector_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	LocalRoot local;
	iterseq *str1, *str2;
	addr root;
	size_t size, size1, size2;

	/* type check */
	if (LispDecl(type) != LISPDECL_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	length_iterseq(str1, &size1);
	length_iterseq(str2, &size2);
	size = size1 + size2;
	vector_size_check(type, size);
	array_upgraded_merge_sequence(&root, type, size);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	*ret = root;
	*result = 1;
	return 0;
}

static int simple_vector_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	LocalRoot local;
	iterseq *str1, *str2;
	addr root;
	size_t size, size1, size2;

	/* type check */
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	length_iterseq(str1, &size1);
	length_iterseq(str2, &size2);
	size = size1 + size2;
	simple_vector_size_check(type, size);
	vector_heap(&root, size);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	*ret = root;
	*result = 1;
	return 0;
}

static int string_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	LocalRoot local;
	iterseq *str1, *str2;
	addr root;
	size_t size, size1, size2;

	/* type check */
	if (! string_decl_p(type)) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	length_iterseq(str1, &size1);
	length_iterseq(str2, &size2);
	size = size1 + size2;
	simple_vector_size_check(type, size);
	strvect_heap(&root, size);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	*ret = root;
	*result = 1;
	return 0;
}

static int array_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	enum LISPDECL decl;
	LocalRoot local;
	iterseq *str1, *str2;
	addr root;
	size_t size, size1, size2;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	length_iterseq(str1, &size1);
	length_iterseq(str2, &size2);
	size = size1 + size2;
	array_size_make_sequence(type, size);
	array_upgraded_merge_sequence(&root, type, size);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	*ret = root;
	*result = 1;
	return 0;
}

static int bitvector_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	enum LISPDECL decl;
	LocalRoot local;
	iterseq *str1, *str2;
	addr root;
	size_t size, size1, size2;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_iterseq_local(local, pos1, 1);
	str2 = make_iterseq_local(local, pos2, 1);
	length_iterseq(str1, &size1);
	length_iterseq(str2, &size2);
	size = size1 + size2;
	simple_vector_size_check(type, size);
	bitmemory_unsafe(NULL, &root, size);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	*ret = root;
	*result = 1;
	return 0;
}

static int execute_merge_sequence(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	int check;

	/* list */
	if (list_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* vector */
	if (vector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* simple-vector */
	if (simple_vector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* string */
	if (string_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* array */
	if (array_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* bitvector */
	if (bitvector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key))
		return 1;
	if (check)
		return 0;

	/* error */
	type_error_stdarg(type, Nil, "Invalid type-specifier ~S.", type, NULL);
	return 1;
}

_g int merge_sequence(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	addr check;

	if (parse_type(ptr, &check, type, Nil))
		return 1;
	if (execute_merge_sequence(ptr, ret, check, pos1, pos2, call, key))
		return 1;

	return typep_asterisk_error(*ret, check);
}


/*
 *  find
 */
static int value_find_sequence(struct count_struct *str, addr *ret)
{
	int check;
	int (*call)(seqrange *, addr *);
	addr value;
	seqrange *range;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		reverse_seqrange(range);
		call = getnext_reverse_seqrange;
	}
	else {
		call = getnext_seqrange;
	}

	/* loop */
	while (! call(range, &value)) {
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check) {
			*ret = value;
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}

static int reverse_find_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_seqrange_vector(local, range, str->pos, str->start, str->end);
	if (value_find_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int find_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("FIND don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item;
	str.pos = pos;

	if (listp && fromp)
		return reverse_find_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_find_sequence(&str, ret);
}

static int find_if_argument_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.pos = pos;

	if (listp && fromp)
		return reverse_find_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_find_sequence(&str, ret);
}

_g int find_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return find_if_argument_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int find_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return find_if_argument_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  position
 */
static int value_position_sequence(struct count_struct *str, addr *ret)
{
	int check;
	int (*call)(seqrange *, addr *);
	addr value;
	seqrange *range;
	size_t count;

	/* initialize */
	count = 0;
	range = &(str->range);
	if (str->fromp) {
		reverse_seqrange(range);
		call = getnext_reverse_seqrange;
	}
	else {
		call = getnext_seqrange;
	}

	/* loop */
	for (count = 0; ! call(range, &value); count++) {
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check) {
			*ret = intsizeh(count);
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}

static int reverse_position_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_seqrange_vector(local, range, str->pos, str->start, str->end);
	if (value_position_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int position_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("POSITION don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item;
	str.pos = pos;

	if (listp && fromp)
		return reverse_position_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_position_sequence(&str, ret);
}

static int position_if_argument_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.pos = pos;

	if (listp && fromp)
		return reverse_position_sequence(&str, ret);
	build_seqrange(range, pos, start, end);
	return value_position_sequence(&str, ret);
}

_g int position_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return position_if_argument_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int position_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return position_if_argument_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  search
 */
struct search_struct {
	unsigned fromp : 1;
	unsigned notp : 1;
	unsigned test : 2;
	Execute ptr;
	LocalRoot local;
	addr pos1, pos2, list1, list2, test1, test2, key;
	addr start1, start2, end1, end2, from;
	seqrange *range1, *range2;
};

static int key_search_sequence(struct search_struct *str, addr *ret, addr value)
{
	if (str->key != Nil) {
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	}
	else {
		*ret = value;
		return 0;
	}
}

static int call_search_sequence(struct search_struct *str,
		int *result, addr a, addr b)
{
	Execute ptr;
	addr test, value;

	switch (str->test) {
		case 1:
			test = str->test1;
			break;

		case 2:
			test = str->test2;
			break;

		default:
			test = Nil;
			break;
	}

	ptr = str->ptr;
	if (test == Nil)
		value = eql_function(a, b)? T: Nil;
	else {
		if (callclang_funcall(ptr, &value, test, a, b, NULL))
			return 1;
	}
	if (str->notp)
		*result = (value == Nil);
	else
		*result = (value != Nil);

	return 0;
}

static int reverse_pattern_search_sequence(struct search_struct *str,
		int *result, seqrange *range1, seqrange *range2, size_t x)
{
	int check;
	addr a, b;

	range2->index = range2->start + x;
	load_seqrange(range1);
	for (;;) {
		if (getnext_seqrange(range1, &a))
			break;
		if (key_search_sequence(str, &a, a))
			return 1;
		if (getnext_seqrange(range2, &b)) {
			*result = 0;
			return 0;
		}
		if (key_search_sequence(str, &b, b))
			return 1;
		if (call_search_sequence(str, &check, a, b))
			return 1;
		if (! check) {
			*result = 0;
			return 0;
		}
	}
	*result = 1;
	return 0;
}

static int reverse_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	seqrange *range1, *range2;
	size_t size1, size2, i;

	range1 = str->range1;
	range2 = str->range2;
	size1 = range1->size;
	size2 = range2->size;
	if (size2 < size1) {
		*ret = Nil;
		return 0;
	}
	if (size1 == 0) {
		*ret = fixnumh(0);
		return 0;
	}

	i = size2 - size1 + 1;
	save_seqrange(range1);
	while (i) {
		i--;
		if (reverse_pattern_search_sequence(str, &check, range1, range2, i))
			return 1;
		if (check) {
			*ret = intsizeh(i);
			return 0;
		}
	}
	*ret = Nil;

	return 0;
}

static int normal_pattern_search_sequence(struct search_struct *str,
		int *result, seqrange *range1, seqrange *range2)
{
	int check;
	addr a, b;

	load_seqrange(range1);
	for (;;) {
		if (getnext_seqrange(range1, &a)) break;
		if (key_search_sequence(str, &a, a))
			return 1;
		if (getnext_seqrange(range2, &b)) {
			*result = 0;
			return 0;
		}
		if (key_search_sequence(str, &b, b))
			return 1;
		if (call_search_sequence(str, &check, a, b))
			return 1;
		if (! check) {
			*result = 0;
			return 0;
		}
	}
	*result = 1;
	return 0;
}

static int normal_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	seqrange *range1, *range2;
	size_t i;

	range1 = str->range1;
	range2 = str->range2;
	if (range1->endp && range2->endp && range2->size < range1->size) {
		*ret = Nil;
		return 0;
	}

	for (i = 0; ; i++) {
		save_seqrange(range2);
		if (normal_pattern_search_sequence(str, &check, range1, range2))
			return 1;
		if (check) {
			*ret = intsizeh(i);
			return 0;
		}
		load_seqrange(range2);
		if (next_seqrange(range2))
			break;
	}
	*ret = Nil;

	return 0;
}

static int search_execute_sequence(Execute ptr, addr *ret,
		addr pos1, addr pos2, addr rest)
{
	unsigned fromp;
	LocalRoot local;
	addr from, key, test1, test2, start1, start2, end1, end2;
	struct search_struct str;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_START1, &start1)) start1 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_START2, &start2)) start2 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END1, &end1)) end1 = Unbound;
	if (getkeyargs(rest, KEYWORD_END2, &end2)) end2 = Unbound;
	if (test1 != Nil && test2 != Nil)
		fmte("SEARCH don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	local = ptr->local;
	str.ptr = ptr;
	str.local = local;
	fromp = (from != Nil);
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start1 = start1;
	str.start2 = start2;
	str.end1 = end1;
	str.end2 = end2;
	str.pos1 = pos1;
	str.pos2 = pos2;
	if (fromp) {
		str.range1 = make_seqrange_endp(local, pos1, start1, end1);
		str.range2 = make_seqrange_vector(local, pos2, start2, end2);
		if (reverse_search_sequence(&str, ret))
			return 1;
	}
	else {
		str.range1 = make_seqrange(local, pos1, start1, end1);
		str.range2 = make_seqrange(local, pos2, start2, end2);
		if (normal_search_sequence(&str, ret))
			return 1;
	}

	return 0;
}

_g int search_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (search_execute_sequence(ptr, ret, pos1, pos2, rest))
		return 1;
	rollback_local(local, stack);

	return 0;
}


/*
 *  mismatch
 */
static int reverse_mismatch_sequence(struct search_struct *str, addr *ret)
{
	int check, check1, check2;
	addr a, b;
	seqrange *range1, *range2;
	size_t i;

	range1 = str->range1;
	range2 = str->range2;
	reverse_seqrange(range1);
	reverse_seqrange(range2);
	for (i = 0; ; i++) {
		check1 = getnext_reverse_seqrange(range1, &a);
		check2 = getnext_reverse_seqrange(range2, &b);
		if (check1 && check2)
			goto result_nil;
		if (check1 || check2)
			goto result_t;
		if (key_search_sequence(str, &a, a))
			return 1;
		if (key_search_sequence(str, &b, b))
			return 1;
		if (call_search_sequence(str, &check, a, b))
			return 1;
		if (! check)
			goto result_t;
	}

result_nil:
	*ret = Nil;
	return 0;

result_t:
	*ret = fixnumh(i);
	return 0;
}

static int normal_mismatch_sequence(struct search_struct *str, addr *ret)
{
	int check, check1, check2;
	seqrange *range1, *range2;
	addr a, b;
	size_t i;

	range1 = str->range1;
	range2 = str->range2;
	for (i = 0; ; i++) {
		check1 = getnext_seqrange(range1, &a);
		check2 = getnext_seqrange(range2, &b);
		if (check1 && check2)
			goto result_nil;
		if (check1 || check2)
			goto result_t;
		if (key_search_sequence(str, &a, a))
			return 1;
		if (key_search_sequence(str, &b, b))
			return 1;
		if (call_search_sequence(str, &check, a, b))
			return 1;
		if (! check)
			goto result_t;
	}

result_nil:
	*ret = Nil;
	return 0;

result_t:
	*ret = fixnumh(i);
	return 0;
}

static int mismatch_execute_sequence(Execute ptr, addr *ret,
		addr pos1, addr pos2, addr rest)
{
	unsigned fromp;
	LocalRoot local;
	addr from, key, test1, test2, start1, start2, end1, end2;
	struct search_struct str;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_START1, &start1)) start1 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_START2, &start2)) start2 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END1, &end1)) end1 = Unbound;
	if (getkeyargs(rest, KEYWORD_END2, &end2)) end2 = Unbound;
	if (test1 != Nil && test2 != Nil)
		fmte("MISMATCH don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	local = ptr->local;
	str.ptr = ptr;
	str.local = local;
	fromp = (from != Nil);
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start1 = start1;
	str.start2 = start2;
	str.end1 = end1;
	str.end2 = end2;
	str.pos1 = pos1;
	str.pos2 = pos2;
	if (fromp) {
		str.range1 = make_seqrange_vector(local, pos1, start1, end1);
		str.range2 = make_seqrange_vector(local, pos2, start2, end2);
		if (reverse_mismatch_sequence(&str, ret))
			return 1;
	}
	else {
		str.range1 = make_seqrange(local, pos1, start1, end1);
		str.range2 = make_seqrange(local, pos2, start2, end2);
		if (normal_mismatch_sequence(&str, ret))
			return 1;
	}

	return 0;
}

_g int mismatch_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (mismatch_execute_sequence(ptr, ret, pos1, pos2, rest)) return 1;
	rollback_local(local, stack);

	return 0;
}


/*
 *  replace
 */
static void replace_sequence_list(LocalRoot local, seqrange *range1, seqrange *range2)
{
	LocalStack stack;
	addr pos, value;
	size_t size, i;

	push_local(local, &stack);
	size = range2->size;
	vector_local(local, &pos, size);
	for (i = 0; ! getnext_seqrange(range2, &value); i++)
		setarray(pos, i, value);

	for (i = 0; i < size; i++) {
		if (endp_seqrange(range1)) break;
		getarray(pos, i, &value);
		set_seqrange(range1, value);
		next_seqrange(range1);
	}
	rollback_local(local, stack);
}

static void replace_sequence_forward(seqrange *range1, seqrange *range2)
{
	addr value;

	while (! getnext_seqrange(range2, &value)) {
		if (endp_seqrange(range1)) break;
		set_seqrange(range1, value);
		next_seqrange(range1);
	}
}

static void replace_sequence_eq(LocalRoot local, seqrange *range1, seqrange *range2)
{
	if (range1->start == range2->start)
		return;
	if (range1->start > range2->start)
		replace_sequence_forward(range1, range2);
	else
		replace_sequence_list(local, range1, range2);
}

_g void replace_sequence(Execute ptr, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	addr start1, start2, end1, end2;
	seqrange range1, range2;

	if (getkeyargs(rest, KEYWORD_START1, &start1)) start1 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_START2, &start2)) start2 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END1, &end1)) end1 = Unbound;
	if (getkeyargs(rest, KEYWORD_END2, &end2)) end2 = Unbound;

	build_seqrange_endp(&range1, pos1, start1, end1);
	build_seqrange_endp(&range2, pos2, start2, end2);
	local = ptr->local;
	if (range1.listp && range2.listp)
		replace_sequence_list(local, &range1, &range2);
	else if (pos1 != pos2)
		replace_sequence_forward(&range1, &range2);
	else
		replace_sequence_eq(local, &range1, &range2);
}


/*
 *  sequence write
 */
struct sequence_write {
	unsigned listp : 1;
	unsigned reverse : 1;
	addr pos;
	size_t index, size, revsize, revbase;
};

typedef struct sequence_write seqwrite;

static void build_seqwrite_list(seqwrite *ptr)
{
	ptr->listp = 1;
	ptr->reverse = 0;
	ptr->pos = Nil;
	ptr->index = 0;
	ptr->size = 0;
}

static void build_seqwrite_result(seqwrite *ptr, addr pos)
{
	ptr->listp = 0;
	ptr->reverse = 0;
	ptr->pos = pos;
	ptr->index = 0;
	ptr->size = 0;
}

static void build_seqwrite(seqwrite *ptr, addr pos)
{
	size_t size;

	if (listp_sequence(pos)) {
		build_seqwrite_list(ptr);
	}
	else {
		size = length_sequence(pos, 1);
		ptr->listp = 0;
		ptr->reverse = 0;
		ptr->pos = pos;
		ptr->size = size;
		ptr->index = 0;
	}
}

static addr nreverse_seqwrite_list(seqwrite *ptr)
{
	nreverse_list_unsafe(&ptr->pos, ptr->pos);
	return ptr->pos;
}

static addr result_seqwrite(seqwrite *ptr)
{
	if (ptr->listp)
		return nreverse_seqwrite_list(ptr);
	else
		return ptr->pos;
}

static void push_seqwrite(seqwrite *ptr, addr pos)
{
	size_t index;
	if (ptr->listp) {
		cons_heap(&ptr->pos, pos, ptr->pos);
		return;
	}
	if (ptr->reverse) {
		ptr->revsize--;
		index = ptr->revbase + ptr->revsize;
		setelt_sequence(ptr->pos, index, pos);
		if (ptr->revsize == 0)
			ptr->reverse = 0;
	}
	else {
		setelt_sequence(ptr->pos, ptr->index, pos);
	}
	ptr->index++;
}

static void before_start_seqwrite(seqwrite *ptr, seqrange *range)
{
	addr pos, value;
	size_t size, i;

	size = range->start;
	pos = range->pos;
	if (range->listp) {
		for (i = 0; i < size; i++) {
			getcons(pos, &value, &pos);
			push_seqwrite(ptr, value);
		}
	}
	else {
		for (i = 0; i < size; i++) {
			getelt_sequence(NULL, pos, i, &value);
			push_seqwrite(ptr, value);
		}
	}
}

static void after_seqwrite(seqwrite *ptr, seqrange *range)
{
	addr pos, value;
	size_t size, i;

	if (range->listp) {
		pos = range->list;
		while (pos != Nil) {
			getcons(pos, &value, &pos);
			push_seqwrite(ptr, value);
		}
	}
	else {
		pos = range->pos;
		size = length_sequence(pos, 1);
		for (i = range->end; i < size; i++) {
			getelt_sequence(NULL, pos, i, &value);
			push_seqwrite(ptr, value);
		}
	}
}

static void reverse_seqwrite(seqwrite *ptr, size_t size)
{
	Check(ptr->listp, "type error");
	if (size <= 1) return;
	ptr->reverse = 1;
	ptr->revsize = size;
	ptr->revbase = ptr->index;
}


/*
 *  substitute
 */
static int boolean_substitute_sequence(struct count_struct *str, int *result, addr pos)
{
	int check;

	if (str->count != Nil) {
		if (str->limit == 0) {
			*result = 0;
			return 0;
		}
		if (boolean_count_sequence(str, &check, pos))
			return 1;
		if (check)
			str->limit--;
		*result = check;
		return 0;
	}

	return boolean_count_sequence(str, result, pos);
}

static int reverse_list_substitute_sequence(struct count_struct *str, seqwrite *ret)
{
	int check;
	addr pos, one;
	seqrange *range;

	range = &(str->range);
	one = str->second;
	save_seqrange(range);
	reverse_seqrange(range);
	before_start_seqwrite(ret, range);
	while (! getnext_reverse_seqrange(range, &pos)) {
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		set_seqrange(range, check? one: pos);
	}

	load_seqrange(range);
	while (! getnext_seqrange(range, &pos))
		push_seqwrite(ret, pos);
	after_seqwrite(ret, range);

	return 0;
}

static int reverse_substitute_sequence(struct count_struct *str, seqwrite *ret)
{
	addr pos1, pos2, pos3, value;
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	local = str->local;
	range = &(str->range);
	push_local(local, &stack);
	pos1 = str->pos;
	build_seqrange_vector_tail(local, range, pos1, str->start, str->end, &pos2, &pos3);
	build_seqwrite_list(ret);
	/* before start */
	while (pos1 != pos2) {
		GetCons(pos1, &value, &pos1);
		push_seqwrite(ret, value);
	}
	/* between start and end */
	if (reverse_list_substitute_sequence(str, ret)) return 1;
	rollback_local(local, stack);
	/* after end */
	while (pos3 != Nil) {
		getcons(pos3, &value, &pos3);
		push_seqwrite(ret, value);
	}

	return 0;
}

static int list_substitute_sequence(struct count_struct *str, seqwrite *ret)
{
	int check;
	addr pos, one;
	seqrange *range;

	range = &(str->range);
	one = str->second;
	build_seqwrite_list(ret);
	before_start_seqwrite(ret, range);
	while (! getnext_seqrange(range, &pos)) {
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		push_seqwrite(ret, check? one: pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int copy_substitute_sequence(struct count_struct *str, seqwrite *ret, addr pos)
{
	int check;
	int (*get)(seqrange *, addr *);
	addr value, one;
	seqrange *range;

	/* initialize */
	range = &(str->range);
	build_seqwrite(ret, pos);
	before_start_seqwrite(ret, range);
	if (str->fromp) {
		get = getnext_reverse_seqrange;
		reverse_seqrange(range);
		reverse_seqwrite(ret, range->size);
	}
	else {
		get = getnext_seqrange;
	}

	/* loop */
	one = str->second;
	while (! get(range, &value)) {
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		push_seqwrite(ret, check? one: value);
	}

	/* after */
	after_seqwrite(ret, range);

	return 0;
}

static void make_vector_array_sequence(addr *ret, addr pos, size_t size)
{
	struct array_struct *info = ArrayInfoStruct(pos);
	make_specialized_sequence(ret, info->type, info->bytesize, size);
}

static void make_vector_size_sequence(addr *ret, addr pos, size_t size)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			vector_heap(ret, size);
			break;

		case LISPTYPE_STRING:
			strvect_heap(ret, size);
			break;

		case LISPTYPE_ARRAY:
			make_vector_array_sequence(ret, pos, size);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_unsafe(NULL, ret, size);
			break;

		default:
			TypeError(pos, SEQUENCE);
			break;
	}
}

static void make_vector_sequence(addr *ret, addr pos)
{
	size_t size = length_sequence(pos, 1);
	make_vector_size_sequence(ret, pos, size);
}

static int normal_substitute_sequence(struct count_struct *str, seqwrite *ret)
{
	addr pos;
	seqrange *range;

	range = &(str->range);
	pos = range->pos;
	if (listp(pos)) {
		if (list_substitute_sequence(str, ret))
			return 1;
	}
	else {
		make_vector_sequence(&pos, pos);
		if (copy_substitute_sequence(str, ret, pos))
			return 1;
	}

	return 0;
}

static void setcount_sequence(struct count_struct *str, addr count)
{
	size_t limit;

	if (count == Nil) {
		str->count = Nil;
		str->limit = 0;
		return;
	}
	if (! integerp(count)) {
		fmte(":COUNT argument ~S must be an integer type.", count, NULL);
	}
	if (minusp_integer(count)) {
		count = fixnumh(0);
		limit = 0;
	}
	else if (getindex_integer(count, &limit)) {
		fmte(":COUNT argument ~S is too large.", count, NULL);
	}
	str->count = count;
	str->limit = limit;
}

_g int substitute_sequence(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	seqrange *range;
	seqwrite write;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("SUBSTITUTE don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item2;  /* olditem */
	str.second = item1;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp) {
		if (reverse_substitute_sequence(&str, &write))
			return 1;
	}
	else {
		build_seqrange(range, pos, start, end);
		if (normal_substitute_sequence(&str, &write))
			return 1;
	}
	*ret = result_seqwrite(&write);

	return 0;
}

static int substitute_if_argument_sequence(Execute ptr, addr *ret,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	seqrange *range;
	seqwrite write;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.second = item;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp) {
		if (reverse_substitute_sequence(&str, &write))
			return 1;
	}
	else {
		build_seqrange(range, pos, start, end);
		if (normal_substitute_sequence(&str, &write))
			return 1;
	}
	*ret = result_seqwrite(&write);

	return 0;
}

_g int substitute_if_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return substitute_if_argument_sequence(ptr, ret, item, call, Nil, pos, rest);
}

_g int substitute_if_not_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return substitute_if_argument_sequence(ptr, ret, item, Nil, call, pos, rest);
}


/*
 *  nsubstitute
 */
static int reverse_list_nsubstitute_sequence(struct count_struct *str)
{
	int check;
	addr pos, one;
	seqrange *range, *write, temp;

	/* write */
	range = &(str->range);
	one = str->second;
	save_seqrange(range);
	reverse_seqrange(range);
	while (! endp_reverse_seqrange(range)) {
		get_reverse_seqrange(range, &pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		set_reverse_seqrange(range, check? one: pos);
		next_reverse_seqrange(range);
	}

	/* reference */
	write = &temp;
	build_seqrange(write, str->pos, str->start, str->end);
	load_seqrange(range);
	while (! getnext_seqrange(range, &pos)) {
		set_seqrange(write, pos);
		next_seqrange(write);
	}

	return 0;
}

static int reverse_nsubstitute_sequence(struct count_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	local = str->local;
	range = &(str->range);
	push_local(local, &stack);
	build_seqrange_vector(local, range, str->pos, str->start, str->end);
	if (reverse_list_nsubstitute_sequence(str)) return 1;
	rollback_local(local, stack);

	return 0;
}

static int normal_nsubstitute_sequence(struct count_struct *str)
{
	int check;
	int (*get)(seqrange *, addr *);
	void (*set)(seqrange *, addr);
	int (*next)(seqrange *);
	int (*endp)(seqrange *);
	addr value, one;
	seqrange *range;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		get = get_reverse_seqrange;
		set = set_reverse_seqrange;
		next = next_reverse_seqrange;
		endp = endp_reverse_seqrange;
		reverse_seqrange(range);
	}
	else {
		get = get_seqrange;
		set = set_seqrange;
		next = next_seqrange;
		endp = endp_seqrange;
	}

	/* loop */
	one = str->second;
	while (! endp(range)) {
		get(range, &value);
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		set(range, check? one: value);
		next(range);
	}

	return 0;
}

_g int nsubstitute_sequence(Execute ptr,
		addr item1, addr item2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("NSUBSTITUTE don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item2;  /* olditem */
	str.second = item1;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp)
		return reverse_nsubstitute_sequence(&str);
	build_seqrange(range, pos, start, end);
	return normal_nsubstitute_sequence(&str);
}

static int nsubstitute_if_argument_sequence(Execute ptr,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	seqrange *range;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.second = item;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp)
		return reverse_nsubstitute_sequence(&str);
	build_seqrange(range, pos, start, end);
	return normal_nsubstitute_sequence(&str);
}

_g int nsubstitute_if_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return nsubstitute_if_argument_sequence(ptr, item, call, Nil, pos, rest);
}

_g int nsubstitute_if_not_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return nsubstitute_if_argument_sequence(ptr, item, Nil, call, pos, rest);
}


/*
 *  concatenate
 */
static int list_concatenate_sequence(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root, value, one;
	size_t size, i;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return 0;

	/* concatenate */
	root = Nil;
	while (rest != Nil) {
		getcons(rest, &value, &rest);
		if (listp_sequence(value)) {
			while (value != Nil) {
				getcons(value, &one, &value);
				cons_heap(&root, one, root);
			}
		}
		else {
			size = length_sequence(value, 1);
			for (i = 0; i < size; i++) {
				getelt_sequence(NULL, value, i, &one);
				cons_heap(&root, one, root);
			}
		}
	}
	nreverse_list_unsafe(ret, root);

	return 1;
}

static size_t length_concatenate_sequence(addr rest)
{
	addr pos;
	size_t size, value;

	for (size = 0; rest != Nil; size += value) {
		getcons(rest, &pos, &rest);
		value = length_sequence(pos, 1);
	}

	return size;
}

static void value_concatenate_sequence(addr root, addr rest)
{
	addr pos;
	size_t index, size, i;
	struct array_value value;

	for (index = 0; rest != Nil; ) {
		GetCons(rest, &pos, &rest);
		size = length_sequence(pos, 1);
		for (i = 0; i < size; i++) {
			getelt_inplace_sequence(pos, i, &value);
			setelt_inplace_sequence(NULL, root, index++, &value);
		}
	}
}

static int vector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LispDecl(type) != LISPDECL_VECTOR)
		return 0;

	/* concatenate */
	size = length_concatenate_sequence(rest);
	array_upgraded_merge_sequence(&root, type, size);
	value_concatenate_sequence(root, rest);
	*ret = root;

	return 1;
}

static int simple_vector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return 0;

	/* concatenate */
	size = length_concatenate_sequence(rest);
	vector_heap(&root, size);
	value_concatenate_sequence(root, rest);
	*ret = root;

	return 1;
}

static int string_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (! string_decl_p(type))
		return 0;

	/* concatenate */
	size = length_concatenate_sequence(rest);
	strvect_heap(&root, size);
	value_concatenate_sequence(root, rest);
	*ret = root;

	return 1;
}

static int array_concatenate_sequence(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return 0;

	/* concatenate */
	size = length_concatenate_sequence(rest);
	array_upgraded_merge_sequence(&root, type, size);
	value_concatenate_sequence(root, rest);
	*ret = root;

	return 1;
}

static int bitvector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return 0;

	/* concatenate */
	size = length_concatenate_sequence(rest);
	bitmemory_unsafe(NULL, &root, size);
	value_concatenate_sequence(root, rest);
	*ret = root;

	return 1;
}

static void type_concatenate_sequence(addr *ret, addr type, addr rest)
{
	/* list */
	if (list_concatenate_sequence(ret, type, rest))
		return;

	/* vector */
	if (vector_concatenate_sequence(ret, type, rest))
		return;

	/* simple-vector */
	if (simple_vector_concatenate_sequence(ret, type, rest))
		return;

	/* string */
	if (string_concatenate_sequence(ret, type, rest))
		return;

	/* array */
	if (array_concatenate_sequence(ret, type, rest))
		return;

	/* bitvector */
	if (bitvector_concatenate_sequence(ret, type, rest))
		return;

	/* error */
	type_error_stdarg(type, Nil, "Invalid type-specifier ~S.", type, NULL);
}

_g int concatenate_sequence(Execute ptr, addr *ret, addr type, addr rest)
{
	addr check;

	if (parse_type(ptr, &check, type, Nil))
		return 1;
	type_concatenate_sequence(ret, check, rest);

	return typep_asterisk_error(*ret, check);
}


/*
 *  remove
 */
static int reverse_list_local_remove_sequence(struct count_struct *str,
		seqwrite *ret, addr table)
{
	int check;
	addr pos;
	seqrange *range;
	size_t size, a, b;

	/* copy */
	range = &(str->range);
	for (size = 0; ! getnext_seqrange(range, &pos); size++)
		setarray(table, size, pos);

	/* remove */
	for (a = b = size; a; ) {
		getarray(table, --a, &pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		if (! check)
			setarray(table, --b, pos);
	}
	while (b < size) {
		getarray(table, b++, &pos);
		push_seqwrite(ret, pos);
	}

	return 0;
}

static int list_reverse_remove_sequence(struct count_struct *str, seqwrite *ret)
{
	addr table;
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	local = str->local;
	range = &(str->range);
	build_seqrange_endp(range, str->pos, str->start, str->end);
	build_seqwrite_list(ret);
	/* before start */
	before_start_seqwrite(ret, range);
	/* between start and end */
	push_local(local, &stack);
	vector_local(local, &table, range->size);
	if (reverse_list_local_remove_sequence(str, ret, table)) return 1;
	rollback_local(local, stack);
	/* after end */
	after_seqwrite(ret, range);

	return 0;
}

static int reverse_list_local_delete_sequence(struct count_struct *str, addr table)
{
	int check;
	addr pos;
	seqrange *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	save_seqrange(range);
	for (size = 0; ! getnext_seqrange(range, &pos); size++)
		setarray(table, size, pos);

	/* copy */
	for (i = size; i; ) {
		i--;
		getarray(table, i, &pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		setarray(table, i, check? T: Nil);
	}

	/* remove */
	load_seqrange(range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == T)
			remove_seqrange(range);
		else
			next_seqrange(range);
	}

	return 0;
}

static int list_reverse_delete_sequence(struct count_struct *str, seqwrite *ret)
{
	addr table;
	LocalRoot local;
	LocalStack stack;
	seqrange *range;

	local = str->local;
	range = &(str->range);
	build_seqrange_endp(range, str->pos, str->start, str->end);
	/* replace */
	push_local(local, &stack);
	vector_local(local, &table, range->size);
	if (reverse_list_local_delete_sequence(str, table)) return 1;
	rollback_local(local, stack);
	/* result */
	build_seqwrite_result(ret, range->pos);

	return 0;
}

static int list_reverse_type_remove_sequence(struct count_struct *str, seqwrite *ret)
{
	if (str->delp)
		return list_reverse_delete_sequence(str, ret);
	else
		return list_reverse_remove_sequence(str, ret);
}

static int list_remove_sequence(struct count_struct *str, seqwrite *ret)
{
	int check;
	addr pos;
	seqrange *range;

	range = &(str->range);
	build_seqwrite_list(ret);
	before_start_seqwrite(ret, range);
	while (! getnext_seqrange(range, &pos)) {
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		if (! check)
			push_seqwrite(ret, pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int list_delete_sequence(struct count_struct *str, seqwrite *ret)
{
	int check;
	addr pos;
	seqrange *range;

	range = &(str->range);
	while (! endp_seqrange(range)) {
		get_seqrange(range, &pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		if (check)
			remove_seqrange(range);
		else
			next_seqrange(range);
	}
	/* result */
	build_seqwrite_result(ret, range->pos);

	return 0;
}

static int copy_normal_remove_sequence(struct count_struct *str,
		seqwrite *ret, addr table)
{
	int check;
	addr pos, value;
	seqrange *range;
	size_t size, loc, i;

	/* loop */
	range = &(str->range);
	pos = range->pos;
	for (loc = 0; ! getnext_seqrange(range, &value); ) {
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		if (! check)
			setarray(table, loc++, value);
	}

	/* copy */
	size = length_sequence(pos, 1) - range->size + loc;
	make_vector_size_sequence(&pos, pos, size);
	build_seqwrite(ret, pos);
	before_start_seqwrite(ret, range);
	for (i = 0; i < loc; i++) {
		getarray(table, i, &value);
		push_seqwrite(ret, value);
	}

	return 0;
}

static int copy_reverse_remove_sequence(struct count_struct *str,
		seqwrite *ret, addr table)
{
	int check;
	addr pos, value;
	seqrange *range;
	size_t size, loc;

	/* loop */
	range = &(str->range);
	pos = range->pos;
	reverse_seqrange(range);
	for (loc = 0; ! getnext_reverse_seqrange(range, &value); ) {
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		if (! check)
			setarray(table, loc++, value);
	}

	/* copy */
	size = length_sequence(pos, 1) - range->size + loc;
	make_vector_size_sequence(&pos, pos, size);
	build_seqwrite(ret, pos);
	before_start_seqwrite(ret, range);
	while (loc) {
		getarray(table, --loc, &value);
		push_seqwrite(ret, value);
	}

	return 0;
}

static int copy_remove_sequence(struct count_struct *str, seqwrite *ret)
{
	addr table;
	seqrange *range;

	range = &(str->range);
	vector_local(str->local, &table, range->size);
	if (str->fromp) {
		if (copy_reverse_remove_sequence(str, ret, table))
			return 1;
	}
	else {
		if (copy_normal_remove_sequence(str, ret, table))
			return 1;
	}
	after_seqwrite(ret, range);

	return 0;
}

static int normal_remove_sequence(struct count_struct *str, seqwrite *ret)
{
	LocalRoot local;
	LocalStack stack;

	if (listp(str->range.pos)) {
		if (str->delp)
			return list_delete_sequence(str, ret);
		else
			return list_remove_sequence(str, ret);
	}
	else {
		local = str->local;
		push_local(local, &stack);
		if (copy_remove_sequence(str, ret))
			return 1;
		rollback_local(local, stack);
		return 0;
	}
}

static int remove_argument_sequence(Execute ptr,
		addr *ret, addr item, addr pos, addr rest, unsigned delp)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	seqrange *range;
	seqwrite write;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("SUBSTITUTE don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.delp = delp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.item = item;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp) {
		if (list_reverse_type_remove_sequence(&str, &write))
			return 1;
	}
	else {
		build_seqrange(range, pos, start, end);
		if (normal_remove_sequence(&str, &write))
			return 1;
	}
	*ret = result_seqwrite(&write);

	return 0;
}

static int remove_if_argument_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest, unsigned delp)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	seqrange *range;
	seqwrite write;

	if (getkeyargs(rest, KEYWORD_COUNT, &count)) count = Nil;
	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.delp = delp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 1;
	if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.pos = pos;
	setcount_sequence(&str, count);

	if (listp && fromp) {
		if (list_reverse_type_remove_sequence(&str, &write))
			return 1;
	}
	else {
		build_seqrange(range, pos, start, end);
		if (normal_remove_sequence(&str, &write))
			return 1;
	}
	*ret = result_seqwrite(&write);

	return 0;
}

_g int remove_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return remove_argument_sequence(ptr, ret, item, pos, rest, 0);
}

_g int remove_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return remove_if_argument_sequence(ptr, ret, call, Nil, pos, rest, 0);
}

_g int remove_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return remove_if_argument_sequence(ptr, ret, Nil, call, pos, rest, 0);
}


/*
 *  delete
 */
_g int delete_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return remove_argument_sequence(ptr, ret, item, pos, rest, 1);
}

_g int delete_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return remove_if_argument_sequence(ptr, ret, call, Nil, pos, rest, 1);
}

_g int delete_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return remove_if_argument_sequence(ptr, ret, Nil, call, pos, rest, 1);
}


/*
 *  remove-duplicates / delete-duplicates
 */
static int list_reverse_sequence_duplicate(struct count_struct *str,
		seqrange *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ! getnext_seqrange(range, &a); i++)
		setarray(table, i, a);

	n = 0;
	for (i = size; i; ) {
		getarray(table, --i, &a);
		if (a == Unbound) continue;
		str->item = a;
		for (k = i; k; ) {
			getarray(table, --k, &b);
			if (b == Unbound) continue;
			if (boolean_substitute_sequence(str, &check, b))
				return 1;
			if (check) {
				setarray(table, k, Unbound);
				n++;
			}
		}
	}
	if (ret) *ret = n;

	return 0;
}

static int list_reverse_delete_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_seqrange(range);
	if (list_reverse_sequence_duplicate(str, range, table, size, NULL))
		return 1;
	load_seqrange(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound)
			remove_seqrange(range);
		else
			next_seqrange(range);
	}
	build_seqwrite_result(ret, range->pos);

	return 0;
}

static int list_reverse_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_reverse_sequence_duplicate(str, range, table, size, NULL))
		return 1;

	/* result */
	build_seqwrite_list(ret);
	before_start_seqwrite(ret, range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound)
			push_seqwrite(ret, pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int list_normal_sequence_duplicate(struct count_struct *str,
		seqrange *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ! getnext_seqrange(range, &a); i++)
		setarray(table, i, a);

	n = 0;
	for (i = 0; i < size; i++) {
		getarray(table, i, &a);
		if (a == Unbound) continue;
		str->item = a;
		for (k = i + 1; k < size; k++) {
			getarray(table, k, &b);
			if (b == Unbound) continue;
			if (boolean_substitute_sequence(str, &check, b))
				return 1;
			if (check) {
				setarray(table, k, Unbound);
				n++;
			}
		}
	}
	if (ret) *ret = n;

	return 0;
}

static int list_normal_delete_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_seqrange(range);
	if (list_normal_sequence_duplicate(str, range, table, size, NULL))
		return 1;
	load_seqrange(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound)
			remove_seqrange(range);
		else
			next_seqrange(range);
	}
	build_seqwrite_result(ret, range->pos);

	return 0;
}

static int list_normal_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_normal_sequence_duplicate(str, range, table, size, NULL))
		return 1;

	/* result */
	build_seqwrite_list(ret);
	before_start_seqwrite(ret, range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound)
			push_seqwrite(ret, pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int list_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	if (str->fromp) {
		if (str->delp)
			return list_reverse_delete_duplicates(str, ret);
		else
			return list_reverse_remove_duplicates(str, ret);
	}
	else {
		if (str->delp)
			return list_normal_delete_duplicates(str, ret);
		else
			return list_normal_remove_duplicates(str, ret);
	}
}

static int vector_reverse_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_reverse_sequence_duplicate(str, range, table, size, &loc))
		return 1;

	/* copy */
	pos = range->pos;
	k = length_sequence(pos, 1) - loc;
	make_vector_size_sequence(&pos, pos, k);
	build_seqwrite(ret, pos);
	before_start_seqwrite(ret, range);
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound)
			push_seqwrite(ret, pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int vector_normal_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	addr table, pos;
	seqrange *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_normal_sequence_duplicate(str, range, table, size, &loc))
		return 1;

	/* copy */
	pos = range->pos;
	k = length_sequence(pos, 1) - loc;
	make_vector_size_sequence(&pos, pos, k);
	build_seqwrite(ret, pos);
	before_start_seqwrite(ret, range);
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound)
			push_seqwrite(ret, pos);
	}
	after_seqwrite(ret, range);

	return 0;
}

static int vector_remove_duplicates(struct count_struct *str, seqwrite *ret)
{
	if (str->fromp)
		return vector_reverse_remove_duplicates(str, ret);
	else
		return vector_normal_remove_duplicates(str, ret);
}

static int argument_remove_duplicates(Execute ptr,
		addr *ret, addr pos, addr rest, unsigned delp)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	LocalRoot local;
	LocalStack stack;
	seqrange *range;
	seqwrite write;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test1)) test1 = Nil;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &test2)) test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		fmte("Arguments don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	local = ptr->local;
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	str.delp = delp;
	str.ptr = ptr;
	str.local = local;
	str.fromp = fromp;
	str.from = from;
	str.key = key;
	str.test1 = test1;
	str.test2 = test2;
	str.single = 0;
	str.count = Nil;
	str.limit = 0;
	str.item = Nil;
	str.second = Nil;
	if (test1 == Nil && test2 == Nil) {
		str.test = 0;
		str.notp = 0;
	}
	else if (test1 != Nil) {
		str.test = 1;
		str.notp = 0;
	}
	else {
		str.test = 2;
		str.notp = 1;
	}
	str.start = start;
	str.end = end;
	str.pos = pos;

	push_local(local, &stack);
	if (listp) {
		build_seqrange_endp(range, pos, start, end);
		if (list_remove_duplicates(&str, &write))
			return 1;
	}
	else {
		build_seqrange(range, pos, start, end);
		if (vector_remove_duplicates(&str, &write))
			return 1;
	}
	rollback_local(local, stack);
	*ret = result_seqwrite(&write);

	return 0;
}

_g int remove_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 0);
}

_g int delete_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 1);
}

