#include "array.h"
#include "array_access.h"
#include "array_copy.h"
#include "array_sequence.h"
#include "array_vector.h"
#include "bit.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "equal.h"
#include "gc.h"
#include "integer.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_iterator.h"
#include "sequence_range.h"
#include "sequence_write.h"
#include "strtype.h"
#include "strvect.h"
#include "type.h"
#include "type_parse.h"
#include "type_upgraded.h"

/*
 *  copy-seq
 */
_g void copy_seq_common(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			break;

		case LISPTYPE_CONS:
			copy_list_heap_safe(ret, var);
			break;

		case LISPTYPE_VECTOR:
			copy_vector_heap(ret, var);
			break;

		case LISPTYPE_STRING:
			string_heap(ret, var);
			break;

		case LISPTYPE_ARRAY:
			array_copy_heap(ret, var);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_copy_heap(ret, var);
			break;

		default:
			TypeError(var, SEQUENCE);
			*ret = NULL;
			break;
	}
}


/*
 *  fill
 */
static void list_fill_sequence(addr list, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	list_start_end_sequence(&list, NULL, start, end, &index1, &index2);

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

static void vector_fill_sequence(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	lenarray(pos, &index1);
	size_start_end_sequence(start, end, index1, &index1, &index2);

	/* fill */
	for (; index1 < index2; index1++)
		setarray(pos, index1, item);
}

_g void fill_common(addr var, addr item, addr start, addr end)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			break;

		case LISPTYPE_CONS:
			list_fill_sequence(var, item, start, end);
			break;

		case LISPTYPE_VECTOR:
			vector_fill_sequence(var, item, start, end);
			break;

		case LISPTYPE_STRING:
			strvect_fill(var, item, start, end);
			break;

		case LISPTYPE_ARRAY:
			array_fill(var, item, start, end);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_fill(var, item, start, end);
			break;

		default:
			TypeError(var, SEQUENCE);
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

static int vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LispDecl(type) != LISPDECL_VECTOR)
		return 0;
	/* vector size */
	vector_check_sequence(type, size);
	/* make-sequence */
	vector_upgraded_make_sequence(ret, type, size, value);

	return 1;
}

static int simple_vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return 0;
	simple_vector_check_sequence(type, size);
	alloc_t_make_sequence(ret, size, value);

	return 1;
}

static int string_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (! type_string_p(type))
		return 0;
	simple_vector_check_sequence(type, size);
	alloc_string_make_sequence(ret, size, value);

	return 1;
}

static int array_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return 0;

	/* dimension check */
	array_check_sequence(type, size);
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
	simple_vector_check_sequence(type, size);
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

_g int make_sequence_common(Execute ptr, addr *ret, addr type, addr size, addr rest)
{
	addr check, element;
	size_t index;
	LocalHold hold;

	if (getkeyargs(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Unbound;
	if (GetIndex_integer(size, &index))
		fmte("Too large index ~S.", size, NULL);

	Return(parse_type(ptr, &check, type, Nil));
	hold = LocalHold_local_push(ptr, check);

	sequence_make_sequence(&rest, check, index, element);
	localhold_push(hold, rest);

	Return(typep_asterisk_error(ptr, rest, check));
	localhold_end(hold);

	*ret = rest;
	return 0;
}


/*
 *  subseq
 */
static void list_subseq_sequence(addr *ret, addr list, addr start, addr end)
{
	addr root, pos;
	struct sequence_range range;

	build_sequence_range(&range, list, start, end);
	root = Nil;
	while (! getnext_sequence_range(&range, &pos))
		cons_heap(&root, pos, root);
	nreverse_list_unsafe(ret, root);
}

static void vector_subseq_sequence(addr *ret, addr vector, addr start, addr end)
{
	size_t index1, index2, i;
	addr root, item;

	/* argument */
	lenarray(vector, &index1);
	size_start_end_sequence(start, end, index1, &index1, &index2);

	/* subseq */
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		getarray(vector, index1, &item);
		setarray(root, i, item);
	}
	*ret = root;
}

_g void subseq_common(addr var, addr start, addr end, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			list_subseq_sequence(ret, var, start, end);
			break;

		case LISPTYPE_VECTOR:
			vector_subseq_sequence(ret, var, start, end);
			break;

		case LISPTYPE_STRING:
			strvect_subseq(ret, var, start, end);
			break;

		case LISPTYPE_ARRAY:
			array_subseq(ret, var, start, end);
			break;

		case LISPTYPE_BITVECTOR:
			bitmemory_subseq(ret, var, start, end);
			break;

		default:
			TypeError(var, SEQUENCE);
			*ret = NULL;
			break;
	}
}

_g void setf_subseq_common(addr root, addr pos, addr start, addr end)
{
	struct array_value value;
	struct sequence_range range1, range2;

	build_sequence_range(&range1, root, start, end);
	build_sequence_range(&range2, pos, Nil, Nil);
	for (;;) {
		if (endp_sequence_range(&range1)) break;
		if (endp_sequence_range(&range2)) break;
		getinplace_sequence_range(&range2, &value);
		setinplace_sequence_range(NULL, &range1, &value); /* heap */
		next_sequence_range(&range1);
		next_sequence_range(&range2);
	}
}


/*
 *  map
 */
static int nil_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	LocalRoot local;
	addr list, temp;
	struct sequence_group *group;

	/* type check */
	if (LispDecl(type) != LISPDECL_NIL) {
		*result = 0;
		return 0;
	}

	/* execute */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	list_sequence_group_local(local, &list, group);
	while (set_sequence_group(group, list)) {
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
	addr list, temp, root;
	struct sequence_group *group;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST) {
		*result = 0;
		return 0;
	}

	/* execute */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	list_sequence_group_local(local, &list, group);
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; set_sequence_group(group, list); ) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		cons_heap(&root, temp, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	*result = 1;
	return 0;
}

static int vector_bitvector_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	bitmemory_unsafe(NULL, &root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		bitmemory_set(root, i, value);
	}
	localhold_end(hold);
	*ret = root;

	return 0;
}

static int vector_string_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	strvect_heap(&root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		strvect_set(root, i, value);
	}
	localhold_end(hold);
	*ret = root;

	return 0;
}

static int vector_signed_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group, enum ARRAY_TYPE type, int bytesize)
{
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	vector_signed_uninit(&root, group->callsize, type, bytesize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		array_set(root, i, value);
	}
	localhold_end(hold);
	*ret = root;

	return 0;
}

static int vector_float_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group, enum ARRAY_TYPE type)
{
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	vector_float_uninit(&root, group->callsize, type);

	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		array_set(root, i, value);
	}
	localhold_end(hold);
	*ret = root;

	return 0;
}

static int vector_general_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	vector_heap(&root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &value, call, list)) return 1;
		setarray(root, i, value);
	}
	localhold_end(hold);
	*ret = root;

	return 0;
}

static int vector_upgraded_map_sequence(Execute ptr,
		addr *ret, addr type, addr call, struct sequence_group *group)
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
	struct sequence_group *group;
	size_t size;

	/* type check */
	if (LispDecl(type) != LISPDECL_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	count_sequence_group(group, &size);
	vector_check_sequence(type, size);
	clear_sequence_group(group);
	list_sequence_group_local(local, NULL, group);

	/* map */
	if (vector_upgraded_map_sequence(ptr, ret, type, call, group)) return 1;
	*result = 1;
	return 0;
}

static int simple_vector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	count_sequence_group(group, &size);
	simple_vector_check_sequence(type, size);
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	vector_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		setarray(root, i, temp);
	}
	localhold_end(hold);

	*result = 1;
	*ret = root;

	return 0;
}

static int string_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (! type_string_p(type)) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	count_sequence_group(group, &size);
	simple_vector_check_sequence(type, size);
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	strvect_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		strvect_set(root, i, temp);
	}
	localhold_end(hold);
	*result = 1;
	*ret = root;

	return 0;
}

static int array_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	LocalRoot local;
	struct sequence_group *group;
	size_t size;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	count_sequence_group(group, &size);
	array_check_sequence(type, size);
	clear_sequence_group(group);
	list_sequence_group_local(local, NULL, group);

	/* make-sequence */
	if (vector_upgraded_map_sequence(ptr, ret, type, call, group)) return 1;
	*result = 1;
	return 0;
}

static int bitvector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	group = make_sequence_group_local(local, rest, 1);
	count_sequence_group(group, &size);
	simple_vector_check_sequence(type, size);
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	bitmemory_unsafe(NULL, &root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &temp, call, list)) return 1;
		bitmemory_set(root, i, temp);
	}
	localhold_end(hold);
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

_g int map_common(Execute ptr, addr *ret, addr type, addr call, addr rest)
{
	addr check;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_type(ptr, &check, type, Nil));
	localhold_push(hold, check);

	Return(execute_map_sequence(ptr, &rest, check, call, rest));
	localhold_push(hold, rest);
	if (type == Nil)
		return 0;

	Return(typep_asterisk_error(ptr, rest, check));
	localhold_end(hold);
	*ret = rest;

	return 0;
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
	struct sequence_iterator *into;
	struct sequence_group *group;
	addr list, pos;
	size_t i;

	/* argument */
	local = ptr->local;
	into = make_sequence_iterator_local(local, var, 0);
	if (end_sequence_iterator(into))
		return 0;
	group = make_sequence_group_local(local, rest, 1);
	list_sequence_group_local(local, &list, group);

	/* map-into */
	for (i = 0; set_sequence_group(group, list); i++) {
		if (callclang_apply(ptr, &pos, call, list))
			return 1;
		if (set_sequence_iterator(into, pos))
			break;
	}

	/* update fill-pointer */
	fill_map_into_sequence(var, i);

	return 0;
}

_g int map_into_common(Execute ptr, addr var, addr call, addr rest)
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
	struct sequence_range range;
};

static int key_reduce_sequence(struct reduce_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	*ret = value;
	return 0;
}

static int throw_reduce_sequence(struct reduce_struct *str, int *result, addr *ret)
{
	addr value, pos;
	struct sequence_range *range;
	LocalHold hold;

	value = str->value;
	range = &(str->range);
	save_sequence_range(range);

	/* empty sequence */
	if (getnext_sequence_range(range, &pos)) {
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
	if (endp_sequence_range(range) && value == Unbound) {
		hold = LocalHold_local_push(str->ptr, pos);
		if (key_reduce_sequence(str, ret, pos))
			return 1;
		localhold_end(hold);
		*result = 1;
		return 0;
	}

	/* multiple value */
	load_sequence_range(range);
	*result = 0;
	return 0;
}

static int value_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	Execute ptr;
	addr pos1, pos2, call;
	struct sequence_range *range;
	LocalHold hold;

	ptr = str->ptr;
	range = &(str->range);
	pos1 = str->value;
	call = str->call;

	/* first */
	hold = LocalHold_array(str->ptr, 2);
	if (pos1 == Unbound) {
		getnext_sequence_range(range, &pos1);
		localhold_set(hold, 0, pos1);
		if (key_reduce_sequence(str, &pos1, pos1)) return 1;
	}
	localhold_set(hold, 0, pos1);

	/* loop */
	while (! getnext_sequence_range(range, &pos2)) {
		localhold_set(hold, 1, pos2);
		if (key_reduce_sequence(str, &pos2, pos2)) return 1;
		localhold_set(hold, 1, pos2);
		if (callclang_funcall(ptr, &pos1, call, pos1, pos2, NULL)) return 1;
		localhold_set(hold, 0, pos1);
	}
	localhold_end(hold);
	*ret = pos1;

	return 0;
}

static int reverse_vector_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	Execute ptr;
	addr pos1, pos2, call;
	struct sequence_range *range;
	LocalHold hold;

	ptr = str->ptr;
	range = &(str->range);
	pos2 = str->value;
	call = str->call;

	/* first */
	reverse_sequence_range(range);
	hold = LocalHold_array(str->ptr, 2);
	if (pos2 == Unbound) {
		getnext_reverse_sequence_range(range, &pos2);
		localhold_set(hold, 1, pos2);
		if (key_reduce_sequence(str, &pos2, pos2)) return 1;
	}
	localhold_set(hold, 1, pos2);

	/* loop */
	while (! getnext_reverse_sequence_range(range, &pos1)) {
		localhold_set(hold, 0, pos1);
		if (key_reduce_sequence(str, &pos1, pos1)) return 1;
		localhold_set(hold, 0, pos1);
		if (callclang_funcall(ptr, &pos2, call, pos1, pos2, NULL)) return 1;
		localhold_set(hold, 1, pos2);
	}
	localhold_end(hold);
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
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_sequence_range_vector(local, range, str->pos, str->start, str->end);
	if (switch_reduce_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int reduce_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr key, start, end, from, value;
	struct reduce_struct str;
	struct sequence_range *range;

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
		build_sequence_range(range, pos, start, end);
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
	size_t limit, start_value;
	struct sequence_range range;
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
	int (*call)(struct sequence_range *, addr *);
	addr value;
	struct sequence_range *range;
	size_t count;
	LocalHold hold;

	/* initialize */
	count = 0;
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call = getnext_reverse_sequence_range;
	}
	else {
		call = getnext_sequence_range;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	while (! call(range, &value)) {
		localhold_set(hold, 0, value);
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check)
			count++;
	}
	localhold_end(hold);
	make_index_integer_alloc(NULL, ret, count);

	return 0;
}

static int reverse_count_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_sequence_range_vector(local, range, str->pos, str->start, str->end);
	if (value_count_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int count_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return value_count_sequence(&str, ret);
}

static int argument_count_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return value_count_sequence(&str, ret);
}

_g int count_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int count_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence(ptr, ret, Nil, call, pos, rest);
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
	struct sequence_iterator *str1, *str2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST) {
		*result = 0;
		return 0;
	}

	/* make list */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	hold = LocalHold_array(ptr, 5); /* a1, b1, a2, b2, root */
	root = Nil;
	if (! object_sequence_iterator(str1, &a1)) {
		goto tail2;
	}
	localhold_set(hold, 0, a1);
	if (! object_sequence_iterator(str2, &b1)) {
		cons_heap(&root, a1, root);
		localhold_set(hold, 4, root);
		goto tail1;
	}
	localhold_set(hold, 1, b1);
	if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	localhold_set(hold, 2, a2);
	if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
	localhold_set(hold, 3, b2);
loop:
	if (callclang_funcall(ptr, &check, call, a2, b2, NULL)) return 1;
	if (check != Nil) {
		cons_heap(&root, a1, root);
		localhold_set(hold, 4, root);
		if (! object_sequence_iterator(str1, &a1)) {
			cons_heap(&root, b1, root);
			localhold_set(hold, 4, root);
			goto tail2;
		}
		localhold_set(hold, 0, a1);
		if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
		localhold_set(hold, 2, a2);
	}
	else {
		cons_heap(&root, b1, root);
		localhold_set(hold, 4, root);
		if (! object_sequence_iterator(str2, &b1)) {
			cons_heap(&root, a1, root);
			localhold_set(hold, 4, root);
			goto tail1;
		}
		localhold_set(hold, 1, b1);
		if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
		localhold_set(hold, 3, b2);
	}
	goto loop;

tail1:
	while (object_sequence_iterator(str1, &a1))
		cons_heap(&root, a1, root);
	goto result;

tail2:
	while (object_sequence_iterator(str2, &b1))
		cons_heap(&root, b1, root);
	goto result;

result:
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);
	*result = 1;
	return 0;
}

static int vector_make_merge_sequence(Execute ptr, addr root,
		struct sequence_iterator *str1, struct sequence_iterator *str2,
		addr call, addr key)
{
	addr a1, a2, b1, b2, check;
	size_t i;
	LocalHold hold;

	/* make list */
	hold = LocalHold_array(ptr, 4); /* a1, b1, a2, b2 */
	i = 0;
	if (! object_sequence_iterator(str1, &a1)) {
		goto tail2;
	}
	localhold_set(hold, 0, a1);
	if (! object_sequence_iterator(str2, &b1)) {
		setelt_sequence(root, i++, a1);
		goto tail1;
	}
	localhold_set(hold, 1, b1);
	if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
	localhold_set(hold, 2, a2);
	if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
	localhold_set(hold, 3, b2);
loop:
	if (callclang_funcall(ptr, &check, call, a2, b2, NULL)) return 1;
	if (check != Nil) {
		setelt_sequence(root, i++, a1);
		if (! object_sequence_iterator(str1, &a1)) {
			setelt_sequence(root, i++, b1);
			goto tail2;
		}
		localhold_set(hold, 0, a1);
		if (key_merge_sequence(ptr, &a2, key, a1)) return 1;
		localhold_set(hold, 2, a2);
	}
	else {
		setelt_sequence(root, i++, b1);
		if (! object_sequence_iterator(str2, &b1)) {
			setelt_sequence(root, i++, a1);
			goto tail1;
		}
		localhold_set(hold, 1, b1);
		if (key_merge_sequence(ptr, &b2, key, b1)) return 1;
		localhold_set(hold, 3, b2);
	}
	goto loop;

tail1:
	while (object_sequence_iterator(str1, &a1))
		setelt_sequence(root, i++, a1);
	return 0;

tail2:
	while (object_sequence_iterator(str2, &b1))
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
	struct sequence_iterator *str1, *str2;
	addr root;
	size_t size, size1, size2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (LispDecl(type) != LISPDECL_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	length_sequence_iterator(str1, &size1);
	length_sequence_iterator(str2, &size2);
	size = size1 + size2;
	vector_check_sequence(type, size);

	array_upgraded_merge_sequence(&root, type, size);
	hold = LocalHold_local_push(ptr, root);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	localhold_end(hold);
	*ret = root;
	*result = 1;

	return 0;
}

static int simple_vector_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	struct sequence_iterator *str1, *str2;
	addr root;
	size_t size, size1, size2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (LispDecl(type) != LISPDECL_SIMPLE_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	length_sequence_iterator(str1, &size1);
	length_sequence_iterator(str2, &size2);
	size = size1 + size2;
	simple_vector_check_sequence(type, size);

	vector_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	localhold_end(hold);
	*ret = root;
	*result = 1;

	return 0;
}

static int string_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	struct sequence_iterator *str1, *str2;
	addr root;
	size_t size, size1, size2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (! type_string_p(type)) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	length_sequence_iterator(str1, &size1);
	length_sequence_iterator(str2, &size2);
	size = size1 + size2;
	simple_vector_check_sequence(type, size);

	strvect_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	localhold_end(hold);
	*ret = root;
	*result = 1;

	return 0;
}

static int array_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	enum LISPDECL decl;
	struct sequence_iterator *str1, *str2;
	addr root;
	size_t size, size1, size2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	length_sequence_iterator(str1, &size1);
	length_sequence_iterator(str2, &size2);
	size = size1 + size2;
	array_check_sequence(type, size);

	array_upgraded_merge_sequence(&root, type, size);
	hold = LocalHold_local_push(ptr, root);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	localhold_end(hold);
	*ret = root;
	*result = 1;

	return 0;
}

static int bitvector_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	enum LISPDECL decl;
	struct sequence_iterator *str1, *str2;
	addr root;
	size_t size, size1, size2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR) {
		*result = 0;
		return 0;
	}

	/* variable */
	local = ptr->local;
	str1 = make_sequence_iterator_local(local, pos1, 1);
	str2 = make_sequence_iterator_local(local, pos2, 1);
	length_sequence_iterator(str1, &size1);
	length_sequence_iterator(str2, &size2);
	size = size1 + size2;
	simple_vector_check_sequence(type, size);

	bitmemory_unsafe(NULL, &root, size);
	hold = LocalHold_local_push(ptr, root);
	if (vector_make_merge_sequence(ptr, root, str1, str2, call, key))
		return 1;
	localhold_end(hold);
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

_g int merge_common(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	addr check;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_type(ptr, &check, type, Nil));
	localhold_push(hold, check);
	Return(execute_merge_sequence(ptr, &call, check, pos1, pos2, call, key));
	localhold_push(hold, call);
	Return(typep_asterisk_error(ptr, call, check));
	*ret = call;

	return 0;
}


/*
 *  find
 */
static int value_find_sequence(struct count_struct *str, addr *ret)
{
	int check;
	int (*call)(struct sequence_range *, addr *);
	addr value;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call = getnext_reverse_sequence_range;
	}
	else {
		call = getnext_sequence_range;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	while (! call(range, &value)) {
		localhold_set(hold, 0, value);
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check) {
			localhold_end(hold);
			*ret = value;
			return 0;
		}
	}
	localhold_end(hold);
	*ret = Nil;

	return 0;
}

static int reverse_find_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_sequence_range_vector(local, range, str->pos, str->start, str->end);
	if (value_find_sequence(str, ret)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int find_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return value_find_sequence(&str, ret);
}

static int argument_find_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return value_find_sequence(&str, ret);
}

_g int find_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int find_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  position
 */
static int value_position_sequence(struct count_struct *str, size_t *ret, int *nilp)
{
	int check;
	int (*call)(struct sequence_range *, addr *);
	addr value;
	struct sequence_range *range;
	size_t count;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call = getnext_reverse_sequence_range;
	}
	else {
		call = getnext_sequence_range;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	for (count = 0; ! call(range, &value); count++) {
		localhold_set(hold, 0, value);
		if (boolean_count_sequence(str, &check, value))
			return 1;
		if (check) {
			localhold_end(hold);
			*ret = count;
			*nilp = 0;
			return 0;
		}
	}
	localhold_end(hold);
	*ret = 0;
	*nilp = 1;
	return 0;
}

static int reverse_position_sequence(struct count_struct *str, addr *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;
	size_t size;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	build_sequence_range_vector(local, range, str->pos, str->start, str->end);
	if (value_position_sequence(str, &size, &check))
		return 1;
	rollback_local(local, stack);

	/* result */
	if (check) {
		*ret = Nil;
		return 0;
	}
	size = str->start_value + range->end - size - 1;
	make_index_integer_heap(ret, size);

	return 0;
}

_g int position_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	int check;
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;
	size_t size;

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
	getindex_integer(start, &(str.start_value));
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
	build_sequence_range(range, pos, start, end);
	if (value_position_sequence(&str, &size, &check))
		return 1;

	/* result */
	if (check) {
		*ret = Nil;
		return 0;
	}
	if (fromp)
		size = range->end - size - 1;
	else
		size += range->start;
	make_index_integer_heap(ret, size);

	return 0;
}

static int argument_position_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	int check;
	unsigned listp, fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;
	size_t size;

	if (getkeyargs(rest, KEYWORD_FROM_END, &from)) from = Nil;
	if (getkeyargs(rest, KEYWORD_START, &start)) start = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END, &end)) end = Unbound;
	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;

	cleartype(str);
	listp = listp_sequence(pos);
	fromp = (from != Nil);
	range = &(str.range);
	getindex_integer(start, &(str.start_value));
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
	build_sequence_range(range, pos, start, end);
	if (value_position_sequence(&str, &size, &check))
		return 1;

	/* result */
	if (check) {
		*ret = Nil;
		return 0;
	}
	if (fromp)
		size = range->end - size - 1;
	else
		size += range->start;
	make_index_integer_heap(ret, size);

	return 0;
}

_g int position_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_position_sequence(ptr, ret, call, Nil, pos, rest);
}

_g int position_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_position_sequence(ptr, ret, Nil, call, pos, rest);
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
	struct sequence_range *range1, *range2;
	size_t start_value;
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
		int *result,
		struct sequence_range *range1,
		struct sequence_range *range2,
		size_t x)
{
	int check;
	addr a, b;
	LocalHold hold;

	range2->index = range2->start + x;
	load_sequence_range(range1);
	hold = LocalHold_array(str->ptr, 2);
	while (! getnext_sequence_range(range1, &a)) {
		localhold_set(hold, 0, a);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);

		if (getnext_sequence_range(range2, &b)) {
			*result = 0;
			return 0;
		}
		localhold_set(hold, 1, b);

		Return(key_search_sequence(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence(str, &check, a, b));
		if (! check) {
			*result = 0;
			goto final;
		}
	}
	*result = 1;
	goto final;

final:
	localhold_end(hold);
	return 0;
}

static int reverse_size_search_sequence(
		struct search_struct *str, size_t *ret, int *nilp)
{
	int check;
	struct sequence_range *range1, *range2;
	size_t size1, size2, i;

	range1 = str->range1;
	range2 = str->range2;
	size1 = range1->size;
	size2 = range2->size;
	if (size2 < size1) {
		*ret = 0;
		*nilp = 1;
		return 0;
	}
	if (size1 == 0) {
		*ret = 0;
		*nilp = 0;
		return 0;
	}

	i = size2 - size1 + 1;
	save_sequence_range(range1);
	while (i) {
		i--;
		if (reverse_pattern_search_sequence(str, &check, range1, range2, i))
			return 1;
		if (check) {
			*ret = i;
			*nilp = 0;
			return 0;
		}
	}
	*ret = 0;
	*nilp = 1;

	return 0;
}

static int reverse_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	if (reverse_size_search_sequence(str, &size, &check))
		return 1;
	if (check) {
		*ret = Nil;
		return 0;
	}
	size += str->start_value;
	make_index_integer_heap(ret, size);

	return 0;
}

static int normal_pattern_search_sequence(struct search_struct *str,
		int *result,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	int check;
	addr a, b;
	LocalHold hold;

	load_sequence_range(range1);
	hold = LocalHold_array(str->ptr, 2);
	while (! getnext_sequence_range(range1, &a)) {
		localhold_set(hold, 0, a);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);

		if (getnext_sequence_range(range2, &b)) {
			*result = 0;
			return 0;
		}
		localhold_set(hold, 1, b);

		Return(key_search_sequence(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence(str, &check, a, b));
		if (! check) {
			*result = 0;
			goto final;
		}
	}
	*result = 1;
	goto final;

final:
	localhold_end(hold);
	return 0;
}

static int normalsize_search_sequence(
		struct search_struct *str, size_t *ret, int *nilp)
{
	int check;
	struct sequence_range *range1, *range2;
	size_t i;

	range1 = str->range1;
	range2 = str->range2;
	if (range1->endp && range2->endp && range2->size < range1->size) {
		*ret = 0;
		*nilp = 1;
		return 0;
	}

	for (i = 0; ; i++) {
		save_sequence_range(range2);
		if (normal_pattern_search_sequence(str, &check, range1, range2))
			return 1;
		if (check) {
			*ret = i;
			*nilp = 0;
			return 0;
		}
		load_sequence_range(range2);
		if (next_sequence_range(range2))
			break;
	}
	*ret = 0;
	*nilp = 1;

	return 0;
}

static int normal_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	if (normalsize_search_sequence(str, &size, &check))
		return 1;
	if (check) {
		*ret = Nil;
		return 0;
	}
	size += str->range2->start;
	make_index_integer_heap(ret, size);

	return 0;
}

static int execute_search_sequence(Execute ptr, addr *ret,
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
	getindex_integer(start2, &(str.start_value));
	if (fromp) {
		str.range1 = make_sequence_range_endp(local, pos1, start1, end1);
		str.range2 = make_sequence_range_vector(local, pos2, start2, end2);
		if (reverse_search_sequence(&str, ret))
			return 1;
	}
	else {
		str.range1 = make_sequence_range(local, pos1, start1, end1);
		str.range2 = make_sequence_range(local, pos2, start2, end2);
		if (normal_search_sequence(&str, ret))
			return 1;
	}

	return 0;
}

_g int search_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (execute_search_sequence(ptr, ret, pos1, pos2, rest))
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
	struct sequence_range *range1, *range2;
	addr a, b;
	size_t i;
	LocalHold hold;

	range1 = str->range1;
	range2 = str->range2;
	reverse_sequence_range(range1);
	reverse_sequence_range(range2);

	hold = LocalHold_array(str->ptr, 2);
	for (i = 0; ; i++) {
		check1 = getnext_reverse_sequence_range(range1, &a);
		check2 = getnext_reverse_sequence_range(range2, &b);
		if (check1 && check2)
			goto result_nil;
		if (check1 || check2)
			goto result_t;

		localhold_set(hold, 0, a);
		localhold_set(hold, 1, b);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);
		Return(key_search_sequence(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence(str, &check, a, b));
		if (! check)
			goto result_t;
	}

result_nil:
	localhold_end(hold);
	*ret = Nil;
	return 0;

result_t:
	localhold_end(hold);
	*ret = fixnumh(i);
	return 0;
}

static int normal_mismatch_sequence(struct search_struct *str, addr *ret)
{
	int check, check1, check2;
	struct sequence_range *range1, *range2;
	addr a, b;
	size_t i;
	LocalHold hold;

	range1 = str->range1;
	range2 = str->range2;

	hold = LocalHold_array(str->ptr, 2);
	for (i = 0; ; i++) {
		check1 = getnext_sequence_range(range1, &a);
		check2 = getnext_sequence_range(range2, &b);
		if (check1 && check2)
			goto result_nil;
		if (check1 || check2)
			goto result_t;

		localhold_set(hold, 0, a);
		localhold_set(hold, 1, b);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);
		Return(key_search_sequence(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence(str, &check, a, b));
		if (! check)
			goto result_t;
	}

result_nil:
	localhold_end(hold);
	*ret = Nil;
	return 0;

result_t:
	localhold_end(hold);
	*ret = fixnumh(i);
	return 0;
}

static int execute_mismatch_sequence(Execute ptr, addr *ret,
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
		str.range1 = make_sequence_range_vector(local, pos1, start1, end1);
		str.range2 = make_sequence_range_vector(local, pos2, start2, end2);
		if (reverse_mismatch_sequence(&str, ret))
			return 1;
	}
	else {
		str.range1 = make_sequence_range(local, pos1, start1, end1);
		str.range2 = make_sequence_range(local, pos2, start2, end2);
		if (normal_mismatch_sequence(&str, ret))
			return 1;
	}

	return 0;
}

_g int mismatch_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	if (execute_mismatch_sequence(ptr, ret, pos1, pos2, rest)) return 1;
	rollback_local(local, stack);

	return 0;
}


/*
 *  replace
 */
static void list_replace_sequence(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	LocalStack stack;
	addr pos, value;
	size_t size, i;

	push_local(local, &stack);
	size = range2->size;
	vector_local(local, &pos, size);
	for (i = 0; ! getnext_sequence_range(range2, &value); i++)
		setarray(pos, i, value);

	for (i = 0; i < size; i++) {
		if (endp_sequence_range(range1)) break;
		getarray(pos, i, &value);
		set_sequence_range(range1, value);
		next_sequence_range(range1);
	}
	rollback_local(local, stack);
}

static void forward_replace_sequence(
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	addr value;

	while (! getnext_sequence_range(range2, &value)) {
		if (endp_sequence_range(range1)) break;
		set_sequence_range(range1, value);
		next_sequence_range(range1);
	}
}

static void eq_replace_sequence(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	if (range1->start == range2->start)
		return;
	if (range1->start > range2->start)
		forward_replace_sequence(range1, range2);
	else
		list_replace_sequence(local, range1, range2);
}

_g void replace_common(Execute ptr, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	addr start1, start2, end1, end2;
	struct sequence_range range1, range2;

	if (getkeyargs(rest, KEYWORD_START1, &start1)) start1 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_START2, &start2)) start2 = fixnumh(0);
	if (getkeyargs(rest, KEYWORD_END1, &end1)) end1 = Unbound;
	if (getkeyargs(rest, KEYWORD_END2, &end2)) end2 = Unbound;

	build_sequence_range_endp(&range1, pos1, start1, end1);
	build_sequence_range_endp(&range2, pos2, start2, end2);
	local = ptr->local;
	if (range1.listp && range2.listp)
		list_replace_sequence(local, &range1, &range2);
	else if (pos1 != pos2)
		forward_replace_sequence(&range1, &range2);
	else
		eq_replace_sequence(local, &range1, &range2);
}


/*
 *  substitute
 */
static int boolean_substitute_sequence(struct count_struct *str, int *result, addr pos)
{
	int check;

	if (str->count == Nil)
		return boolean_count_sequence(str, result, pos);
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

static int reverse_list_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos, one;
	struct sequence_range *range;
	LocalHold hold;

	range = &(str->range);
	one = str->second;
	save_sequence_range(range);
	reverse_sequence_range(range);
	before_sequence_write(ret, range);

	hold = LocalHold_array(str->ptr, 1);
	while (! getnext_reverse_sequence_range(range, &pos)) {
		localhold_set(hold, 0, pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		set_sequence_range(range, check? one: pos);
	}
	localhold_end(hold);

	load_sequence_range(range);
	while (! getnext_sequence_range(range, &pos))
		push_sequence_write(ret, pos);
	after_sequence_write(ret, range);

	return 0;
}

static int reverse_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	addr pos1, pos2, pos3, value;
	struct sequence_range *range;
	LocalRoot local;
	LocalStack stack;

	local = str->local;
	range = &(str->range);
	push_local(local, &stack);
	pos1 = str->pos;
	build_sequence_range_vector2(local,
			range, pos1, str->start, str->end, &pos2, &pos3);
	build_sequence_write_list(ret);
	/* before start */
	while (pos1 != pos2) {
		GetCons(pos1, &value, &pos1);
		push_sequence_write(ret, value);
	}
	/* between start and end */
	gchold_push_local(local, ret->pos);
	if (reverse_list_substitute_sequence(str, ret)) return 1;
	rollback_local(local, stack);
	/* after end */
	while (pos3 != Nil) {
		getcons(pos3, &value, &pos3);
		push_sequence_write(ret, value);
	}

	return 0;
}

static int list_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos, one;
	struct sequence_range *range;
	LocalHold hold;

	range = &(str->range);
	one = str->second;
	build_sequence_write_list(ret);
	before_sequence_write(ret, range);

	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	while (! getnext_sequence_range(range, &pos)) {
		localhold_set(hold, 0, pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		push_sequence_write(ret, check? one: pos);
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);
	after_sequence_write(ret, range);

	return 0;
}

static int copy_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret, addr pos)
{
	int check;
	int (*get)(struct sequence_range *, addr *);
	addr value, one;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	build_sequence_write(ret, pos);
	before_sequence_write(ret, range);
	if (str->fromp) {
		get = getnext_reverse_sequence_range;
		reverse_sequence_range(range);
		reverse_sequence_write(ret, range->size);
	}
	else {
		get = getnext_sequence_range;
	}

	/* loop */
	one = str->second;
	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	while (! get(range, &value)) {
		localhold_set(hold, 0, value);
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		push_sequence_write(ret, check? one: value);
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);

	/* after */
	after_sequence_write(ret, range);

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

static void vector_substitute_sequence(addr *ret, addr pos)
{
	size_t size = length_sequence(pos, 1);
	make_vector_size_sequence(ret, pos, size);
}

static int normal_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	addr pos;
	struct sequence_range *range;
	LocalHold hold;

	range = &(str->range);
	pos = range->pos;
	if (listp(pos)) {
		if (list_substitute_sequence(str, ret))
			return 1;
	}
	else {
		vector_substitute_sequence(&pos, pos);
		hold = LocalHold_local_push(str->ptr, pos);
		if (copy_substitute_sequence(str, ret, pos))
			return 1;
		localhold_end(hold);
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
	else if (GetIndex_integer(count, &limit)) {
		fmte(":COUNT argument ~S is too large.", count, NULL);
	}
	str->count = count;
	str->limit = limit;
}

_g int substitute_common(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

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
		build_sequence_range(range, pos, start, end);
		if (normal_substitute_sequence(&str, &write))
			return 1;
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_substitute_sequence(Execute ptr, addr *ret,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

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
		build_sequence_range(range, pos, start, end);
		if (normal_substitute_sequence(&str, &write))
			return 1;
	}
	*ret = result_sequence_write(&write);

	return 0;
}

_g int substitute_if_common(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return argument_substitute_sequence(ptr, ret, item, call, Nil, pos, rest);
}

_g int substitute_if_not_common(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return argument_substitute_sequence(ptr, ret, item, Nil, call, pos, rest);
}


/*
 *  nsubstitute
 */
static int reverse_list_nsubstitute_sequence(struct count_struct *str)
{
	int check;
	addr pos, one;
	struct sequence_range *range, *write, temp;
	LocalHold hold;

	/* write */
	range = &(str->range);
	one = str->second;
	save_sequence_range(range);
	reverse_sequence_range(range);

	hold = LocalHold_array(str->ptr, 1);
	while (! endp_reverse_sequence_range(range)) {
		get_reverse_sequence_range(range, &pos);
		localhold_set(hold, 0, pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		set_reverse_sequence_range(range, check? one: pos);
		next_reverse_sequence_range(range);
	}
	localhold_end(hold);

	/* reference */
	write = &temp;
	build_sequence_range(write, str->pos, str->start, str->end);
	load_sequence_range(range);
	while (! getnext_sequence_range(range, &pos)) {
		set_sequence_range(write, pos);
		next_sequence_range(write);
	}

	return 0;
}

static int reverse_nsubstitute_sequence(struct count_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	local = str->local;
	range = &(str->range);
	push_local(local, &stack);
	build_sequence_range_vector(local, range, str->pos, str->start, str->end);
	if (reverse_list_nsubstitute_sequence(str)) return 1;
	rollback_local(local, stack);

	return 0;
}

static int normal_nsubstitute_sequence(struct count_struct *str)
{
	int check;
	int (*get)(struct sequence_range *, addr *);
	void (*set)(struct sequence_range *, addr);
	int (*next)(struct sequence_range *);
	int (*endp)(struct sequence_range *);
	addr value, one;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		get = get_reverse_sequence_range;
		set = set_reverse_sequence_range;
		next = next_reverse_sequence_range;
		endp = endp_reverse_sequence_range;
		reverse_sequence_range(range);
	}
	else {
		get = get_sequence_range;
		set = set_sequence_range;
		next = next_sequence_range;
		endp = endp_sequence_range;
	}

	/* loop */
	one = str->second;
	hold = LocalHold_array(str->ptr, 1);
	while (! endp(range)) {
		get(range, &value);
		localhold_set(hold, 0, value);
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		set(range, check? one: value);
		next(range);
	}
	localhold_end(hold);

	return 0;
}

_g int nsubstitute_common(Execute ptr,
		addr item1, addr item2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return normal_nsubstitute_sequence(&str);
}

static int argument_nsubstitute_sequence(Execute ptr,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;

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
	build_sequence_range(range, pos, start, end);
	return normal_nsubstitute_sequence(&str);
}

_g int nsubstitute_if_common(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence(ptr, item, call, Nil, pos, rest);
}

_g int nsubstitute_if_not_common(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence(ptr, item, Nil, call, pos, rest);
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
	if (! type_string_p(type))
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

_g int concatenate_common(Execute ptr, addr *ret, addr type, addr rest)
{
	addr check;

	if (parse_type(ptr, &check, type, Nil))
		return 1;
	type_concatenate_sequence(ret, check, rest);

	return typep_asterisk_error(ptr, *ret, check);
}


/*
 *  remove
 */
static int reverse_list_local_remove_sequence(
		struct count_struct *str, struct sequence_write *ret, addr table)
{
	int check;
	addr pos;
	struct sequence_range *range;
	size_t size, a, b;

	/* copy */
	range = &(str->range);
	for (size = 0; ! getnext_sequence_range(range, &pos); size++)
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
		push_sequence_write(ret, pos);
	}

	return 0;
}

static int list_reverse_remove_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table;
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	local = str->local;
	range = &(str->range);
	build_sequence_range_endp(range, str->pos, str->start, str->end);
	build_sequence_write_list(ret);
	/* before start */
	before_sequence_write(ret, range);
	/* between start and end */
	push_local(local, &stack);
	vector_local(local, &table, range->size);
	if (reverse_list_local_remove_sequence(str, ret, table)) return 1;
	rollback_local(local, stack);
	/* after end */
	after_sequence_write(ret, range);

	return 0;
}

static int reverse_list_local_delete_sequence(struct count_struct *str, addr table)
{
	int check;
	addr pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	save_sequence_range(range);
	for (size = 0; ! getnext_sequence_range(range, &pos); size++)
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
	load_sequence_range(range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == T)
			remove_sequence_range(range);
		else
			next_sequence_range(range);
	}

	return 0;
}

static int list_reverse_delete_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table;
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	local = str->local;
	range = &(str->range);
	build_sequence_range_endp(range, str->pos, str->start, str->end);
	/* replace */
	push_local(local, &stack);
	vector_local(local, &table, range->size);
	if (reverse_list_local_delete_sequence(str, table)) return 1;
	rollback_local(local, stack);
	/* result */
	build_sequence_write_result(ret, range->pos);

	return 0;
}

static int list_reverse_type_remove_sequence(
		struct count_struct *str, struct sequence_write *ret)
{
	if (str->delp)
		return list_reverse_delete_sequence(str, ret);
	else
		return list_reverse_remove_sequence(str, ret);
}

static int list_remove_sequence(struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos;
	struct sequence_range *range;
	LocalHold hold;

	range = &(str->range);
	build_sequence_write_list(ret);
	before_sequence_write(ret, range);
	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	while (! getnext_sequence_range(range, &pos)) {
		localhold_set(hold, 0, pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		if (! check) {
			push_sequence_write(ret, pos);
			localhold_set(hold, 1, ret->pos);
		}
	}
	localhold_end(hold);
	after_sequence_write(ret, range);

	return 0;
}

static int list_delete_sequence(struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos;
	struct sequence_range *range;

	range = &(str->range);
	while (! endp_sequence_range(range)) {
		get_sequence_range(range, &pos);
		if (boolean_substitute_sequence(str, &check, pos))
			return 1;
		if (check)
			remove_sequence_range(range);
		else
			next_sequence_range(range);
	}
	/* result */
	build_sequence_write_result(ret, range->pos);

	return 0;
}

static int copy_normal_remove_sequence(
		struct count_struct *str, struct sequence_write *ret, addr table)
{
	int check;
	addr pos, value;
	struct sequence_range *range;
	size_t size, loc, i;
	LocalHold hold;

	/* loop */
	range = &(str->range);
	pos = range->pos;
	hold = LocalHold_array(str->ptr, 1);
	for (loc = 0; ! getnext_sequence_range(range, &value); ) {
		localhold_set(hold, 0, value);
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	size = length_sequence(pos, 1) - range->size + loc;
	make_vector_size_sequence(&pos, pos, size);
	build_sequence_write(ret, pos);
	before_sequence_write(ret, range);
	for (i = 0; i < loc; i++) {
		getarray(table, i, &value);
		push_sequence_write(ret, value);
	}

	return 0;
}

static int copy_reverse_remove_sequence(
		struct count_struct *str, struct sequence_write *ret, addr table)
{
	int check;
	addr pos, value;
	struct sequence_range *range;
	size_t size, loc;
	LocalHold hold;

	/* loop */
	range = &(str->range);
	pos = range->pos;
	reverse_sequence_range(range);
	hold = LocalHold_array(str->ptr, 1);
	for (loc = 0; ! getnext_reverse_sequence_range(range, &value); ) {
		localhold_set(hold, 0, value);
		if (boolean_substitute_sequence(str, &check, value))
			return 1;
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	size = length_sequence(pos, 1) - range->size + loc;
	make_vector_size_sequence(&pos, pos, size);
	build_sequence_write(ret, pos);
	before_sequence_write(ret, range);
	while (loc) {
		getarray(table, --loc, &value);
		push_sequence_write(ret, value);
	}

	return 0;
}

static int copy_remove_sequence(struct count_struct *str, struct sequence_write *ret)
{
	addr table;
	struct sequence_range *range;

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
	after_sequence_write(ret, range);

	return 0;
}

static int normal_remove_sequence(struct count_struct *str, struct sequence_write *ret)
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

static int argument_remove_sequence(Execute ptr,
		addr *ret, addr item, addr pos, addr rest, unsigned delp)
{
	unsigned listp, fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

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
		build_sequence_range(range, pos, start, end);
		if (normal_remove_sequence(&str, &write))
			return 1;
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_remove_if_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest, unsigned delp)
{
	unsigned listp, fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

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
		build_sequence_range(range, pos, start, end);
		if (normal_remove_sequence(&str, &write))
			return 1;
	}
	*ret = result_sequence_write(&write);

	return 0;
}

_g int remove_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence(ptr, ret, item, pos, rest, 0);
}

_g int remove_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, call, Nil, pos, rest, 0);
}

_g int remove_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, Nil, call, pos, rest, 0);
}


/*
 *  delete
 */
_g int delete_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence(ptr, ret, item, pos, rest, 1);
}

_g int delete_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, call, Nil, pos, rest, 1);
}

_g int delete_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, Nil, call, pos, rest, 1);
}


/*
 *  remove-duplicates / delete-duplicates
 */
static int list_reverse_duplicates_sequence(struct count_struct *str,
		struct sequence_range *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ! getnext_sequence_range(range, &a); i++)
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

static int list_reverse_delete_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_sequence_range(range);
	if (list_reverse_duplicates_sequence(str, range, table, size, NULL))
		return 1;
	load_sequence_range(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound)
			remove_sequence_range(range);
		else
			next_sequence_range(range);
	}
	build_sequence_write_result(ret, range->pos);

	return 0;
}

static int list_reverse_remove_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_reverse_duplicates_sequence(str, range, table, size, NULL))
		return 1;

	/* result */
	build_sequence_write_list(ret);
	before_sequence_write(ret, range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound)
			push_sequence_write(ret, pos);
	}
	after_sequence_write(ret, range);

	return 0;
}

static int list_normal_duplicates_sequence(struct count_struct *str,
		struct sequence_range *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ! getnext_sequence_range(range, &a); i++)
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

static int list_normal_delete_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_sequence_range(range);
	if (list_normal_duplicates_sequence(str, range, table, size, NULL))
		return 1;
	load_sequence_range(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound)
			remove_sequence_range(range);
		else
			next_sequence_range(range);
	}
	build_sequence_write_result(ret, range->pos);

	return 0;
}

static int list_normal_remove_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_normal_duplicates_sequence(str, range, table, size, NULL))
		return 1;

	/* result */
	build_sequence_write_list(ret);
	before_sequence_write(ret, range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound)
			push_sequence_write(ret, pos);
	}
	after_sequence_write(ret, range);

	return 0;
}

static int list_remove_duplicates(struct count_struct *str, struct sequence_write *ret)
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

static int vector_reverse_remove_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_reverse_duplicates_sequence(str, range, table, size, &loc))
		return 1;

	/* copy */
	pos = range->pos;
	k = length_sequence(pos, 1) - loc;
	make_vector_size_sequence(&pos, pos, k);
	build_sequence_write(ret, pos);
	before_sequence_write(ret, range);
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound)
			push_sequence_write(ret, pos);
	}
	after_sequence_write(ret, range);

	return 0;
}

static int vector_normal_remove_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	if (list_normal_duplicates_sequence(str, range, table, size, &loc))
		return 1;

	/* copy */
	pos = range->pos;
	k = length_sequence(pos, 1) - loc;
	make_vector_size_sequence(&pos, pos, k);
	build_sequence_write(ret, pos);
	before_sequence_write(ret, range);
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound)
			push_sequence_write(ret, pos);
	}
	after_sequence_write(ret, range);

	return 0;
}

static int vector_remove_duplicates_sequence(
		struct count_struct *str, struct sequence_write *ret)
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
	struct sequence_range *range;
	struct sequence_write write;

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
		build_sequence_range_endp(range, pos, start, end);
		if (list_remove_duplicates(&str, &write))
			return 1;
	}
	else {
		build_sequence_range(range, pos, start, end);
		if (vector_remove_duplicates_sequence(&str, &write))
			return 1;
	}
	rollback_local(local, stack);
	*ret = result_sequence_write(&write);

	return 0;
}

_g int remove_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 0);
}

_g int delete_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 1);
}

