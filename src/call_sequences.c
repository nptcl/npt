#include "array.h"
#include "array_access.h"
#include "array_copy.h"
#include "array_sequence.h"
#include "array_vector.h"
#include "bit.h"
#include "call_sequences.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "equal.h"
#include "hold.h"
#include "integer.h"
#include "sequence.h"
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
int copy_seq_common(addr var, addr *ret)
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
			return string_heap_(ret, var);

		case LISPTYPE_ARRAY:
			return array_copy_heap_(ret, var);

		case LISPTYPE_BITVECTOR:
			bitmemory_copy_heap(ret, var);
			break;

		default:
			*ret = Nil;
			return TypeError_(var, SEQUENCE);
	}

	return 0;
}


/*
 *  fill
 */
static int list_fill_sequence(addr list, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	Return(list_start_end_sequence_(&list, NULL, start, end, &index1, &index2));

	/* fill */
	for (;;) {
		if (end != Nil) {
			if (index2 <= index1)
				break;
			if (list == Nil) {
				return fmte_(":END ~A "
						"must be less than equal to list length.", end, NULL);
			}
		}
		else if (list == Nil) {
			break;
		}
		if (! consp(list))
			return fmte_("Don't accept the dotted list ~S.", list, NULL);
		SetCar(list, item);
		GetCdr(list, &list);
		index1++;
	}

	return 0;
}

static int vector_fill_sequence_(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;

	/* argument */
	lenarray(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* fill */
	for (; index1 < index2; index1++)
		setarray(pos, index1, item);

	return 0;
}

int fill_common(addr var, addr item, addr start, addr end)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			return 0;

		case LISPTYPE_CONS:
			return list_fill_sequence(var, item, start, end);

		case LISPTYPE_VECTOR:
			return vector_fill_sequence_(var, item, start, end);

		case LISPTYPE_STRING:
			return strvect_fill_(var, item, start, end);

		case LISPTYPE_ARRAY:
			return array_fill_(var, item, start, end);

		case LISPTYPE_BITVECTOR:
			return bitmemory_fill_(var, item, start, end);

		default:
			return TypeError_(var, SEQUENCE);
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
	if (value == Unbound)
		value = Nil;
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(ret, Unbound);

	/* make-sequence */
	root = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&root, value, root);

	return Result(ret, root);
}

static int alloc_t_make_sequence(addr *ret, size_t size, addr value)
{
	addr pos;
	size_t i;

	if (value == Unbound)
		value = Nil;
	vector_heap(&pos, size);
	for (i = 0; i < size; i++)
		setarray(pos, i, value);

	return Result(ret, pos);
}

static int alloc_bitvector_make_sequence(addr *ret, size_t size, addr value)
{
	int bit;

	if (value == Unbound)
		bit = 0;
	else if (bit_getint(value, &bit))
		return TypeError_(value, BIT);
	bitmemory_unsafe(NULL, &value, size);
	bitmemory_memset(value, bit);

	return Result(ret, value);
}

static int alloc_string_make_sequence(addr *ret, size_t size, addr value)
{
	unicode c;

	if (value == Unbound)
		c = 0;
	else if (characterp(value))
		GetCharacter(value, &c);
	else
		return TypeError_(value, CHARACTER);
	strvect_heap(&value, size);
	Return(strvect_setall_(value, c));

	return Result(ret, value);
}

static int vector_upgraded_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
	switch (upgraded) {
		case ARRAY_TYPE_BIT:
			return alloc_bitvector_make_sequence(ret, size, value);

		case ARRAY_TYPE_CHARACTER:
			return alloc_string_make_sequence(ret, size, value);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return vector_signed_(ret, size, upgraded, upsize, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_(ret, size, upgraded, value);

		default:
			return alloc_t_make_sequence(ret, size, value);
	}
}

static int vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(ret, Unbound);
	/* vector size */
	Return(vector_check_sequence_(type, size));
	/* make-sequence */
	return vector_upgraded_make_sequence(ret, type, size, value);
}

static int simple_vector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(ret, Unbound);
	Return(simple_vector_check_sequence_(type, size));
	return alloc_t_make_sequence(ret, size, value);
}

static int string_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	if (! type_string_p(type))
		return Result(ret, Unbound);
	Return(simple_vector_check_sequence_(type, size));
	return alloc_string_make_sequence(ret, size, value);
}

static int array_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(ret, Unbound);

	/* dimension check */
	Return(array_check_sequence_(type, size));
	/* make-sequence */
	return vector_upgraded_make_sequence(ret, type, size, value);
}

static int bitvector_make_sequence(addr *ret, addr type, size_t size, addr value)
{
	enum LISPDECL decl;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(ret, Unbound);

	/* make-sequence */
	Return(simple_vector_check_sequence_(type, size));
	return alloc_bitvector_make_sequence(ret, size, value);
}

static int sequence_make_sequence(Execute ptr,
		addr *ret, addr type, size_t size, addr value)
{
	addr pos;

	/* list */
	Return(list_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* vector */
	Return(vector_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* simple-vector */
	Return(simple_vector_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* string */
	Return(string_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* array */
	Return(array_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* bitvector */
	Return(bitvector_make_sequence(&pos, type, size, value));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int make_sequence_common(Execute ptr, addr *ret, addr type, addr size, addr rest)
{
	addr check, element;
	size_t index;
	LocalHold hold;

	if (GetKeyArgs(rest, KEYWORD_INITIAL_ELEMENT, &element))
		element = Unbound;
	if (GetIndex_integer(size, &index))
		return fmte_("Too large index ~S.", size, NULL);

	Return(parse_type(ptr, &check, type, Nil));
	hold = LocalHold_local_push(ptr, check);

	Return(sequence_make_sequence(ptr, &rest, check, index, element));
	localhold_push(hold, rest);

	Return(call_typep_asterisk_error_(ptr, rest, check));
	localhold_end(hold);

	return Result(ret, rest);
}


/*
 *  subseq
 */
static int list_subseq_sequence_(addr *ret, addr list, addr start, addr end)
{
	int check;
	addr root, pos;
	struct sequence_range range;

	Return(build_sequence_range_(&range, list, start, end));
	root = Nil;
	for (;;) {
		Return(getnext_sequence_range_(&range, &pos, &check));
		if (check)
			break;
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int vector_subseq_sequence_(addr *ret, addr vector, addr start, addr end)
{
	size_t index1, index2, i;
	addr root, item;

	/* argument */
	lenarray(vector, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));

	/* subseq */
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		getarray(vector, index1, &item);
		setarray(root, i, item);
	}

	return Result(ret, root);
}

int subseq_common(addr var, addr start, addr end, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_subseq_sequence_(ret, var, start, end);

		case LISPTYPE_VECTOR:
			return vector_subseq_sequence_(ret, var, start, end);

		case LISPTYPE_STRING:
			return strvect_subseq_(ret, var, start, end);

		case LISPTYPE_ARRAY:
			return array_subseq_(ret, var, start, end);

		case LISPTYPE_BITVECTOR:
			return bitmemory_subseq_(ret, var, start, end);

		default:
			*ret = Nil;
			return TypeError_(var, SEQUENCE);
	}
}

int setf_subseq_common_(addr root, addr pos, addr start, addr end)
{
	int ignore;
	struct array_value value;
	struct sequence_range range1, range2;

	Return(build_sequence_range_(&range1, root, start, end));
	Return(build_sequence_range_(&range2, pos, Nil, Nil));
	for (;;) {
		if (endp_sequence_range(&range1))
			break;
		if (endp_sequence_range(&range2))
			break;
		Return(getinplace_sequence_range_(&range2, &value));
		Return(setinplace_sequence_range_(NULL, &range1, &value)); /* heap */
		Return(next_sequence_range_(&range1, &ignore));
		Return(next_sequence_range_(&range2, &ignore));
	}

	return 0;
}


/*
 *  map
 */
static int nil_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	int check;
	LocalRoot local;
	addr list, temp;
	struct sequence_group *group;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_NIL)
		return Result(result, 0);

	/* execute */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	list_sequence_group_local(local, &list, group);
	for (;;) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &temp, call, list));
	}
	*ret = Nil;
	return Result(result, 1);
}

static int list_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	int check;
	enum LISPDECL decl;
	addr list, temp, root;
	struct sequence_group *group;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(result, 0);

	/* execute */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	list_sequence_group_local(local, &list, group);
	hold = LocalHold_array(ptr, 1);
	root = Nil;
	for (;;) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &temp, call, list));
		cons_heap(&root, temp, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return Result(result, 1);
}

static int vector_bitvector_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	int check;
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	bitmemory_unsafe(NULL, &root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &value, call, list));
		Return(bitmemory_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_string_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	int check;
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	strvect_heap(&root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &value, call, list));
		Return(strvect_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_signed_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group, enum ARRAY_TYPE type, int bytesize)
{
	int check;
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	Return(vector_signed_uninit_(&root, group->callsize, type, bytesize));
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &value, call, list));
		Return(array_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_float_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group, enum ARRAY_TYPE type)
{
	int check;
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	Return(vector_float_uninit_(&root, group->callsize, type));

	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &value, call, list));
		Return(array_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_general_map_sequence(Execute ptr, addr *ret,
		addr call, struct sequence_group *group)
{
	int check;
	addr list, root, value;
	size_t i;
	LocalHold hold;

	list = group->list;
	vector_heap(&root, group->callsize);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &value, call, list));
		setarray(root, i, value);
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_upgraded_map_sequence(Execute ptr,
		addr *ret, addr type, addr call, struct sequence_group *group)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
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
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	Return(count_sequence_group_(group, &size));
	Return(vector_check_sequence_(type, size));
	clear_sequence_group(group);
	list_sequence_group_local(local, NULL, group);

	/* map */
	Return(vector_upgraded_map_sequence(ptr, ret, type, call, group));
	return Result(result, 1);
}

static int simple_vector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	int check;
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	Return(count_sequence_group_(group, &size));
	Return(simple_vector_check_sequence_(type, size));
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	vector_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &temp, call, list));
		setarray(root, i, temp);
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int string_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	int check;
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	if (! type_string_p(type))
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	Return(count_sequence_group_(group, &size));
	Return(simple_vector_check_sequence_(type, size));
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	strvect_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &temp, call, list));
		Return(strvect_set_(root, i, temp));
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int array_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	enum LISPDECL decl;
	LocalRoot local;
	struct sequence_group *group;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	Return(count_sequence_group_(group, &size));
	Return(array_check_sequence_(type, size));
	clear_sequence_group(group);
	list_sequence_group_local(local, NULL, group);

	/* make-sequence */
	Return(vector_upgraded_map_sequence(ptr, ret, type, call, group));
	return Result(result, 1);
}

static int bitvector_map_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr call, addr rest)
{
	int check;
	enum LISPDECL decl;
	addr list, temp, root;
	struct sequence_group *group;
	size_t size, i;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	Return(count_sequence_group_(group, &size));
	Return(simple_vector_check_sequence_(type, size));
	clear_sequence_group(group);
	list_sequence_group_local(local, &list, group);

	/* execute */
	bitmemory_unsafe(NULL, &root, size);
	hold = LocalHold_local_push(ptr, root);
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &temp, call, list));
		Return(bitmemory_set_(root, i, temp));
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int execute_map_sequence(Execute ptr, addr *ret,
		addr type, addr call, addr rest)
{
	int check;

	/* nil */
	Return(nil_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* list */
	Return(list_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* vector */
	Return(vector_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* simple-vector */
	Return(simple_vector_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* string */
	Return(string_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* array */
	Return(array_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* bitvector */
	Return(bitvector_map_sequence(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* error */
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int map_common(Execute ptr, addr *ret, addr type, addr call, addr rest)
{
	addr check;
	LocalHold hold;

	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few map arguments.", NULL);
	}
	hold = LocalHold_local(ptr);
	Return(parse_type(ptr, &check, type, Nil));
	localhold_push(hold, check);

	Return(execute_map_sequence(ptr, &rest, check, call, rest));
	localhold_push(hold, rest);
	if (type == Nil)
		return 0;

	Return(call_typep_asterisk_error_(ptr, rest, check));
	localhold_end(hold);

	return Result(ret, rest);
}


/*
 *  map-into
 */
static void fill_map_into_sequence(addr var, size_t size)
{
	struct array_struct *str;

	if (! arrayp(var))
		return;
	str = ArrayInfoStruct(var);
	if (! str->fillpointer)
		return;
	str->front = size;
}

static int execute_map_into_sequence(Execute ptr, addr var, addr call, addr rest)
{
	int check;
	LocalRoot local;
	struct sequence_iterator *into;
	struct sequence_group *group;
	addr list, pos;
	size_t i;

	/* argument */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, var, 0, &into));
	if (end_sequence_iterator(into))
		return 0;
	Return(make_sequence_group_local_(local, rest, 1, &group));
	list_sequence_group_local(local, &list, group);

	/* map-into */
	for (i = 0; ; i++) {
		Return(set_sequence_group_(group, list, &check));
		if (! check)
			break;
		Return(callclang_apply(ptr, &pos, call, list));
		Return(set_sequence_iterator_(into, pos, &check));
		if (check)
			break;
	}

	/* update fill-pointer */
	fill_map_into_sequence(var, i);

	return 0;
}

int map_into_common(Execute ptr, addr var, addr call, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_map_into_sequence(ptr, var, call, rest));
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
	else
		return Result(ret, value);
}

static int throw_reduce_sequence(struct reduce_struct *str, int *result, addr *ret)
{
	int check;
	addr value, pos;
	struct sequence_range *range;
	LocalHold hold;

	value = str->value;
	range = &(str->range);
	save_sequence_range(range);

	/* empty sequence */
	Return(getnext_sequence_range_(range, &pos, &check));
	if (check) {
		if (value == Unbound) {
			Return(callclang_apply(str->ptr, ret, str->call, Nil));
		}
		else {
			*ret = value;
		}
		return Result(result, 1);
	}

	/* single value */
	if (endp_sequence_range(range) && value == Unbound) {
		hold = LocalHold_local_push(str->ptr, pos);
		Return(key_reduce_sequence(str, ret, pos));
		localhold_end(hold);
		return Result(result, 1);
	}

	/* multiple value */
	load_sequence_range(range);
	return Result(result, 0);
}

static int value_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	int check;
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
		Return(getnext_sequence_range_(range, &pos1, &check)); /* ignore */
		localhold_set(hold, 0, pos1);
		Return(key_reduce_sequence(str, &pos1, pos1));
	}
	localhold_set(hold, 0, pos1);

	/* loop */
	for (;;) {
		Return(getnext_sequence_range_(range, &pos2, &check));
		if (check)
			break;
		localhold_set(hold, 1, pos2);
		Return(key_reduce_sequence(str, &pos2, pos2));
		localhold_set(hold, 1, pos2);
		Return(callclang_funcall(ptr, &pos1, call, pos1, pos2, NULL));
		localhold_set(hold, 0, pos1);
	}
	localhold_end(hold);

	return Result(ret, pos1);
}

static int reverse_vector_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	int check;
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
		Return(getnext_reverse_sequence_range_(range, &pos2, &check)); /* ignore */
		localhold_set(hold, 1, pos2);
		Return(key_reduce_sequence(str, &pos2, pos2));
	}
	localhold_set(hold, 1, pos2);

	/* loop */
	for (;;) {
		Return(getnext_reverse_sequence_range_(range, &pos1, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos1);
		Return(key_reduce_sequence(str, &pos1, pos1));
		localhold_set(hold, 0, pos1);
		Return(callclang_funcall(ptr, &pos2, call, pos1, pos2, NULL));
		localhold_set(hold, 1, pos2);
	}
	localhold_end(hold);

	return Result(ret, pos2);
}

static int switch_reduce_sequence(struct reduce_struct *str, addr *ret)
{
	int check;

	Return(throw_reduce_sequence(str, &check, ret));
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
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(switch_reduce_sequence(str, ret));
	rollback_local(local, stack);

	return 0;
}

int reduce_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr key, start, end, from, value;
	struct reduce_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_INITIAL_VALUE, &value))
		value = Unbound;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
		Return(build_sequence_range_(range, pos, start, end));
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
		Return(callclang_funcall(ptr, &value, str->key, value, NULL));
	}
	if (str->single) {
		Return(callclang_funcall(ptr, &value, test, value, NULL));
	}
	else if (test == Nil) {
		value = eql_function(str->item, value)? T: Nil;
	}
	else {
		Return(callclang_funcall(ptr, &value, test, str->item, value, NULL));
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
	int (*call_)(struct sequence_range *, addr *, int *);
	addr value;
	struct sequence_range *range;
	size_t count;
	LocalHold hold;

	/* initialize */
	count = 0;
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call_ = getnext_reverse_sequence_range_;
	}
	else {
		call_ = getnext_sequence_range_;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	for (;;) {
		Return((*call_)(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_count_sequence(str, &check, value));
		if (check)
			count++;
	}
	localhold_end(hold);
	make_index_integer_heap(ret, count);

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
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(value_count_sequence(str, ret));
	rollback_local(local, stack);

	return 0;
}

int count_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		return fmte_("COUNT don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(build_sequence_range_(range, pos, start, end));
	return value_count_sequence(&str, ret);
}

static int argument_count_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(build_sequence_range_(range, pos, start, end));
	return value_count_sequence(&str, ret);
}

int count_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence(ptr, ret, call, Nil, pos, rest);
}

int count_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  merge
 */
static int key_merge_sequence(Execute ptr, addr *ret, addr key, addr value)
{
	if (key != Nil)
		return callclang_funcall(ptr, ret, key, value, NULL);
	else
		return Result(ret, value);
}

static int list_merge_sequence(Execute ptr, int *result, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	int check;
	enum LISPDECL decl;
	addr root, a1, a2, b1, b2, value;
	struct sequence_iterator *str1, *str2;
	LocalRoot local;
	LocalHold hold;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(result, 0);

	/* make list */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	hold = LocalHold_array(ptr, 5); /* a1, b1, a2, b2, root */
	root = Nil;
	Return(object_sequence_iterator_(str1, &a1, &check));
	if (! check) {
		goto tail2;
	}
	localhold_set(hold, 0, a1);
	Return(object_sequence_iterator_(str2, &b1, &check));
	if (! check) {
		cons_heap(&root, a1, root);
		localhold_set(hold, 4, root);
		goto tail1;
	}
	localhold_set(hold, 1, b1);
	Return(key_merge_sequence(ptr, &a2, key, a1));
	localhold_set(hold, 2, a2);
	Return(key_merge_sequence(ptr, &b2, key, b1));
	localhold_set(hold, 3, b2);
loop:
	Return(callclang_funcall(ptr, &value, call, a2, b2, NULL));
	if (value != Nil) {
		cons_heap(&root, a1, root);
		localhold_set(hold, 4, root);
		Return(object_sequence_iterator_(str1, &a1, &check));
		if (! check) {
			cons_heap(&root, b1, root);
			localhold_set(hold, 4, root);
			goto tail2;
		}
		localhold_set(hold, 0, a1);
		Return(key_merge_sequence(ptr, &a2, key, a1));
		localhold_set(hold, 2, a2);
	}
	else {
		cons_heap(&root, b1, root);
		localhold_set(hold, 4, root);
		Return(object_sequence_iterator_(str2, &b1, &check));
		if (! check) {
			cons_heap(&root, a1, root);
			localhold_set(hold, 4, root);
			goto tail1;
		}
		localhold_set(hold, 1, b1);
		Return(key_merge_sequence(ptr, &b2, key, b1));
		localhold_set(hold, 3, b2);
	}
	goto loop;

tail1:
	for (;;) {
		Return(object_sequence_iterator_(str1, &a1, &check));
		if (! check)
			break;
		cons_heap(&root, a1, root);
	}
	goto result;

tail2:
	for (;;) {
		Return(object_sequence_iterator_(str2, &b1, &check));
		if (! check)
			break;
		cons_heap(&root, b1, root);
	}
	goto result;

result:
	localhold_end(hold);
	nreverse(ret, root);
	return Result(result, 1);
}

static int vector_make_merge_sequence(Execute ptr, addr root,
		struct sequence_iterator *str1, struct sequence_iterator *str2,
		addr call, addr key)
{
	int check;
	addr a1, a2, b1, b2, value;
	size_t i;
	LocalHold hold;

	/* make list */
	hold = LocalHold_array(ptr, 4); /* a1, b1, a2, b2 */
	i = 0;
	Return(object_sequence_iterator_(str1, &a1, &check));
	if (! check) {
		goto tail2;
	}
	localhold_set(hold, 0, a1);
	Return(object_sequence_iterator_(str2, &b1, &check));
	if (! check) {
		Return(setelt_sequence_(root, i++, a1));
		goto tail1;
	}
	localhold_set(hold, 1, b1);
	Return(key_merge_sequence(ptr, &a2, key, a1));
	localhold_set(hold, 2, a2);
	Return(key_merge_sequence(ptr, &b2, key, b1));
	localhold_set(hold, 3, b2);
loop:
	Return(callclang_funcall(ptr, &value, call, a2, b2, NULL));
	if (value != Nil) {
		Return(setelt_sequence_(root, i++, a1));
		Return(object_sequence_iterator_(str1, &a1, &check));
		if (! check) {
			Return(setelt_sequence_(root, i++, b1));
			goto tail2;
		}
		localhold_set(hold, 0, a1);
		Return(key_merge_sequence(ptr, &a2, key, a1));
		localhold_set(hold, 2, a2);
	}
	else {
		Return(setelt_sequence_(root, i++, b1));
		Return(object_sequence_iterator_(str2, &b1, &check));
		if (! check) {
			Return(setelt_sequence_(root, i++, a1));
			goto tail1;
		}
		localhold_set(hold, 1, b1);
		Return(key_merge_sequence(ptr, &b2, key, b1));
		localhold_set(hold, 3, b2);
	}
	goto loop;

tail1:
	for (;;) {
		Return(object_sequence_iterator_(str1, &a1, &check));
		if (! check)
			break;
		Return(setelt_sequence_(root, i++, a1));
	}
	return 0;

tail2:
	for (;;) {
		Return(object_sequence_iterator_(str2, &b1, &check));
		if (! check)
			break;
		Return(setelt_sequence_(root, i++, b1));
	}
	return 0;
}

static int make_specialized_sequence(addr *ret,
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
			return vector_signed_uninit_(ret, size, type, bytesize);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_uninit_(ret, size, type);

		default:
			*ret = Nil;
			return fmte_("Invalid array type.", NULL);
	}

	return 0;
}

static int array_upgraded_merge_sequence(addr *ret, addr type, size_t size)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
	return make_specialized_sequence(ret, upgraded, upsize, size);
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
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	Return(length_sequence_iterator_(str1, &size1));
	Return(length_sequence_iterator_(str2, &size2));
	size = size1 + size2;
	Return(vector_check_sequence_(type, size));

	Return(array_upgraded_merge_sequence(&root, type, size));
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
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
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	Return(length_sequence_iterator_(str1, &size1));
	Return(length_sequence_iterator_(str2, &size2));
	size = size1 + size2;
	Return(simple_vector_check_sequence_(type, size));

	vector_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
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
	if (! type_string_p(type))
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	Return(length_sequence_iterator_(str1, &size1));
	Return(length_sequence_iterator_(str2, &size2));
	size = size1 + size2;
	Return(simple_vector_check_sequence_(type, size));

	strvect_heap(&root, size);
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
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
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	Return(length_sequence_iterator_(str1, &size1));
	Return(length_sequence_iterator_(str2, &size2));
	size = size1 + size2;
	Return(array_check_sequence_(type, size));

	Return(array_upgraded_merge_sequence(&root, type, size));
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
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
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(result, 0);

	/* variable */
	local = ptr->local;
	Return(make_sequence_iterator_local_(local, pos1, 1, &str1));
	Return(make_sequence_iterator_local_(local, pos2, 1, &str2));
	Return(length_sequence_iterator_(str1, &size1));
	Return(length_sequence_iterator_(str2, &size2));
	size = size1 + size2;
	Return(simple_vector_check_sequence_(type, size));

	bitmemory_unsafe(NULL, &root, size);
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int execute_merge_sequence(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	int check;

	/* list */
	Return(list_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* vector */
	Return(vector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* simple-vector */
	Return(simple_vector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* string */
	Return(string_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* array */
	Return(array_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* bitvector */
	Return(bitvector_merge_sequence(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* error */
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int merge_common(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	addr check;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_type(ptr, &check, type, Nil));
	localhold_push(hold, check);
	/* pos2 -> pos1 */
	Return(execute_merge_sequence(ptr, &call, check, pos2, pos1, call, key));
	localhold_push(hold, call);
	Return(call_typep_asterisk_error_(ptr, call, check));

	return Result(ret, call);
}


/*
 *  find
 */
static int value_find_sequence(struct count_struct *str, addr *ret)
{
	int check;
	int (*call_)(struct sequence_range *, addr *, int *);
	addr value;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call_ = getnext_reverse_sequence_range_;
	}
	else {
		call_ = getnext_sequence_range_;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	for (;;) {
		Return((*call_)(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_count_sequence(str, &check, value));
		if (check) {
			localhold_end(hold);
			return Result(ret, value);
		}
	}
	localhold_end(hold);
	return Result(ret, Nil);
}

static int reverse_find_sequence(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(value_find_sequence(str, ret));
	rollback_local(local, stack);

	return 0;
}

int find_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		return fmte_("FIND don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(build_sequence_range_(range, pos, start, end));
	return value_find_sequence(&str, ret);
}

static int argument_find_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(build_sequence_range_(range, pos, start, end));
	return value_find_sequence(&str, ret);
}

int find_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence(ptr, ret, call, Nil, pos, rest);
}

int find_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence(ptr, ret, Nil, call, pos, rest);
}


/*
 *  position
 */
static int value_position_sequence(struct count_struct *str, size_t *ret, int *nilp)
{
	int check;
	int (*call_)(struct sequence_range *, addr *, int *);
	addr value;
	struct sequence_range *range;
	size_t count;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		reverse_sequence_range(range);
		call_ = getnext_reverse_sequence_range_;
	}
	else {
		call_ = getnext_sequence_range_;
	}

	/* loop */
	hold = LocalHold_array(str->ptr, 1);
	for (count = 0; ; count++) {
		Return((*call_)(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_count_sequence(str, &check, value));
		if (check) {
			localhold_end(hold);
			*ret = count;
			return Result(nilp, 0);
		}
	}
	localhold_end(hold);
	*ret = 0;
	return Result(nilp, 1);
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
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(value_position_sequence(str, &size, &check));
	rollback_local(local, stack);

	/* result */
	if (check)
		return Result(ret, Nil);
	size = str->start_value + range->end - size - 1;
	make_index_integer_heap(ret, size);

	return 0;
}

int position_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	int listp, check;
	unsigned fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	struct sequence_range *range;
	size_t size;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil)
		return fmte_("POSITION don't accept both :test and :test-not parameter.", NULL);

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	fromp = (from != Nil);
	range = &(str.range);
	Return(getindex_integer_(start, &(str.start_value)));
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
	Return(build_sequence_range_(range, pos, start, end));
	Return(value_position_sequence(&str, &size, &check));

	/* result */
	if (check)
		return Result(ret, Nil);
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
	int listp, check;
	unsigned fromp;
	addr from, start, end, key;
	struct count_struct str;
	struct sequence_range *range;
	size_t size;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	fromp = (from != Nil);
	range = &(str.range);
	Return(getindex_integer_(start, &(str.start_value)));
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
	Return(build_sequence_range_(range, pos, start, end));
	Return(value_position_sequence(&str, &size, &check));

	/* result */
	if (check)
		return Result(ret, Nil);
	if (fromp)
		size = range->end - size - 1;
	else
		size += range->start;
	make_index_integer_heap(ret, size);

	return 0;
}

int position_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_position_sequence(ptr, ret, call, Nil, pos, rest);
}

int position_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
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
	if (str->key != Nil)
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
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
		Return(callclang_funcall(ptr, &value, test, a, b, NULL));
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
	for (;;) {
		Return(getnext_sequence_range_(range1, &a, &check));
		if (check)
			break;
		localhold_set(hold, 0, a);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);

		Return(getnext_sequence_range_(range2, &b, &check));
		if (check)
			return Result(result, 0);
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
		return Result(nilp, 1);
	}
	if (size1 == 0) {
		*ret = 0;
		return Result(nilp, 0);
	}

	i = size2 - size1 + 1;
	save_sequence_range(range1);
	while (i) {
		i--;
		Return(reverse_pattern_search_sequence(str, &check, range1, range2, i));
		if (check) {
			*ret = i;
			return Result(nilp, 0);
		}
	}
	*ret = 0;
	return Result(nilp, 1);
}

static int reverse_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	Return(reverse_size_search_sequence(str, &size, &check));
	if (check)
		return Result(ret, Nil);
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
	for (;;) {
		Return(getnext_sequence_range_(range1, &a, &check));
		if (check)
			break;
		localhold_set(hold, 0, a);

		Return(key_search_sequence(str, &a, a));
		localhold_set(hold, 0, a);

		Return(getnext_sequence_range_(range2, &b, &check));
		if (check)
			return Result(result, 0);
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
		return Result(nilp, 1);
	}

	for (i = 0; ; i++) {
		save_sequence_range(range2);
		Return(normal_pattern_search_sequence(str, &check, range1, range2));
		if (check) {
			*ret = i;
			return Result(nilp, 0);
		}
		load_sequence_range(range2);
		Return(next_sequence_range_(range2, &check));
		if (check)
			break;
	}
	*ret = 0;
	return Result(nilp, 1);
}

static int normal_search_sequence(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	Return(normalsize_search_sequence(str, &size, &check));
	if (check)
		return Result(ret, Nil);
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

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_START1, &start1))
		fixnum_heap(&start1, 0);
	if (GetKeyArgs(rest, KEYWORD_START2, &start2))
		fixnum_heap(&start2, 0);
	if (GetKeyArgs(rest, KEYWORD_END1, &end1))
		end1 = Unbound;
	if (GetKeyArgs(rest, KEYWORD_END2, &end2))
		end2 = Unbound;
	if (test1 != Nil && test2 != Nil)
		return fmte_("SEARCH don't accept both :test and :test-not parameter.", NULL);

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
	Return(getindex_integer_(start2, &(str.start_value)));
	if (fromp) {
		Return(make_sequence_range_endp_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_vector_(local, pos2, start2, end2, &(str.range2)));
		Return(reverse_search_sequence(&str, ret));
	}
	else {
		Return(make_sequence_range_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_(local, pos2, start2, end2, &(str.range2)));
		Return(normal_search_sequence(&str, ret));
	}

	return 0;
}

int search_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_search_sequence(ptr, ret, pos1, pos2, rest));
	rollback_local(local, stack);

	return 0;
}


/*
 *  mismatch
 */
static int reverse_result_mismatch_sequence_(
		addr *ret,
		struct sequence_range *range,
		size_t i)
{
	size_t size;

	size = range->end - i;
	make_index_integer_heap(ret, size);

	return 0;
}

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
		Return(getnext_reverse_sequence_range_(range1, &a, &check1));
		Return(getnext_reverse_sequence_range_(range2, &b, &check2));
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
			goto result_diff;
	}

result_nil:
	localhold_end(hold);
	return Result(ret, Nil);

result_t:
	localhold_end(hold);
	return reverse_result_mismatch_sequence_(ret, range1, i);

result_diff:
	localhold_end(hold);
	return reverse_result_mismatch_sequence_(ret, range1, i);
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
		Return(getnext_sequence_range_(range1, &a, &check1));
		Return(getnext_sequence_range_(range2, &b, &check2));
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
	return Result(ret, Nil);

result_t:
	localhold_end(hold);
	make_index_integer_heap(ret, i + range1->start);
	return 0;
}

static int execute_mismatch_sequence(Execute ptr, addr *ret,
		addr pos1, addr pos2, addr rest)
{
	unsigned fromp;
	LocalRoot local;
	addr from, key, test1, test2, start1, start2, end1, end2;
	struct search_struct str;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_START1, &start1))
		fixnum_heap(&start1, 0);
	if (GetKeyArgs(rest, KEYWORD_START2, &start2))
		fixnum_heap(&start2, 0);
	if (GetKeyArgs(rest, KEYWORD_END1, &end1))
		end1 = Unbound;
	if (GetKeyArgs(rest, KEYWORD_END2, &end2))
		end2 = Unbound;
	if (test1 != Nil && test2 != Nil)
		return fmte_("MISMATCH don't accept both :test and :test-not parameter.", NULL);

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
		Return(make_sequence_range_mismatch_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_mismatch_(local, pos2, start2, end2, &(str.range2)));
		Return(reverse_mismatch_sequence(&str, ret));
	}
	else {
		Return(make_sequence_range_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_(local, pos2, start2, end2, &(str.range2)));
		Return(normal_mismatch_sequence(&str, ret));
	}

	return 0;
}

int mismatch_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_mismatch_sequence(ptr, ret, pos1, pos2, rest));
	rollback_local(local, stack);

	return 0;
}


/*
 *  replace
 */
static int list_replace_sequence_(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	int check;
	LocalStack stack;
	addr pos, value;
	size_t size, i;

	push_local(local, &stack);
	size = range2->size;
	vector_local(local, &pos, size);
	for (i = 0; ; i++) {
		Return(getnext_sequence_range_(range2, &value, &check));
		if (check)
			break;
		setarray(pos, i, value);
	}

	for (i = 0; i < size; i++) {
		if (endp_sequence_range(range1))
			break;
		getarray(pos, i, &value);
		Return(set_sequence_range_(range1, value));
		Return(next_sequence_range_(range1, &check));
	}
	rollback_local(local, stack);

	return 0;
}

static int forward_replace_sequence_(
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	int check;
	addr value;

	for (;;) {
		Return(getnext_sequence_range_(range2, &value, &check));
		if (check)
			break;
		if (endp_sequence_range(range1))
			break;
		Return(set_sequence_range_(range1, value));
		Return(next_sequence_range_(range1, &check));
	}

	return 0;
}

static int eq_replace_sequence_(LocalRoot local,
		struct sequence_range *range1,
		struct sequence_range *range2)
{
	if (range1->start == range2->start)
		return 0;
	if (range2->end < range1->start || range2->end < range1->start)
		return forward_replace_sequence_(range1, range2);
	else
		return list_replace_sequence_(local, range1, range2);
}

int replace_common_(Execute ptr, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	addr start1, start2, end1, end2;
	struct sequence_range range1, range2;

	if (GetKeyArgs(rest, KEYWORD_START1, &start1))
		fixnum_heap(&start1, 0);
	if (GetKeyArgs(rest, KEYWORD_START2, &start2))
		fixnum_heap(&start2, 0);
	if (GetKeyArgs(rest, KEYWORD_END1, &end1))
		end1 = Unbound;
	if (GetKeyArgs(rest, KEYWORD_END2, &end2))
		end2 = Unbound;

	Return(build_sequence_range_endp_(&range1, pos1, start1, end1));
	Return(build_sequence_range_endp_(&range2, pos2, start2, end2));
	local = ptr->local;
	if (range1.listp && range2.listp)
		return list_replace_sequence_(local, &range1, &range2);
	else if (pos1 != pos2)
		return forward_replace_sequence_(&range1, &range2);
	else
		return eq_replace_sequence_(local, &range1, &range2);
}


/*
 *  substitute
 */
static int boolean_substitute_sequence(struct count_struct *str, int *ret, addr pos)
{
	int check;

	if (str->count == Nil)
		return boolean_count_sequence(str, ret, pos);
	if (str->limit == 0)
		return Result(ret, 0);
	Return(boolean_count_sequence(str, &check, pos));
	if (check)
		str->limit--;

	return Result(ret, check);
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
	Return(before_sequence_write_(ret, range));

	hold = LocalHold_array(str->ptr, 1);
	for (;;) {
		Return(getnext_reverse_sequence_range_(range, &pos, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		Return(set_sequence_range_(range, check? one: pos));
	}
	localhold_end(hold);

	load_sequence_range(range);
	for (;;) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		Return(push_sequence_write_(ret, pos));
	}
	Return(after_sequence_write_(ret, range));

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
	Return(build_sequence_range_vector2_(local,
				range, pos1, str->start, str->end, &pos2, &pos3));
	build_sequence_write_list(ret);
	/* before start */
	while (pos1 != pos2) {
		GetCons(pos1, &value, &pos1);
		Return(push_sequence_write_(ret, value));
	}
	/* between start and end */
	gchold_push_local(local, ret->pos);
	Return(reverse_list_substitute_sequence(str, ret));
	rollback_local(local, stack);
	/* after end */
	while (pos3 != Nil) {
		Return_getcons(pos3, &value, &pos3);
		Return(push_sequence_write_(ret, value));
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
	Return(before_sequence_write_(ret, range));

	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	for (;;) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		Return(push_sequence_write_(ret, check? one: pos));
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int copy_substitute_sequence(
		struct count_struct *str, struct sequence_write *ret, addr pos)
{
	int check;
	int (*get_)(struct sequence_range *, addr *, int *);
	addr value, one;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	if (str->fromp) {
		get_ = getnext_reverse_sequence_range_;
		reverse_sequence_range(range);
		reverse_sequence_write(ret, range->size);
	}
	else {
		get_ = getnext_sequence_range_;
	}

	/* loop */
	one = str->second;
	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	for (;;) {
		Return((*get_)(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_substitute_sequence(str, &check, value));
		Return(push_sequence_write_(ret, check? one: value));
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);

	/* after */
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int make_vector_array_sequence(addr *ret, addr pos, size_t size)
{
	struct array_struct *info = ArrayInfoStruct(pos);
	return make_specialized_sequence(ret, info->type, info->bytesize, size);
}

static int make_vector_size_sequence(addr *ret, addr pos, size_t size)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			vector_heap(ret, size);
			break;

		case LISPTYPE_STRING:
			strvect_heap(ret, size);
			break;

		case LISPTYPE_ARRAY:
			return make_vector_array_sequence(ret, pos, size);

		case LISPTYPE_BITVECTOR:
			bitmemory_unsafe(NULL, ret, size);
			break;

		default:
			return TypeError_(pos, SEQUENCE);
	}

	return 0;
}

static int vector_substitute_sequence(addr *ret, addr pos)
{
	size_t size;
	Return(length_sequence_(pos, 1, &size));
	return make_vector_size_sequence(ret, pos, size);
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
		Return(list_substitute_sequence(str, ret));
	}
	else {
		Return(vector_substitute_sequence(&pos, pos));
		hold = LocalHold_local_push(str->ptr, pos);
		Return(copy_substitute_sequence(str, ret, pos));
		localhold_end(hold);
	}

	return 0;
}

static int setcount_sequence(struct count_struct *str, addr count)
{
	int check;
	size_t limit;

	if (count == Nil) {
		str->count = Nil;
		str->limit = 0;
		return 0;
	}
	if (! integerp(count)) {
		return fmte_(":COUNT argument ~S must be an integer type.", count, NULL);
	}
	Return(minusp_integer_(count, &check));
	if (check) {
		fixnum_heap(&count, 0);
		limit = 0;
	}
	else if (GetIndex_integer(count, &limit)) {
		return fmte_(":COUNT argument ~S is too large.", count, NULL);
	}
	str->count = count;
	str->limit = limit;

	return 0;
}

int substitute_common(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil) {
		return fmte_("SUBSTITUTE don't accept "
				"both :test and :test-not parameter.", NULL);
	}

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp) {
		Return(reverse_substitute_sequence(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_substitute_sequence(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_substitute_sequence(Execute ptr, addr *ret,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp) {
		Return(reverse_substitute_sequence(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_substitute_sequence(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

int substitute_if_common(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return argument_substitute_sequence(ptr, ret, item, call, Nil, pos, rest);
}

int substitute_if_not_common(Execute ptr,
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
		Return(get_reverse_sequence_range_(range, &pos, &check));
		localhold_set(hold, 0, pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		Return(set_reverse_sequence_range_(range, check? one: pos));
		Return(next_reverse_sequence_range_(range, &check));
	}
	localhold_end(hold);

	/* reference */
	write = &temp;
	Return(build_sequence_range_(write, str->pos, str->start, str->end));
	load_sequence_range(range);
	for (;;) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		Return(set_sequence_range_(write, pos));
		Return(next_sequence_range_(write, &check));
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
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(reverse_list_nsubstitute_sequence(str));
	rollback_local(local, stack);

	return 0;
}

static int normal_nsubstitute_sequence(struct count_struct *str)
{
	int check;
	int (*get_)(struct sequence_range *, addr *, int *);
	int (*set_)(struct sequence_range *, addr);
	int (*next_)(struct sequence_range *, int *);
	int (*endp)(struct sequence_range *);
	addr value, one;
	struct sequence_range *range;
	LocalHold hold;

	/* initialize */
	range = &(str->range);
	if (str->fromp) {
		get_ = get_reverse_sequence_range_;
		set_ = set_reverse_sequence_range_;
		next_ = next_reverse_sequence_range_;
		endp = endp_reverse_sequence_range;
		reverse_sequence_range(range);
	}
	else {
		get_ = get_sequence_range_;
		set_ = set_sequence_range_;
		next_ = next_sequence_range_;
		endp = endp_sequence_range;
	}

	/* loop */
	one = str->second;
	hold = LocalHold_array(str->ptr, 1);
	while (! (*endp)(range)) {
		Return((*get_)(range, &value, &check));
		localhold_set(hold, 0, value);
		Return(boolean_substitute_sequence(str, &check, value));
		Return((*set_)(range, check? one: value));
		Return((*next_)(range, &check));
	}
	localhold_end(hold);

	return 0;
}

int nsubstitute_common(Execute ptr,
		addr item1, addr item2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil) {
		return fmte_("NSUBSTITUTE don't accept "
				"both :test and :test-not parameter.", NULL);
	}

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp)
		return reverse_nsubstitute_sequence(&str);
	Return(build_sequence_range_(range, pos, start, end));
	return normal_nsubstitute_sequence(&str);
}

static int argument_nsubstitute_sequence(Execute ptr,
		addr item, addr test1, addr test2, addr pos, addr rest)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp)
		return reverse_nsubstitute_sequence(&str);
	Return(build_sequence_range_(range, pos, start, end));
	return normal_nsubstitute_sequence(&str);
}

int nsubstitute_if_common(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence(ptr, item, call, Nil, pos, rest);
}

int nsubstitute_if_not_common(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence(ptr, item, Nil, call, pos, rest);
}


/*
 *  concatenate
 */
static int list_concatenate_sequence(addr *ret, addr type, addr rest)
{
	int check;
	enum LISPDECL decl;
	addr root, value, one;
	size_t size, i;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_CONS && decl != LISPDECL_LIST)
		return Result(ret, Unbound);

	/* concatenate */
	root = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &value, &rest);
		Return(listp_sequence_(value, &check));
		if (check) {
			while (value != Nil) {
				Return_getcons(value, &one, &value);
				cons_heap(&root, one, root);
			}
		}
		else {
			Return(length_sequence_(value, 1, &size));
			for (i = 0; i < size; i++) {
				Return(getelt_sequence_(NULL, value, i, &one));
				cons_heap(&root, one, root);
			}
		}
	}
	nreverse(ret, root);
	return 0;
}

static int length_concatenate_sequence(addr rest, size_t *ret)
{
	addr pos;
	size_t size, value;

	for (size = 0; rest != Nil; size += value) {
		Return_getcons(rest, &pos, &rest);
		Return(length_sequence_(pos, 1, &value));
	}

	return Result(ret, size);
}

static int value_concatenate_sequence_(addr root, addr rest)
{
	addr pos;
	size_t index, size, i;
	struct array_value value;

	for (index = 0; rest != Nil; ) {
		GetCons(rest, &pos, &rest);
		Return(length_sequence_(pos, 1, &size));
		for (i = 0; i < size; i++) {
			Return(getelt_inplace_sequence_(pos, i, &value));
			Return(setelt_inplace_sequence_(NULL, root, index++, &value));
		}
	}

	return 0;
}

static int vector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence(rest, &size));
	Return(array_upgraded_merge_sequence(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int simple_vector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence(rest, &size));
	vector_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int string_concatenate_sequence(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (! type_string_p(type))
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence(rest, &size));
	strvect_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int array_concatenate_sequence(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence(rest, &size));
	Return(array_upgraded_merge_sequence(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int bitvector_concatenate_sequence(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence(rest, &size));
	bitmemory_unsafe(NULL, &root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int type_concatenate_sequence(Execute ptr, addr *ret, addr type, addr rest)
{
	addr pos;

	/* list */
	Return(list_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* vector */
	Return(vector_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* simple-vector */
	Return(simple_vector_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* string */
	Return(string_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* array */
	Return(array_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* bitvector */
	Return(bitvector_concatenate_sequence(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int concatenate_common(Execute ptr, addr *ret, addr type, addr rest)
{
	addr check;

	Return(parse_type(ptr, &check, type, Nil));
	Return(type_concatenate_sequence(ptr, ret, check, rest));

	return call_typep_asterisk_error_(ptr, *ret, check);
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
	for (size = 0; ; size++) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		setarray(table, size, pos);
	}

	/* remove */
	for (a = b = size; a; ) {
		getarray(table, --a, &pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		if (! check)
			setarray(table, --b, pos);
	}
	while (b < size) {
		getarray(table, b++, &pos);
		Return(push_sequence_write_(ret, pos));
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
	Return(build_sequence_range_endp_(range, str->pos, str->start, str->end));
	build_sequence_write_list(ret);
	/* before start */
	Return(before_sequence_write_(ret, range));
	/* between start and end */
	push_local(local, &stack);
	gchold_push_local(local, ret->pos);
	vector_local(local, &table, range->size);
	Return(reverse_list_local_remove_sequence(str, ret, table));
	rollback_local(local, stack);
	/* after end */
	Return(after_sequence_write_(ret, range));

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
	for (size = 0; ; size++) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		setarray(table, size, pos);
	}

	/* copy */
	for (i = size; i; ) {
		i--;
		getarray(table, i, &pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		setarray(table, i, check? T: Nil);
	}

	/* remove */
	load_sequence_range(range);
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == T) {
			Return(remove_sequence_range_(range));
		}
		else {
			Return(next_sequence_range_(range, &check));
		}
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
	Return(build_sequence_range_endp_(range, str->pos, str->start, str->end));
	/* replace */
	push_local(local, &stack);
	vector_local(local, &table, range->size);
	Return(reverse_list_local_delete_sequence(str, table));
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
	Return(before_sequence_write_(ret, range));
	hold = LocalHold_array(str->ptr, 2);
	localhold_set(hold, 1, ret->pos);
	for (;;) {
		Return(getnext_sequence_range_(range, &pos, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos);
		Return(boolean_substitute_sequence(str, &check, pos));
		if (! check) {
			Return(push_sequence_write_(ret, pos));
			localhold_set(hold, 1, ret->pos);
		}
	}
	localhold_end(hold);
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int list_delete_sequence(struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos;
	struct sequence_range *range;

	range = &(str->range);
	while (! endp_sequence_range(range)) {
		Return(get_sequence_range_(range, &pos, &check));
		Return(boolean_substitute_sequence(str, &check, pos));
		if (check) {
			Return(remove_sequence_range_(range));
		}
		else {
			Return(next_sequence_range_(range, &check));
		}
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
	loc = 0;
	for (;;) {
		Return(getnext_sequence_range_(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_substitute_sequence(str, &check, value));
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	Return(length_sequence_(pos, 1, &size));
	size = size - range->size + loc;
	Return(make_vector_size_sequence(&pos, pos, size));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	for (i = 0; i < loc; i++) {
		getarray(table, i, &value);
		Return(push_sequence_write_(ret, value));
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
	loc = 0;
	for (;;) {
		Return(getnext_reverse_sequence_range_(range, &value, &check));
		if (check)
			break;
		localhold_set(hold, 0, value);
		Return(boolean_substitute_sequence(str, &check, value));
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	Return(length_sequence_(pos, 1, &size));
	size = size - range->size + loc;
	Return(make_vector_size_sequence(&pos, pos, size));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	while (loc) {
		getarray(table, --loc, &value);
		Return(push_sequence_write_(ret, value));
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
		Return(copy_reverse_remove_sequence(str, ret, table));
	}
	else {
		Return(copy_normal_remove_sequence(str, ret, table));
	}
	Return(after_sequence_write_(ret, range));

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
		Return(copy_remove_sequence(str, ret));
		rollback_local(local, stack);
		return 0;
	}
}

static int argument_remove_sequence(Execute ptr,
		addr *ret, addr item, addr pos, addr rest, unsigned delp)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil) {
		return fmte_("SUBSTITUTE don't accept "
				"both :test and :test-not parameter.", NULL);
	}

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp) {
		Return(list_reverse_type_remove_sequence(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_remove_sequence(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_remove_if_sequence(Execute ptr, addr *ret,
		addr test1, addr test2, addr pos, addr rest, unsigned delp)
{
	int listp;
	unsigned fromp;
	addr from, start, end, key, count;
	struct count_struct str;
	struct sequence_range *range;
	struct sequence_write write;

	if (GetKeyArgs(rest, KEYWORD_COUNT, &count))
		count = Nil;
	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
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
	Return(setcount_sequence(&str, count));

	if (listp && fromp) {
		Return(list_reverse_type_remove_sequence(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_remove_sequence(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

int remove_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence(ptr, ret, item, pos, rest, 0);
}

int remove_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, call, Nil, pos, rest, 0);
}

int remove_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, Nil, call, pos, rest, 0);
}


/*
 *  delete
 */
int delete_common(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence(ptr, ret, item, pos, rest, 1);
}

int delete_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, call, Nil, pos, rest, 1);
}

int delete_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence(ptr, ret, Nil, call, pos, rest, 1);
}


/*
 *  remove-duplicates / delete-duplicates
 */
static int list_normal_duplicates_sequence(struct count_struct *str,
		struct sequence_range *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ; i++) {
		Return(getnext_sequence_range_(range, &a, &check));
		if (check)
			break;
		setarray(table, i, a);
	}

	n = 0;
	for (i = size; i; ) {
		getarray(table, --i, &a);
		if (a == Unbound)
			continue;
		if (str->key != Nil) {
			Return(callclang_funcall(str->ptr, &a, str->key, a, NULL));
		}
		str->item = a;
		for (k = i; k; ) {
			getarray(table, --k, &b);
			if (b == Unbound)
				continue;
			Return(boolean_substitute_sequence(str, &check, b));
			if (check) {
				setarray(table, k, Unbound);
				n++;
			}
		}
	}
	if (ret)
		*ret = n;

	return 0;
}

static int list_normal_delete_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_sequence_range(range);
	Return(list_normal_duplicates_sequence(str, range, table, size, NULL));
	load_sequence_range(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound) {
			Return(remove_sequence_range_(range));
		}
		else {
			Return(next_sequence_range_(range, &check));
		}
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
	Return(list_normal_duplicates_sequence(str, range, table, size, NULL));

	/* result */
	build_sequence_write_list(ret);
	Return(before_sequence_write_(ret, range));
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound) {
			Return(push_sequence_write_(ret, pos));
		}
	}
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int list_reverse_duplicates_sequence(struct count_struct *str,
		struct sequence_range *range, addr table, size_t size, size_t *ret)
{
	int check;
	addr a, b;
	size_t i, k, n;

	for (i = 0; ; i++) {
		Return(getnext_sequence_range_(range, &a, &check));
		if (check)
			break;
		setarray(table, i, a);
	}

	n = 0;
	for (i = 0; i < size; i++) {
		getarray(table, i, &a);
		if (a == Unbound)
			continue;
		if (str->key != Nil) {
			Return(callclang_funcall(str->ptr, &a, str->key, a, NULL));
		}
		str->item = a;
		for (k = i + 1; k < size; k++) {
			getarray(table, k, &b);
			if (b == Unbound)
				continue;
			Return(boolean_substitute_sequence(str, &check, b));
			if (check) {
				setarray(table, k, Unbound);
				n++;
			}
		}
	}
	if (ret)
		*ret = n;

	return 0;
}

static int list_reverse_delete_duplicates(
		struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	save_sequence_range(range);
	Return(list_reverse_duplicates_sequence(str, range, table, size, NULL));
	load_sequence_range(range);

	/* result */
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos == Unbound) {
			Return(remove_sequence_range_(range));
		}
		else {
			Return(next_sequence_range_(range, &check));
		}
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
	Return(list_reverse_duplicates_sequence(str, range, table, size, NULL));

	/* result */
	build_sequence_write_list(ret);
	Return(before_sequence_write_(ret, range));
	for (i = 0; i < size; i++) {
		getarray(table, i, &pos);
		if (pos != Unbound) {
			Return(push_sequence_write_(ret, pos));
		}
	}
	Return(after_sequence_write_(ret, range));

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
	Return(list_normal_duplicates_sequence(str, range, table, size, &loc));

	/* copy */
	pos = range->pos;
	Return(length_sequence_(pos, 1, &k));
	k = k - loc;
	Return(make_vector_size_sequence(&pos, pos, k));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound) {
			Return(push_sequence_write_(ret, pos));
		}
	}
	Return(after_sequence_write_(ret, range));

	return 0;
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
	Return(list_reverse_duplicates_sequence(str, range, table, size, &loc));

	/* copy */
	pos = range->pos;
	Return(length_sequence_(pos, 1, &k));
	k = k - loc;
	Return(make_vector_size_sequence(&pos, pos, k));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	for (k = 0; k < size; k++) {
		getarray(table, k, &pos);
		if (pos != Unbound) {
			Return(push_sequence_write_(ret, pos));
		}
	}
	Return(after_sequence_write_(ret, range));

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
	int listp;
	unsigned fromp;
	addr from, start, end, key, test1, test2;
	struct count_struct str;
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;
	struct sequence_write write;

	if (GetKeyArgs(rest, KEYWORD_FROM_END, &from))
		from = Nil;
	if (GetKeyArgs(rest, KEYWORD_START, &start))
		fixnum_heap(&start, 0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Unbound;
	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST, &test1))
		test1 = Nil;
	if (GetKeyArgs(rest, KEYWORD_TEST_NOT, &test2))
		test2 = Nil;
	if (test1 != Nil && test2 != Nil) {
		return fmte_("Arguments don't accept "
				"both :test and :test-not parameter.", NULL);
	}

	cleartype(str);
	local = ptr->local;
	Return(listp_sequence_(pos, &listp));
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
		Return(build_sequence_range_endp_(range, pos, start, end));
		Return(list_remove_duplicates(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(vector_remove_duplicates_sequence(&str, &write));
	}
	rollback_local(local, stack);
	*ret = result_sequence_write(&write);

	return 0;
}

int remove_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 0);
}

int delete_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates(ptr, ret, pos, rest, 1);
}

