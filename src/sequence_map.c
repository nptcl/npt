#include "array_access.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "cons_list.h"
#include "control_execute.h"
#include "local.h"
#include "hold.h"
#include "sequence.h"
#include "sequence_iterator.h"
#include "sequence_map.h"
#include "strvect.h"
#include "type.h"
#include "type_memory.h"
#include "type_parse.h"
#include "type_upgraded.h"
#include "typedef.h"

/*
 *  map
 */
static int nil_map_sequence_(Execute ptr, int *result, addr *ret,
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
		Return(apply1_control_(ptr, &temp, call, list));
	}
	*ret = Nil;
	return Result(result, 1);
}

static int list_map_sequence_(Execute ptr, int *result, addr *ret,
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
		Return(apply1_control_(ptr, &temp, call, list));
		cons_heap(&root, temp, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return Result(result, 1);
}

static int vector_bitvector_map_sequence_(Execute ptr, addr *ret,
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
		Return(apply1_control_(ptr, &value, call, list));
		Return(bitmemory_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_string_map_sequence_(Execute ptr, addr *ret,
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
		Return(apply1_control_(ptr, &value, call, list));
		Return(strvect_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_signed_map_sequence_(Execute ptr, addr *ret,
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
		Return(apply1_control_(ptr, &value, call, list));
		Return(array_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_float_map_sequence_(Execute ptr, addr *ret,
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
		Return(apply1_control_(ptr, &value, call, list));
		Return(array_set_(root, i, value));
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_general_map_sequence_(Execute ptr, addr *ret,
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
		Return(apply1_control_(ptr, &value, call, list));
		setarray(root, i, value);
	}
	localhold_end(hold);

	return Result(ret, root);
}

static int vector_upgraded_map_sequence_(Execute ptr,
		addr *ret, addr type, addr call, struct sequence_group *group)
{
	enum ARRAY_TYPE upgraded;
	int upsize;

	GetArrayType(type, 0, &type);
	Return(upgraded_array_value_(type, &upgraded, &upsize));
	switch (upgraded) {
		case ARRAY_TYPE_BIT:
			return vector_bitvector_map_sequence_(ptr, ret, call, group);

		case ARRAY_TYPE_CHARACTER:
			return vector_string_map_sequence_(ptr, ret, call, group);

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return vector_signed_map_sequence_(ptr, ret, call, group, upgraded, upsize);

		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return vector_float_map_sequence_(ptr, ret, call, group, upgraded);

		default:
			return vector_general_map_sequence_(ptr, ret, call, group);
	}
}

static int vector_map_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(vector_upgraded_map_sequence_(ptr, ret, type, call, group));
	return Result(result, 1);
}

static int simple_vector_map_sequence_(Execute ptr, int *result, addr *ret,
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
		Return(apply1_control_(ptr, &temp, call, list));
		setarray(root, i, temp);
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int string_map_sequence_(Execute ptr, int *result, addr *ret,
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
		Return(apply1_control_(ptr, &temp, call, list));
		Return(strvect_set_(root, i, temp));
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int array_map_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(vector_upgraded_map_sequence_(ptr, ret, type, call, group));
	return Result(result, 1);
}

static int bitvector_map_sequence_(Execute ptr, int *result, addr *ret,
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
		Return(apply1_control_(ptr, &temp, call, list));
		Return(bitmemory_set_(root, i, temp));
	}
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int execute_map_sequence_(Execute ptr, addr *ret,
		addr type, addr call, addr rest)
{
	int check;

	/* nil */
	Return(nil_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* list */
	Return(list_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* vector */
	Return(vector_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* simple-vector */
	Return(simple_vector_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* string */
	Return(string_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* array */
	Return(array_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* bitvector */
	Return(bitvector_map_sequence_(ptr, &check, ret, type, call, rest));
	if (check)
		return 0;

	/* error */
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int map_common_(Execute ptr, addr *ret, addr type, addr call, addr rest)
{
	addr check;
	LocalHold hold;

	if (rest == Nil) {
		*ret = Nil;
		return fmte_("Too few map arguments.", NULL);
	}
	hold = LocalHold_local(ptr);
	Return(parse_type_(ptr, &check, type, Nil));
	localhold_push(hold, check);

	Return(execute_map_sequence_(ptr, &rest, check, call, rest));
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

static int execute_map_into_sequence_(Execute ptr, addr var, addr call, addr rest)
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
		Return(apply1_control_(ptr, &pos, call, list));
		Return(set_sequence_iterator_(into, pos, &check));
		if (check)
			break;
	}

	/* update fill-pointer */
	fill_map_into_sequence(var, i);

	return 0;
}

int map_into_common_(Execute ptr, addr var, addr call, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_map_into_sequence_(ptr, var, call, rest));
	rollback_local(local, stack);

	return 0;
}

