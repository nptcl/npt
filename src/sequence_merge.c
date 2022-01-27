#include "bit.h"
#include "condition.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "local.h"
#include "hold.h"
#include "control_execute.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_iterator.h"
#include "sequence_merge.h"
#include "strvect.h"
#include "type.h"
#include "type_parse.h"
#include "typedef.h"

/*
 *  merge
 */
static int key_merge_sequence_(Execute ptr, addr *ret, addr key, addr value)
{
	if (key != Nil)
		return funcall1_control_(ptr, ret, key, value, NULL);
	else
		return Result(ret, value);
}

static int list_merge_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(key_merge_sequence_(ptr, &a2, key, a1));
	localhold_set(hold, 2, a2);
	Return(key_merge_sequence_(ptr, &b2, key, b1));
	localhold_set(hold, 3, b2);
loop:
	Return(funcall1_control_(ptr, &value, call, a2, b2, NULL));
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
		Return(key_merge_sequence_(ptr, &a2, key, a1));
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
		Return(key_merge_sequence_(ptr, &b2, key, b1));
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

static int vector_make_merge_sequence_(Execute ptr, addr root,
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
	Return(key_merge_sequence_(ptr, &a2, key, a1));
	localhold_set(hold, 2, a2);
	Return(key_merge_sequence_(ptr, &b2, key, b1));
	localhold_set(hold, 3, b2);
loop:
	Return(funcall1_control_(ptr, &value, call, a2, b2, NULL));
	if (value != Nil) {
		Return(setelt_sequence_(root, i++, a1));
		Return(object_sequence_iterator_(str1, &a1, &check));
		if (! check) {
			Return(setelt_sequence_(root, i++, b1));
			goto tail2;
		}
		localhold_set(hold, 0, a1);
		Return(key_merge_sequence_(ptr, &a2, key, a1));
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
		Return(key_merge_sequence_(ptr, &b2, key, b1));
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

static int vector_merge_sequence_(Execute ptr, int *result, addr *ret,
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

	Return(array_upgraded_merge_sequence_(&root, type, size));
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence_(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int simple_vector_merge_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(vector_make_merge_sequence_(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int string_merge_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(vector_make_merge_sequence_(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int array_merge_sequence_(Execute ptr, int *result, addr *ret,
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

	Return(array_upgraded_merge_sequence_(&root, type, size));
	hold = LocalHold_local_push(ptr, root);
	Return(vector_make_merge_sequence_(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int bitvector_merge_sequence_(Execute ptr, int *result, addr *ret,
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
	Return(vector_make_merge_sequence_(ptr, root, str1, str2, call, key));
	localhold_end(hold);
	*ret = root;
	return Result(result, 1);
}

static int execute_merge_sequence_(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	int check;

	/* list */
	Return(list_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* vector */
	Return(vector_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* simple-vector */
	Return(simple_vector_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* string */
	Return(string_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* array */
	Return(array_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* bitvector */
	Return(bitvector_merge_sequence_(ptr, &check, ret, type, pos1, pos2, call, key));
	if (check)
		return 0;

	/* error */
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

static int merge_call_common_(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key)
{
	addr check;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(parse_type_(ptr, &check, type, Nil));
	localhold_push(hold, check);
	/* pos2 -> pos1 */
	Return(execute_merge_sequence_(ptr, &call, check, pos2, pos1, call, key));
	localhold_push(hold, call);
	Return(call_typep_asterisk_error_(ptr, call, check));

	return Result(ret, call);
}

int merge_common_(Execute ptr,
		addr type, addr pos1, addr pos2, addr call, addr rest, addr *ret)
{
	addr key;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return merge_call_common_(ptr, ret, type, pos1, pos2, call, key);
}

