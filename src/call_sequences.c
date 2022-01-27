#include "array_copy.h"
#include "array_sequence.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "call_sequences.h"
#include "hold.h"
#include "local.h"
#include "integer.h"
#include "memory.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_range.h"
#include "sort.h"
#include "strtype.h"
#include "strvect.h"
#include "type.h"
#include "type_parse.h"
#include "type_upgraded.h"
#include "typedef.h"

/*
 *  copy-seq
 */
int copy_seq_common_(addr var, addr *ret)
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
 *  elt
 */
int elt_common_(addr var, addr index, addr *ret)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index ~S.", index, NULL);
	return getelt_sequence_(NULL, var, size, ret);
}


/*
 *  (setf elt)
 */
int setf_elt_common_(addr value, addr pos, addr index)
{
	size_t size;

	if (GetIndex_integer(index, &size))
		return fmte_("Too large index ~S.", index, NULL);
	return setelt_sequence_(pos, size, value);
}


/*
 *  fill
 */
static int list_fill_sequence_(addr list, addr item, addr start, addr end)
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

static int fill_call_common_(addr var, addr item, addr start, addr end)
{
	switch (GetType(var)) {
		case LISPTYPE_NIL:
			return 0;

		case LISPTYPE_CONS:
			return list_fill_sequence_(var, item, start, end);

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

int fill_common_(addr var, addr item, addr rest)
{
	addr start, end;

	if (GetKeyArgs(rest, KEYWORD_START, &start))
		start = fixnumh(0);
	if (GetKeyArgs(rest, KEYWORD_END, &end))
		end = Nil;

	return fill_call_common_(var, item, start, end);
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

int subseq_common_(addr var, addr start, addr end, addr *ret)
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

static int key_reduce_sequence_(struct reduce_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return funcall1_control_(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
}

static int throw_reduce_sequence_(struct reduce_struct *str, int *result, addr *ret)
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
			Return(apply1_control_(str->ptr, ret, str->call, Nil));
		}
		else {
			*ret = value;
		}
		return Result(result, 1);
	}

	/* single value */
	if (endp_sequence_range(range) && value == Unbound) {
		hold = LocalHold_local_push(str->ptr, pos);
		Return(key_reduce_sequence_(str, ret, pos));
		localhold_end(hold);
		return Result(result, 1);
	}

	/* multiple value */
	load_sequence_range(range);
	return Result(result, 0);
}

static int value_reduce_sequence_(struct reduce_struct *str, addr *ret)
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
		Return(key_reduce_sequence_(str, &pos1, pos1));
	}
	localhold_set(hold, 0, pos1);

	/* loop */
	for (;;) {
		Return(getnext_sequence_range_(range, &pos2, &check));
		if (check)
			break;
		localhold_set(hold, 1, pos2);
		Return(key_reduce_sequence_(str, &pos2, pos2));
		localhold_set(hold, 1, pos2);
		Return(funcall1_control_(ptr, &pos1, call, pos1, pos2, NULL));
		localhold_set(hold, 0, pos1);
	}
	localhold_end(hold);

	return Result(ret, pos1);
}

static int reverse_vector_reduce_sequence_(struct reduce_struct *str, addr *ret)
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
		Return(key_reduce_sequence_(str, &pos2, pos2));
	}
	localhold_set(hold, 1, pos2);

	/* loop */
	for (;;) {
		Return(getnext_reverse_sequence_range_(range, &pos1, &check));
		if (check)
			break;
		localhold_set(hold, 0, pos1);
		Return(key_reduce_sequence_(str, &pos1, pos1));
		localhold_set(hold, 0, pos1);
		Return(funcall1_control_(ptr, &pos2, call, pos1, pos2, NULL));
		localhold_set(hold, 1, pos2);
	}
	localhold_end(hold);

	return Result(ret, pos2);
}

static int switch_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	int check;

	Return(throw_reduce_sequence_(str, &check, ret));
	if (check)
		return 0;
	else if (str->range.listp)
		return value_reduce_sequence_(str, ret);
	else if (str->fromp)
		return reverse_vector_reduce_sequence_(str, ret);
	else
		return value_reduce_sequence_(str, ret);
}

static int reverse_reduce_sequence_(struct reduce_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(switch_reduce_sequence_(str, ret));
	rollback_local(local, stack);

	return 0;
}

int reduce_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
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
		return reverse_reduce_sequence_(&str, ret);
	else {
		Return(build_sequence_range_(range, pos, start, end));
		return switch_reduce_sequence_(&str, ret);
	}
}


/*
 *  sort
 */
int sort_common_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return quick_sort_sequence_(ptr, pos, call, key);
}


/*
 *  stable-sort
 */
int stable_sort_common_(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (GetKeyArgs(rest, KEYWORD_KEY, &key))
		key = Nil;
	return merge_sort_sequence_(ptr, pos, call, key);
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
 *  concatenate
 */
static int list_concatenate_sequence_(addr *ret, addr type, addr rest)
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

static int length_concatenate_sequence_(addr rest, size_t *ret)
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

static int vector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	Return(array_upgraded_merge_sequence_(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int simple_vector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (LowLispDecl(type) != LISPDECL_SIMPLE_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	vector_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int string_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	addr root;
	size_t size;

	/* type check */
	if (! type_string_p(type))
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	strvect_heap(&root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int array_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	Return(array_upgraded_merge_sequence_(&root, type, size));
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int bitvector_concatenate_sequence_(addr *ret, addr type, addr rest)
{
	enum LISPDECL decl;
	addr root;
	size_t size;

	/* type check */
	decl = LowLispDecl(type);
	if (decl != LISPDECL_BIT_VECTOR && decl != LISPDECL_SIMPLE_BIT_VECTOR)
		return Result(ret, Unbound);

	/* concatenate */
	Return(length_concatenate_sequence_(rest, &size));
	bitmemory_unsafe(NULL, &root, size);
	Return(value_concatenate_sequence_(root, rest));

	return Result(ret, root);
}

static int type_concatenate_sequence_(Execute ptr, addr *ret, addr type, addr rest)
{
	addr pos;

	/* list */
	Return(list_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* vector */
	Return(vector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* simple-vector */
	Return(simple_vector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* string */
	Return(string_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* array */
	Return(array_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* bitvector */
	Return(bitvector_concatenate_sequence_(&pos, type, rest));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	return call_type_error_va_(ptr, type, Nil,
			"Invalid type-specifier ~S.", type, NULL);
}

int concatenate_common_(Execute ptr, addr *ret, addr type, addr rest)
{
	addr check;

	Return(parse_type_(ptr, &check, type, Nil));
	Return(type_concatenate_sequence_(ptr, ret, check, rest));

	return call_typep_asterisk_error_(ptr, *ret, check);
}

