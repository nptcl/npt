#include "condition.h"
#include "cons_plist.h"
#include "hold.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_count.h"
#include "sequence_remove.h"
#include "sequence_substitute.h"
#include "sequence_write.h"
#include "typedef.h"

/*
 *  remove
 */
static int reverse_list_local_remove_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, pos));
		if (! check)
			setarray(table, --b, pos);
	}
	while (b < size) {
		getarray(table, b++, &pos);
		Return(push_sequence_write_(ret, pos));
	}

	return 0;
}

static int list_reverse_remove_sequence_(
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
	Return(reverse_list_local_remove_sequence_(str, ret, table));
	rollback_local(local, stack);
	/* after end */
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int reverse_list_local_delete_sequence_(struct count_struct *str, addr table)
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
		Return(boolean_substitute_sequence_(str, &check, pos));
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

static int list_reverse_delete_sequence_(
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
	Return(reverse_list_local_delete_sequence_(str, table));
	rollback_local(local, stack);
	/* result */
	build_sequence_write_result(ret, range->pos);

	return 0;
}

static int list_reverse_type_remove_sequence_(
		struct count_struct *str, struct sequence_write *ret)
{
	if (str->delp)
		return list_reverse_delete_sequence_(str, ret);
	else
		return list_reverse_remove_sequence_(str, ret);
}

static int list_remove_sequence_(struct count_struct *str, struct sequence_write *ret)
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
		Return(boolean_substitute_sequence_(str, &check, pos));
		if (! check) {
			Return(push_sequence_write_(ret, pos));
			localhold_set(hold, 1, ret->pos);
		}
	}
	localhold_end(hold);
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int list_delete_sequence_(struct count_struct *str, struct sequence_write *ret)
{
	int check;
	addr pos;
	struct sequence_range *range;

	range = &(str->range);
	while (! endp_sequence_range(range)) {
		Return(get_sequence_range_(range, &pos, &check));
		Return(boolean_substitute_sequence_(str, &check, pos));
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

static int copy_normal_remove_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, value));
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	Return(length_sequence_(pos, 1, &size));
	size = size - range->size + loc;
	Return(make_vector_size_sequence_(&pos, pos, size));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	for (i = 0; i < loc; i++) {
		getarray(table, i, &value);
		Return(push_sequence_write_(ret, value));
	}

	return 0;
}

static int copy_reverse_remove_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, value));
		if (! check)
			setarray(table, loc++, value);
	}
	localhold_end(hold);

	/* copy */
	Return(length_sequence_(pos, 1, &size));
	size = size - range->size + loc;
	Return(make_vector_size_sequence_(&pos, pos, size));
	Return(build_sequence_write_(ret, pos));
	Return(before_sequence_write_(ret, range));
	while (loc) {
		getarray(table, --loc, &value);
		Return(push_sequence_write_(ret, value));
	}

	return 0;
}

static int copy_remove_sequence_(struct count_struct *str, struct sequence_write *ret)
{
	addr table;
	struct sequence_range *range;

	range = &(str->range);
	vector_local(str->local, &table, range->size);
	if (str->fromp) {
		Return(copy_reverse_remove_sequence_(str, ret, table));
	}
	else {
		Return(copy_normal_remove_sequence_(str, ret, table));
	}
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int normal_remove_sequence_(struct count_struct *str, struct sequence_write *ret)
{
	LocalRoot local;
	LocalStack stack;

	if (listp(str->range.pos)) {
		if (str->delp)
			return list_delete_sequence_(str, ret);
		else
			return list_remove_sequence_(str, ret);
	}
	else {
		local = str->local;
		push_local(local, &stack);
		Return(copy_remove_sequence_(str, ret));
		rollback_local(local, stack);
		return 0;
	}
}

static int argument_remove_sequence_(Execute ptr,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp) {
		Return(list_reverse_type_remove_sequence_(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_remove_sequence_(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_remove_if_sequence_(Execute ptr, addr *ret,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp) {
		Return(list_reverse_type_remove_sequence_(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_remove_sequence_(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

int remove_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence_(ptr, ret, item, pos, rest, 0);
}

int remove_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence_(ptr, ret, call, Nil, pos, rest, 0);
}

int remove_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence_(ptr, ret, Nil, call, pos, rest, 0);
}


/*
 *  delete
 */
int delete_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest)
{
	return argument_remove_sequence_(ptr, ret, item, pos, rest, 1);
}

int delete_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence_(ptr, ret, call, Nil, pos, rest, 1);
}

int delete_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_remove_if_sequence_(ptr, ret, Nil, call, pos, rest, 1);
}

