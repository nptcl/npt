#include "condition.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_count.h"
#include "sequence_duplicates.h"
#include "sequence_range.h"
#include "sequence_substitute.h"
#include "sequence_write.h"
#include "typedef.h"

/*
 *  remove-duplicates / delete-duplicates
 */
static int list_normal_duplicates_sequence_(struct count_struct *str,
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
			Return(funcall1_control_(str->ptr, &a, str->key, a, NULL));
		}
		str->item = a;
		for (k = i; k; ) {
			getarray(table, --k, &b);
			if (b == Unbound)
				continue;
			Return(boolean_substitute_sequence_(str, &check, b));
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

static int list_normal_delete_duplicates_(
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
	Return(list_normal_duplicates_sequence_(str, range, table, size, NULL));
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

static int list_normal_remove_duplicates_(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	Return(list_normal_duplicates_sequence_(str, range, table, size, NULL));

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

static int list_reverse_duplicates_sequence_(struct count_struct *str,
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
			Return(funcall1_control_(str->ptr, &a, str->key, a, NULL));
		}
		str->item = a;
		for (k = i + 1; k < size; k++) {
			getarray(table, k, &b);
			if (b == Unbound)
				continue;
			Return(boolean_substitute_sequence_(str, &check, b));
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

static int list_reverse_delete_duplicates_(
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
	Return(list_reverse_duplicates_sequence_(str, range, table, size, NULL));
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

static int list_reverse_remove_duplicates_(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, i;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	Return(list_reverse_duplicates_sequence_(str, range, table, size, NULL));

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

static int list_remove_duplicates_(struct count_struct *str, struct sequence_write *ret)
{
	if (str->fromp) {
		if (str->delp)
			return list_reverse_delete_duplicates_(str, ret);
		else
			return list_reverse_remove_duplicates_(str, ret);
	}
	else {
		if (str->delp)
			return list_normal_delete_duplicates_(str, ret);
		else
			return list_normal_remove_duplicates_(str, ret);
	}
}

static int vector_normal_remove_duplicates_(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	Return(list_normal_duplicates_sequence_(str, range, table, size, &loc));

	/* copy */
	pos = range->pos;
	Return(length_sequence_(pos, 1, &k));
	k = k - loc;
	Return(make_vector_size_sequence_(&pos, pos, k));
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

static int vector_reverse_remove_duplicates_(
		struct count_struct *str, struct sequence_write *ret)
{
	addr table, pos;
	struct sequence_range *range;
	size_t size, loc, k;

	/* copy */
	range = &(str->range);
	size = range->size;
	vector_local(str->local, &table, size);
	Return(list_reverse_duplicates_sequence_(str, range, table, size, &loc));

	/* copy */
	pos = range->pos;
	Return(length_sequence_(pos, 1, &k));
	k = k - loc;
	Return(make_vector_size_sequence_(&pos, pos, k));
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

static int vector_remove_duplicates_sequence_(
		struct count_struct *str, struct sequence_write *ret)
{
	if (str->fromp)
		return vector_reverse_remove_duplicates_(str, ret);
	else
		return vector_normal_remove_duplicates_(str, ret);
}

static int argument_remove_duplicates_(Execute ptr,
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
		Return(list_remove_duplicates_(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(vector_remove_duplicates_sequence_(&str, &write));
	}
	rollback_local(local, stack);
	*ret = result_sequence_write(&write);

	return 0;
}

int remove_duplicates_common_(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates_(ptr, ret, pos, rest, 0);
}

int delete_duplicates_common_(Execute ptr, addr *ret, addr pos, addr rest)
{
	return argument_remove_duplicates_(ptr, ret, pos, rest, 1);
}

