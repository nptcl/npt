#include "condition.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "equal.h"
#include "local.h"
#include "hold.h"
#include "integer.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_count.h"
#include "sequence_find.h"
#include "sequence_range.h"
#include "typedef.h"

/*
 *  find
 */
static int value_find_sequence_(struct count_struct *str, addr *ret)
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
		Return(boolean_count_sequence_(str, &check, value));
		if (check) {
			localhold_end(hold);
			return Result(ret, value);
		}
	}
	localhold_end(hold);
	return Result(ret, Nil);
}

static int reverse_find_sequence_(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(value_find_sequence_(str, ret));
	rollback_local(local, stack);

	return 0;
}

int find_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest)
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
		return reverse_find_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	return value_find_sequence_(&str, ret);
}

static int argument_find_sequence_(Execute ptr, addr *ret,
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
		return reverse_find_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	return value_find_sequence_(&str, ret);
}

int find_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence_(ptr, ret, call, Nil, pos, rest);
}

int find_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_find_sequence_(ptr, ret, Nil, call, pos, rest);
}


/*
 *  position
 */
static int value_position_sequence_(struct count_struct *str, size_t *ret, int *nilp)
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
		Return(boolean_count_sequence_(str, &check, value));
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

static int reverse_position_sequence_(struct count_struct *str, addr *ret)
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
	Return(value_position_sequence_(str, &size, &check));
	rollback_local(local, stack);

	/* result */
	if (check)
		return Result(ret, Nil);
	size = str->start_value + range->end - size - 1;
	make_index_integer_heap(ret, size);

	return 0;
}

int position_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest)
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
		return reverse_position_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	Return(value_position_sequence_(&str, &size, &check));

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

static int argument_position_sequence_(Execute ptr, addr *ret,
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
		return reverse_position_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	Return(value_position_sequence_(&str, &size, &check));

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

int position_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_position_sequence_(ptr, ret, call, Nil, pos, rest);
}

int position_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_position_sequence_(ptr, ret, Nil, call, pos, rest);
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

static int key_search_sequence_(struct search_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return funcall1_control_(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
}

static int call_search_sequence_(struct search_struct *str,
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
		Return(funcall1_control_(ptr, &value, test, a, b, NULL));
	}
	if (str->notp)
		*result = (value == Nil);
	else
		*result = (value != Nil);

	return 0;
}

static int reverse_pattern_search_sequence_(struct search_struct *str,
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

		Return(key_search_sequence_(str, &a, a));
		localhold_set(hold, 0, a);

		Return(getnext_sequence_range_(range2, &b, &check));
		if (check)
			return Result(result, 0);
		localhold_set(hold, 1, b);

		Return(key_search_sequence_(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence_(str, &check, a, b));
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

static int reverse_size_search_sequence_(
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
		Return(reverse_pattern_search_sequence_(str, &check, range1, range2, i));
		if (check) {
			*ret = i;
			return Result(nilp, 0);
		}
	}
	*ret = 0;
	return Result(nilp, 1);
}

static int reverse_search_sequence_(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	Return(reverse_size_search_sequence_(str, &size, &check));
	if (check)
		return Result(ret, Nil);
	size += str->start_value;
	make_index_integer_heap(ret, size);

	return 0;
}

static int normal_pattern_search_sequence_(struct search_struct *str,
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

		Return(key_search_sequence_(str, &a, a));
		localhold_set(hold, 0, a);

		Return(getnext_sequence_range_(range2, &b, &check));
		if (check)
			return Result(result, 0);
		localhold_set(hold, 1, b);

		Return(key_search_sequence_(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence_(str, &check, a, b));
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

static int normalsize_search_sequence_(
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
		Return(normal_pattern_search_sequence_(str, &check, range1, range2));
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

static int normal_search_sequence_(struct search_struct *str, addr *ret)
{
	int check;
	size_t size;

	Return(normalsize_search_sequence_(str, &size, &check));
	if (check)
		return Result(ret, Nil);
	size += str->range2->start;
	make_index_integer_heap(ret, size);

	return 0;
}

static int execute_search_sequence_(Execute ptr, addr *ret,
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
		Return(reverse_search_sequence_(&str, ret));
	}
	else {
		Return(make_sequence_range_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_(local, pos2, start2, end2, &(str.range2)));
		Return(normal_search_sequence_(&str, ret));
	}

	return 0;
}

int search_common_(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_search_sequence_(ptr, ret, pos1, pos2, rest));
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

static int reverse_mismatch_sequence_(struct search_struct *str, addr *ret)
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

		Return(key_search_sequence_(str, &a, a));
		localhold_set(hold, 0, a);
		Return(key_search_sequence_(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence_(str, &check, a, b));
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

static int normal_mismatch_sequence_(struct search_struct *str, addr *ret)
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

		Return(key_search_sequence_(str, &a, a));
		localhold_set(hold, 0, a);
		Return(key_search_sequence_(str, &b, b));
		localhold_set(hold, 1, b);

		Return(call_search_sequence_(str, &check, a, b));
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

static int execute_mismatch_sequence_(Execute ptr, addr *ret,
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
		Return(reverse_mismatch_sequence_(&str, ret));
	}
	else {
		Return(make_sequence_range_(local, pos1, start1, end1, &(str.range1)));
		Return(make_sequence_range_(local, pos2, start2, end2, &(str.range2)));
		Return(normal_mismatch_sequence_(&str, ret));
	}

	return 0;
}

int mismatch_common_(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(execute_mismatch_sequence_(ptr, ret, pos1, pos2, rest));
	rollback_local(local, stack);

	return 0;
}

