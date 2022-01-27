#include "condition.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "equal.h"
#include "integer.h"
#include "hold.h"
#include "sequence.h"
#include "sequence_count.h"
#include "typedef.h"

/*
 *  count
 */
int boolean_count_sequence_(struct count_struct *str, int *result, addr value)
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
		Return(funcall1_control_(ptr, &value, str->key, value, NULL));
	}
	if (str->single) {
		Return(funcall1_control_(ptr, &value, test, value, NULL));
	}
	else if (test == Nil) {
		value = eql_function(str->item, value)? T: Nil;
	}
	else {
		Return(funcall1_control_(ptr, &value, test, str->item, value, NULL));
	}
	if (str->notp)
		*result = (value == Nil);
	else
		*result = (value != Nil);

	return 0;
}

static int value_count_sequence_(struct count_struct *str, addr *ret)
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
		Return(boolean_count_sequence_(str, &check, value));
		if (check)
			count++;
	}
	localhold_end(hold);
	make_index_integer_heap(ret, count);

	return 0;
}

static int reverse_count_sequence_(struct count_struct *str, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	range = &(str->range);
	local = str->local;
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(value_count_sequence_(str, ret));
	rollback_local(local, stack);

	return 0;
}

int count_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest)
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
		return reverse_count_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	return value_count_sequence_(&str, ret);
}

static int argument_count_sequence_(Execute ptr, addr *ret,
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
		return reverse_count_sequence_(&str, ret);
	Return(build_sequence_range_(range, pos, start, end));
	return value_count_sequence_(&str, ret);
}

int count_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence_(ptr, ret, call, Nil, pos, rest);
}

int count_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest)
{
	return argument_count_sequence_(ptr, ret, Nil, call, pos, rest);
}

