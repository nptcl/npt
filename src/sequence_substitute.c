#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "hold.h"
#include "sequence.h"
#include "sequence_common.h"
#include "sequence_count.h"
#include "sequence_substitute.h"
#include "sequence_write.h"
#include "typedef.h"

/*
 *  substitute
 */
int boolean_substitute_sequence_(struct count_struct *str, int *ret, addr pos)
{
	int check;

	if (str->count == Nil)
		return boolean_count_sequence_(str, ret, pos);
	if (str->limit == 0)
		return Result(ret, 0);
	Return(boolean_count_sequence_(str, &check, pos));
	if (check)
		str->limit--;

	return Result(ret, check);
}

static int reverse_list_substitute_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, pos));
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

static int reverse_substitute_sequence_(
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
	Return(reverse_list_substitute_sequence_(str, ret));
	rollback_local(local, stack);
	/* after end */
	while (pos3 != Nil) {
		Return_getcons(pos3, &value, &pos3);
		Return(push_sequence_write_(ret, value));
	}

	return 0;
}

static int list_substitute_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, pos));
		Return(push_sequence_write_(ret, check? one: pos));
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int copy_substitute_sequence_(
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
		Return(boolean_substitute_sequence_(str, &check, value));
		Return(push_sequence_write_(ret, check? one: value));
		localhold_set(hold, 1, ret->pos);
	}
	localhold_end(hold);

	/* after */
	Return(after_sequence_write_(ret, range));

	return 0;
}

static int vector_substitute_sequence_(addr *ret, addr pos)
{
	size_t size;
	Return(length_sequence_(pos, 1, &size));
	return make_vector_size_sequence_(ret, pos, size);
}

static int normal_substitute_sequence_(
		struct count_struct *str, struct sequence_write *ret)
{
	addr pos;
	struct sequence_range *range;
	LocalHold hold;

	range = &(str->range);
	pos = range->pos;
	if (listp(pos)) {
		Return(list_substitute_sequence_(str, ret));
	}
	else {
		Return(vector_substitute_sequence_(&pos, pos));
		hold = LocalHold_local_push(str->ptr, pos);
		Return(copy_substitute_sequence_(str, ret, pos));
		localhold_end(hold);
	}

	return 0;
}

int substitute_common_(Execute ptr,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp) {
		Return(reverse_substitute_sequence_(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_substitute_sequence_(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

static int argument_substitute_sequence_(Execute ptr, addr *ret,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp) {
		Return(reverse_substitute_sequence_(&str, &write));
	}
	else {
		Return(build_sequence_range_(range, pos, start, end));
		Return(normal_substitute_sequence_(&str, &write));
	}
	*ret = result_sequence_write(&write);

	return 0;
}

int substitute_if_common_(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return argument_substitute_sequence_(ptr, ret, item, call, Nil, pos, rest);
}

int substitute_if_not_common_(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest)
{
	return argument_substitute_sequence_(ptr, ret, item, Nil, call, pos, rest);
}


/*
 *  nsubstitute
 */
static int reverse_list_nsubstitute_sequence_(struct count_struct *str)
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
		Return(boolean_substitute_sequence_(str, &check, pos));
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

static int reverse_nsubstitute_sequence_(struct count_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	struct sequence_range *range;

	local = str->local;
	range = &(str->range);
	push_local(local, &stack);
	Return(build_sequence_range_vector_(local, range, str->pos, str->start, str->end));
	Return(reverse_list_nsubstitute_sequence_(str));
	rollback_local(local, stack);

	return 0;
}

static int normal_nsubstitute_sequence_(struct count_struct *str)
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
		Return(boolean_substitute_sequence_(str, &check, value));
		Return((*set_)(range, check? one: value));
		Return((*next_)(range, &check));
	}
	localhold_end(hold);

	return 0;
}

int nsubstitute_common_(Execute ptr,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp)
		return reverse_nsubstitute_sequence_(&str);
	Return(build_sequence_range_(range, pos, start, end));
	return normal_nsubstitute_sequence_(&str);
}

static int argument_nsubstitute_sequence_(Execute ptr,
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
	Return(setcount_sequence_(&str, count));

	if (listp && fromp)
		return reverse_nsubstitute_sequence_(&str);
	Return(build_sequence_range_(range, pos, start, end));
	return normal_nsubstitute_sequence_(&str);
}

int nsubstitute_if_common_(Execute ptr,
		addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence_(ptr, item, call, Nil, pos, rest);
}

int nsubstitute_if_not_common_(Execute ptr, addr item, addr call, addr pos, addr rest)
{
	return argument_nsubstitute_sequence_(ptr, item, Nil, call, pos, rest);
}

