#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "sequence.h"
#include "sort.h"

#define LISP_MERGE_SORT_LIMIT 16


/*
 *  unsafe
 */
int simplesort_cons_unsafe_(addr *ret,
		addr cons, int (*call_)(addr left, addr right, int *ret))
{
	int check;
	addr value, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &value);
		while (value != Nil) {
			GetCar(value, &right);
			Return((*call_)(left, right, &check));
			if (! check) {
				/* swap */
				SetCar(cons, right);
				SetCar(value, left);
				left = right;
			}
			GetCdr(value, &value);
		}
		GetCdr(cons, &cons);
	}

	return 0;
}

int simplesort_info_cons_unsafe_(addr *ret, addr cons, addr info,
		int (*call_)(addr info, addr left, addr right, int *ret))
{
	int check;
	addr value, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &value);
		while (value != Nil) {
			GetCar(value, &right);
			Return((*call_)(info, left, right, &check));
			if (! check) {
				/* swap */
				SetCar(cons, right);
				SetCar(value, left);
				left = right;
			}
			GetCdr(value, &value);
		}
		GetCdr(cons, &cons);
	}

	return 0;
}


/*
 *  simple-sort
 */
struct sort_struct {
	unsigned listp : 1;
	Execute ptr;
	LocalRoot local;
	addr pos, call, key, mem;
	size_t size;
};

static int key_sort_sequence_(struct sort_struct *str, addr *ret, addr value)
{
	if (str->key != Nil)
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	else
		return Result(ret, value);
}

static int call_sort_sequence_(struct sort_struct *str, int *ret, addr a2, addr b2)
{
	Return(callclang_funcall(str->ptr, &a2, str->call, a2, b2, NULL));
	return Result(ret, (a2 != Nil));
}

static void swap_list_sort_sequence(addr a, addr b)
{
	addr c1, c2;

	GetCar(a, &c1);
	GetCar(b, &c2);
	SetCar(a, c2);
	SetCar(b, c1);
}

static int swap_vector_sort_sequence_(addr pos, size_t a, size_t b)
{
	struct array_value v1, v2;

	Return(getelt_inplace_sequence_(pos, a, &v1));
	Return(getelt_inplace_sequence_(pos, b, &v2));
	Return(setelt_inplace_sequence_(NULL, pos, a, &v2));
	Return(setelt_inplace_sequence_(NULL, pos, b, &v1));

	return 0;
}

static int simple_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a1, a2, a3, a4, b1, b2, b3, b4, c;

	for (a1 = p1; a1 != p2; a1 = a4) {
		Return_getcons(a1, &a2, &a4);
		Return(key_sort_sequence_(str, &a3, a2));
		Return_getcdr(a1, &b1);
		for (; b1 != p2; b1 = b4) {
			Return_getcons(b1, &b2, &b4);
			Return(key_sort_sequence_(str, &b3, b2));
			Return(call_sort_sequence_(str, &check, a3, b3));
			if (! check) {
				swap_list_sort_sequence(a1, b1);
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

static int simple_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	size_t a1, b1;
	addr a2, a3, b2, b3, c;

	for (a1 = p1; a1 < p2; a1++) {
		Return(getelt_sequence_(NULL, pos, a1, &a2));
		Return(key_sort_sequence_(str, &a3, a2));
		for (b1 = a1 + 1; b1 < p2; b1++) {
			Return(getelt_sequence_(NULL, pos, b1, &b2));
			Return(key_sort_sequence_(str, &b3, b2));
			Return(call_sort_sequence_(str, &check, a3, b3));
			if (! check) {
				Return(swap_vector_sort_sequence_(pos, a1, b1));
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

int simple_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(simple_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(simple_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  bubble-sort
 */
static int bubble_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check, swap;
	addr a1, a2, b1, b2, b3;

	while (p1 != p2) {
		a1 = p1;
		Return_getcons(a1, &a2, &b1);
		if (b1 == p2)
			break;
		Return(key_sort_sequence_(str, &a2, a2));
		for (swap = 0; b1 != p2; b1 = b3) {
			Return_getcons(b1, &b2, &b3);
			Return(key_sort_sequence_(str, &b2, b2));
			Return(call_sort_sequence_(str, &check, b2, a2));
			if (check) {
				swap_list_sort_sequence(a1, b1);
				swap = 1;
			}
			else {
				a2 = b2;
			}
			a1 = b1;
		}
		if (swap == 0)
			break;
		p2 = a1;
	}

	return 0;
}

static int bubble_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check, swap;
	size_t a1, b1;
	addr a2, b2;

	if (p2 - p1 <= 1)
		return 0;
	for (p2--; p1 < p2; p2--) {
		a1 = p1;
		Return(getelt_sequence_(NULL, pos, a1, &a2));
		Return(key_sort_sequence_(str, &a2, a2));
		for (swap = 0; a1 < p2; a1++) {
			b1 = a1 + 1;
			Return(getelt_sequence_(NULL, pos, b1, &b2));
			Return(key_sort_sequence_(str, &b2, b2));
			Return(call_sort_sequence_(str, &check, b2, a2));
			if (check) {
				Return(swap_vector_sort_sequence_(pos, a1, b1));
				swap = 1;
			}
			else {
				a2 = b2;
			}
		}
		if (swap == 0)
			break;
	}

	return 0;
}

int bubble_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(bubble_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(bubble_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  quick-sort
 */
static int quick_sort_list_sequence_(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a2, a3, a4, b1, b2, b3, b4, c1, c5;

	/* initialize */
	if (p1 == p2)
		return 0;
	Return_getcons(p1, &a2, &a3);
	if (a3 == p2)
		return 0;
	Return(key_sort_sequence_(str, &a4, a2));
	c1 = a3;
	c5 = Nil;

	/* loop */
	for (b1 = c1; b1 != p2; b1 = b3) {
		Return_getcons(b1, &b2, &b3);
		Return(key_sort_sequence_(str, &b4, b2));
		Return(call_sort_sequence_(str, &check, a4, b4));
		if (! check) {
			swap_list_sort_sequence(c1, b1);
			c5 = c1;
			Return_getcdr(c1, &c1);
		}
	}

	/* recursive call */
	if (c1 == a3) {
		Return(quick_sort_list_sequence_(str, a3, p2));
	}
	else if (c1 == p2) {
		swap_list_sort_sequence(p1, c5);
		Return(quick_sort_list_sequence_(str, p1, c5));
	}
	else {
		swap_list_sort_sequence(p1, c5);
		Return(quick_sort_list_sequence_(str, p1, c5));
		Return(quick_sort_list_sequence_(str, c1, p2));
	}

	return 0;
}

static int quick_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	addr a2, a4, b2, b4;
	size_t a3, b1, c1, c5;

	/* initialize */
	Check(p2 < p1, "index error");
	if ((p2 - p1) <= 1)
		return 0;
	Return(getelt_sequence_(NULL, pos, p1, &a2));
	Return(key_sort_sequence_(str, &a4, a2));
	c1 = a3 = p1 + 1;
	c5 = 0;

	/* loop */
	for (b1 = c1; b1 < p2; b1++) {
		Return(getelt_sequence_(NULL, pos, b1, &b2));
		Return(key_sort_sequence_(str, &b4, b2));
		Return(call_sort_sequence_(str, &check, a4, b4));
		if (! check) {
			Return(swap_vector_sort_sequence_(pos, c1, b1));
			c5 = c1;
			c1++;
		}
	}

	/* recursive call */
	if (c1 == a3) {
		Return(quick_sort_vector_sequence_(str, pos, a3, p2));
	}
	else if (c1 == p2) {
		Return(swap_vector_sort_sequence_(pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, p1, c5));
	}
	else {
		Return(swap_vector_sort_sequence_(pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, p1, c5));
		Return(quick_sort_vector_sequence_(str, pos, c1, p2));
	}

	return 0;
}

int quick_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		Return(quick_sort_list_sequence_(&str, pos, Nil));
	}
	else {
		Return(length_sequence_(pos, 1, &(str.size)));
		Return(quick_sort_vector_sequence_(&str, pos, 0, str.size));
	}

	return 0;
}


/*
 *  merge-sort
 */
static int memory_copy_list_merge_sequence_(addr vector, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return_getcons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return 0;
}

static int merge_sort_list_merge_sequence_(struct sort_struct *str,
		addr a, size_t s1, addr b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	Return(memory_copy_list_merge_sequence_(mem, a, s1));
	ai = bi = 0;
	Return_getcar(a, &a1);
	Return_getcar(b, &b1);
	Return(key_sort_sequence_(str, &a2, a1));
	Return(key_sort_sequence_(str, &b2, b1));

	/* merge */
loop:
	Return(call_sort_sequence_(str, &check, b2, a2));
	if (check) {
		Return_setcar(a, b1);
		Return_getcdr(a, &a);
		bi++;
		if (s2 <= bi)
			goto tail2;
		Return_getcdr(b, &b);
		Return_getcar(b, &b1);
		Return(key_sort_sequence_(str, &b2, b1));
	}
	else {
		Return_setcar(a, a1);
		Return_getcdr(a, &a);
		ai++;
		if (s1 <= ai)
			goto tail1;
		getarray(mem, ai, &a1);
		Return(key_sort_sequence_(str, &a2, a1));
	}
	goto loop;

tail1:
	Return_setcar(a, b1);
	for (bi++; bi < s2; bi++) {
		Return_getcdr(a, &a);
		Return_getcdr(b, &b);
		Return_getcar(b, &c);
		Return_setcar(a, c);
	}
	return 0;

tail2:
	Return_setcar(a, a1);
	for (ai++; ai < s1; ai++) {
		Return_getcdr(a, &a);
		getarray(mem, ai, &c);
		Return_setcar(a, c);
	}
	return 0;
}

static int merge_sort_list_sequence_(struct sort_struct *str,
		addr a, addr c, size_t s0)
{
	int check;
	addr b;
	size_t s1, s2;
	LocalStack stack;

	/* bubble-sort */
	if (s0 < LISP_MERGE_SORT_LIMIT)
		return bubble_sort_list_sequence_(str, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	Return(getnthcdr_(a, s1, &b));

	/* memory */
	stack = NULL;
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	Return(merge_sort_list_sequence_(str, a, b, s1));
	Return(merge_sort_list_sequence_(str, b, c, s2));
	Return(merge_sort_list_merge_sequence_(str, a, s1, b, s2));

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

static int memory_copy_vector_merge_sequence_(addr vector,
		addr pos, size_t index, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		Return(getelt_sequence_(NULL, pos, index++, &value));
		setarray(vector, i, value);
	}

	return 0;
}

static int merge_sort_vector_merge_sequence_(struct sort_struct *str,
		addr pos, size_t a, size_t s1, size_t b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	Return(memory_copy_vector_merge_sequence_(mem, pos, a, s1));
	ai = bi = 0;
	getarray(mem, 0, &a1);
	Return(getelt_sequence_(NULL, pos, b, &b1));
	Return(key_sort_sequence_(str, &a2, a1));
	Return(key_sort_sequence_(str, &b2, b1));

	/* merge */
loop:
	Return(call_sort_sequence_(str, &check, b2, a2));
	if (check) {
		Return(setelt_sequence_(pos, a, b1));
		a++;
		bi++;
		if (s2 <= bi)
			goto tail2;
		b++;
		Return(getelt_sequence_(NULL, pos, b, &b1));
		Return(key_sort_sequence_(str, &b2, b1));
	}
	else {
		Return(setelt_sequence_(pos, a, a1));
		a++;
		ai++;
		if (s1 <= ai)
			goto tail1;
		getarray(mem, ai, &a1);
		Return(key_sort_sequence_(str, &a2, a1));
	}
	goto loop;

tail1:
	Return(setelt_sequence_(pos, a, b1));
	for (bi++; bi < s2; bi++) {
		a++;
		b++;
		Return(getelt_sequence_(NULL, pos, b, &c));
		Return(setelt_sequence_(pos, a, c));
	}
	return 0;

tail2:
	Return(setelt_sequence_(pos, a, a1));
	for (ai++; ai < s1; ai++) {
		a++;
		getarray(mem, ai, &c);
		Return(setelt_sequence_(pos, a, c));
	}
	return 0;
}

static int merge_sort_vector_sequence_(struct sort_struct *str,
		addr pos, size_t a, size_t c)
{
	int check;
	size_t s0, s1, s2, b;
	LocalStack stack;

	/* bubble-sort */
	Check(c < a, "index error");
	s0 = c - a;
	if (s0 < LISP_MERGE_SORT_LIMIT)
		return bubble_sort_vector_sequence_(str, pos, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	b = a + s1;

	/* memory */
	stack = NULL;
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	Return(merge_sort_vector_sequence_(str, pos, a, b));
	Return(merge_sort_vector_sequence_(str, pos, b, c));
	Return(merge_sort_vector_merge_sequence_(str, pos, a, s1, b, s2));

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

int merge_sort_sequence_(Execute ptr, addr pos, addr call, addr key)
{
	int listp;
	struct sort_struct str;

	cleartype(str);
	Return(listp_sequence_(pos, &listp));
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	str.mem = Nil;
	Return(length_sequence_(pos, 1, &(str.size)));
	if (listp)
		return merge_sort_list_sequence_(&str, pos, Nil, str.size);
	else
		return merge_sort_vector_sequence_(&str, pos, 0, str.size);
}

