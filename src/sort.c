#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "sequence.h"
#include "sort.h"

#define LISP_MERGE_SORT_LIMIT 16


/*
 *  unsafe
 */
_g void simplesort_cons_unsafe(addr *ret, addr cons, int (*call)(addr left, addr right))
{
	addr check, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &check);
		while (check != Nil) {
			GetCar(check, &right);
			if (! call(left, right)) {
				/* swap */
				SetCar(cons, right);
				SetCar(check, left);
				left = right;
			}
			GetCdr(check, &check);
		}
		GetCdr(cons, &cons);
	}
}

_g void simplesort_info_cons_unsafe(addr *ret, addr cons, addr info,
		int (*call)(addr info, addr left, addr right))
{
	addr check, left, right;

	*ret = cons;
	while (cons != Nil) {
		GetCons(cons, &left, &check);
		while (check != Nil) {
			GetCar(check, &right);
			if (! call(info, left, right)) {
				/* swap */
				SetCar(cons, right);
				SetCar(check, left);
				left = right;
			}
			GetCdr(check, &check);
		}
		GetCdr(cons, &cons);
	}
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

static int key_sort_sequence(struct sort_struct *str, addr *ret, addr value)
{
	if (str->key != Nil) {
		return callclang_funcall(str->ptr, ret, str->key, value, NULL);
	}
	else {
		*ret = value;
		return 0;
	}
}

static int call_sort_sequence(struct sort_struct *str, int *result, addr a2, addr b2)
{
	if (callclang_funcall(str->ptr, &a2, str->call, a2, b2, NULL)) return 1;
	*result = (a2 != Nil);
	return 0;
}

static void swap_list_sort_sequence(addr a, addr b)
{
	addr c1, c2;

	GetCar(a, &c1);
	GetCar(b, &c2);
	SetCar(a, c2);
	SetCar(b, c1);
}

static void swap_vector_sort_sequence(addr pos, size_t a, size_t b)
{
	struct array_value v1, v2;

	getelt_inplace_sequence(pos, a, &v1);
	getelt_inplace_sequence(pos, b, &v2);
	setelt_inplace_sequence(NULL, pos, a, &v2);
	setelt_inplace_sequence(NULL, pos, b, &v1);
}

static int simple_sort_list_sequence(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a1, a2, a3, a4, b1, b2, b3, b4, c;

	for (a1 = p1; a1 != p2; a1 = a4) {
		getcons(a1, &a2, &a4);
		if (key_sort_sequence(str, &a3, a2)) return 1;
		getcdr(a1, &b1);
		for (; b1 != p2; b1 = b4) {
			getcons(b1, &b2, &b4);
			if (key_sort_sequence(str, &b3, b2)) return 1;
			if (call_sort_sequence(str, &check, a3, b3)) return 1;
			if (! check) {
				swap_list_sort_sequence(a1, b1);
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

static int simple_sort_vector_sequence(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	size_t a1, b1;
	addr a2, a3, b2, b3, c;

	for (a1 = p1; a1 < p2; a1++) {
		getelt_sequence(NULL, pos, a1, &a2);
		if (key_sort_sequence(str, &a3, a2)) return 1;
		for (b1 = a1 + 1; b1 < p2; b1++) {
			getelt_sequence(NULL, pos, b1, &b2);
			if (key_sort_sequence(str, &b3, b2)) return 1;
			if (call_sort_sequence(str, &check, a3, b3)) return 1;
			if (! check) {
				swap_vector_sort_sequence(pos, a1, b1);
				c = a2; a2 = b2; b2 = c;
				c = a3; a3 = b3; b3 = c;
			}
		}
	}

	return 0;
}

_g int simple_sort_sequence(Execute ptr, addr pos, addr call, addr key)
{
	unsigned listp;
	struct sort_struct str;

	cleartype(str);
	listp = listp_sequence(pos);
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		if (simple_sort_list_sequence(&str, pos, Nil))
			return 1;
	}
	else {
		str.size = length_sequence(pos, 1);
		if (simple_sort_vector_sequence(&str, pos, 0, str.size))
			return 1;
	}

	return 0;
}


/*
 *  bubble-sort
 */
static int bubble_sort_list_sequence(struct sort_struct *str, addr p1, addr p2)
{
	int check, swap;
	addr a1, a2, b1, b2, b3;

	while (p1 != p2) {
		a1 = p1;
		getcons(a1, &a2, &b1);
		if (b1 == p2)
			break;
		if (key_sort_sequence(str, &a2, a2)) return 1;
		for (swap = 0; b1 != p2; b1 = b3) {
			getcons(b1, &b2, &b3);
			if (key_sort_sequence(str, &b2, b2)) return 1;
			if (call_sort_sequence(str, &check, b2, a2)) return 1;
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

static int bubble_sort_vector_sequence(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check, swap;
	size_t a1, b1;
	addr a2, b2;

	if (p2 - p1 <= 1)
		return 0;
	for (p2--; p1 < p2; p2--) {
		a1 = p1;
		getelt_sequence(NULL, pos, a1, &a2);
		if (key_sort_sequence(str, &a2, a2)) return 1;
		for (swap = 0; a1 < p2; a1++) {
			b1 = a1 + 1;
			getelt_sequence(NULL, pos, b1, &b2);
			if (key_sort_sequence(str, &b2, b2)) return 1;
			if (call_sort_sequence(str, &check, b2, a2)) return 1;
			if (check) {
				swap_vector_sort_sequence(pos, a1, b1);
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

_g int bubble_sort_sequence(Execute ptr, addr pos, addr call, addr key)
{
	unsigned listp;
	struct sort_struct str;

	cleartype(str);
	listp = listp_sequence(pos);
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		if (bubble_sort_list_sequence(&str, pos, Nil))
			return 1;
	}
	else {
		str.size = length_sequence(pos, 1);
		if (bubble_sort_vector_sequence(&str, pos, 0, str.size))
			return 1;
	}

	return 0;
}


/*
 *  quick-sort
 */
static int quick_sort_list_sequence(struct sort_struct *str, addr p1, addr p2)
{
	int check;
	addr a2, a3, a4, b1, b2, b3, b4, c1, c5;

	/* initialize */
	if (p1 == p2) return 0;
	getcons(p1, &a2, &a3);
	if (a3 == p2) return 0;
	if (key_sort_sequence(str, &a4, a2)) return 1;
	c1 = a3;
	c5 = Nil;

	/* loop */
	for (b1 = c1; b1 != p2; b1 = b3) {
		getcons(b1, &b2, &b3);
		if (key_sort_sequence(str, &b4, b2)) return 1;
		if (call_sort_sequence(str, &check, a4, b4)) return 1;
		if (! check) {
			swap_list_sort_sequence(c1, b1);
			c5 = c1;
			getcdr(c1, &c1);
		}
	}

	/* recursive call */
	if (c1 == a3) {
		if (quick_sort_list_sequence(str, a3, p2)) return 1;
	}
	else if (c1 == p2) {
		swap_list_sort_sequence(p1, c5);
		if (quick_sort_list_sequence(str, p1, c5)) return 1;
	}
	else {
		swap_list_sort_sequence(p1, c5);
		if (quick_sort_list_sequence(str, p1, c5)) return 1;
		if (quick_sort_list_sequence(str, c1, p2)) return 1;
	}

	return 0;
}

static int quick_sort_vector_sequence(struct sort_struct *str,
		addr pos, size_t p1, size_t p2)
{
	int check;
	addr a2, a4, b2, b4;
	size_t a3, b1, c1, c5;

	/* initialize */
	Check(p2 < p1, "index error");
	if ((p2 - p1) <= 1) return 0;
	getelt_sequence(NULL, pos, p1, &a2);
	if (key_sort_sequence(str, &a4, a2)) return 1;
	c1 = a3 = p1 + 1;
	c5 = 0;

	/* loop */
	for (b1 = c1; b1 < p2; b1++) {
		getelt_sequence(NULL, pos, b1, &b2);
		if (key_sort_sequence(str, &b4, b2)) return 1;
		if (call_sort_sequence(str, &check, a4, b4)) return 1;
		if (! check) {
			swap_vector_sort_sequence(pos, c1, b1);
			c5 = c1;
			c1++;
		}
	}

	/* recursive call */
	if (c1 == a3) {
		if (quick_sort_vector_sequence(str, pos, a3, p2)) return 1;
	}
	else if (c1 == p2) {
		swap_vector_sort_sequence(pos, p1, c5);
		if (quick_sort_vector_sequence(str, pos, p1, c5)) return 1;
	}
	else {
		swap_vector_sort_sequence(pos, p1, c5);
		if (quick_sort_vector_sequence(str, pos, p1, c5)) return 1;
		if (quick_sort_vector_sequence(str, pos, c1, p2)) return 1;
	}

	return 0;
}

_g int quick_sort_sequence(Execute ptr, addr pos, addr call, addr key)
{
	unsigned listp;
	struct sort_struct str;

	cleartype(str);
	listp = listp_sequence(pos);
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	if (listp) {
		str.size = 0;
		if (quick_sort_list_sequence(&str, pos, Nil))
			return 1;
	}
	else {
		str.size = length_sequence(pos, 1);
		if (quick_sort_vector_sequence(&str, pos, 0, str.size))
			return 1;
	}

	return 0;
}


/*
 *  merge-sort
 */
static void memory_copy_list_merge_sequence(addr vector, addr pos, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		getcons(pos, &value, &pos);
		setarray(vector, i, value);
	}
}

static int merge_sort_list_merge_sequence(struct sort_struct *str,
		addr a, size_t s1, addr b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	memory_copy_list_merge_sequence(mem, a, s1);
	ai = bi = 0;
	getcar(a, &a1);
	getcar(b, &b1);
	if (key_sort_sequence(str, &a2, a1)) return 1;
	if (key_sort_sequence(str, &b2, b1)) return 1;

	/* merge */
loop:
	if (call_sort_sequence(str, &check, b2, a2)) return 1;
	if (check) {
		setcar(a, b1);
		getcdr(a, &a);
		bi++;
		if (s2 <= bi) goto tail2;
		getcdr(b, &b);
		getcar(b, &b1);
		if (key_sort_sequence(str, &b2, b1)) return 1;
	}
	else {
		setcar(a, a1);
		getcdr(a, &a);
		ai++;
		if (s1 <= ai) goto tail1;
		getarray(mem, ai, &a1);
		if (key_sort_sequence(str, &a2, a1)) return 1;
	}
	goto loop;

tail1:
	setcar(a, b1);
	for (bi++; bi < s2; bi++) {
		getcdr(a, &a);
		getcdr(b, &b);
		getcar(b, &c);
		setcar(a, c);
	}
	return 0;

tail2:
	setcar(a, a1);
	for (ai++; ai < s1; ai++) {
		getcdr(a, &a);
		getarray(mem, ai, &c);
		setcar(a, c);
	}
	return 0;
}

static int merge_sort_list_sequence(struct sort_struct *str,
		addr a, addr c, size_t s0)
{
	int check;
	addr b;
	size_t s1, s2;
	LocalStack stack;

	/* bubble-sort */
	if (s0 < LISP_MERGE_SORT_LIMIT)
		return bubble_sort_list_sequence(str, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	getnthcdr(a, s1, &b);

	/* memory */
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	if (merge_sort_list_sequence(str, a, b, s1)) return 1;
	if (merge_sort_list_sequence(str, b, c, s2)) return 1;
	if (merge_sort_list_merge_sequence(str, a, s1, b, s2)) return 1;

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

static void memory_copy_vector_merge_sequence(addr vector,
		addr pos, size_t index, size_t size)
{
	addr value;
	size_t i;

	for (i = 0; i < size; i++) {
		getelt_sequence(NULL, pos, index++, &value);
		setarray(vector, i, value);
	}
}

static int merge_sort_vector_merge_sequence(struct sort_struct *str,
		addr pos, size_t a, size_t s1, size_t b, size_t s2)
{
	int check;
	addr a1, a2, b1, b2, mem, c;
	size_t ai, bi;

	/* variable */
	mem = str->mem;
	memory_copy_vector_merge_sequence(mem, pos, a, s1);
	ai = bi = 0;
	getarray(mem, 0, &a1);
	getelt_sequence(NULL, pos, b, &b1);
	if (key_sort_sequence(str, &a2, a1)) return 1;
	if (key_sort_sequence(str, &b2, b1)) return 1;

	/* merge */
loop:
	if (call_sort_sequence(str, &check, b2, a2)) return 1;
	if (check) {
		setelt_sequence(pos, a, b1);
		a++;
		bi++;
		if (s2 <= bi) goto tail2;
		b++;
		getelt_sequence(NULL, pos, b, &b1);
		if (key_sort_sequence(str, &b2, b1)) return 1;
	}
	else {
		setelt_sequence(pos, a, a1);
		a++;
		ai++;
		if (s1 <= ai) goto tail1;
		getarray(mem, ai, &a1);
		if (key_sort_sequence(str, &a2, a1)) return 1;
	}
	goto loop;

tail1:
	setelt_sequence(pos, a, b1);
	for (bi++; bi < s2; bi++) {
		a++;
		b++;
		getelt_sequence(NULL, pos, b, &c);
		setelt_sequence(pos, a, c);
	}
	return 0;

tail2:
	setelt_sequence(pos, a, a1);
	for (ai++; ai < s1; ai++) {
		a++;
		getarray(mem, ai, &c);
		setelt_sequence(pos, a, c);
	}
	return 0;
}

static int merge_sort_vector_sequence(struct sort_struct *str,
		addr pos, size_t a, size_t c)
{
	int check;
	size_t s0, s1, s2, b;
	LocalStack stack;

	/* bubble-sort */
	Check(c < a, "index error");
	s0 = c - a;
	if (s0 < LISP_MERGE_SORT_LIMIT)
		return bubble_sort_vector_sequence(str, pos, a, c);

	/* index */
	s1 = s0 / 2;
	s2 = s0 - s1;
	b = a + s1;

	/* memory */
	check = (str->mem == Nil);
	if (check) {
		push_local(str->local, &stack);
		vector_local(str->local, &(str->mem), s1);
	}

	/* merge-sort */
	if (merge_sort_vector_sequence(str, pos, a, b)) return 1;
	if (merge_sort_vector_sequence(str, pos, b, c)) return 1;
	if (merge_sort_vector_merge_sequence(str, pos, a, s1, b, s2)) return 1;

	/* rollback */
	if (check) {
		rollback_local(str->local, stack);
	}

	return 0;
}

_g int merge_sort_sequence(Execute ptr, addr pos, addr call, addr key)
{
	unsigned listp;
	struct sort_struct str;

	cleartype(str);
	listp = listp_sequence(pos);
	str.listp = listp;
	str.ptr = ptr;
	str.local = ptr->local;
	str.pos = pos;
	str.call = call;
	str.key = key;
	str.mem = Nil;
	str.size = length_sequence(pos, 1);
	if (listp)
		return merge_sort_list_sequence(&str, pos, Nil, str.size);
	else
		return merge_sort_vector_sequence(&str, pos, 0, str.size);
}

