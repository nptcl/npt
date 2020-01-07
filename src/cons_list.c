#include "bignum.h"
#include "build.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "equal.h"
#include "function.h"
#include "integer.h"
#include "object.h"
#include "sequence.h"

/*
 *  nth
 */
_g void getnth(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		getcdr(cons, &cons);
	}
	getcar(cons, ret);
}

_g void getnth_large(addr cons, addr index, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_counter_alloc(local, &index, index);
	while (! zerop_bignum(index)) {
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		getcdr(cons, &cons);
		decf_bignum(index, 1);
	}
	getcar(cons, ret);
finish:
	rollback_local(local, stack);
}

_g void getnth_unbound_unsafe(addr cons, size_t index, addr *ret)
{
	for (;;) {
		if (cons == Nil) {
			*ret = Unbound;
			return;
		}
		if (index-- == 0)
			break;
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

_g void getnth_unsafe(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

_g void getnthcdr(addr cons, size_t index, addr *ret)
{
	for (; index; index--) {
		if (cons == Nil)
			break;
		getcdr(cons, &cons);
	}
	*ret = cons;
}

_g void getnthcdr_large(addr cons, addr index, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_counter_alloc(local, &index, index);
	while (! zerop_bignum(index)) {
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		getcdr(cons, &cons);
		decf_bignum(index, 1);
	}
	*ret = cons;
finish:
	rollback_local(local, stack);
}

_g void getnthcdr_unsafe(addr cons, size_t index, addr *ret)
{
	while (index--) {
		if (cons == Nil) {
			*ret = Nil;
			return;
		}
		GetCdr(cons, &cons);
	}
	*ret = cons;
}

_g void setnth(addr cons, size_t index, addr value)
{
	addr cdr;

	getnthcdr(cons, index, &cdr);
	if (! consp(cdr))
		fmte("Cannot (setf nth) ~S.", cons, NULL);
	SetCar(cdr, value);
}

_g void setnth_unsafe(addr cons, size_t index, addr value)
{
	addr cdr;

	getnthcdr_unsafe(cons, index, &cdr);
	Check(! consp(cdr), "type error");
	SetCar(cdr, value);
}


/*
 *  length
 */
_g size_t length_list_safe(addr right)
{
	size_t size;

	for (size = 0; right != Nil; size++) {
		if (GetType(right) != LISPTYPE_CONS)
			fmte("cdr position must be a list type.", NULL);
		GetCdr(right, &right);
	}

	return size;
}

_g size_t length_list_safe_dotted(addr right)
{
	size_t size;

	for (size = 0; GetType(right) == LISPTYPE_CONS; size++) {
		GetCdr(right, &right);
	}

	return size;
}

_g size_t length_list_unsafe(addr cons)
{
	size_t size;

	for (size = 0; cons != Nil; size++) {
		GetCdr(cons, &cons);
	}

	return size;
}

_g int length_list_p(addr list, size_t *ret)
{
	size_t i;

	for (i = 0; list != Nil; i++) {
		if (GetType(list) != LISPTYPE_CONS)
			return 1;
		GetCdr(list, &list);
	}
	*ret = i;

	return 0;
}


/*
 *  list
 */
_g void nconc2_safe(addr left, addr right, addr *ret)
{
	addr check;

	if (left == Nil) {
		*ret = right;
		return;
	}
	*ret = left;
	if (right == Nil) {
		return;
	}
	for (;;) {
		getcdr(left, &check);
		if (! consp(check)) {
			SetCdr(left, right);
			break;
		}
		left = check;
	}
}

_g void nconc2_unsafe(addr left, addr right, addr *ret)
{
	addr check;

	if (left == Nil) {
		*ret = right;
		return;
	}
	*ret = left;
	if (right == Nil) {
		return;
	}
	for (;;) {
		GetCdr(left, &check);
		if (! consp(check)) {
			SetCdr(left, right);
			break;
		}
		left = check;
	}
}

_g void append2_safe(addr left, addr right, addr *ret)
{
	addr root, pos;

	if (left == Nil) {
		*ret = right;
		return;
	}
	if (right == Nil) {
		*ret = left;
		return;
	}
	for (root = Nil; left != Nil; ) {
		getcons(left, &pos, &left);
		cons_heap(&root, pos, root);
	}
	nreconc_unsafe(ret, root, right);
}

_g void append2_alloc_unsafe(LocalRoot local, addr list1, addr list2, addr *ret)
{
	addr stack, left;

	if (list1 == Nil) {
		*ret = list2;
		return;
	}
	if (list2 == Nil) {
		*ret = list1;
		return;
	}

	stack = Nil;
	do {
		GetCons(list1, &left, &list1);
		cons_alloc(local, &stack, left, stack);
	} while (list1 != Nil);

	for (;;) {
		GetCdr(stack, &list1);
		SetCdr(stack, list2);
		if (list1 == Nil) break;
		list2 = stack;
		stack = list1;
	}
	*ret = stack;
}

_g void append2_heap_unsafe(addr list1, addr list2, addr *ret)
{
	append2_alloc_unsafe(NULL, list1, list2, ret);
}

_g void append2_local_unsafe(LocalRoot local, addr list1, addr list2, addr *ret)
{
	Check(local == NULL, "local error");
	append2_alloc_unsafe(local, list1, list2, ret);
}

_g void butandlast_safe(addr *but, addr *last, addr list, size_t index)
{
	addr root, pos;
	size_t size;

	size = length_list_safe_dotted(list);
	if (size <= index) {
		*but = Nil;
		*last = list;
		return;
	}
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(but, root);
	*last = list;
}

_g void setlastcdr_safe(addr list, addr cdr)
{
	addr check;

	for (;;) {
		getcdr(list, &check);
		if (! consp(check)) {
			SetCdr(list, cdr);
			return;
		}
		list = check;
	}
}


/*
 *  find
 */
_g int find_list_eq_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (check == key) return 1;
	}

	return 0;
}

_g int find_list_eq_safe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		getcons(cons, &check, &cons);
		if (check == key) return 1;
	}

	return 0;
}

_g int find_list_eql_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (eql_function(check, key)) return 1;
	}

	return 0;
}

_g int find_list_eql_safe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		getcons(cons, &check, &cons);
		if (eql_function(check, key)) return 1;
	}

	return 0;
}

_g int find_list_equal_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (equal_function(check, key)) return 1;
	}

	return 0;
}

_g int find_list_equal_safe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		getcons(cons, &check, &cons);
		if (equal_function(check, key)) return 1;
	}

	return 0;
}

_g int position_list_eq_unsafe(addr key, addr cons, size_t *ret)
{
	addr check;
	size_t i;

	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &check, &cons);
		if (check == key) {
			*ret = i;
			return 1;
		}
	}

	return 0;
}


/*
 *  pushnew
 */
_g int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret)
{
	if (! find_list_eq_unsafe(value, list)) {
		cons_alloc(local, ret, value, list);
		return 1;
	}

	return 0;
}

_g int pushnew_local(LocalRoot local, addr list, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnew_alloc(local, list, value, ret);
}

_g int pushnew_heap(addr list, addr value, addr *ret)
{
	return pushnew_alloc(NULL, list, value, ret);
}

_g int pushnew_equal_alloc(LocalRoot local, addr list, addr value, addr *ret)
{
	if (! find_list_equal_unsafe(value, list)) {
		cons_alloc(local, ret, value, list);
		return 1;
	}

	return 0;
}

_g int pushnew_equal_local(LocalRoot local, addr list, addr value, addr *ret)
{
	CheckLocal(local);
	return pushnew_equal_alloc(local, list, value, ret);
}

_g int pushnew_equal_heap(addr list, addr value, addr *ret)
{
	return pushnew_equal_alloc(NULL, list, value, ret);
}


/*
 *  nreverse
 */
_g void nreverse_list_unsafe(addr *ret, addr cons)
{
	addr tail, next;

	/* nil */
	if (cons == Nil) {
		*ret = Nil;
		return;
	}

	/* loop */
	for (tail = Nil; ; tail = cons, cons = next) {
		GetCdr(cons, &next);
		SetCdr(cons, tail);
		if (next == Nil) break;
	}
	*ret = cons;
}

_g addr nreverse_list_unsafe_inplace(addr cons)
{
	nreverse_list_unsafe(&cons, cons);
	return cons;
}

_g void nreverse_list_unsafe_dotted(addr *ret, addr cons, addr tail)
{
	addr next;

	/* nil */
	if (cons == Nil) {
		*ret = tail;
		return;
	}

	/* loop */
	for (;;) {
		GetCdr(cons, &next);
		SetCdr(cons, tail);
		if (next == Nil) break;
		tail = cons;
		cons = next;
	}
	*ret = cons;
}

_g void nreverse_list_safe(addr *ret, addr cons)
{
	addr tail, next;

	/* nil */
	if (cons == Nil) {
		*ret = Nil;
		return;
	}

	/* loop */
	for (tail = Nil; ; tail = cons, cons = next) {
		getcdr(cons, &next);
		setcdr(cons, tail);
		if (next == Nil) break;
	}
	*ret = cons;
}

_g addr nreverse_list_safe_inplace(addr cons)
{
	nreverse_list_safe(&cons, cons);
	return cons;
}


/*
 *  reverse
 */
_g void reverse_list_heap_unsafe(addr *ret, addr cons)
{
	reverse_list_alloc_unsafe(NULL, ret, cons);
}

_g void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	reverse_list_alloc_unsafe(local, ret, cons);
}

_g void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	*ret = root;
}

_g void reverse_list_heap_safe(addr *ret, addr cons)
{
	reverse_list_alloc_safe(NULL, ret, cons);
}

_g void reverse_list_local_safe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	reverse_list_alloc_safe(local, ret, cons);
}

_g void reverse_list_alloc_safe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		getcons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	*ret = root;
}


/*
 *  callname
 */
_g int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret)
{
	if (! find_list_callname_unsafe(callname, list)) {
		cons_alloc(local, ret, callname, list);
		return 1;
	}

	return 0;
}

_g int pushnewlist_callname_heap(addr list, addr callname, addr *ret)
{
	return pushnewlist_callname_alloc(NULL, list, callname, ret);
}

_g int find_list_callname_unsafe(addr callname, addr list)
{
	addr check;

	while (list != Nil) {
		GetCons(list, &check, &list);
		if (equal_callname(check, callname)) return 1;
	}

	return 0;
}


/*
 *  copy-list
 */
_g void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons)
{
	addr root, left;

	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		cons_alloc(local, &root, left, root);
	}
	nreverse_list_unsafe(ret, root);
}
_g void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_unsafe(local, ret, cons);
}
_g void copy_list_heap_unsafe(addr *ret, addr cons)
{
	copy_list_alloc_unsafe(NULL, ret, cons);
}

_g void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons)
{
	addr root, last, pos;

	root = last = Nil;
	for (;;) {
		if (GetType(cons) == LISPTYPE_CONS) {
			GetCons(cons, &pos, &cons);
			cons_alloc(local, &root, pos, root);
		}
		else {
			last = cons;
			break;
		}
	}
	nreverse_list_unsafe_dotted(ret, root, last);
}
_g void copy_list_local_safe(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	copy_list_alloc_safe(local, ret, cons);
}
_g void copy_list_heap_safe(addr *ret, addr cons)
{
	copy_list_alloc_safe(NULL, ret, cons);
}


/*
 *  delete / remove
 */
static int delete_list_call_unsafe(addr key, addr cons, addr *ret,
		int (*call)(addr, addr))
{
	int update;
	addr check, cons1, cons2;

	update = 0;
	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (call(check, key)) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			update++;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return update;
}

_g int delete_list_eq_unsafe(addr key, addr cons, addr *ret)
{
	return delete_list_call_unsafe(key, cons, ret, eq_function);
}

_g int delete_list_equal_unsafe(addr key, addr cons, addr *ret)
{
	return delete_list_call_unsafe(key, cons, ret, equal_function);
}

_g int delete1_list_eq_unsafe(addr key, addr cons, addr *ret)
{
	addr check, cons1, cons2;

	*ret = cons;
	cons2 = Nil;
	while (cons != Nil) {
		GetCons(cons, &check, &cons1);
		if (check == key) {
			if (cons2 == Nil)
				*ret = cons1;
			else
				SetCdr(cons2, cons1);
			return 1;
		}
		else {
			cons2 = cons;
		}
		cons = cons1;
	}

	return 0;
}

_g void remove_list_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	addr result, check;

	for (result = Nil; cons != Nil; ) {
		GetCons(cons, &check, &cons);
		if (check != key)
			cons_alloc(local, &result, check, result);
	}
	nreverse_list_unsafe(ret, result);
}

_g void remove_list_eq_unsafe_heap(addr key, addr cons, addr *ret)
{
	remove_list_eq_unsafe_alloc(NULL, key, cons, ret);
}

_g void remove_list_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret)
{
	Check(local == NULL, "local error");
	remove_list_eq_unsafe_alloc(local, key, cons, ret);
}

