#include "bignum.h"
#include "build.h"
#include "condition.h"
#include "cons.h"
#include "copy.h"
#include "equal.h"
#include "function.h"
#include "object.h"
#include "sequence.h"

/*
 *  list
 */
_g void getcons(addr cons, addr *left, addr *right)
{
	if (cons != Nil && GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	GetCons(cons, left, right);
}

_g void getcar(addr cons, addr *left)
{
	if (cons != Nil && GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	GetCar(cons, left);
}

_g void getcdr(addr cons, addr *right)
{
	if (cons != Nil && GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	GetCdr(cons, right);
}

_g void setcons(addr cons, addr left, addr right)
{
	if (GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	SetCons(cons, left, right);
}

_g void setcar(addr cons, addr left)
{
	if (GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	SetCar(cons, left);
}

_g void setcdr(addr cons, addr right)
{
	if (GetType(cons) != LISPTYPE_CONS)
		TypeError(cons, CONS);
	SetCdr(cons, right);
}

_g void getnth(addr cons, size_t index, addr *ret)
{
	while (index--) {
		if (cons == Nil) {
			*ret = Nil;
			return;
		}
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
		decf_bignum(index, 1);
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		getcdr(cons, &cons);
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
		if (index-- == 0) break;
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

_g void getnth_unsafe(addr cons, size_t index, addr *ret)
{
	while (index--) {
		if (cons == Nil) {
			*ret = Nil;
			return;
		}
		GetCdr(cons, &cons);
	}
	GetCar(cons, ret);
}

_g void getnthcdr(addr cons, size_t index, addr *ret)
{
	while (index--) {
		if (cons == Nil) {
			*ret = Nil;
			return;
		}
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
		decf_bignum(index, 1);
		if (cons == Nil) {
			*ret = Nil;
			goto finish;
		}
		getcdr(cons, &cons);
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

_g int length_list_check(addr list, size_t *ret)
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

_g int equal_length_list_unsafe(addr left, addr right)
{
	while (left != Nil) {
		if (right == Nil) return 0;
		GetCdr(left, &left);
		GetCdr(right, &right);
	}

	return right == Nil;
}

static void nconc_concat(addr list, addr args)
{
	addr last;

	last = list;
	for (;;) {
		/* update lastcdr */
		while (list != Nil) {
			last = list;
			getcdr(list, &list);
		}

		for (;;) {
			GetCons(args, &list, &args);
			if (args == Nil) {
				setcdr(last, list);
				return;
			}
			if (list == Nil) {
				continue;
			}
			if (IsCons(list)) {
				setcdr(last, list);
				break;
			}
			fmte("nconc argument ~S must be a list.", list, NULL);
		}
	}
}

_g void nconc_safe(addr args, addr *ret)
{
	addr pos;

	/* (nconc) */
	if (args == Nil) {
		*ret = Nil;
		return;
	}

	/* (nconc object) */
	for (;;) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			*ret = pos;
			return;
		}
		if (pos == Nil) {
			continue;
		}
		if (IsCons(pos)) {
			break;
		}
		fmte("nconc argument ~S must be a list.", pos, NULL);
	}

	/* (nconc x x ...) */
	nconc_concat(pos, args);
	*ret = pos;
}

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

static addr push_append(addr root, addr last)
{
	addr pos;

	if (! IsCons(last))
		fmte("The argument ~S must be a list.", last, NULL);
	while (last != Nil) {
		getcons(last, &pos, &last);
		cons_heap(&root, pos, root);
	}
	return root;
}

static void append_concat(addr last, addr args, addr *ret)
{
	addr pos, root;

	for (root = Nil; args != Nil; ) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			if (pos != Nil) {
				root = push_append(root, last);
				last = pos;
			}
			break;
		}
		if (pos == Nil) {
			continue;
		}
		root = push_append(root, last);
		last = pos;
	}
	nreverse_list_unsafe_dotted(ret, root, last);
}

_g void append_safe(addr args, addr *ret)
{
	addr pos;

	/* (append) */
	if (args == Nil) {
		*ret = Nil;
		return;
	}

	/* (append object) */
	for (;;) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			*ret = pos;
			return;
		}
		if (pos == Nil) {
			continue;
		}
		if (IsCons(pos)) {
			break;
		}
		fmte("append argument ~S must be a list.", pos, NULL);
	}

	/* (append x x ...) */
	append_concat(pos, args, &pos);
	*ret = pos;
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

_g void butlast_safe(addr *ret, addr list, size_t index)
{
	addr root, pos;
	size_t size;

	size = length_list_safe_dotted(list);
	if (size <= index) {
		*ret = Nil;
		return;
	}
	size -= index;
	for (root = Nil; size--; ) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

_g void butlast_large(addr *ret, addr list, addr index)
{
	fmte("TODO: large", NULL);
}

_g void nbutlast_safe(addr *ret, addr list, size_t index)
{
	size_t size;

	size = length_list_safe_dotted(list);
	if (size <= index) {
		*ret = Nil;
		return;
	}
	size -= index + 1;
	while (size--)
		GetCdr(list, &list);
	SetCdr(list, Nil);
}

_g void nbutlast_large(addr *ret, addr list, addr index)
{
	fmte("TODO: large", NULL);
}

_g void last_safe(addr *ret, addr list, size_t index)
{
	size_t size;

	size = length_list_safe_dotted(list);
	if (size < index) {
		*ret = Nil;
		return;
	}
	size -= index;
	while (size--)
		getcdr(list, &list);
	*ret = list;
}

_g void last_large(addr *ret, addr list, addr index)
{
	fmte("TODO: large", NULL);
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

_g int find_list_equal_unsafe(addr key, addr cons)
{
	addr check;

	while (cons != Nil) {
		GetCons(cons, &check, &cons);
		if (equal_function(check, key)) return 1;
	}

	return 0;
}

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
 *  list
 */
/* `(list* ,first ,@cons) */
_g void lista_alloc_safe(LocalRoot local, addr *ret, addr first, addr cons)
{
	addr pos, root;

	for (root = Nil; cons != Nil; first = pos) {
		if (GetType(cons) != LISPTYPE_CONS)
			fmte("The argument ~S must be a list.", cons, NULL);
		GetCons(cons, &pos, &cons);
		cons_alloc(local, &root, first, root);
	}
	nreverse_list_unsafe_dotted(ret, root, first);
}
_g void lista_local_safe(LocalRoot local, addr *ret, addr first, addr cons)
{
	Check(local == NULL, "local error");
	lista_alloc_safe(local, ret, first, cons);
}
_g void lista_heap_safe(addr *ret, addr first, addr cons)
{
	lista_alloc_safe(NULL, ret, first, cons);
}

_g void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	Check(pos1 == NULL, "lista at least one argument.");
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}

	/* result */
	conscar_alloc(local, &cons, pos1);
	*ret = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			return;
		}

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

_g addr lista_allocr(LocalRoot local, ...)
{
	addr pos;
	va_list args;

	va_start(args, local);
	lista_stdarg_alloc(local, &pos, args);
	va_end(args);

	return pos;
}

_g addr lista_localr(LocalRoot local, ...)
{
	addr pos;
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, local);
	lista_stdarg_alloc(local, &pos, args);
	va_end(args);

	return pos;
}

_g addr lista_heapr(addr pos, ...)
{
	addr pos1, pos2, pos3, cons;
	va_list args;

	/* nil */
	Check(pos == NULL, "lista at least one argument.");
	if (pos == NULL) {
		return Nil; /* error */
	}
	pos1 = pos;

	/* dot list */
	va_start(args, pos);
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		va_end(args);
		return pos1;
	}

	/* result */
	conscar_heap(&cons, pos1);
	pos = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			break;
		}

		/* (pos1 pos2 . ?) */
		conscar_heap(&pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
	va_end(args);

	return pos;
}

_g void lista_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

_g void lista_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	lista_stdarg_alloc(local, ret, args);
	va_end(args);
}

_g void lista_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lista_stdarg_alloc(NULL, ret, args);
	va_end(args);
}

_g void List_bind(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			Check(list != Nil, "list nil error");
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);
}

_g void list_bind(addr list, ...)
{
	addr pos, *ret;
	va_list args;

	va_start(args, list);
	for (;;) {
		ret = va_arg(args, addr *);
		if (ret == NULL) {
			if (list != Nil)
				fmte("Too many list argument.", NULL);
			break;
		}
		if (list == Nil)
			fmte("Too few list argument.", NULL);
		getcons(list, &pos, &list);
		*ret = pos;
	}
	va_end(args);
}

_g void Lista_bind(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		Check(list != Nil, "list nil error");
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		Check(! consp(list), "list error");
		GetCons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);
}

_g void lista_bind(addr list, ...)
{
	addr *ret1, *ret2, pos;
	va_list args;

	va_start(args, list);
	ret1 = va_arg(args, addr *);
	if (ret1 == NULL) {
		if (list != Nil)
			fmte("Too few argument.", NULL);
		goto finish;
	}
	for (;;) {
		ret2 = va_arg(args, addr *);
		if (ret2 == NULL) {
			*ret1 = list;
			break;
		}
		if (! consp(list))
			fmte("Too few argument.", NULL);
		getcons(list, &pos, &list);
		*ret1 = pos;
		ret1 = ret2;
	}
finish:
	va_end(args);
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

_g void revappend_safe(addr *ret, addr list, addr tail)
{
	addr pos;

	/* nil */
	if (list == Nil) {
		*ret = tail;
		return;
	}

	/* loop */
	while (list != Nil) {
		getcons(list, &pos, &list);
		cons_heap(&tail, pos, tail);
	}
	*ret = tail;
}

_g void nreconc_safe(addr *ret, addr list, addr tail)
{
	addr next;

	/* nil */
	if (list == Nil) {
		*ret = tail;
		return;
	}

	/* loop */
	for (;;) {
		getcdr(list, &next);
		setcdr(list, tail);
		if (next == Nil) break;
		tail = list;
		list = next;
	}
	*ret = list;
}

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

_g void copy_tree_alloc(LocalRoot local, addr *ret, addr cdr)
{
	addr car;

	if (GetType(cdr) != LISPTYPE_CONS) {
		*ret = cdr;
	}
	else {
		GetCons(cdr, &car, &cdr);
		copy_tree_alloc(local, &car, car);
		copy_tree_alloc(local, &cdr, cdr);
		cons_alloc(local, ret, car, cdr);
	}
}

_g void copy_tree_local(LocalRoot local, addr *ret, addr list)
{
	Check(local == NULL, "local error");
	copy_tree_alloc(local, ret, list);
}

_g void copy_tree_heap(addr *ret, addr list)
{
	copy_tree_alloc(NULL, ret, list);
}

_g void pushva_heap(addr *list, ...)
{
	addr root, pos;
	va_list args;

	root = *list;
	va_start(args, list);
	for (;;) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		cons_heap(&root, pos, root);
	}
	va_end(args);
	*list = root;
}


/*
 *  plist
 */
/* 0:find-value, 1:not-found(Nil) */
_g int getplist(addr plist, addr key, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(plist, ret);
			return 0;
		}
		GetCdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

_g int getplist_safe(addr plist, addr key, addr *ret)
{
	addr check;

	while (plist != Nil) {
		getcons(plist, &check, &plist);
		if (check == key) {
			getcar(plist, ret);
			return 0;
		}
		getcdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

/* 0:find-and-set, 1:make-new-cons */
_g int setplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int setplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_alloc(local, plist, key, value, ret);
}
_g int setplist_heap(addr plist, addr key, addr value, addr *ret)
{
	return setplist_alloc(NULL, plist, key, value, ret);
}

/* 0:find-and-set, 1:make-new-cons */
_g int setplist_alloc_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		getcons(cons, &check, &cons);
		if (check == key) {
			setcar(cons, value);
			return 0;
		}
		getcdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int setplist_local_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_alloc_safe(local, plist, key, value, ret);
}
_g int setplist_heap_safe(addr plist, addr key, addr value, addr *ret)
{
	return setplist_alloc_safe(NULL, plist, key, value, ret);
}

_g int pushplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr cons, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &check);
			cons_alloc(local, &value, value, check);
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int pushplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushplist_alloc(local, plist, key, value, ret);
}
_g int pushplist_heap(addr plist, addr key, addr value, addr *ret)
{
	return pushplist_alloc(NULL, plist, key, value, ret);
}
_g int pushnewplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr cons, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &check);
			if (pushnew_alloc(local, check, value, &check)) {
				SetCar(cons, check);
			}
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int pushnewplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnewplist_alloc(local, plist, key, value, ret);
}
_g int pushnewplist_heap(addr plist, addr key, addr value, addr *ret)
{
	return pushnewplist_alloc(NULL, plist, key, value, ret);
}

_g enum RemPlist remplist_check(addr plist, addr key, addr *ret)
{
	addr root, check;

	/* first */
	if (plist == Nil) {
		*ret = Nil;
		return RemPlist_NotFound;
	}
	CheckType2(plist, LISPTYPE_CONS, "type left error");
	GetCons(plist, &check, &root);
	CheckType2(root, LISPTYPE_CONS, "type right error");
	if (check == key) {
		GetCdr(root, ret);
		return RemPlist_Update;
	}

	/* second */
	*ret = plist;
	for (;;) {
		GetCdr(root, &plist);
		if (plist == Nil) break;

		CheckType2(plist, LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCdr(plist, &plist);
			SetCdr(root, plist);
			return RemPlist_Delete;
		}
		root = plist;
	}

	return RemPlist_NotFound;
}

_g enum RemPlist remplist_check_safe(addr plist, addr key, addr *ret)
{
	addr root, check;

	/* first */
	if (plist == Nil) {
		*ret = Nil;
		return RemPlist_NotFound;
	}
	getcons(plist, &check, &root);
	if (check == key) {
		getcdr(root, ret);
		return RemPlist_Update;
	}

	/* second */
	*ret = plist;
	for (;;) {
		getcdr(root, &plist);
		if (plist == Nil) break;

		getcons(plist, &check, &plist);
		if (check == key) {
			getcdr(plist, &plist);
			setcdr(root, plist);
			return RemPlist_Delete;
		}
		root = plist;
	}

	return RemPlist_NotFound;
}

_g int remplist(addr plist, addr key, addr *ret)
{
	return remplist_check(plist, key, ret) == RemPlist_Update;
}

_g int remplist_alloc(LocalRoot local, addr plist, addr key, addr *ret)
{
	int result;
	addr root, pos, value;

	result = 0;
	for (root = Nil; plist != Nil; ) {
		getcons(plist, &pos, &plist);
		getcons(plist, &value, &plist);
		if (pos != key) {
			cons_alloc(local, &root, pos, root);
			cons_alloc(local, &root, value, root);
		}
		else {
			result = 1;
		}
	}
	nreverse_list_unsafe(ret, root);

	return result;
}

_g int remplist_local(LocalRoot local, addr plist, addr key, addr *ret)
{
	CheckLocal(local);
	return remplist_alloc(local, plist, key, ret);
}

_g int remplist_heap(addr plist, addr key, addr *ret)
{
	return remplist_alloc(NULL, plist, key, ret);
}


/* 0:find-value, 1:not-found(Nil) */
_g int getplist_constant(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	Check(key == Unbound, "unbound error");
	return getplist(plist, key, ret);
}
_g int getplist_constant_safe(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	Check(key == Unbound, "unbound error");
	return getplist_safe(plist, key, ret);
}

/* 0:find-and-set, 1:make-new-cons */
_g int setplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return setplist_alloc(local, plist, key, value, ret);
}
_g int setplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_constant_alloc(local, plist, index, value, ret);
}
_g int setplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret)
{
	return setplist_constant_alloc(NULL, plist, index, value, ret);
}
_g int pushplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return pushplist_alloc(local, plist, key, value, ret);
}
_g int pushplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushplist_constant_alloc(local, plist, index, value, ret);
}
_g int pushplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret)
{
	return pushplist_constant_alloc(NULL, plist, index, value, ret);
}
_g int pushnewplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return pushnewplist_alloc(local, plist, key, value, ret);
}
_g int pushnewplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnewplist_constant_alloc(local, plist, index, value, ret);
}
_g int pushnewplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret)
{
	return pushnewplist_constant_alloc(NULL, plist, index, value, ret);
}

_g enum RemPlist remplist_check_constant(addr plist,
		constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return remplist_check(plist, key, ret);
}
_g int remplist_constant(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return remplist(plist, key, ret);
}

_g int getplistplist(addr plist, addr key1, addr key2, addr *ret)
{
	(void)getplist(plist, key1, &plist);
	return getplist(plist, key2, ret);
}

_g int setplistplist(addr plist, addr key1, addr key2, addr value, addr *ret)
{
	addr check;

	if (getplist(plist, key1, &plist)) {
		return 1;
	}
	while (plist != Nil) {
		GetCons(plist, &check, &plist);
		CheckType(plist, LISPTYPE_CONS);
		if (check == key2) {
			SetCar(plist, value);
			return 0;
		}
		GetCdr(plist, &plist);
	}

	return 1;
}

_g int setplistplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key1) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if (check == key2) {
					SetCar(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			cons_alloc(local, &child, value, child);
			cons_alloc(local, &child, key2, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &value, key2, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key1, plist);
	return 1;
}
_g int setplistplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplistplist_alloc(local, plist, key1, key2, value, ret);
}
_g int setplistplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret)
{
	return setplistplist_alloc(NULL, plist, key1, key2, value, ret);
}

_g int setplistplist_heap_force(addr plist, addr key1, addr key2, addr value, addr *ret)
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key1) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if (check == key2) {
					SetCar_force(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			conscdr_heap(&child, child);
			SetCar_force(child, value);
			cons_heap(&child, key2, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	consnil_heap(&check);
	SetCar_force(check, value);
	cons_heap(&check, key2, check);
	cons_heap(&plist, check, plist);
	cons_heap(ret, key1, plist);
	return 1;
}


/*
 *  eql
 */
/* 0:find-value, 1:not-found(Nil) */
_g int getplist_eql(addr plist, addr name, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (equal_function(check, name)) {
			GetCar(plist, ret);
			return 0;
		}
		GetCdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

_g int setplist_eql_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (eql_function(check, key)) {
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);

	return 1;
}
_g int setplist_eql_local(LocalRoot local, addr plist, addr key, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_eql_alloc(local, plist, key, value, ret);
}
_g int setplist_eql_heap(addr plist, addr key, addr value, addr *ret)
{
	return setplist_eql_alloc(NULL, plist, key, value, ret);
}

_g int getplistplist_eql(addr plist, addr key, addr name, addr *ret)
{
	(void)getplist(plist, key, &plist);
	return getplist_eql(plist, name, ret);
}
_g int setplistplist_eql_alloc(LocalRoot local,
		addr plist, addr key, addr name, addr value, addr *ret)
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if (equal_function(check, name)) {
					SetCar(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			cons_alloc(local, &child, value, child);
			cons_alloc(local, &child, name, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &value, name, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int setplistplist_eql_local(LocalRoot local,
		addr plist, addr key, addr name, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplistplist_eql_alloc(local, plist, key, name, value, ret);
}
_g int setplistplist_eql_heap(addr plist, addr key, addr name, addr value, addr *ret)
{
	return setplistplist_eql_alloc(NULL, plist, key, name, value, ret);
}


/*
 *  callname
 */
/* 0:find-value, 1:not-found(Nil) */
_g int getplist_callname(addr plist, addr callname, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (equal_callname(check, callname)) {
			GetCar(plist, ret);
			return 0;
		}
		GetCdr(plist, &plist);
	}
	*ret = Nil;

	return 1;
}

_g int setplist_callname_alloc(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret)
{
	addr check, cons;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (equal_callname(check, callname)) {
			SetCar(cons, value);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, callname, plist);
	return 1;
}

_g int setplist_callname_local(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplist_callname_alloc(local, plist, callname, value, ret);
}
_g int setplist_callname_heap(addr plist, addr callname, addr value, addr *ret)
{
	return setplist_callname_alloc(NULL, plist, callname, value, ret);
}

_g int pushnewplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret)
{
	addr cons, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &check);
			if (pushnewlist_callname_alloc(local, check, callname, &check)) {
				SetCar(cons, check);
			}
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &callname, callname);
	cons_alloc(local, &plist, callname, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int pushnewplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret)
{
	Check(local == NULL, "local error");
	return pushnewplist_callname_alloc(local, plist, key, callname, ret);
}
_g int pushnewplist_callname_heap(addr plist,
		addr key, addr callname, addr *ret)
{
	return pushnewplist_callname_alloc(NULL, plist, key, callname, ret);
}

_g int getplistplist_callname(addr plist, addr key, addr callname, addr *ret)
{
	(void)getplist(plist, key, &plist);
	return getplist_callname(plist, callname, ret);
}
_g int setplistplist_callname(addr plist, addr key, addr callname, addr value)
{
	addr check;

	if (getplist(plist, key, &plist)) {
		return 1;
	}
	while (plist != Nil) {
		GetCons(plist, &check, &plist);
		CheckType(plist, LISPTYPE_CONS);
		if (check == callname) {
			SetCar(plist, value);
			return 0;
		}
		GetCdr(plist, &plist);
	}

	return 1;
}

_g int setplistplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if (equal_callname(check, callname)) {
					SetCar(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			cons_alloc(local, &child, value, child);
			cons_alloc(local, &child, callname, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	conscar_alloc(local, &value, value);
	cons_alloc(local, &value, callname, value);
	cons_alloc(local, &plist, value, plist);
	cons_alloc(local, ret, key, plist);
	return 1;
}
_g int setplistplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setplistplist_callname_alloc(local, plist, key, callname, value, ret);
}
_g int setplistplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret)
{
	return setplistplist_callname_alloc(NULL, plist, key, callname, value, ret);
}
_g int setplistplist_callname_heap_force(addr plist,
		addr key, addr callname, addr value, addr *ret)
{
	addr cons, find, child, check;

	for (cons = plist; cons != Nil; ) {
		Check(GetType(cons) != LISPTYPE_CONS, "type left error");
		GetCons(cons, &check, &cons);
		Check(GetType(cons) != LISPTYPE_CONS, "type right error");
		if (check == key) {
			GetCar(cons, &child);
			for (find = child; find != Nil; ) {
				Check(GetType(find) != LISPTYPE_CONS, "type find left error");
				GetCons(find, &check, &find);
				Check(GetType(find) != LISPTYPE_CONS, "type find right error");
				if (equal_callname(check, callname)) {
					SetCar_force(find, value);
					return 0;
				}
				GetCdr(find, &find);
			}
			conscdr_heap(&child, child);
			SetCar_force(child, value);
			cons_heap(&child, callname, child);
			SetCar(cons, child);
			return 0;
		}
		GetCdr(cons, &cons);
	}
	consnil_heap(&check);
	SetCar_force(check, value);
	cons_heap(&check, callname, check);
	cons_heap(&plist, check, plist);
	cons_heap(ret, key, plist);
	return 1;
}

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
 *  assoc
 */
_g int getassoc(addr key, addr list, addr *ret)
{
	addr left, right;

	while (list != Nil) {
		CheckType(list, LISPTYPE_CONS);
		GetCons(list, &left, &list);
		CheckType(left, LISPTYPE_CONS);
		GetCar(left, &right);
		if (key == right) {
			GetCdr(left, ret);
			return 0;
		}
	}
	*ret = Nil;

	return 1;
}

_g int getrassoc(addr key, addr list, addr *ret)
{
	addr left, right;

	while (list != Nil) {
		CheckType(list, LISPTYPE_CONS);
		GetCons(list, &left, &list);
		CheckType(left, LISPTYPE_CONS);
		GetCdr(left, &right);
		if (key == right) {
			GetCar(left, ret);
			return 0;
		}
	}
	*ret = Nil;

	return 1;
}


/*
 *  list-sequence
 */
_g void list_nil_heap(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

