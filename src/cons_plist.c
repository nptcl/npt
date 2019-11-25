#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "equal.h"
#include "function.h"

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

