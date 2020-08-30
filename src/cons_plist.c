#include "callname.h"
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

	for (;;) {
		if (GetType(plist) != LISPTYPE_CONS)
			break;
		GetCons(plist, &check, &plist);
		if (check == key) {
			if (GetType(plist) != LISPTYPE_CONS)
				break;
			GetCar(plist, ret);
			return 0;
		}
		if (GetType(plist) != LISPTYPE_CONS)
			break;
		GetCdr(plist, &plist);
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
		if (GetType(cons) != LISPTYPE_CONS)
			break;
		GetCons(cons, &check, &cons);
		if (check == key) {
			if (GetType(cons) != LISPTYPE_CONS)
				break;
			SetCar(cons, value);
			return 0;
		}
		if (GetType(cons) != LISPTYPE_CONS)
			break;
		GetCdr(cons, &cons);
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

_g int remplist_safe_(addr plist, addr key, addr *value, enum RemPlist *ret)
{
	addr root, check;

	/* first */
	if (plist == Nil) {
		*value = Nil;
		return Result(ret, RemPlist_NotFound);
	}
	Return_getcons(plist, &check, &root);
	if (check == key) {
		Return_getcdr(root, value);
		return Result(ret, RemPlist_Update);
	}

	/* second */
	*value = plist;
	for (;;) {
		Return_getcdr(root, &plist);
		if (plist == Nil)
			break;

		Return_getcons(plist, &check, &plist);
		if (check == key) {
			Return_getcdr(plist, &plist);
			Return_setcdr(root, plist);
			return Result(ret, RemPlist_Delete);
		}
		root = plist;
	}

	return Result(ret, RemPlist_NotFound);
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
		if (plist == Nil)
			break;

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

_g int remplist(addr plist, addr key, addr *ret)
{
	return remplist_check(plist, key, ret) == RemPlist_Update;
}

static int remplist_alloc_(LocalRoot local, addr list, addr key, addr *value, int *ret)
{
	int deletep;
	addr root, pos, next;

	deletep = 0;
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return_getcons(list, &next, &list);
		if (pos != key) {
			cons_alloc(local, &root, pos, root);
			cons_alloc(local, &root, next, root);
		}
		else {
			deletep = 1;
		}
	}
	nreverse(value, root);

	return Result(ret, deletep);
}

_g int remplist_local_(LocalRoot local, addr list, addr key, addr *value, int *ret)
{
	CheckLocal(local);
	return remplist_alloc_(local, list, key, value, ret);
}

_g int remplist_heap_(addr list, addr key, addr *value, int *ret)
{
	return remplist_alloc_(NULL, list, key, value, ret);
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

_g int remplist_constant(addr plist, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return remplist(plist, key, ret);
}

_g int getpplist(addr plist, addr key1, addr key2, addr *ret)
{
	(void)getplist(plist, key1, &plist);
	return getplist(plist, key2, ret);
}

static int setpplist_equal_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret,
		int (*equal)(addr, addr))
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
				if ((*equal)(check, key2)) {
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
_g int setpplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	return setpplist_equal_alloc(local, plist, key1, key2, value, ret, eq_function);
}
_g int setpplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setpplist_alloc(local, plist, key1, key2, value, ret);
}
_g int setpplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret)
{
	return setpplist_alloc(NULL, plist, key1, key2, value, ret);
}


/*
 *  callname
 */
/* 0:find-value, 1:not-found(Nil) */
static int equal_callname_plist(addr x, addr y)
{
	if (x == y)
		return 1;
	return (GetType(x) == LISPTYPE_CALLNAME)
		&& (GetType(y) == LISPTYPE_CALLNAME)
		&& equal_callname(x, y);
}

_g int getplist_callname(addr plist, addr callname, addr *ret)
{
	addr check;

	while (plist != Nil) {
		Check(GetType(plist) != LISPTYPE_CONS, "type left error");
		GetCons(plist, &check, &plist);
		Check(GetType(plist) != LISPTYPE_CONS, "type right error");
		if (equal_callname_plist(check, callname)) {
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
		if (equal_callname_plist(check, callname)) {
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

_g int getpplist_callname(addr plist, addr key, addr callname, addr *ret)
{
	(void)getplist(plist, key, &plist);
	return getplist_callname(plist, callname, ret);
}

_g int setpplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	return setpplist_equal_alloc(local, plist, key, callname, value, ret,
			equal_callname_plist);
}
_g int setpplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret)
{
	Check(local == NULL, "local error");
	return setpplist_callname_alloc(local, plist, key, callname, value, ret);
}
_g int setpplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret)
{
	return setpplist_callname_alloc(NULL, plist, key, callname, value, ret);
}

