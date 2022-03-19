#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "package.h"
#include "package_designer.h"
#include "package_object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

int packagep(addr pos)
{
	return GetType(pos) == LISPTYPE_PACKAGE;
}


/*
 *  package function
 */
int getname_package_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
	return 0;
}

int getnickname_package_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
	return 0;
}

int getuselist_package_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
	return 0;
}

int getusedbylist_package_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
	return 0;
}

int getshadow_package_(addr pos, addr *ret)
{
	Return(package_designer_(pos, &pos));
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
	return 0;
}

void getdocument_package(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_DOCUMENT, ret);
}

void setdocument_package(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetPackage(pos, PACKAGE_INDEX_DOCUMENT, value);
}


void getname_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
}

void getnickname_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
}

void getuselist_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
}

void getusedbylist_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
}

void getshadow_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
}

void getexport_package_unsafe(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, ret);
}

int get_readonly_package(addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	return GetUser(pos);
}

void set_readonly_package(addr pos, int value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetUser(pos, value != 0);
}


/*
 *  push package list
 */
static void push_list_eq_package(addr package, addr pos, enum PACKAGE_INDEX index)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);
#ifdef LISP_DEBUG
	if (find_list_eq_unsafe(pos, list)) {
		Abort("push_list_eq_package error.");
	}
#endif
	cons_heap(&list, pos, list);
	SetPackage(package, index, list);
}

void push_list_nicknames_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_NICKNAME);
}

void push_list_use_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_USE);
}

void push_list_used_package(addr package, addr pos)
{
	push_list_eq_package(package, pos, PACKAGE_INDEX_USED);
}

#ifdef LISP_DEBUG
static int find_list_string_package_(addr name, addr list, int *ret)
{
	int check;
	addr pos;

	Check(! stringp(name), "type error");
	Check(! listp(list), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_equal_(pos, name, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
#endif

int push_list_export_package_(addr package, addr name)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);
#ifdef LISP_DEBUG
	Return(find_list_string_package_(name, list, &check));
	if (check) {
		Abort("push_list_string_package_ error.");
	}
#endif
	cons_heap(&list, name, list);
	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}

#ifdef LISP_DEBUG
static int find_list_shadow_package_(addr symbol, addr list, int *ret)
{
	int check;
	addr pos;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");
	GetNameSymbol(symbol, &symbol);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(pos, symbol, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}
#endif

int push_list_shadow_package_(addr package, addr symbol)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);
#ifdef LISP_DEBUG
	Return(find_list_shadow_package_(symbol, list, &check));
	if (check) {
		Abort("push_list_shadow_package_ error.");
	}
#endif
	cons_heap(&list, symbol, list);
	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}


/*
 *  delete package list
 */
#ifdef LISP_DEBUG
static void delete_list_eq_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);

	/* delete */
	if (delete1_list_eq_unsafe(pos, list, &list) == 0) {
		/* Abnormal error */
		Abort("delete error.");
	}

	/* check */
	if (find_list_eq_unsafe(pos, list)) {
		/* Abnormal error */
		Abort("check error.");
	}

	SetPackage(package, index, list);
}
#else
static void delete_list_eq_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	GetPackage(package, index, &list);
	(void)delete1_list_eq_unsafe(pos, list, &list);
	SetPackage(package, index, list);
}
#endif

void delete_list_use_package(addr package, addr pos)
{
	delete_list_eq_package(package, PACKAGE_INDEX_USE, pos);
}

void delete_list_used_package(addr package, addr pos)
{
	delete_list_eq_package(package, PACKAGE_INDEX_USED, pos);
}


/*
 *  delete export list
 */
static int delete1_list_string_package_(addr name, addr list, addr *value, int *ret)
{
	int check;
	addr pos, list1, list2;

	Check(! listp(list), "type error");
	*value = list;
	list2 = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list1);
		Return(string_equal_(pos, name, &check));
		if (check) {
			if (list2 == Nil)
				*value = list1;
			else
				SetCdr(list2, list1);
			return Result(ret, 1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}

	return Result(ret, 0);
}

#ifdef LISP_DEBUG
int delete_list_export_package_(addr package, addr name)
{
	int check;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);

	/* delete */
	Return(delete1_list_string_package_(name, list, &list, &check));
	if (! check) {
		/* Abnormal error */
		return fmte_("There is no ~S in export list.", name, NULL);
	}

	/* check */
	Return(find_list_string_package_(name, list, &check));
	if (check) {
		/* Abnormal error */
		return fmte_("Invalid export list.", NULL);
	}

	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}
#else
int delete_list_export_package_(addr package, addr name)
{
	int ignore;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! stringp(name), "type error");
	GetPackage(package, PACKAGE_INDEX_EXPORT, &list);
	Return(delete1_list_string_package_(name, list, &list, &ignore));
	SetPackage(package, PACKAGE_INDEX_EXPORT, list);

	return 0;
}
#endif


/*
 *  delete shadowing-symbols
 */
static int delete1_list_shadow_package_(addr symbol, addr list, addr *value, int *ret)
{
	int check;
	addr pos, list1, list2;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");
	GetNameSymbol(symbol, &symbol);
	*value = list;
	list2 = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list1);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(pos, symbol, &check));
		if (check) {
			if (list2 == Nil)
				*value = list1;
			else
				SetCdr(list2, list1);
			return Result(ret, 1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}

	return Result(ret, 0);
}

#ifdef LISP_DEBUG
int delete_list_shadow_package_(addr package, addr symbol)
{
	int check;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);

	/* delete */
	Return(delete1_list_shadow_package_(symbol, list, &list, &check));
	if (! check) {
		/* Abnormal error */
		return fmte_("There is no ~S in shadow list.", symbol, NULL);
	}

	/* check */
	Return(find_list_shadow_package_(symbol, list, &check));
	if (check) {
		/* Abnormal error */
		return fmte_("Invalid shadow list.", NULL);
	}

	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}
#else
int delete_list_shadow_package_(addr package, addr symbol)
{
	int ignore;
	addr list;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	GetPackage(package, PACKAGE_INDEX_SHADOW, &list);
	Return(delete1_list_shadow_package_(symbol, list, &list, &ignore));
	SetPackage(package, PACKAGE_INDEX_SHADOW, list);

	return 0;
}
#endif

