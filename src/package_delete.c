#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "execute_object.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_delete.h"
#include "package_use.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  name error
 */
static void delete_package_name_restart(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PROGRAM_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int delete_package_name_call_(addr pos, addr *ret)
{
	addr check;

	Return(find_package_(pos, &check));
	if (check != Nil)
		return Result(ret, check);

	*ret = Nil;
	return call_simple_package_error_va_(NULL, "No such a package ~S.", pos, NULL);
}

static int delete_package_name_(addr pos, addr *ret)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	delete_package_name_restart(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	if (delete_package_name_call_(pos, ret)) {
		if (ptr->throw_control == control) {
			normal_throw_control(ptr);
			*ret = Nil;
		}
	}

	return pop_control_(ptr, control);
}


/*
 *  use-package error
 */
static void delete_package_used_restart(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "unuse-package.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PROGRAM_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int delete_package_used_call_(addr pos)
{
	addr list;

	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	if (list != Nil) {
		return call_simple_package_error_va_(NULL,
				"Package ~S is used by ~S.", pos, list, NULL);
	}

	return 0;
}

static int delete_package_used_unuse_(addr pos)
{
	addr list, x;

	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(unuse_package_(x, pos));
	}
#ifdef LISP_DEBUG
	GetPackage(pos, PACKAGE_INDEX_USED, &list);
	Check(list != Nil, "used-list error.");
#endif

	return 0;
}

static int delete_package_used_(addr pos)
{
	addr restart, control;
	Execute ptr;

	ptr = Execute_Thread;
	delete_package_used_restart(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	if (delete_package_used_call_(pos)) {
		if (ptr->throw_control == control) {
			normal_throw_control(ptr);
			(void)delete_package_used_unuse_(pos);
		}
	}

	return pop_control_(ptr, control);
}


/*
 *  readonly
 */
static int delete_package_readonly_(addr pos)
{
	if (! get_readonly_package(pos))
		return 0;

	return fmte_("~S is a readonly package.", pos, NULL);
}


/*
 *  delete_package
 */
static int allunintern_uselist_package_(addr pos)
{
	addr left, right, root;

	GetPackage(pos, PACKAGE_INDEX_EXPORT, &root);
	GetPackage(pos, PACKAGE_INDEX_USE, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		delete_list_used_package(left, pos);
	}

	return 0;
}

static void all_unintern_package(addr pos)
{
	addr table, array, left, right;
	size_t i, size;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &table);

	/* all unintern */
	GetTableHash(table, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCdr(left, &left);

			/* package nil */
			if (StructBitType(left)->base) {
				GetBitTypeSymbol(left, &left);
				SetPackageSymbol(left, Nil);
			}
		}
	}
	clear_hashtable(table);

	SetPackage(pos, PACKAGE_INDEX_USE, Nil);
	SetPackage(pos, PACKAGE_INDEX_SHADOW, Nil);
	SetPackage(pos, PACKAGE_INDEX_EXPORT, Nil);
}

/*
 *  return 0:  delete package.
 *  return 1:  package name is nil.
 */
int delete_package_(addr pos, int *ret)
{
	int check;
	addr name, right, table;

	/* name check */
	Return(delete_package_name_(pos, &pos));
	if (pos == Nil) {
		/* Do nothing */
		return Result(ret, 1);
	}
	CheckType(pos, LISPTYPE_PACKAGE);

	/* delete check */
	GetPackage(pos, PACKAGE_INDEX_NAME, &name);
	if (name == Nil) {
		/* package object already deleted. */
		return Result(ret, 1);
	}

	/* readonly */
	Return(delete_package_readonly_(pos));

	/* used-by-list */
	Return(delete_package_used_(pos));

	/* all symbon unintern in use-list. */
	Return(allunintern_uselist_package_(pos));

	/* all symbol unintern in my package. */
	all_unintern_package(pos);

	/* delete name and nickname */
	PackageTable(&table);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &right);
	Return(delete_hashtable_(table, name, &check));
	while (right != Nil) {
		GetCons(right, &name, &right);
		Return(delete_hashtable_(table, name, &check));
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);
	SetPackage(pos, PACKAGE_INDEX_NAME, Nil);

	return Result(ret, 0);
}

