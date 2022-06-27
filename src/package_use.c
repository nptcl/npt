#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "execute_object.h"
#include "format.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designator.h"
#include "package_intern.h"
#include "package_shadow.h"
#include "package_use.h"
#include "restart.h"
#include "type_table.h"
#include "typedef.h"

/****************************************************************************
 *  Function USE-PACKAGE
 ****************************************************************************/
/*
 *  restart
 */
static int shadow_execute_use_package_(addr pg, addr name)
{
	enum PACKAGE_TYPE ignore;
	addr pos;

	Return(intern_package_(pg, name, &pos, &ignore));
	return shadow_package_(pg, pos);
}

static int restart_shadow_report_use_package_(
		Execute ptr, addr pg, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	Return(find_symbol_package_(pg, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL)
		pos = name;

	return format_string_(ptr, ret,
			"Use the original ~S, and make shadowing.",
			pos, NULL);
}

static int restart_shadow_use_package_(Execute ptr, addr pg, addr name, addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(restart_shadow_report_use_package_(ptr, pg, name, &pos));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int unintern_execute_use_package_(addr pg, addr name)
{
	enum PACKAGE_TYPE ignore1;
	int ignore2;
	addr pos;

	Return(intern_package_(pg, name, &pos, &ignore1));
	return unintern_package_(pg, pos, &ignore2);
}

static int restart_unintern_report_use_package_(
		Execute ptr, addr pg, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	Return(find_symbol_package_(pg, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL)
		pos = name;

	return format_string_(ptr, ret,
			"Use the inherited ~S, and unintern the original symbol.",
			pos, NULL);
}

static int restart_unintern_use_package_(Execute ptr, addr pg, addr name, addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_UNINTERN, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(restart_unintern_report_use_package_(ptr, pg, name, &pos));
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	return Result(ret, restart);
}

static int shadow_use_package_(addr pg, addr name)
{
	Execute ptr;
	addr restart1, restart2, control;

	ptr = Execute_Thread;
	Return(restart_unintern_use_package_(ptr, pg, name, &restart1));
	Return(restart_shadow_use_package_(ptr, pg, name, &restart2));

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);

	(void)call_simple_package_error_va_(NULL,
			"The name ~S causes a conflict in the ~S package.",
			name, pg, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* unintern */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		Return(unintern_execute_use_package_(pg, name));
		goto escape;
	}

	/* shadow */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		Return(shadow_execute_use_package_(pg, name));
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  use-package
 */
static int package_designator_use_package_(addr pos, addr *ret)
{
	addr keyword;

	Return(package_designator_(pos, &pos));
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (pos == keyword) {
		*ret = Nil;
		return fmte_("Cannot use the ~S package.", pos, NULL);
	}

	return Result(ret, pos);
}

static int check_already_use_package(addr package, addr pos)
{
	addr list;

	if (package == pos)
		return 1;
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	return find_list_eq_unsafe(pos, list);
}

static int check_conflict_use_package_(addr package, addr pos, addr name)
{
	enum PACKAGE_TYPE type;
	addr hash, bit;

	/* package check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findnil_hashtable_(hash, name, &bit));
	if (bit == Nil)
		return 0;
	if (StructBitType(bit)->shadow)
		return 0;

	/* symbol check */
	Return(find_symbol_package_(pos, name, &pos, &type));
	Check(type == PACKAGE_TYPE_NIL, "find_symbol error.");
	GetBitTypeSymbol(bit, &bit);
	if (bit != pos) {
		Return(shadow_use_package_(package, name));
	}

	return 0;
}

static int check_use_package_(addr package, addr pos)
{
	addr list, name;

	/* fiest check */
	if (check_already_use_package(package, pos))
		return 0;

	/* loop */
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(check_conflict_use_package_(package, pos, name));
	}

	return 0;
}

static int check_name_use_package_(addr package, addr hash1, addr hash2, addr name)
{
	enum PACKAGE_TYPE type;
	addr bit1, bit2, pos;

	/* left */
	Return(findnil_hashtable_(hash1, name, &bit1));
	if (bit1 == Nil)
		return 0;
	if (StructBitType(bit1)->expt == 0)
		return 0;

	/* right */
	Return(findnil_hashtable_(hash2, name, &bit2));
	Check(bit2 == Nil, "hashtable error.");
	if (bit2 == Nil)
		return 0;
	if (StructBitType(bit2)->expt == 0)
		return 0;

	/* package */
	Return(find_symbol_package_(package, name, &pos, &type));
	if (type == PACKAGE_TYPE_NIL) {
		GetBitTypeSymbol(bit1, &bit1);
		GetBitTypeSymbol(bit2, &bit2);
		if (bit1 != bit2)
			return shadow_use_package_(package, name);
	}

	return 0;
}

static int check_export_use_package_(addr package, addr hash1, addr pos)
{
	addr hash2, list, name;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash2);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(check_name_use_package_(package, hash1, hash2, name));
	}

	return 0;
}

static int check_loop_use_package_(addr package, addr pos, addr list)
{
	addr hash1, check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash1);
	while (list != Nil) {
		GetCons(list, &check, &list);
		Return(package_designator_use_package_(check, &check));
		if (check == pos)
			continue;
		if (check == package)
			continue;

		Return(check_export_use_package_(package, hash1, check));
	}

	return 0;
}

static int check_list_use_package_(addr package, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designator_use_package_(pos, &pos));
		if (package == pos)
			continue;
		Return(check_loop_use_package_(package, pos, list));
	}

	return 0;
}

static int intern_use_package_(addr hash1, addr hash2, addr name)
{
	addr cons, bit, symbol;

	Return(intern_hashheap_(hash1, name, &cons));
	GetCdr(cons, &bit);
	if (bit != Nil)
		return 0;

	Return(findnil_hashtable_(hash2, name, &bit));
	Check(bit == Nil, "use-package error");
	GetBitTypeSymbol(bit, &symbol);
	inheritedbitpackage(&bit, symbol);
	SetCdr(cons, bit);

	return 0;
}

static int execute_use_package_(addr package, addr pos)
{
	addr hash1, hash2, list, name;

	/* ignore */
	Return(package_designator_use_package_(pos, &pos));
	if (check_already_use_package(package, pos))
		return 0;

	/* execute */
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash1);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash2);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_use_package_(hash1, hash2, name));
	}
	push_list_use_package(package, pos);
	push_list_used_package(pos, package);

	return 0;
}

static int package_use_package_(addr package, addr pos)
{
	Return(package_designator_use_package_(pos, &pos));
	Return(check_use_package_(package, pos));
	Return(execute_use_package_(package, pos));

	return 0;
}

static int list_use_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, args, type,
					"USE-PACKAGE ~S must be a package-designator.", pos, NULL);
		}
	}

	/* conflict check */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designator_use_package_(pos, &pos));
		Return(check_use_package_(package, pos));
	}
	Return(check_list_use_package_(package, args));

	/* use-package */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(execute_use_package_(package, pos));
	}

	return 0;
}

int use_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	Return(package_designator_use_package_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return package_use_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_use_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignatorList);
			return call_type_error_va_(NULL, pos, type,
					"USE-PACKAGE ~S must be a package-designator or list.", pos, NULL);
	}
}


/****************************************************************************
 *  Function UNUSE-PACKAGE
 ****************************************************************************/
static int check_uselist_package(addr package, addr pos)
{
	addr list;
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	return find_list_eq_unsafe(pos, list);
}

static int execute_unuse_package_(addr package, addr pos)
{
	int check;
	addr hash, list, name, bit;

	Return(package_designator_(pos, &pos));
	if (check_uselist_package(package, pos) == 0)
		return 0;

	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(findnil_hashtable_(hash, name, &bit));
		Check(bit == Nil, "unuse-package error");
		if (StructBitType(bit)->inherit) {
			Return(delete_hashtable_(hash, name, &check));
		}
	}
	delete_list_use_package(package, pos);
	delete_list_used_package(pos, package);

	return 0;
}

static int list_unuse_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, args, type,
					"UNUSE-PACKAGE ~S must be a package-designator.", pos, NULL);
		}
	}

	/* unuse-package */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(execute_unuse_package_(package, pos));
	}

	return 0;
}

int unuse_package_(addr package, addr pos)
{
	addr type;

	Return(package_designator_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return execute_unuse_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_unuse_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignatorList);
			return call_type_error_va_(NULL, pos, type,
					"UNUSE-PACKAGE ~S must be a package-designator or list.", pos, NULL);
	}
}

