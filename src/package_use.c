#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_use.h"
#include "type_table.h"
#include "typedef.h"

/****************************************************************************
 *  Function USE-PACKAGE
 ****************************************************************************/
static int package_designer_use_package_(addr pos, addr *ret)
{
	addr keyword;

	Return(package_designer_(pos, &pos));
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

static int check_conflict_use_package_(addr package, addr pos)
{
	enum PACKAGE_TYPE type;
	addr table, list, name, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(pos, PACKAGE_INDEX_EXPORT, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(findnil_hashtable_(table, name, &bit));
		if (bit == Nil)
			continue;
		if (StructBitType(bit)->shadow)
			continue;

		GetBitTypeSymbol(bit, &bit);
		Return(find_symbol_package_(pos, name, &check, &type));
		Check(type == PACKAGE_TYPE_NIL, "find_symbol error.");
		if (bit != check)
			return fmte_("Symbol ~S conflict occered.", name, NULL);
	}

	return 0;
}

static int check_use_package_(addr package, addr pos)
{
	if (check_already_use_package(package, pos))
		return 0;
	return check_conflict_use_package_(package, pos);
}

static int check_export_use_package_(addr hash1, addr hash2, addr list)
{
	addr pos, bit1, bit2;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(findnil_hashtable_(hash2, pos, &bit1));
		if (bit1 == Nil)
			continue;
		if (StructBitType(bit1)->expt == 0)
			continue;

		/* Conflict occured */
		Return(findnil_hashtable_(hash1, pos, &bit2));
		if (bit2 != Nil && StructBitType(bit2)->shadow)
			continue;
		GetBitTypeSymbol(bit1, &bit1);
		GetBitTypeSymbol(bit2, &bit2);
		/* eq symbol */
		if (bit1 == bit2)
			continue;
		/* error */
		return fmte_("The name ~S causes a conflict in the package.", pos, NULL);
	}

	return 0;
}

static int check_loop_use_package_(addr package, addr pos, addr list)
{
	addr hash1, hash2, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &hash1);
	GetPackage(pos, PACKAGE_INDEX_TABLE, &hash2);
	while (list != Nil) {
		GetCons(list, &check, &list);
		Return(package_designer_use_package_(check, &check));
		if (check == pos)
			continue;
		if (check == package)
			continue;

		GetPackage(check, PACKAGE_INDEX_EXPORT, &check);
		Return(check_export_use_package_(hash1, hash2, check));
	}

	return 0;
}

static int check_list_use_package_(addr package, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designer_use_package_(pos, &pos));
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
	Return(package_designer_use_package_(pos, &pos));
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
	Return(package_designer_use_package_(pos, &pos));
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
		if (! package_designer_p(pos)) {
			GetTypeTable(&type, PackageDesigner);
			return call_type_error_va_(NULL, args, type,
					"USE-PACKAGE ~S must be a package-designer.", pos, NULL);
		}
	}

	/* conflict check */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(package_designer_use_package_(pos, &pos));
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

_g int use_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_use_package_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return package_use_package_(package, pos);

		case LISPTYPE_CONS:
			return list_use_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignerList);
			return call_type_error_va_(NULL, pos, type,
					"USE-PACKAGE ~S must be a package-designer or list.", pos, NULL);
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

	Return(package_designer_(pos, &pos));
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
		if (! package_designer_p(pos)) {
			GetTypeTable(&type, PackageDesigner);
			return call_type_error_va_(NULL, args, type,
					"UNUSE-PACKAGE ~S must be a package-designer.", pos, NULL);
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

_g int unuse_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_PACKAGE:
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return execute_unuse_package_(package, pos);

		case LISPTYPE_CONS:
			return list_unuse_package_(package, pos);

		default:
			GetTypeTable(&type, PackageDesignerList);
			return call_type_error_va_(NULL, pos, type,
					"UNUSE-PACKAGE ~S must be a package-designer or list.", pos, NULL);
	}
}

