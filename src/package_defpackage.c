#include "character.h"
#include "common_header.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "document.h"
#include "execute_object.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "package.h"
#include "package_defpackage.h"
#include "package_delete.h"
#include "package_designator.h"
#include "package_export.h"
#include "package_import.h"
#include "package_intern.h"
#include "package_make.h"
#include "package_object.h"
#include "package_shadow.h"
#include "package_use.h"
#include "pointer.h"
#include "restart.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  find string
 */
static int defpackage_find_list_(addr x, addr list, int *ret)
{
	int check;
	addr y;

	while (list != Nil) {
		GetCons(list, &y, &list);
		Return(string_designator_equal_(x, y, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int defpackage_find_import_(addr x, addr list, int *ret)
{
	int check;
	addr row, y;

	while (list != Nil) {
		GetCons(list, &row, &list);
		GetCdr(row, &row); /* package-name */
		while (row != Nil) {
			GetCons(row, &y, &row);
			Return(string_designator_equal_(x, y, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}


/*****************************************************************************
 *  Function LISP-SYSTEM::DEFPACKAGE
 *****************************************************************************/
/*
 *  restart
 */
static void defpackage_restart_import_symbol(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Intern the symbol.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int defpackage_import_symbol_(addr package, addr name, addr *ret)
{
	enum PACKAGE_TYPE type;
	addr pos, restart, control;
	Execute ptr;

	/* find symbol */
	Return(find_symbol_package_(package, name, &pos, &type));
	if (type != PACKAGE_TYPE_NIL)
		return Result(ret, pos);

	/* restart */
	ptr = Execute_Thread;
	defpackage_restart_import_symbol(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);

	*ret = Nil;
	(void)call_simple_package_error_va_(ptr,
			"The symbol ~S is not exist in the ~S package.",
			name, package, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart) {
		normal_throw_control(ptr);
		Return(intern_package_(package, name, ret, NULL));
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  defpackage
 */
static int defpackage_update_nicknames_(addr pos, addr list)
{
	addr name;

	getname_package_unsafe(pos, &name);
	return rename_package_(pos, name, list, &pos);
}

static int defpackage_update_shadowing_(addr pos, addr list)
{
	addr child, package, key;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designator_(package, &package));
		while (child != Nil) {
			GetCons(child, &key, &child);
			Return(string_designator_heap_(&key, key, NULL));
			Return(defpackage_import_symbol_(package, key, &key));
			Return(shadowing_import_package_(pos, key));
		}
	}

	return 0;
}

static int defpackage_update_import_(LocalRoot local, addr pos, addr list)
{
	addr child, package, args, symbol;
	LocalStack stack;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designator_(package, &package));
		push_local(local, &stack);
		for (args = Nil; child != Nil; ) {
			GetCons(child, &symbol, &child);
			Return(string_designator_heap_(&symbol, symbol, NULL));
			Return(defpackage_import_symbol_(package, symbol, &symbol));
			cons_local(local, &args, symbol, args);
		}
		nreverse(&args, args);
		Return(import_package_(pos, args));
		rollback_local(local, stack);
	}

	return 0;
}

static int defpackage_update_intern_(addr pos, addr list)
{
	addr name;

	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_table_(pos, name, &name, NULL));
	}

	return 0;
}

static int defpackage_export_(addr pos, addr list)
{
	enum PACKAGE_TYPE type;
	addr name, root, symbol;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_(pos, name, &symbol, &type));
		cons_heap(&root, symbol, root);
	}

	return export_package_(pos, root);
}

static int defpackage_rest_nicknames_(addr pg, addr rest, addr *ret)
{
	int check;
	addr list, pos, type;

	if (GetKeyArgs(rest, KEYWORD_NICKNAMES, &list))
		return Result(ret, Unbound);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}

		/* nickname check */
		Return(find_package_(pos, &pos));
		if (pos != Nil && pos != pg) {
			return call_simple_package_error_va_(NULL,
					":NICKNAMES ~S is already used.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_rest_use_(addr rest, addr *ret)
{
	addr list, pos, type;

	if (GetKeyArgs(rest, KEYWORD_USE, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		if (! package_designator_p(pos)) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_string_(addr rest, constindex index, addr *ret)
{
	addr list, pos, type;

	if (getplist_constant_safe(rest, index, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designator_p(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_import_line_(addr list)
{
	addr pos, type;

	/* package */
	Return_getcons(list, &pos, &list);
	if (! package_designator_p(pos)) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_(NULL, pos, type);
	}

	/* string */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designator_p(pos)) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_(NULL, pos, type);
		}
	}

	return 0;
}

static int defpackage_rest_import_(addr rest, constindex index, addr *ret)
{
	addr list, pos;

	if (getplist_constant_safe(rest, index, &list))
		return Result(ret, Nil);

	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(defpackage_rest_import_line_(pos));
	}

	return 0;
}

static int defpackage_disjoin_shadow_(addr shadow,
		addr intern, addr import, addr shadowing)
{
	int check;
	addr pos;

	while (shadow != Nil) {
		Return_getcons(shadow, &pos, &shadow);
		Check(! string_designator_p(pos), "type error");

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :INTERN.", pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :IMPORT-FROM.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOW ~S already exists in :SHADOWING-IMPORT-FROM.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_import_list_(addr list,
		addr shadow, addr intern, addr shadowing)
{
	int check;
	addr pos;

	/* package name */
	Return_getcdr(list, &list);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :SHADOW.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :SHADOWING-IMPORT-FROM.",
					pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":IMPORT-FROM ~S already exists in :INTERN.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_import_(addr import,
		addr shadow, addr intern, addr shadowing)
{
	addr list;

	while (import != Nil) {
		Return_getcons(import, &list, &import);
		Return(defpackage_disjoin_import_list_(list, shadow, intern, shadowing));
	}

	return 0;
}

static int defpackage_disjoin_shadowing_list_(addr list,
		addr shadow, addr intern, addr import)
{
	int check;
	addr pos;

	/* package name */
	Return_getcdr(list, &list);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :SHADOW.",
					pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :IMPORT-FROM.",
					pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":SHADOWING-IMPORT-FROM ~S already exists in :INTERN.",
					pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_shadowing_(addr shadowing,
		addr shadow, addr intern, addr import)
{
	addr list;

	while (shadowing != Nil) {
		Return_getcons(shadowing, &list, &shadowing);
		Return(defpackage_disjoin_shadowing_list_(list, shadow, intern, import));
	}

	return 0;
}

static int defpackage_disjoin_intern_(addr intern,
		addr shadow, addr import, addr shadowing, addr expt)
{
	int check;
	addr pos;

	while (intern != Nil) {
		Return_getcons(intern, &pos, &intern);
		Check(! string_designator_p(pos), "type error");

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :SHADOW.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :SHADOWING-IMPORT-FROM.",
					pos, NULL);
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN~S already exists in :IMPORT-FROM.", pos, NULL);
		}

		/* export check */
		Return(defpackage_find_list_(pos, expt, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":INTERN ~S already exists in :EXPORT.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_disjoin_export_(addr expt, addr intern)
{
	int check;
	addr pos;

	while (expt != Nil) {
		Return_getcons(expt, &pos, &expt);
		Check(! string_designator_p(pos), "type error");

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_package_error_va_(NULL,
					":EXPORT ~S already exists in :INTERN.", pos, NULL);
		}
	}

	return 0;
}

static int defpackage_update_(Execute ptr, addr pos, addr rest)
{
	addr names, use, shadow, shadowing, import, expt, intern;

	/* &rest */
	Return(defpackage_rest_nicknames_(pos, rest, &names));
	Return(defpackage_rest_use_(rest, &use));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_SHADOW, &shadow));
	Return(defpackage_rest_import_(rest,
				CONSTANT_KEYWORD_SHADOWING_IMPORT_FROM, &shadowing));
	Return(defpackage_rest_import_(rest,
				CONSTANT_KEYWORD_IMPORT_FROM, &import));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_EXPORT, &expt));
	Return(defpackage_rest_string_(rest, CONSTANT_KEYWORD_INTERN, &intern));

	/* Check disjoin */
	Return(defpackage_disjoin_shadow_(shadow, intern, import, shadowing));
	Return(defpackage_disjoin_import_(import, shadow, intern, shadowing));
	Return(defpackage_disjoin_shadowing_(shadowing, shadow, intern, import));
	Return(defpackage_disjoin_intern_(intern, shadow, import, shadowing, expt));
	Return(defpackage_disjoin_export_(expt, intern));

	/*
	 *  The order is
	 *    0. :nicknames
	 *    1. :shadow and :shadowing-import-from.
	 *    2. :use.
	 *    3. :import-from and :intern.
	 *    4. :export.
	 */
	/* nicknames */
	if (names != Unbound) {
		Return(defpackage_update_nicknames_(pos, names));
	}

	/* shadow, shadowing-symbols */
	if (shadow != Nil) {
		Return(shadow_package_(pos, shadow));
	}
	if (shadowing != Nil) {
		Return(defpackage_update_shadowing_(pos, shadowing));
	}

	/* use */
	if (use != Nil) {
		Return(use_package_(pos, use));
	}

	/* import-from */
	if (import != Nil) {
		Return(defpackage_update_import_(ptr->local, pos, import));
	}

	/* intern */
	if (intern != Nil) {
		Return(defpackage_update_intern_(pos, intern));
	}

	/* export */
	if (expt != Nil) {
		Return(defpackage_export_(pos, expt));
	}

	return 0;
}

static int defpackage_make_(Execute ptr, addr pos, addr rest)
{
	int check;
	addr control, save;

	if (! defpackage_update_(ptr, pos, rest))
		return 0;

	/* escape */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (delete_package_(pos, &check))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}


/* size */
static int defpackage_size_(addr rest, int *sizep, size_t *ret)
{
	addr value;

	if (GetKeyArgs(rest, KEYWORD_SIZE, &value)) {
		*sizep = 0;
		*ret = 0;
		return 0;
	}

	*sizep = 1;
	if (GetIndex_integer(value, ret)) {
		*ret = 0;
		return fmte_(":SIZE ~S is too large.", value, NULL);
	}

	return 0;
}


/* documentation */
static int defpackage_documentation_(addr rest, addr *ret)
{
	addr pos;

	/* keyword */
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &pos))
		return Result(ret, Nil);

	/* type check */
	if (! stringp(pos)) {
		*ret = Nil;
		return TypeError_(pos, STRING);
	}

	return Result(ret, pos);
}


/*
 *  defpackage
 */
static int defpackage_execute_make_(Execute ptr,
		addr var, addr rest, int sizep, size_t size, addr *ret)
{
	addr pos;

	if (sizep) {
		Return(package_size_heap_(&pos, var, size));
	}
	else {
		Return(package_heap_(&pos, var));
	}

	Return(defpackage_make_(ptr, pos, rest));
	return Result(ret, pos);
}

static int defpackage_execute_update_(Execute ptr,
		addr pos, addr rest, int sizep, size_t size)
{
	addr hash;

	if (sizep) {
		GetPackage(pos, PACKAGE_INDEX_TABLE, &hash);
		Return(force_resize_hashtable_(hash, size));
	}

	return defpackage_update_(ptr, pos, rest);
}

int defpackage_execute_(Execute ptr, addr var, addr rest, addr *ret)
{
	int sizep;
	addr doc, pos;
	size_t size;

	/* name */
	Return(string_designator_heap_(&var, var, NULL));

	/* :SIZE */
	Return(defpackage_size_(rest, &sizep, &size));

	/* :DOCUMENTATION */
	Return(defpackage_documentation_(rest, &doc));

	/* package */
	Return(find_package_(var, &pos));
	if (pos == Nil) {
		Return(defpackage_execute_make_(ptr, var, rest, sizep, size, &pos));
	}
	else {
		Return(package_designator_update_p_(pos, &pos));
		Return(defpackage_execute_update_(ptr, pos, rest, sizep, size));
	}
	setdocument_package(pos, doc);
	return Result(ret, pos);
}


/*****************************************************************************
 *  Macro COMMON-LISP:DEFPACKAGE
 *****************************************************************************/
static int defpackage_package_designator_common_(addr *value, addr pos, int *ret)
{
	/* type check */
	if (! package_designator_p(pos)) {
		*ret = 0;
		return 0;
	}

	/* object */
	if (packagep(pos))
		getname_package_unsafe(pos, &pos);
	return string_designator_heap_(value, pos, ret);
}

static int defpackage_nicknames_common_(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":NICKNAME ~S must be a string-designator.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
}

static int defpackage_documentation_common_(addr *ret, addr info, addr list)
{
	addr doc, check, type;

	*ret = Nil;
	if (! consp_getcons(list, &doc, &check)) {
		return fmte_(":DOCUMENTATION option ~S don't allow a dotted list.", list, NULL);
	}

	if (! stringp(doc)) {
		GetTypeTable(&type, String);
		return call_type_error_va_(NULL, doc, type,
				":DOCUMENTATION ~S must be a string.", doc, NULL);
	}

	if (check != Nil) {
		return fmte_(":DOCUMENTATION argument ~S must be a single list.", list, NULL);
	}

	if (info != Nil) {
		return call_simple_program_error_va_(NULL,
				":DOCUMENTATION option don't accept "
				"multiple defines ~S and ~S.", info, doc, NULL);
	}

	return Result(ret, doc);
}

static int defpackage_use_common_(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(defpackage_package_designator_common_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, PackageDesignator);
			return call_type_error_va_(NULL, pos, type,
					":USE ~S must be a package-designator.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
}

static int defpackage_shadow_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":SHADOW ~S must be a string-designator.", pos, NULL);
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":SHADOW ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&shadow, pos, shadow);
	}

	return Result(ret, shadow);
}

static int defpackage_shadowing_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	*ret = Nil;
	Return_getcons(list, &pos, &list);
	Return(defpackage_package_designator_common_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_va_(NULL, pos, type,
				":SHADOWING-IMPORT-FROM first argument ~S "
				"must be a package-designator.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":SHADOWING-IMPORT-FROM ~S "
					"must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :SHADOW.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":SHADOWING-IMPORT-FROM ~S "
						"already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, shadowing);
	return 0;
}

static int defpackage_import_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	*ret = Nil;
	Return_getcons(list, &pos, &list);
	Return(defpackage_package_designator_common_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesignator);
		return call_type_error_va_(NULL, pos, type,
				":IMPORT-FROM first argument ~S "
				"must be a package-designator.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":IMPORT-FROM ~S must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S already exists in :SHADOW.", pos, NULL));
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":IMPORT-FROM ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, import);
	return 0;
}

static int defpackage_export_common_(addr *ret, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":EXPORT ~S must be a string-designator.", pos, NULL);
		}

		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			Return(fmtw_(":EXPORT ~S already exists in :INTERN.", pos, NULL));
		}

		/* push */
		cons_heap(&expt, pos, expt);
	}

	return Result(ret, expt);
}

static int defpackage_intern_common_(addr *ret,
		addr shadow, addr shadowing, addr import, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	*ret = Nil;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designator_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesignator);
			return call_type_error_va_(NULL, pos, type,
					":INTERN ~S must be a string-designator.", pos, NULL);
		}

		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :SHADOW.", pos, NULL));
		}

		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S "
						"already exists in :SHADOWING-IMPORT-FROM.", pos, NULL));
		}

		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :IMPORT-FROM.", pos, NULL));
		}

		/* export check */
		Return(defpackage_find_list_(pos, expt, &check));
		if (check) {
			Return(fmtw_(":INTERN ~S already exists in :EXPORT.", pos, NULL));
		}

		/* push */
		cons_heap(&intern, pos, intern);
	}

	return Result(ret, intern);
}

static int defpackage_size_common_(addr *ret, addr info, addr list)
{
	int check;
	addr size, value, type;

	*ret = Nil;
	if (! consp_getcons(list, &size, &value)) {
		return fmte_(":SIZE option ~S don't allow a dotted list.", list, NULL);
	}

	if (! integerp(size)) {
		GetTypeTable(&type, Intplus);
		return call_type_error_va_(NULL, size, type,
				":SIZE ~S must be a positive integer.", size, NULL);
	}

	Return(minusp_integer_(size, &check));
	if (check) {
		GetTypeTable(&type, Intplus);
		return call_type_error_va_(NULL, size, type,
				":SIZE ~S must be a positive integer.", size, NULL);
	}

	if (value != Nil) {
		return fmte_(":SIZE argument ~S must be a single list.", list, NULL);
	}

	if (info != Nil) {
		return call_simple_program_error_va_(NULL,
				":SIZE option don't accept "
				"multiple defines ~S and ~S.", info, size, NULL);
	}

	return Result(ret, size);
}

static int defpackage_expand_common_(addr name, addr form, addr *ret)
{
	addr args, key, list, quote;
	addr knick, kdoc, kuse, kshadow, kshadowing, kimport, kexport, kintern, ksize;
	addr nicknames, doc, use, shadow, shadowing, import, expt, intern, size;

	GetConst(KEYWORD_NICKNAMES, &knick);
	GetConst(KEYWORD_DOCUMENTATION, &kdoc);
	GetConst(KEYWORD_USE, &kuse);
	GetConst(KEYWORD_SHADOW, &kshadow);
	GetConst(KEYWORD_SHADOWING_IMPORT_FROM, &kshadowing);
	GetConst(KEYWORD_IMPORT_FROM, &kimport);
	GetConst(KEYWORD_EXPORT, &kexport);
	GetConst(KEYWORD_INTERN, &kintern);
	GetConst(KEYWORD_SIZE, &ksize);

	nicknames = doc = use = shadow = shadowing = import = expt = intern = size = Nil;
	for (args = form; args != Nil; ) {
		if (! consp_getcons(args, &list, &args)) {
			return fmte_("The defpackage option ~S "
					"don't allow a dotted list.", form, NULL);
		}
		if (! consp_getcons(list, &key, &list)) {
			return call_simple_program_error_va_(NULL,
					"The defpackage option ~S must be a cons.", list, NULL);
		}
		if (key == knick) {
			Return(defpackage_nicknames_common_(&nicknames, nicknames, list));
		}
		else if (key == kdoc) {
			Return(defpackage_documentation_common_(&doc, doc, list));
		}
		else if (key == kuse) {
			Return(defpackage_use_common_(&use, use, list));
		}
		else if (key == kshadow) {
			Return(defpackage_shadow_common_(&shadow,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kshadowing) {
			Return(defpackage_shadowing_common_(&shadowing,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kimport) {
			Return(defpackage_import_common_(&import,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kexport) {
			Return(defpackage_export_common_(&expt, expt, intern, list));
		}
		else if (key == kintern) {
			Return(defpackage_intern_common_(&intern,
						shadow, shadowing, import, expt, intern, list));
		}
		else if (key == ksize) {
			Return(defpackage_size_common_(&size, size, list));
		}
		else {
			return call_simple_program_error_va_(NULL,
					"Invalid defpackage option ~S.", key, NULL);
		}
	}

	/* lisp-system::defpackage */
	nreverse(&nicknames, nicknames);
	nreverse(&use, use);
	nreverse(&shadow, shadow);
	nreverse(&shadowing, shadowing);
	nreverse(&import, import);
	nreverse(&intern, intern);
	GetConst(SYSTEM_DEFPACKAGE, &form);
	GetConst(COMMON_QUOTE, &quote);
	/* (lisp-system::defpackage name ...) */
	list = Nil;
	cons_heap(&list, form, list);
	cons_heap(&list, name, list);
	/* :SIZE */
	if (size != Nil) {
		cons_heap(&list, ksize, list);
		cons_heap(&list, size, list);
	}
	/* :DOCUMENTATION */
	if (doc != Nil) {
		cons_heap(&list, kdoc, list);
		cons_heap(&list, doc, list);
	}
	/* :NICKNAMES */
	if (nicknames != Nil) {
		cons_heap(&list, knick, list);
		list_heap(&nicknames, quote, nicknames, NULL);
		cons_heap(&list, nicknames, list);
	}
	/* :USE */
	if (use != Nil) {
		cons_heap(&list, kuse, list);
		list_heap(&use, quote, use, NULL);
		cons_heap(&list, use, list);
	}
	/* :SHADOW */
	if (shadow != Nil) {
		cons_heap(&list, kshadow, list);
		list_heap(&shadow, quote, shadow, NULL);
		cons_heap(&list, shadow, list);
	}
	/* :SHADOWING-IMPORT-FROM */
	if (shadowing != Nil) {
		cons_heap(&list, kshadowing, list);
		list_heap(&shadowing, quote, shadowing, NULL);
		cons_heap(&list, shadowing, list);
	}
	/* :IMPORT-FROM */
	if (import != Nil) {
		cons_heap(&list, kimport, list);
		list_heap(&import, quote, import, NULL);
		cons_heap(&list, import, list);
	}
	/* :EXPORT */
	if (expt != Nil) {
		cons_heap(&list, kexport, list);
		list_heap(&expt, quote, expt, NULL);
		cons_heap(&list, expt, list);
	}
	/* :INTERN */
	if (intern != Nil) {
		cons_heap(&list, kintern, list);
		list_heap(&intern, quote, intern, NULL);
		cons_heap(&list, intern, list);
	}
	/* result */
	nreverse(ret, list);
	return 0;
}

int defpackage_common_(addr form, addr env, addr *ret)
{
	int check;
	addr name, type;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &name, &form)) {
		return fmte_("DEFPACKAGE argument ~S "
				"must be (name &rest options).", form, NULL);
	}
	Return(string_designator_heap_(&name, name, &check));
	if (! check) {
		GetTypeTable(&type, StringDesignator);
		return call_type_error_va_(NULL, name, type,
				"DEFPACKAGE name ~S must be a string-designator.", name, NULL);
	}

	Return(defpackage_expand_common_(name, form, &form));
	return Result(ret, form);
}

