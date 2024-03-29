#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute_object.h"
#include "function.h"
#include "hashtable.h"
#include "hold.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designator.h"
#include "package_make.h"
#include "package_shadow.h"
#include "prompt_for.h"
#include "restart.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  restart name
 *    continue  Use (find-package name).
 *    input     Use input name.
 */
static void restart_continue_name_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Return the existing package.");
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

static void compiled_input_name_make_package(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_make_package_input);
	*ret = pos;
}

static void restart_input_name_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(SYSTEM_INPUT, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Input another package name.");
	setreport_restart(restart, pos);
	/* function */
	GetConst(FUNCTION_VALUES, &pos);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_input_name_make_package(&pos);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static int function_make_package_input(Execute ptr)
{
	addr type, prompt, pos;

	GetTypeTable(&type, PackageDesignator);
	strvect_char_heap(&prompt, "Input another package name: ");
	Return(prompt_for_stream_(ptr, type, prompt, &pos));
	list_heap(&pos, pos, NULL);
	setresult_control(ptr, pos);

	return 0;
}

static int error_make_package_(addr name, addr *ret)
{
	addr table, pos;

	PackageTable(&table);
	Return(findcons_hashtable_(table, name, &pos));
	if (pos != Nil) {
		return call_simple_package_error_va_(NULL,
				"Package ~S already exists.", name, NULL);
	}

	/* name ok */
	*ret = name;
	return 0;
}

enum MakePackageType {
	MakePackageName,
	MakePackageObject,
	MakePackageLoop
};

static int throw_name_make_package_(Execute ptr, addr name,
		addr pos1, addr pos2, addr *value, enum MakePackageType *ret)
{
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*ret = MakePackageObject;
		return find_package_(name, value);
	}
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		*ret = MakePackageLoop;
		getresult_control(ptr, value);
		return 0;
	}

	/* invalid operator */
	*ret = MakePackageName;
	return 1;
}

static int loop_name_make_package_(Execute ptr,
		addr name, addr *value, enum MakePackageType *ret)
{
	addr pos1, pos2, control;

	restart_continue_name_make_package(&pos1);
	restart_input_name_make_package(&pos2);
	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	*value = Nil;
	*ret = MakePackageName;
	if (error_make_package_(name, value)) {
		if (ptr->throw_control == control)
			(void)throw_name_make_package_(ptr, name, pos1, pos2, value, ret);
	}

	return pop_control_(ptr, control);
}

static int name_make_package_(Execute ptr, LocalHold hold,
		addr name, addr *value, int *ret)
{
	enum MakePackageType type;

	do {
		Return(string_designator_heap_(&name, name, NULL));
		localhold_set(hold, 0, name);
		Return(loop_name_make_package_(ptr, name, &name, &type));
		localhold_set(hold, 0, name);
	}
	while (type == MakePackageLoop);

	*ret = (type == MakePackageObject);
	return Result(value, name);
}


/*
 *  restart nicknames
 *    continue  Remove list.
 */
static void restart_continue_nicknames_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Remove names of conflict in :nicknames list.");
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

static int error_nicknames_make_package_(Execute ptr, addr remove)
{
	addr restart, control;

	restart_continue_nicknames_make_package(&restart);
	push_control(ptr, &control);
	pushrestart_control(ptr, restart);
	(void)call_simple_package_error_va_(NULL,
			"Nicknames ~S already exist.", remove, NULL);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	normal_throw_control(ptr);
escape:
	return pop_control_(ptr, control);
}

static int remove_nicknames_make_package_(Execute ptr, addr list, addr *ret)
{
	addr pos, cons, table, root;

	PackageTable(&table);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, NULL));
		Return(findcons_hashtable_(table, pos, &cons));
		if (cons == Nil)
			cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static int conflict_nicknames_make_package_(addr list, addr *ret)
{
	addr pos, cons, table, root;

	PackageTable(&table);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(string_designator_heap_(&pos, pos, NULL));
		Return(findcons_hashtable_(table, pos, &cons));
		if (cons != Nil) {
			GetCdr(cons, &cons);
			cons_heap(&root, cons, root);
		}
	}

	return Result(ret, root);
}

static int nicknames_make_package_(Execute ptr, addr list, addr *ret)
{
	addr remove;

	Return(conflict_nicknames_make_package_(list, &remove));
	if (remove != Nil) {
		Return(error_nicknames_make_package_(ptr, remove));
		Return(remove_nicknames_make_package_(ptr, list, &list));
	}

	return Result(ret, list);
}


/*
 *  restart use
 *    continue    Ignore :use list.
 *    shadow      Make shadowing symbols.
 */
static void restart_continue_use_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore :use list.");
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

static void restart_shadow_use_make_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Make newly symbols shadowing.");
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

static int error_use_make_package_(Execute ptr,
		addr list, addr shadow,
		addr *rlist, addr *rshadow)
{
	addr pos1, pos2, control;

	restart_continue_use_make_package(&pos1);
	restart_shadow_use_make_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);

	(void)call_simple_package_error_va_(NULL,
			"Symbols ~S conflict occured.", shadow, NULL);
	*rlist = list;
	*rshadow = shadow;

	/* others */
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*rlist = *rshadow = Nil;
		goto escape;
	}

	/* shadowing */
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int pushnew_use_make_package_(addr pos, addr name, addr list, addr *ret)
{
	enum PACKAGE_TYPE type;

	Return(find_symbol_package_(pos, name, &pos, &type));
	Check(pos == Nil, "find_symbol error");
	pushnew_heap(list, pos, &list);

	return Result(ret, list);
}

static int equal_use_make_package_(addr pos1, addr pos2, addr x, addr y, int *ret)
{
	int check;
	enum PACKAGE_TYPE ignore;

	Return(string_equal_(x, y, &check));
	if (! check)
		return Result(ret, 0);

	Return(find_symbol_package_(pos1, x, &x, &ignore));
	Return(find_symbol_package_(pos2, y, &y, &ignore));
	return Result(ret, x != y);
}

static int conflict_use_make_package_(addr pos1, addr pos2, addr shadow, addr *ret)
{
	int check;
	addr exp1, exp2, x, y, loop;

	GetPackage(pos1, PACKAGE_INDEX_EXPORT, &exp1);
	GetPackage(pos2, PACKAGE_INDEX_EXPORT, &exp2);
	while (exp1 != Nil) {
		GetCons(exp1, &x, &exp1);
		for (loop = exp2; loop != Nil; ) {
			GetCons(loop, &y, &loop);
			Return(equal_use_make_package_(pos1, pos2, x, y, &check));
			if (check) {
				Return(pushnew_use_make_package_(pos1, x, shadow, &shadow));
				Return(pushnew_use_make_package_(pos2, y, shadow, &shadow));
			}
		}
	}

	return Result(ret, shadow);
}

static int check_use_make_package_(addr list, addr *ret)
{
	addr x, y, loop, shadow;

	shadow = Nil;
	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(package_designator_(x, &x));
		for (loop = list; loop != Nil; ) {
			GetCons(loop, &y, &loop);
			Return(package_designator_(y, &y));
			Return(conflict_use_make_package_(x, y, shadow, &shadow));
		}
	}

	return Result(ret, shadow);
}

static int use_make_package_(Execute ptr, LocalHold hold,
		addr list, addr *ret, addr *rshadow)
{
	addr shadow;

	Return(check_use_make_package_(list, &shadow));
	if (shadow == Nil) {
		*rshadow = Nil;
		return Result(ret, list);
	}

	/* error */
	localhold_set(hold, 3, shadow);
	Return(error_use_make_package_(ptr, list, shadow, &list, &shadow));
	localhold_set(hold, 2, list);
	localhold_set(hold, 3, shadow);

	*rshadow = shadow;
	return Result(ret, list);
}


/*
 *  make-package
 */
static int append_exportname_package_(addr pos, addr left, addr name)
{
	addr bit, cons, one;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &one);
	Return(intern_hashheap_(one, name, &cons));

	/* duplicate check (if same package in use list.) */
	GetCdr(cons, &one);
	if (one == Nil) {
		/* make bitpackage */
		Return(findnil_hashtable_(left, name, &one));
		Check(one == Nil, "export nil error");
		Check(StructBitType(one)->expt == 0, "export error");
		GetBitTypeSymbol(one, &one);
		inheritedbitpackage(&bit, one);
		/* push package */
		SetCdr(cons, bit);
	}

	return 0;
}

static void pushnew_list_package(addr package, enum PACKAGE_INDEX index, addr pos)
{
	addr list;

	GetPackage(package, index, &list);
	if (find_list_eq_unsafe(pos, list) == 0) {
		cons_heap(&list, pos, list);
		SetPackage(package, index, list);
	}
}

static int append_usepackage_package_(addr pos, addr list)
{
	addr pg, hash, expt, name;

	while (list != Nil) {
		/* intern export */
		GetCons(list, &pg, &list);
		Return(package_designator_(pg, &pg));
		GetPackage(pg, PACKAGE_INDEX_EXPORT, &expt);
		GetPackage(pg, PACKAGE_INDEX_TABLE, &hash);
		while (expt != Nil) {
			GetCons(expt, &name, &expt);
			Return(append_exportname_package_(pos, hash, name));
		}

		/* push use-list, used-by-list */
		pushnew_list_package(pos, PACKAGE_INDEX_USE, pg);
		pushnew_list_package(pg, PACKAGE_INDEX_USED, pos);
	}

	return 0;
}

int make_package_(Execute ptr, addr name, addr names, addr use, addr *ret)
{
	int check;
	addr pos, shadow;
	LocalHold hold;

	hold = LocalHold_array(ptr, 4);
	localhold_set(hold, 0, name);
	localhold_set(hold, 1, names);
	localhold_set(hold, 2, use);

	/* name */
	Return(name_make_package_(ptr, hold, name, &name, &check));
	if (check)
		return Result(ret, name);

	/* nicknames */
	Return(nicknames_make_package_(ptr, names, &names));
	localhold_set(hold, 1, names);

	/* use-package */
	Return(use_make_package_(ptr, hold, use, &use, &shadow));

	/* make package */
	Return(package_heap_(&pos, name));
	if (shadow != Nil) {
		Return(shadow_package_(pos, shadow));
	}
	Return(append_nicknames_package_(pos, names));
	Return(append_usepackage_package_(pos, use));

	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  initialize
 */
void init_package_make(void)
{
	SetPointerCall(defun, empty, make_package_input);
}

