#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "execute_object.h"
#include "execute_values.h"
#include "format.h"
#include "function.h"
#include "hashtable.h"
#include "hold.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_intern.h"
#include "package_shadow.h"
#include "pointer.h"
#include "restart.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/****************************************************************************
 *  Function INTERN
 ****************************************************************************/
_g int intern_package_table_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	int check;
	enum PACKAGE_TYPE result;
	addr keyword, bit;

	/* intern */
	CheckType(package, LISPTYPE_PACKAGE);
	Return(intern_bitpackage_(package, name, &bit, &check));
	result = check? PACKAGE_TYPE_NIL: StructBitType(bit)->intern;
	GetBitTypeSymbol(bit, value);

	/* keyword */
	GetConst(PACKAGE_KEYWORD, &keyword);
	if (keyword == package) {
		Return(setkeyword_package_(*value));
	}

	/* result */
	if (ret)
		return Result(ret, result);

	return 0;
}

_g int intern_package_(addr package, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	Check(! stringp(name), "type error");

	Return(package_designer_(package, &package));
	return intern_package_table_(package, name, value, ret);
}

_g int intern_char_package_(addr package, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr symbol;

	Check(package == NULL, "null error");
	Check(package == Nil, "nil error");
	CheckType(package, LISPTYPE_PACKAGE);

	/* find symbol */
	Return(find_char_bitpackage_(package, name, &symbol));
	if (symbol != Nil) {
		GetBitTypeSymbol(symbol, value);
		if (ret)
			return Result(ret, StructBitType(symbol)->intern);
		return 0;
	}

	/* intern */
	strvect_char_heap(&symbol, name);
	return intern_package_(package, symbol, value, ret);
}


/****************************************************************************
 *  Function UNINTERN
 ****************************************************************************/
/*
 *  Recovery conflict.
 */
static int recovery_update_unintern_package_(addr package, addr symbol, addr var)
{
	addr name, hash, bit, cons;

	Return(delete_list_shadow_package_(package, symbol));
	Return(push_list_shadow_package_(package, var));

	/* Update package */
	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findcons_hashtable_(hash, name, &cons));
	Check(cons == Nil, "find error.");
	importbitpackage(&bit, var);
	SetBitTypeShadow(bit, 1);
	SetCdr(cons, bit);

	return 0;
}

static int recovery_check_name_unintern_package_(addr symbol, addr var)
{
	int check;
	addr x, y;

	GetNameSymbol(symbol, &x);
	GetNameSymbol(var, &y);
	Return(string_equal_(x, y, &check));
	if (! check)
		return fmte_("The argument ~S is not a conflict symbol.", var, NULL);

	return 0;
}

static int recovery_check_intern_unintern_package_(addr package, addr symbol)
{
	addr name, hash, bit;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	Return(findnil_hashtable_(hash, name, &bit));
	if (bit == Nil)
		goto error;
	str = StructBitType(bit);
	if (str->shadow == 0)
		goto error;
	GetBitTypeSymbol(bit, &bit);
	if (bit != symbol)
		goto error;
	return 0;

error:
	return fmte_("Invalid closure data.", NULL);
}

static int recovery_check_inherit_unintern_package_(addr package, addr var)
{
	addr name, list, pos;

	GetNameSymbol(var, &name);
	GetPackage(package, PACKAGE_INDEX_USE, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
		Return(findnil_hashtable_(pos, name, &pos));
		if (pos == Nil)
			continue;
		if (StructBitType(pos)->expt == 0)
			continue;

		return 0;
	}

	return fmte_("The package don't occure conflict.", NULL);
}

static int recovery_unintern_package_(addr package, addr symbol, addr var)
{
	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");
	Check(! symbolp(var), "type error");

	/* check */
	Return(recovery_check_name_unintern_package_(symbol, var));
	Return(recovery_check_intern_unintern_package_(package, symbol));
	Return(recovery_check_inherit_unintern_package_(package, var));
	Return(unexport_package_(package, symbol));

	/* update */
	return recovery_update_unintern_package_(package, symbol, var);
}


/*
 *  restart
 */
static int function_unintern_call(Execute ptr, addr var, addr ignore)
{
	addr list, package, symbol;

	/* type check */
	if (! symbolp(var))
		return TypeError_(var, SYMBOL);

	/* closure */
	getdata_control(ptr, &list);
	List_bind(list, &package, &symbol, NULL);

	/* make shadow */
	return recovery_unintern_package_(package, symbol, var);
}

static int export_symbol_unintern_package_(addr pos, addr name, addr *ret)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	Return(findnil_hashtable_(pos, name, &pos));
	if (pos != Nil && StructBitType(pos)->expt) {
		GetBitTypeSymbol(pos, ret);
		return 0;
	}

	return Result(ret, Unbound);
}

static int list_export_unintern_package_(addr package, addr symbol, addr *ret)
{
	addr list, name, pos, root;

	GetPackage(package, PACKAGE_INDEX_USE, &list);
	GetNameSymbol(symbol, &name);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(export_symbol_unintern_package_(pos, name, &pos));
		if (pos != Unbound)
			cons_heap(&root, pos, root);
	}

	return Result(ret, root);
}

static int function_unintern_input(Execute ptr)
{
	addr list, package, symbol, stream, pos;
	LocalHold hold;

	/* closure */
	getdata_control(ptr, &list);
	List_bind(list, &package, &symbol, NULL);

	hold = LocalHold_array(ptr, 1);
loop:
	Return(list_export_unintern_package_(package, symbol, &list));
	localhold_set(hold, 0, list);
	Return(query_io_stream_(ptr, &stream));
	Return(format_stream(ptr, stream,
				"~2&Select making shadowing-symbols.~2%~5T~S~2%",
				list, NULL));
	strvect_char_heap(&pos, "Input symbol: ");
	Return(prompt_for_stream(ptr, T, pos, &pos));
	if (! find_list_eq_unsafe(pos, list)) {
		Return(format_stream(ptr, stream,
					"~%ERROR: Invalid input ~S.~%Please input again.~2%",
					pos, NULL));
		goto loop;
	}
	Return(format_stream(ptr, stream, "~%Select ~S symbol.~2%", pos, NULL));

	/* result */
	list_heap(&list, pos, NULL);
	setresult_control(ptr, list);

	return 0;
}

static void compiled_call_unintern_package_(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_var1opt1(pos, p_defun_unintern_call);
	*ret = pos;
}

static void compiled_input_unintern_package_(addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_unintern_input);
	*ret = pos;
}

static void restart_shadow_unintern_package_(addr *ret, addr package, addr symbol)
{
	addr restart, pos, list;

	/* closure */
	list_heap(&list, package, symbol, NULL);
	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Make an another symbol shadowing.");
	setreport_restart(restart, pos);
	/* function */
	compiled_call_unintern_package_(&pos);
	SetDataFunction(pos, list);
	setfunction_restart(restart, pos);
	/* interactive */
	compiled_input_unintern_package_(&pos);
	SetDataFunction(pos, list);
	setinteractive_restart(restart, pos);
	/* condition */
	GetConst(CONDITION_PACKAGE_ERROR, &pos);
	setcondition_restart(restart, pos);
	/* restart-case */
	setescape_restart(restart, 1);

	*ret = restart;
}

static void restart_continue_unintern_package_(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IGNORE, &pos);
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

static int restart_unintern_package_(addr package, addr symbol, int *ret)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_shadow_unintern_package_(&pos1, package, symbol);
	restart_continue_unintern_package_(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	*ret = 0;
	(void)call_simple_package_error_va_(ptr,
			"Name conflict occured ~S in the ~S package.",
			symbol, package, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* shadow */
	if (ptr->throw_handler == pos1) {
		normal_throw_control(ptr);
		*ret = 1;
		goto escape;
	}

	/* continue */
	if (ptr->throw_handler == pos2) {
		normal_throw_control(ptr);
		*ret = 0;
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  unintern
 */
static int check_export_unintern_package_(addr pos, addr name, int *ret)
{
	int check;

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	Return(findnil_hashtable_(pos, name, &pos));
	check = (pos != Nil && StructBitType(pos)->expt);

	return Result(ret, check);
}

static int check_shadowing_unintern_package_(addr package, addr name, int *ret)
{
	int loop, check;
	addr pos, list;

	GetPackage(package, PACKAGE_INDEX_USE, &list);
	loop = 0;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(check_export_unintern_package_(pos, name, &check));
		if (check) {
			if (loop)
				return Result(ret, 1);
			loop = 1;
		}
	}

	return Result(ret, 0);
}

static int check_unintern_package_(addr package, addr symbol, int *ret, int *value)
{
	int check;
	addr bit, name, pos;

	/* exist check */
	GetPackage(package, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(pos, name, &bit));
	*value = 0;
	/* no symbol */
	if (bit == Nil)
		return Result(ret, 0);
	/* other symbol */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return Result(ret, 0);
	/* inherit symbol */
	if (StructBitType(bit)->inherit)
		return Result(ret, 0);

	/* conflict check */
	if (StructBitType(bit)->shadow) {
		Return(check_shadowing_unintern_package_(package, name, &check));
		if (check) {
			Return(restart_unintern_package_(package, symbol, &check));
			*value = 1;
			return Result(ret, check);
		}
	}

	return Result(ret, 1);
}

static int remove_unintern_package_(addr package, addr symbol, int *ret)
{
	int check;
	addr name, table, bit, pos;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findnil_hashtable_(table, name, &bit));
	Check(bit == Nil, "hashtable error");
	str = StructBitType(bit);
	/* base */
	if (str->base) {
		GetBitTypeSymbol(bit, &pos);
		SetPackageSymbol(pos, Nil); /* gensym */
	}
	Return(delete_hashtable_(table, name, &check));

	return Result(ret, StructBitType(bit)->shadow);
}

static int intern_inherited_unintern_package_(addr package, addr name)
{
	addr pos, table, list, bit, cons;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetPackage(package, PACKAGE_INDEX_USE, &list);

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
		Return(findnil_hashtable_(pos, name, &bit));
		if (bit == Nil)
			continue;
		if (StructBitType(bit)->expt == 0)
			continue;

		/* intern inherited */
		GetBitTypeSymbol(bit, &pos);
		inheritedbitpackage(&bit, pos);
		Return(intern_hashheap_(table, name, &cons));
		SetCdr(cons, bit);
		return 0;
	}

	return 0;
}

static int symbol_unintern_package_(addr package, addr symbol)
{
	int check;
	addr name;

	GetNameSymbol(symbol, &name);
	Return(remove_unintern_package_(package, symbol, &check));
	if (check) {
		/* shadowing-symbols */
		Return(intern_inherited_unintern_package_(package, name));
		Return(delete_list_shadow_package_(package, symbol));
	}

	return 0;
}

_g int unintern_package_(addr package, addr symbol, int *ret)
{
	int check, value;

	Check(! symbolp(symbol), "type error");
	Return(package_designer_(package, &package));

	/* check */
	Return(check_unintern_package_(package, symbol, &check, &value));
	if (ret)
		*ret = check;
	if (value)
		return 0;
	if (! check)
		return 0;

	/* unintern */
	return symbol_unintern_package_(package, symbol);
}


/****************************************************************************
 *  Interface
 ****************************************************************************/
_g int setkeyword_package_(addr pos)
{
	addr check, package;

	GetConst(PACKAGE_KEYWORD, &package);
	Return(export_package_(package, pos));
	GetValueSymbol(pos, &check);
	if (check == Unbound) {
		SetValueSymbol(pos, pos);
		SetStatusReadOnly(pos);
	}

	return 0;
}

_g int intern_default_package_(Execute ptr, addr name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_package_(package, name, value, ret);
}

_g int internchar_(const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package, name;

	Check(pname == NULL, "argument package error");
	Check(sname == NULL, "argument name error");

	/* find package */
	Return(find_char_package_(pname, &package));
	if (package == Nil) {
		strvect_char_heap(&name, pname);
		return fmte_("No such a package ~S.", name, NULL);
	}

	return intern_char_package_(package, sname, value, ret);
}

_g int internchar_default_(Execute ptr, const char *name,
		addr *value, enum PACKAGE_TYPE *ret)
{
	addr package;
	Return(getpackage_(ptr, &package));
	return intern_char_package_(package, name, value, ret);
}

_g int internchar_null_(Execute ptr, const char *pname, const char *sname,
		addr *value, enum PACKAGE_TYPE *ret)
{
	if (pname)
		return internchar_(pname, sname, value, ret);
	else
		return internchar_default_(ptr, sname, value, ret);
}

_g int internchar_keyword_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr pos;

	GetConst(PACKAGE_KEYWORD, &pos);
	Return(intern_char_package_(pos, name, &pos, &type));
	Return(setkeyword_package_(pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}

_g int interncommon_(const char *name, addr *value, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	addr package, pos;

	GetConst(PACKAGE_COMMON_LISP, &package);
	Return(intern_char_package_(package, name, &pos, &type));
	Return(export_package_(package, pos));
	*value = pos;

	if (ret)
		return Result(ret, type);
	else
		return 0;
}


/****************************************************************************
 *  Debug
 ****************************************************************************/
_g void internchar_debug(const char *pname, const char *sname, addr *value)
{
	Error(internchar_(pname, sname, value, NULL));
}

_g void internchar_keyword_debug(const char *name, addr *value)
{
	Error(internchar_keyword_(name, value, NULL));
}

_g void interncommon_debug(const char *name, addr *value)
{
	Error(interncommon_(name, value, NULL));
}

_g addr interncharr_debug(const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_(pname, sname, &pos, NULL));
	return pos;
}

_g addr interncharr_null_debug(Execute ptr, const char *pname, const char *sname)
{
	addr pos;
	pos = NULL;
	Error(internchar_null_(ptr, pname, sname, &pos, NULL));
	return pos;
}

_g addr interncommonr_debug(const char *name)
{
	addr pos;
	pos = NULL;
	Error(interncommon_(name, &pos, NULL));
	return pos;
}


/*
 *  initialize
 */
_g void init_package_intern(void)
{
	SetPointerCall(defun, var1opt1, unintern_call);
	SetPointerCall(defun, empty, unintern_input);
}

