#include "condition.h"
#include "condition_define.h"
#include "control_object.h"
#include "control_operator.h"
#include "cons.h"
#include "execute_object.h"
#include "format.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_symbol.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  restart conflict
 *    shadow    Make ~S accessible (shadowing ~S).
 *    unintern  Make ~S accessible (unintern ~S).
 *    ignore    Ignore export.
 */
static int restart_shadow_export_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_SHADOW, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string(ptr, &pos,
				"Keep ~S accessible. (shadowing ~S)",
				symbol2, symbol1, NULL));
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

static int restart_unintern_export_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_UNINTERN, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string(ptr, &pos,
				"Make ~S accessible. (unintern ~S)",
				symbol1, symbol2, NULL));
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

static void restart_ignore_export_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(SYSTEM_IGNORE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore export.");
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

static int restart_conflict_export_package_(
		addr package1, addr symbol1,
		addr package2, addr symbol2)
{
	int check;
	addr shadow, unintern, ignore, control;
	Execute ptr;

	ptr = Execute_Thread;
	Return(restart_shadow_export_package_(ptr, &shadow, symbol1, symbol2));
	Return(restart_unintern_export_package_(ptr, &unintern, symbol1, symbol2));
	restart_ignore_export_package(&ignore);

	push_control(ptr, &control);
	pushrestart_control(ptr, ignore);
	pushrestart_control(ptr, unintern);
	pushrestart_control(ptr, shadow);
	(void)call_simple_package_error_va_(NULL,
			"The symbol ~S occer conflict between ~S and ~S.",
			symbol1, package1, package2, NULL);
	if (ptr->throw_control != control)
		goto escape;

	/* shadow */
	if (ptr->throw_handler == shadow) {
		normal_throw_control(ptr);
		Return(shadow_package_(package2, symbol1));
		goto escape;
	}

	/* unintern */
	if (ptr->throw_handler == unintern) {
		normal_throw_control(ptr);
		Return(unintern_package_(package2, symbol2, &check));
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == ignore) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  restart accessible
 *    import  Import the symbol.
 *    ignore  Ignore export.
 */
static void restart_import_export_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IMPORT, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Import the symbol.");
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

static int restart_pop_export_package_(Execute ptr,
		addr control, addr import, addr ignore,
		addr package, addr symbol)
{
	if (ptr->throw_control != control)
		goto escape;

	/* import */
	if (ptr->throw_handler == import) {
		normal_throw_control(ptr);
		Return(import_package_(package, symbol));
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == ignore) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int restart_exist_export_package_(addr package, addr symbol)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_import_export_package(&pos1);
	restart_ignore_export_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	(void)call_simple_package_error_va_(NULL,
			"There is no symbol ~S in package ~S.",
			symbol, package, NULL);
	return restart_pop_export_package_(ptr, control, pos1, pos2, package, symbol);
}

static int restart_access_export_package_(addr package, addr symbol)
{
	addr pos1, pos2, control;
	Execute ptr;

	ptr = Execute_Thread;
	restart_import_export_package(&pos1);
	restart_ignore_export_package(&pos2);

	push_control(ptr, &control);
	pushrestart_control(ptr, pos2);
	pushrestart_control(ptr, pos1);
	(void)call_simple_package_error_va_(NULL,
			"The symbol ~S is not accessible in package ~S.",
			symbol, package, NULL);
	return restart_pop_export_package_(ptr, control, pos1, pos2, package, symbol);
}

static int test_conflict_export_package_(addr package, addr symbol,
		addr *ret, addr *rsymbol)
{
	addr pos, bit, list;

	GetNameSymbol(symbol, &symbol);
	GetPackage(package, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(find_bitpackage_(pos, symbol, &bit));
		if (bit != Nil && ! StructBitType(bit)->shadow) {
			if (rsymbol)
				GetBitTypeSymbol(bit, rsymbol);
			return Result(ret, pos);
		}
	}

	return Result(ret, Nil);
}

static int check_export_package_(addr package, addr symbol)
{
	addr name, bit, pos, sym;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &bit);
	Return(findnil_hashtable_(bit, name, &bit));

	/* exist check */
	if (bit == Nil)
		return restart_exist_export_package_(package, symbol);

	/* accessible check */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return restart_access_export_package_(package, symbol);

	/* already exported */
	if (StructBitType(bit)->expt)
		return 0;

	/* conflict check */
	Return(test_conflict_export_package_(package, symbol, &pos, &sym));
	if (pos != Nil)
		return restart_conflict_export_package_(package, symbol, pos, sym);

	return 0;
}


/*
 *  export
 */
static int intern_export_package_(addr package, addr symbol, addr name)
{
	addr left, right, bit;

	GetPackage(package, PACKAGE_INDEX_USED, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		GetPackage(left, PACKAGE_INDEX_TABLE, &left);
		Return(intern_hashheap_(left, name, &left));
		GetCdr(left, &bit);
		if (bit == Nil) {
			inheritedbitpackage(&bit, symbol);
			SetCdr(left, bit);
		}
		/* If left != Nil, the symbol may be a shadowing symbol. */
	}

	return 0;
}

static int execute_export_package_(addr package, addr symbol)
{
	addr table, bit, name, pos;
	struct bittype_struct *str;

	GetNameSymbol(symbol, &name);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	Return(findnil_hashtable_(table, name, &bit));

	/* exist check */
	if (bit == Nil)
		return 0; /* error, ignore */

	/* accessible check */
	GetBitTypeSymbol(bit, &pos);
	if (pos != symbol)
		return 0; /* error, ignore */

	/* already exported */
	str = StructBitType(bit);
	if (str->expt) {
		/* symbol is already exported. */
		return 0;
	}

	/* conflict check */
	Return(test_conflict_export_package_(package, symbol, &pos, NULL));
	if (pos != Nil)
		return 0; /* error, ignore */

	/* export */
	if (str->inherit) {
		/* If the symbol type is inherited, the type change to import.  */
		str->inherit = 0;
		str->import = 1;
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	else {
		/* export symbol */
		str->expt = 1;
		str->intern = PACKAGE_TYPE_EXTERNAL;
	}
	Return(intern_export_package_(package, symbol, name));
	pushlist_package(package, PACKAGE_INDEX_EXPORT, name);

	return 0;
}

static int symbol_export_package_(addr package, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	Return(check_export_package_(package, symbol));
	return execute_export_package_(package, symbol);
}

static int list_export_package_(addr package, addr args)
{
	addr pos, type, list;

	/* type check */
	Check(! listp(args), "type error");
	for (list = args; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		if (! symbolp(pos)) {
			GetTypeTable(&type, Symbol);
			return call_type_error_va_(NULL, pos, type,
					"export ~S must be a symbol type.", pos, NULL);
		}
	}

	/* check */
	for (list = args; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(check_export_package_(package, pos));
	}

	/* export */
	for (list = args; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(execute_export_package_(package, pos));
	}

	return 0;
}

_g int export_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_export_package_(package, pos);

		case LISPTYPE_CONS:
			return list_export_package_(package, pos);

		default:
			GetTypeTable(&type, SymbolList);
			return call_type_error_va_(NULL, pos, type,
					"export ~S must be a symbol or list.", pos, NULL);
	}
}

