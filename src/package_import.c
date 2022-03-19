#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "constant.h"
#include "control_object.h"
#include "execute_object.h"
#include "format.h"
#include "hashtable.h"
#include "hold.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_import.h"
#include "package_intern.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  restart
 */
static int restart_force_import_package_(Execute ptr,
		addr *ret, addr symbol1, addr symbol2)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IMPORT, &pos);
	restart_heap(&restart, pos);
	/* report */
	Return(format_string_(ptr, &pos,
				"Import ~S and unintern ~S.", symbol1, symbol2, NULL));
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

static void restart_ignore_import_package(addr *ret)
{
	addr restart, pos;

	/* name */
	GetConst(COMMON_IGNORE, &pos);
	restart_heap(&restart, pos);
	/* report */
	strvect_char_heap(&pos, "Ignore import.");
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

static int restart_import_package_(Execute ptr,
		addr package, addr symbol1, addr symbol2)
{
	addr force, ignore, control;

	Return(restart_force_import_package_(ptr, &force, symbol1, symbol2));
	restart_ignore_import_package(&ignore);

	push_control(ptr, &control);
	pushrestart_control(ptr, ignore);
	pushrestart_control(ptr, force);
	(void)call_simple_package_error_va_(ptr,
			"IMPORT symbol ~S and ~S occer conflict.", symbol1, symbol2, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* force import */
	if (ptr->throw_handler == force) {
		normal_throw_control(ptr);
		Return(unintern_package_(package, symbol2, NULL));
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
 *  import
 */
static int exist_import_package(addr symbol, addr bit, addr *ret)
{
	enum PACKAGE_TYPE type;
	struct bittype_struct *str;
	addr check;

	GetBitTypeIntern(bit, &type);
	if (type == PACKAGE_TYPE_INHERITED) {
		/* inherited -> import */
		str = StructBitType(bit);
		str->intern = PACKAGE_TYPE_INTERNAL;
		str->import = 1;
		str->inherit = 0;
		*ret = bit;
		return 0;
	}
	else {
		/* conflict or shadowing-symbol */
		*ret = bit;
		GetBitTypeSymbol(bit, &check);
		return check != symbol;
	}
}

int import_bitpackage_(addr package, addr symbol, addr *value, int *ret)
{
	addr table, name, bit, check;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetNameSymbol(symbol, &name);

	/* intern check */
	Return(findnil_hashtable_(table, name, &bit));
	if (bit != Nil) {
		*ret = exist_import_package(symbol, bit, value);
		return 0;
	}

	/* intern hashtable */
	GetPackageSymbol(symbol, &check);
	if (check == Nil) {
		/* intern */
		Return(intern_hashheap_(table, name, &check));
		internbitpackage(&bit, symbol);
		SetCdr(check, bit);
		/* set package */
		SetPackageSymbol(symbol, package);
	}
	else {
		/* import */
		Return(intern_hashheap_(table, name, &check));
		importbitpackage(&bit, symbol);
		SetCdr(check, bit);
	}

	*value = bit;
	return Result(ret, 0);
}

static int execute1_import_package_(Execute ptr, addr package, addr table, addr pos)
{
	addr type, bit;

	/* type check */
	if (! symbolp(pos)) {
		GetTypeTable(&type, Symbol);
		return call_type_error_va_(NULL, pos, type,
				"IMPORT ~S must be a symbol type.", pos, NULL);
	}

	/* intern check */
	GetNameSymbol(pos, &bit);
	Return(findnil_hashtable_(table, bit, &bit));
	if (bit != Nil) {
		GetBitTypeSymbol(bit, &bit);
		if (pos != bit) {
			Return(restart_import_package_(ptr, package, pos, bit));
		}
	}

	return 0;
}

static int execute_import_package_(Execute ptr, addr package, addr list)
{
	int check;
	addr table, pos;

	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(execute1_import_package_(ptr, package, table, pos));
		Return(import_bitpackage_(package, pos, &pos, &check));
	}

	return 0;
}

static int list_import_package_(addr package, addr args)
{
	addr control;
	Execute ptr;
	LocalHold hold;

	ptr = Execute_Thread;
	push_control(ptr, &control);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, package, args, NULL);
	(void)execute_import_package_(ptr, package, args);
	return pop_control_(ptr, control);
}

static int symbol_import_package_(addr package, addr pos)
{
	list_heap(&pos, pos, NULL);
	return list_import_package_(package, pos);
}

int import_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_update_p_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_import_package_(package, pos);

		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return list_import_package_(package, pos);

		default:
			GetTypeTable(&type, SymbolList);
			return call_type_error_va_(NULL, pos, type,
					"IMPORT ~S must be a symbol or list.", pos, NULL);
	}
}

