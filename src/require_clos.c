#include "callname.h"
#include "condition.h"
#include "constant.h"
#include "control_object.h"
#include "execute.h"
#include "execute_object.h"
#include "mop_reader.h"
#include "package_object.h"
#include "require.h"
#include "restart.h"
#include "require_clos.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

static void require_clos_restart_continue(Execute ptr, addr *ret)
{
	addr restart, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&restart, pos);
	strvect_char_heap(&pos, "Retry to make function.");
	setreport_restart(restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setescape_restart(restart, 1);
	*ret = restart;
}

static void require_clos_restart_delete(Execute ptr, addr *ret)
{
	addr restart, pos;

	GetConst(COMMON_DELETE, &pos);
	restart_heap(&restart, pos);
	strvect_char_heap(&pos, "Delete an original function, and retry.");
	setreport_restart(restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setescape_restart(restart, 1);
	*ret = restart;
}

static void require_clos_restart_ignore(Execute ptr, addr *ret)
{
	addr restart, pos;

	GetConst(COMMON_IGNORE, &pos);
	restart_heap(&restart, pos);
	strvect_char_heap(&pos, "Keep an original function, and ignore making.");
	setreport_restart(restart, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(restart, pos);
	setescape_restart(restart, 1);
	*ret = restart;
}

static void require_clos_unbound(addr pos)
{
	CallNameType type;
	addr symbol;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &symbol);
	GetCallNameType(pos, &type);
	if (type == CALLNAME_SYMBOL) {
		SetFunctionSymbol(symbol, Unbound);
	}
	else {
		remsetf_symbol(symbol);
	}
}

static int require_clos_error_(Execute ptr, addr callname, int *ret)
{
	addr restart1, restart2, restart3, control;

	require_clos_restart_continue(ptr, &restart1);
	require_clos_restart_delete(ptr, &restart2);
	require_clos_restart_ignore(ptr, &restart3);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart3);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);

	(void)fmte_("Generic function ~S is already exist.", callname, NULL);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* retry */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		*ret = 1;
		goto escape;
	}

	/* delete */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		require_clos_unbound(callname);
		*ret = 1;
		goto escape;
	}

	/* ignore */
	if (ptr->throw_handler == restart3) {
		normal_throw_control(ptr);
		*ret = 0;
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}

static int require_clos_function_(Execute ptr, constindex index, int (*make)(Execute))
{
	int restartp;
	addr pos, value;

restart:
	GetConstant(index, &pos);
	GetFunctionSymbol(pos, &value);
	if (value == Unbound)
		return (*make)(ptr);

	/* error */
	callname_heap(&pos, pos, CALLNAME_SYMBOL);
	Return(require_clos_error_(ptr, pos, &restartp));
	if (restartp)
		goto restart;

	return 0;
}

static int require_clos_setf_(Execute ptr, constindex index, int (*make)(Execute))
{
	int restartp;
	addr pos, value;

restart:
	GetConstant(index, &pos);
	getsetf_symbol(pos, &value);
	if (value == Unbound)
		return (*make)(ptr);

	/* error */
	callname_heap(&pos, pos, CALLNAME_SETF);
	Return(require_clos_error_(ptr, pos, &restartp));
	if (restartp)
		goto restart;

	return 0;
}

#define Return_defgeneric_class_(ptr, x, y) { \
	Return(require_clos_function_(ptr, \
				CONSTANT_CLOSNAME_CLASS_##x, \
				defgeneric_class_##y##_)); \
}
#define Return_defgeneric_slot_definition_(ptr, x, y) { \
	Return(require_clos_function_(ptr, \
				CONSTANT_CLOSNAME_SLOT_DEFINITION_##x, \
				defgeneric_slot_definition_##y##_)); \
}
#define Return_defgeneric_generic_function_(ptr, x, y) { \
	Return(require_clos_function_(ptr, \
				CONSTANT_CLOSNAME_GENERIC_FUNCTION_##x, \
				defgeneric_generic_function_##y##_)); \
}
#define Return_defgeneric_method_(ptr, x, y) { \
	Return(require_clos_function_(ptr, \
				CONSTANT_CLOSNAME_METHOD_##x, \
				defgeneric_method_##y##_)); \
}

static int require_clos_module_call_(Execute ptr)
{
	/* class */
	Return_defgeneric_class_(ptr, SLOTS, slots);
	Return_defgeneric_class_(ptr, DIRECT_SLOTS, direct_slots);
	Return_defgeneric_class_(ptr, DEFAULT_INITARGS, default_initargs);
	Return_defgeneric_class_(ptr, DIRECT_DEFAULT_INITARGS, direct_default_initargs);
	Return_defgeneric_class_(ptr, PRECEDENCE_LIST, precedence_list);
	Return_defgeneric_class_(ptr, DIRECT_SUPERCLASSES, direct_superclasses);
	Return_defgeneric_class_(ptr, DIRECT_SUBCLASSES, direct_subclasses);
	Return_defgeneric_class_(ptr, FINALIZED_P, finalized_p);
	Return_defgeneric_class_(ptr, PROTOTYPE, prototype);
	/* slot-definitions */
	Return_defgeneric_slot_definition_(ptr, NAME, name);
	Return_defgeneric_slot_definition_(ptr, TYPE, type);
	Return_defgeneric_slot_definition_(ptr, ALLOCATION, allocation);
	Return_defgeneric_slot_definition_(ptr, INITARGS, initargs);
	Return_defgeneric_slot_definition_(ptr, INITFORM, initform);
	Return_defgeneric_slot_definition_(ptr, INITFUNCTION, initfunction);
	/* generic-functions */
	Return_defgeneric_generic_function_(ptr, NAME, name);
	Return(require_clos_setf_(ptr,
				CONSTANT_CLOSNAME_GENERIC_FUNCTION_NAME,
				defgeneric_setf_generic_function_name_));
	Return_defgeneric_generic_function_(ptr, METHODS, methods);
	Return_defgeneric_generic_function_(ptr, LAMBDA_LIST, lambda_list);
	Return_defgeneric_generic_function_(ptr, ARGUMENT_PRECEDENCE_ORDER, argument_precedence_order);
	Return_defgeneric_generic_function_(ptr, DECLARATIONS, declarations);
	Return_defgeneric_generic_function_(ptr, METHOD_CLASS, method_class);
	Return_defgeneric_generic_function_(ptr, METHOD_COMBINATION, method_combination);
	/* methods */
	Return_defgeneric_method_(ptr, FUNCTION, function);
	Return_defgeneric_method_(ptr, GENERIC_FUNCTION, generic_function);
	Return_defgeneric_method_(ptr, LAMBDA_LIST, lambda_list);
	Return_defgeneric_method_(ptr, SPECIALIZERS, specializers);
	Return(require_clos_function_(ptr,
				CONSTANT_CLOSNAME_ACCESSOR_METHOD_SLOT_DEFINITION,
				defgeneric_accessor_method_slot_definition_));

	return 0;
}

static void unrequire_clos_unintern_function(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	SetFunctionSymbol(symbol, Unbound);
}

static void unrequire_clos_unintern_setf(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	remsetf_symbol(symbol);
}

#define UnRequireUninternFunction(x) \
	unrequire_clos_unintern_function(CONSTANT_CLOSNAME_##x)
#define UnRequireUninternSetf(x) \
	unrequire_clos_unintern_setf(CONSTANT_CLOSNAME_##x)

static int unrequire_clos_module_call_(Execute ptr)
{
	/* class */
	UnRequireUninternFunction(CLASS_SLOTS);
	UnRequireUninternFunction(CLASS_DIRECT_SLOTS);
	UnRequireUninternFunction(CLASS_DEFAULT_INITARGS);
	UnRequireUninternFunction(CLASS_DIRECT_DEFAULT_INITARGS);
	UnRequireUninternFunction(CLASS_PRECEDENCE_LIST);
	UnRequireUninternFunction(CLASS_DIRECT_SUPERCLASSES);
	UnRequireUninternFunction(CLASS_DIRECT_SUBCLASSES);
	UnRequireUninternFunction(CLASS_FINALIZED_P);
	UnRequireUninternFunction(CLASS_PROTOTYPE);
	/* slot-definitions */
	UnRequireUninternFunction(SLOT_DEFINITION_NAME);
	UnRequireUninternFunction(SLOT_DEFINITION_TYPE);
	UnRequireUninternFunction(SLOT_DEFINITION_ALLOCATION);
	UnRequireUninternFunction(SLOT_DEFINITION_INITARGS);
	UnRequireUninternFunction(SLOT_DEFINITION_INITFORM);
	UnRequireUninternFunction(SLOT_DEFINITION_INITFUNCTION);
	/* generic-functions */
	UnRequireUninternFunction(GENERIC_FUNCTION_NAME);
	UnRequireUninternSetf(GENERIC_FUNCTION_NAME);
	UnRequireUninternFunction(GENERIC_FUNCTION_METHODS);
	UnRequireUninternFunction(GENERIC_FUNCTION_LAMBDA_LIST);
	UnRequireUninternFunction(GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER);
	UnRequireUninternFunction(GENERIC_FUNCTION_DECLARATIONS);
	UnRequireUninternFunction(GENERIC_FUNCTION_METHOD_CLASS);
	UnRequireUninternFunction(GENERIC_FUNCTION_METHOD_COMBINATION);
	/* methods */
	UnRequireUninternFunction(METHOD_FUNCTION);
	UnRequireUninternFunction(METHOD_GENERIC_FUNCTION);
	UnRequireUninternFunction(METHOD_LAMBDA_LIST);
	UnRequireUninternFunction(METHOD_SPECIALIZERS);
	UnRequireUninternFunction(ACCESSOR_METHOD_SLOT_DEFINITION);

	return 0;
}

static int require_clos_module_(Execute ptr)
{
	int check, readonlyp;
	addr package;

	GetConst(PACKAGE_CLOS, &package);
	readonlyp = get_readonly_package(package);
	set_readonly_package(package, 0);
	check = require_clos_module_call_(ptr);
	set_readonly_package(package, readonlyp);

	return check;
}

static int unrequire_clos_module_(Execute ptr)
{
	int check, readonlyp;
	addr package;

	GetConst(PACKAGE_CLOS, &package);
	readonlyp = get_readonly_package(package);
	set_readonly_package(package, 0);
	check = unrequire_clos_module_call_(ptr);
	set_readonly_package(package, readonlyp);

	return check;
}


/*
 *  interface
 */
int require_clos_(Execute ptr, addr var, int forcep, int *ret)
{
	int check;

	/* module-name */
	Return(string_equal_char_(var, LISPNAME "-CLOS", &check));
	if (! check)
		return Result(ret, 0);

	/* forcep */
	if (forcep)
		goto append_module;

	/* *modules* */
	Return(modules_find_(ptr, var, &check));
	if (check)
		return Result(ret, 1);

	/* append */
append_module:
	Return(require_clos_module_(ptr));
	Return(provide_common_(ptr, var));
	return Result(ret, 1);
}

int unrequire_clos_(Execute ptr, addr var, int forcep, int *ret)
{
	int check;

	/* module-name */
	Return(string_equal_char_(var, LISPNAME "-CLOS", &check));
	if (! check)
		return Result(ret, 0);

	/* force */
	if (forcep)
		goto delete_module;

	/* *modules* */
	Return(modules_find_(ptr, var, &check));
	if (! check)
		return Result(ret, 1);

	/* delete */
delete_module:
	Return(unrequire_clos_module_(ptr));
	Return(modules_delete_(ptr, var));
	return Result(ret, 1);
}

