#include "call_data.h"
#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "execute_object.h"
#include "function.h"
#include "hold.h"
#include "pointer.h"
#include "prompt_for.h"
#include "restart.h"
#include "restart_value.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  symbol restart
 */
static int restart_symbol_use_function(Execute ptr, addr value)
{
	setresult_control(ptr, value);
	return 0;
}

static int restart_symbol_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, str);
	Return(prompt_for_stream_(ptr, T, prompt, &eval));
	Return(eval_result_partial_form_(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_symbol_use_interactive(Execute ptr)
{
	return restart_symbol_use_interactive_char_(ptr, "Use new value: ");
}

static int restart_symbol_use_test(Execute ptr, addr pos)
{
	setresult_control(ptr, T);
	return 0;
}

static void symbol_use_restart(addr *ret)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_use_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Use specific value.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int restart_symbol_store_special(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
	setresult_control(ptr, value);

	return 0;
}

static int restart_symbol_store_interactive(Execute ptr)
{
	return restart_symbol_use_interactive_char_(ptr, "Store new value: ");
}

static void symbol_store_restart(addr *ret, addr symbol)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, symbol);
	setcompiled_var1(value, p_restart_symbol_store_special);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific value.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int symbol_restart_call_(Execute ptr, addr symbol, addr *ret)
{
	addr control, restart1, restart2;

	symbol_use_restart(&restart1);
	symbol_store_restart(&restart2, symbol);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	(void)call_unbound_variable_(ptr, symbol);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int symbol_special_restart(Execute ptr, addr symbol, addr *ret)
{
	addr value;

	getspecial_local(ptr, symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return symbol_restart_call_(ptr, symbol, ret);
}


/*
 *  function restart
 */
static int restart_function_use_function(Execute ptr, addr value)
{
	if (! functionp(value))
		return TypeError_(value, FUNCTION);
	setresult_control(ptr, value);

	return 0;
}

static int restart_function_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new function: ");
	for (;;) {
		Return(prompt_for_stream_(ptr, T, prompt, &eval));
		Return(eval_result_partial_form_(ptr, eval, &eval));
		if (functionp(eval))
			break;
		/* error */
		strvect_char_heap(&prompt, "Please answer FUNCTION type: ");
	}

	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_function_use_interactive(Execute ptr)
{
	return restart_function_use_interactive_char_(ptr, "Use new function: ");
}

static void function_use_restart(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_function_use_function);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_function_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	strvect_char_heap(&value, "Use specific function.");
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

static int restart_function_store_function(Execute ptr, addr value)
{
	addr name;

	if (! functionp(value))
		return TypeError_(value, FUNCTION);

	getdata_control(ptr, &name);
	Return(setglobal_callname_(name, value));
	setresult_control(ptr, value);

	return 0;
}

static int restart_function_store_interactive(Execute ptr)
{
	return restart_function_use_interactive_char_(ptr, "Store new function: ");
}

static void function_store_restart(addr *ret, addr name)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, name);
	setcompiled_var1(value, p_restart_function_store_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_function_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific function.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int function_restart_call(Execute ptr, addr name, addr *ret)
{
	addr control, restart1, restart2;

	function_use_restart(&restart1);
	function_store_restart(&restart2, name);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	name_callname_heap(name, &name);
	(void)call_undefined_function_(ptr, name);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int callname_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! callnamep(name), "type error");
	getglobal_callname(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return function_restart_call(ptr, name, ret);
}

int function_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	GetFunctionSymbol(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SYMBOL);
	return function_restart_call(ptr, name, ret);
}

int setf_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	getsetf_symbol(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SETF);
	return function_restart_call(ptr, name, ret);
}


/*
 *  fdefinition
 */
static int restart_fdefinition_use_function(Execute ptr, addr value)
{
	if (! functionp(value))
		return TypeError_(value, FUNCTION);
	setresult_control(ptr, value);

	return 0;
}

static int restart_fdefinition_use_interactive_p_(addr pos, addr *ret)
{
	addr value;

	if (functionp(pos))
		return Result(ret, pos);

	/* parse callname */
	if (parse_callname_heap(&pos, pos))
		return Result(ret, Nil); /* parse error */

	/* function */
	getglobal_callname(pos, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* setf */
	if (setfp_callname(pos))
		return Result(ret, Nil); /* error */

	/* macro */
	GetCallName(pos, &value);
	getmacro_symbol(value, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* undefined-function */
	name_callname_heap(pos, &pos);
	return call_undefined_function_(NULL, pos);
}

static int restart_fdefinition_use_interactive_char_(Execute ptr, const char *str)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new function: ");
	for (;;) {
		Return(prompt_for_stream_(ptr, T, prompt, &eval));
		Return(eval_result_partial_form_(ptr, eval, &eval));
		Return(restart_fdefinition_use_interactive_p_(eval, &eval));
		if (eval != Nil)
			break;
		/* error */
		strvect_char_heap(&prompt, "Please answer FUNCTION-NAME type: ");
	}

	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_fdefinition_use_interactive(Execute ptr)
{
	return restart_fdefinition_use_interactive_char_(ptr, "Use new function: ");
}

static void fdefinition_use_restart(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_fdefinition_use_function);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_fdefinition_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	strvect_char_heap(&value, "Use specific function.");
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

static int restart_fdefinition_store_function(Execute ptr, addr value)
{
	addr name;

	if (! functionp(value))
		return TypeError_(value, FUNCTION);

	getdata_control(ptr, &name);
	if (setfp_callname(name) || funcall_function_p(value)) {
		/* normal function */
		Return(setglobal_callname_(name, value));
	}
	else {
		/* macro-function */
		GetCallName(name, &name);
		Return(alldelete_function_(name));
		Return(setmacro_symbol_(name, value));
	}
	setresult_control(ptr, value);

	return 0;
}

static int restart_fdefinition_store_interactive(Execute ptr)
{
	return restart_fdefinition_use_interactive_char_(ptr, "Store new function: ");
}

static void fdefinition_store_restart(addr *ret, addr name)
{
	addr restart, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &restart);
	restart_heap(&restart, restart);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, name);
	setcompiled_var1(value, p_restart_fdefinition_store_function);
	setfunction_restart(restart, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_fdefinition_store_interactive);
	setinteractive_restart(restart, value);
	/* report */
	strvect_char_heap(&value, "Store specific function.");
	setreport_restart(restart, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(restart, value);
	/* escape */
	setescape_restart(restart, 1);  /* restart-case */
	/* result */
	*ret = restart;
}

static int fdefinition_restart_call_(Execute ptr, addr name, addr *ret)
{
	addr control, restart1, restart2;

	fdefinition_use_restart(&restart1);
	fdefinition_store_restart(&restart2, name);

	push_control(ptr, &control);
	pushrestart_control(ptr, restart2);
	pushrestart_control(ptr, restart1);
	name_callname_heap(name, &name);
	(void)call_undefined_function_(ptr, name);

	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* use-value */
	if (ptr->throw_handler == restart1) {
		normal_throw_control(ptr);
		goto escape;
	}

	/* store-value */
	if (ptr->throw_handler == restart2) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	Return(pop_control_(ptr, control));
	getresult_control(ptr, ret);
	return 0;
}

int fdefinition_restart_(Execute ptr, addr name, addr *ret)
{
	CallNameType type;
	addr symbol, value;

	Check(! callnamep(name), "type error");
	GetCallNameType(name, &type);
	GetCallName(name, &symbol);

	/* setf */
	if (type == CALLNAME_SETF)
		return setf_global_restart(ptr, symbol, ret);

	/* function */
	GetFunctionSymbol(symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* macro */
	getmacro_symbol(symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* error, restart */
	return fdefinition_restart_call_(ptr, name, ret);
}


/*
 *  abort
 */
void abort_restart_char_heap(addr *ret, const char *str)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_ABORT, &pos);
	restart_heap(&pos, pos);
	/* function */
	GetConst(FUNCTION_NIL, &value);
	setfunction_restart(pos, value);
	/* interactive */
	setinteractive_restart(pos, Nil);
	/* report */
	strvect_char_heap(&value, str);
	setreport_restart(pos, value);
	/* test */
	constantly_common(T, &value);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

void abort_restart_char_control(Execute ptr, const char *str)
{
	addr restart;
	abort_restart_char_heap(&restart, str);
	pushrestart_control(ptr, restart);
}


/*
 *  initialize
 */
void init_restart_value(void)
{
	SetPointerType(var1, restart_symbol_use_function);
	SetPointerType(empty, restart_symbol_use_interactive);
	SetPointerType(var1, restart_symbol_use_test);
	SetPointerType(var1, restart_symbol_store_special);
	SetPointerType(empty, restart_symbol_store_interactive);
	SetPointerType(var1, restart_function_use_function);
	SetPointerType(empty, restart_function_use_interactive);
	SetPointerType(var1, restart_function_store_function);
	SetPointerType(empty, restart_function_store_interactive);
	SetPointerType(var1, restart_fdefinition_use_function);
	SetPointerType(empty, restart_fdefinition_use_interactive);
	SetPointerType(var1, restart_fdefinition_store_function);
	SetPointerType(empty, restart_fdefinition_store_interactive);
}

