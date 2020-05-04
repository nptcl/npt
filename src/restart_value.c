#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval.h"
#include "function.h"
#include "gc.h"
#include "pointer.h"
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

static int restart_symbol_use_interactive(Execute ptr)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new value: ");
	Return(prompt_for_stream(ptr, T, prompt, &eval));
	Return(eval_object(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_symbol_use_report(Execute ptr, addr stream)
{
	print_ascii_stream(stream, "Use specific value.");
	return 0;
}

static int restart_symbol_use_test(Execute ptr, addr pos)
{
	setresult_control(ptr, T);
	return 0;
}

static void symbol_use_restart(Execute ptr)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_USE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_function);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_report);
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	push_restart_control(ptr, pos);
}

static int restart_symbol_store_global(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	SetValueSymbol(symbol, value);
	setresult_control(ptr, value);

	return 0;
}

static int restart_symbol_store_lexical(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	setlexical_local(ptr, symbol, value);
	setresult_control(ptr, value);

	return 0;
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
	addr prompt, eval;

	strvect_char_heap(&prompt, "Store new value: ");
	Return(prompt_for_stream(ptr, T, prompt, &eval));
	Return(eval_object(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);

	return 0;
}

static int restart_symbol_store_report(Execute ptr, addr stream)
{
	print_ascii_stream(stream, "Store specific value.");
	return 0;
}

static void symbol_store_restart(Execute ptr, addr symbol, pointer call)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_STORE_VALUE, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	SetDataFunction(value, symbol);
	setcompiled_var1(value, call);
	setfunction_restart(pos, value);
	/* interactive */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_restart_symbol_store_interactive);
	setinteractive_restart(pos, value);
	/* report */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_store_report);
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	push_restart_control(ptr, pos);
}

struct symbol_restart_struct {
	addr symbol;
	pointer call;
};

static int symbol_restart_code(Execute ptr, void *voidp)
{
	struct symbol_restart_struct *str;

	str = (struct symbol_restart_struct *)voidp;
	symbol_store_restart(ptr, str->symbol, str->call);
	symbol_use_restart(ptr);
	unbound_variable(str->symbol);

	return 0;
}

static int symbol_restart_control(Execute ptr, addr symbol, pointer call)
{
	addr control;
	struct symbol_restart_struct str;

	push_return_control(ptr, &control);
	str.symbol = symbol;
	str.call = call;
	Return(restart_control(ptr, symbol_restart_code, (void *)&str));

	return free_control_(ptr, control);
}

static int symbol_restart_call(Execute ptr, addr symbol, pointer call, addr *ret)
{
	addr control, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	Return(symbol_restart_control(ptr, symbol, call));
	getresult_control(ptr, &value);
	localhold_set(hold, 0, value);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, value);
}

_g int symbol_global_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_global;
	addr value;

	GetValueSymbol(symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}

_g int symbol_lexical_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_lexical;
	addr value;

	getlexical_local(ptr, symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}

_g int symbol_special_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_special;
	addr value;

	getspecial_local(ptr, symbol, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}


/*
 *  function restart
 */
static int restart_function_use_function(Execute ptr, addr value)
{
	if (! functionp(value)) {
		TypeError(value, FUNCTION);
		return 0;
	}
	setresult_control(ptr, value);

	return 0;
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
	setcompiled_empty(value, p_restart_symbol_use_interactive);
	setinteractive_restart(pos, value);
	/* report */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_restart_symbol_use_report);
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

static int function_restart_code(Execute ptr, addr name)
{
	name_callname_heap(name, &name);
	undefined_function(name);
	return 0;
}

static int function_restart_control(Execute ptr, addr name)
{
	addr restart, control;

	push_return_control(ptr, &control);
	function_use_restart(&restart);
	Return(restart1_control(ptr, restart, function_restart_code, name));

	return free_control_(ptr, control);
}

static int function_restart_call(Execute ptr, addr name, addr *ret)
{
	addr control, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	Return(function_restart_control(ptr, name));
	getresult_control(ptr, &value);
	localhold_set(hold, 0, value);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, value);
}

_g int callname_global_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! callnamep(name), "type error");
	getfunction_callname_global(name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	return function_restart_call(ptr, name, ret);
}

_g int function_global_restart(Execute ptr, addr name, addr *ret)
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

_g int function_local_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	getfunction_local(ptr, name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SYMBOL);
	return function_restart_call(ptr, name, ret);
}

_g int setf_global_restart(Execute ptr, addr name, addr *ret)
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

_g int setf_local_restart(Execute ptr, addr name, addr *ret)
{
	addr value;

	Check(! symbolp(name), "type error");
	getsetf_local(ptr, name, &value);
	if (value != Unbound)
		return Result(ret, value);

	/* restart */
	callname_heap(&name, name, CALLNAME_SETF);
	return function_restart_call(ptr, name, ret);
}


/*
 *  initialize
 */
_g void init_restart_value(void)
{
	SetPointerType(var1, restart_symbol_use_function);
	SetPointerType(empty, restart_symbol_use_interactive);
	SetPointerType(var1, restart_symbol_use_report);
	SetPointerType(var1, restart_symbol_use_test);
	SetPointerType(var1, restart_symbol_store_global);
	SetPointerType(var1, restart_symbol_store_lexical);
	SetPointerType(var1, restart_symbol_store_special);
	SetPointerType(empty, restart_symbol_store_interactive);
	SetPointerType(var1, restart_symbol_store_report);
	SetPointerType(var1, restart_function_use_function);
}

