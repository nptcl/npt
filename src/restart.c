#include "condition.h"
#include "cons.h"
#include "control.h"
#include "eval.h"
#include "function.h"
#include "gc.h"
#include "pointer.h"
#include "stream.h"
#include "symbol.h"

/*
 *  restart code
 */
_g void function_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	GetFunctionSymbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function(symbol);
	*ret = pos;
}

_g void function_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getfunctioncheck_local(ptr, symbol, ret);
}

_g void setf_global_restart(Execute ptr, addr symbol, addr *ret)
{
	addr pos;

	getsetf_symbol(symbol, &pos);
	if (pos == Unbound)
		undefined_function_setf(symbol);
	*ret = pos;
}

_g void setf_local_restart(Execute ptr, addr symbol, addr *ret)
{
	getsetfcheck_local(ptr, symbol, ret);
}


/*
 *  symbol restart
 */
static void restart_symbol_use_function(Execute ptr, addr value)
{
	setresult_control(ptr, value);
}

static void restart_symbol_use_interactive(Execute ptr)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Use new value: ");
	Return0(prompt_for_stream(ptr, T, prompt, &eval));
	Return0(eval_object(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);
}

static void restart_symbol_use_report(Execute ptr, addr stream)
{
	print_ascii_stream(stream, "Use specific value.");
}

static void restart_symbol_use_test(Execute ptr, addr pos)
{
	setresult_control(ptr, T);
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
	pushobject_restart_control(ptr, pos);
}

static void restart_symbol_store_global(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	SetValueSymbol(symbol, value);
	setresult_control(ptr, value);
}

static void restart_symbol_store_lexical(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	setlexical_local(ptr, symbol, value);
	setresult_control(ptr, value);
}

static void restart_symbol_store_special(Execute ptr, addr value)
{
	addr symbol;

	getdata_control(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
	setresult_control(ptr, value);
}

static void restart_symbol_store_interactive(Execute ptr)
{
	addr prompt, eval;

	strvect_char_heap(&prompt, "Store new value: ");
	Return0(prompt_for_stream(ptr, T, prompt, &eval));
	Return0(eval_object(ptr, eval, &eval));
	list_heap(&eval, eval, NULL);
	setresult_control(ptr, eval);
}

static void restart_symbol_store_report(Execute ptr, addr stream)
{
	print_ascii_stream(stream, "Store specific value.");
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
	pushobject_restart_control(ptr, pos);
}

static int symbol_restart_code(Execute ptr, addr symbol)
{
	unbound_variable(symbol);
	return 0;
}

static int symbol_restart_bind(Execute ptr, addr symbol, pointer call)
{
	int check;
	addr control;
	codejump jump;

	/* execute */
	push_restart_initialize_control(ptr, &control);
	check = 0;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		symbol_store_restart(ptr, symbol, call);
		symbol_use_restart(ptr);
		check = symbol_restart_code(ptr, symbol);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control))
			throw_switch(&jump);
		/* restart */
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	return free_control(ptr, control);
}

static int symbol_restart_call(Execute ptr, addr symbol, pointer call, addr *ret)
{
	int check;
	addr control, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	check = symbol_restart_bind(ptr, symbol, call);
	if (check)
		return free_check_control(ptr, control, 1);
	getresult_control(ptr, &value);
	localhold_set(hold, 0, value);
	Return1(free_check_control(ptr, control, 0));
	localhold_end(hold);
	*ret = value;

	return 0;
}

_g int symbol_global_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_global;
	addr value;

	GetValueSymbol(symbol, &value);
	if (value != Unbound) {
		*ret = value;
		return 0;
	}

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}

_g int symbol_lexical_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_lexical;
	addr value;

	getlexical_local(ptr, symbol, &value);
	if (value != Unbound) {
		*ret = value;
		return 0;
	}

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}

_g int symbol_special_restart(Execute ptr, addr symbol, addr *ret)
{
	static const pointer call = p_restart_symbol_store_special;
	addr value;

	getspecial_local(ptr, symbol, &value);
	if (value != Unbound) {
		*ret = value;
		return 0;
	}

	/* restart */
	return symbol_restart_call(ptr, symbol, call, ret);
}


/*
 *  initialize
 */
_g void init_restart(void)
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
}

