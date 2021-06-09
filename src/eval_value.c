#include "constant.h"
#include "control_object.h"
#include "eval_value.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  toplevel
 */
static void symbol_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_TOPLEVEL, ret);
}

int get_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int toplevelp_eval(Execute ptr)
{
	addr pos;
	symbol_toplevel_eval(&pos);
	getspecial_local(ptr, pos, &pos);

	return pos != Unbound && pos != Nil;
}


/*
 *  compile-time
 */
static void symbol_compile_time_eval(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TIME, ret);
}

int get_compile_time_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_time_too_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_compile_time_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :compile-toplevel
 */
static void symbol_compile_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TOPLEVEL, ret);
}

int get_compile_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_toplevel_p_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_compile_toplevel_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :load-toplevel
 */
static void symbol_load_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_LOAD_TOPLEVEL, ret);
}

int get_load_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int load_toplevel_p_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_load_toplevel_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval-when :execute
 */
static void symbol_execute_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_EXECUTE, ret);
}

int get_execute_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int executep_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_execute_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  parse-declare
 */
static void symbol_parse_declare(addr *ret)
{
	GetConst(SYSTEM_PARSE_DECLARE, ret);
}

void push_parse_declare(Execute ptr, addr value)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int get_parse_declare_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void get_nocheck_parse_declare(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	getspecial_local(ptr, symbol, ret);
}

void set_parse_declare(Execute ptr, addr value)
{
	addr symbol;
	symbol_parse_declare(&symbol);
	setspecial_local(ptr, symbol, value);
}

int add_parse_declare_(Execute ptr, addr value, addr *ret)
{
	addr symbol, list;

	symbol_parse_declare(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, value, list);
	setspecial_local(ptr, symbol, list);
	if (ret)
		*ret = list;

	return 0;
}

