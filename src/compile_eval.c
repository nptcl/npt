#include "code_make.h"
#include "compile_file.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_execute.h"
#include "eval_stack.h"
#include "execute.h"
#include "hold.h"
#include "load_time_value.h"
#include "optimize_parse.h"
#include "parse.h"
#include "parse_object.h"
#include "scope.h"
#include "scope_call.h"
#include "scope_declare.h"
#include "typedef.h"

static int compile_eval_scope(Execute ptr, LocalHold hold, addr pos);

/*
 *  eval-when check
 */
static int compile_eval_execute_p(Execute ptr)
{
	return load_toplevel_p_eval(ptr);
}

static int compile_eval_compile_p(Execute ptr)
{
	return compile_toplevel_p_eval(ptr)
		|| compile_time_too_eval(ptr);
}


/*
 *  execute
 */
static int compile_eval_execute(Execute ptr, LocalHold hold, addr pos)
{
	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_compile(ptr, &pos, pos));

	/* code generator */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);

	/* execute */
	if (compile_eval_execute_p(ptr)) {
		localhold_set(hold, 0, pos);
		Return(eval_compile_file(ptr, pos));
	}

	/* compile-toplevel */
	if (compile_eval_compile_p(ptr)) {
		localhold_set(hold, 0, pos);
		Return(runcode_control(ptr, pos));
	}

	return 0;
}


/*
 *  progn
 */
static int compile_eval_progn(Execute ptr, LocalHold hold, addr pos)
{
	addr list;

	GetEvalParse(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope(ptr, hold, pos));
	}

	return 0;
}


/*
 *  locally
 */
static int compile_eval_implicit(Execute ptr, LocalHold hold,
		addr args, addr decl, addr list)
{
	addr stack, free, pos;

	/* new stack */
	stack = newstack_nil(ptr);
	apply_declare(ptr, stack, decl, &free);

	/* symbol-macrolet */
	if (args != Nil)
		apply_symbol_macrolet(stack, args);

	/* locally */
	eval_parse_heap(&pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, Nil);
	SetEvalParse(pos, 2, free);
	localhold_set(hold, 1, pos);
	Return(compile_eval_execute(ptr, hold, pos));
	localhold_set(hold, 1, Nil);

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope(ptr, hold, pos));
	}

	/* free stack */
	freestack_eval(ptr, stack);

	return 0;
}

static int compile_eval_locally(Execute ptr, LocalHold hold, addr pos)
{
	addr decl, list;

	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &list);

	return compile_eval_implicit(ptr, hold, Nil, decl, list);
}


/*
 *  symbol-macrolet
 */
static int compile_eval_symbol_macrolet(Execute ptr, LocalHold hold, addr pos)
{
	addr args, decl, list;

	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &list);

	return compile_eval_implicit(ptr, hold, args, decl, list);
}


/*
 *  eval-when
 */
static int compile_eval_eval_when(Execute ptr, LocalHold hold, addr pos)
{
	addr list;
	addr compile, load, exec, mode;
	addr compile1, load1, exec1, mode1;

	GetEvalParse(pos, 0, &list);
	GetEvalParse(pos, 1, &compile);
	GetEvalParse(pos, 2, &load);
	GetEvalParse(pos, 3, &exec);
	GetEvalParse(pos, 5, &mode);

	/* save */
	get_compile_time_eval(ptr, &mode1);
	get_compile_toplevel_eval(ptr, &compile1);
	get_load_toplevel_eval(ptr, &load1);
	get_execute_eval(ptr, &exec1);

	/* set */
	set_compile_time_eval(ptr, mode);
	set_compile_toplevel_eval(ptr, compile);
	set_load_toplevel_eval(ptr, load);
	set_execute_eval(ptr, exec);

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope(ptr, hold, pos));
	}

	/* rollback */
	set_compile_time_eval(ptr, mode1);
	set_compile_toplevel_eval(ptr, compile1);
	set_load_toplevel_eval(ptr, load1);
	set_execute_eval(ptr, exec1);

	return 0;
}


/*
 *  interface
 */
static int compile_eval_scope(Execute ptr, LocalHold hold, addr pos)
{
	EvalParse type;

	GetEvalParseType(pos, &type);
	switch (type) {
		case EVAL_PARSE_PROGN:
			return compile_eval_progn(ptr, hold, pos);

		case EVAL_PARSE_LOCALLY:
			return compile_eval_locally(ptr, hold, pos);

		case EVAL_PARSE_SYMBOL_MACROLET:
			return compile_eval_symbol_macrolet(ptr, hold, pos);

		case EVAL_PARSE_EVAL_WHEN:
			return compile_eval_eval_when(ptr, hold, pos);

		default:
			return compile_eval_execute(ptr, hold, pos);
	}
}

_g int compile_eval(Execute ptr, addr pos)
{
	addr control;
	LocalHold hold;

	push_new_control(ptr, &control);
	/* special variable */
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	/* init */
	init_scope_load_time_value(ptr);
	begin_eval_stack(ptr);
	free_eval_stack(ptr);
	/* parse */
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, pos);
	Return(eval_parse(ptr, &pos, pos, T));
	/* optimize parse */
	localhold_set(hold, 0, pos);
	optimize_parse(ptr->local, &pos, pos);
	/* scope */
	Return(compile_eval_scope(ptr, hold, pos));
	/* free */
	localhold_end(hold);
	return free_control_(ptr, control);
}

