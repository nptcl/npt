#include "code_make.h"
#include "compile_eval.h"
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

static int compile_eval_scope_(Execute ptr, addr pos);

/*
 *  eval-when check
 */
static int compile_eval_execute_p_(Execute ptr, int *ret)
{
	return load_toplevel_p_eval_(ptr, ret);
}

static int compile_eval_compile_p_(Execute ptr, int *ret)
{
	Return(compile_toplevel_p_eval_(ptr, ret));
	if (*ret)
		return 0;

	return compile_time_too_eval_(ptr, ret);
}


/*
 *  execute
 */
static int compile_eval_execute_call_(Execute ptr, addr pos)
{
	int check;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	localhold_push(hold, pos);

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_compile(ptr, &pos, pos));

	/* code generator */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);

	/* execute */
	Return(compile_eval_execute_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(eval_compile_file(ptr, pos));
	}

	/* compile-toplevel */
	Return(compile_eval_compile_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(runcode_control_(ptr, pos));
	}

	/* end */
	localhold_end(hold);
	return 0;
}

static int compile_eval_execute_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_execute_call_(ptr, pos);
	return pop_control_(ptr, control);
}


/*
 *  progn
 */
static int compile_eval_progn(Execute ptr, addr pos)
{
	addr list;

	GetEvalParse(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos));
	}

	return 0;
}


/*
 *  locally
 */
static int compile_eval_implicit(Execute ptr, addr args, addr decl, addr list)
{
	addr stack, free, pos;

	/* new stack */
	Return(newstack_nil_(ptr, &stack));
	Return(apply_declare_(ptr, stack, decl, &free));

	/* symbol-macrolet */
	if (args != Nil)
		apply_symbol_macrolet(stack, args);

	/* locally */
	eval_parse_heap(&pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, Nil);
	SetEvalParse(pos, 2, free);
	Return(compile_eval_execute_(ptr, pos));

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos));
	}

	/* free stack */
	return freestack_eval_(ptr, stack);
}

static int compile_eval_locally(Execute ptr, addr pos)
{
	addr decl, list;

	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &list);

	return compile_eval_implicit(ptr, Nil, decl, list);
}


/*
 *  symbol-macrolet
 */
static int compile_eval_symbol_macrolet(Execute ptr, addr pos)
{
	addr args, decl, list;

	GetEvalParse(pos, 0, &args);
	GetEvalParse(pos, 1, &decl);
	GetEvalParse(pos, 2, &list);

	return compile_eval_implicit(ptr, args, decl, list);
}


/*
 *  eval-when
 */
static int compile_eval_eval_when(Execute ptr, addr pos)
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
	Return(get_compile_time_eval_(ptr, &mode1));
	Return(get_compile_toplevel_eval_(ptr, &compile1));
	Return(get_load_toplevel_eval_(ptr, &load1));
	Return(get_execute_eval_(ptr, &exec1));

	/* set */
	set_compile_time_eval(ptr, mode);
	set_compile_toplevel_eval(ptr, compile);
	set_load_toplevel_eval(ptr, load);
	set_execute_eval(ptr, exec);

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos));
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
static int compile_eval_hold(Execute ptr, addr pos)
{
	EvalParse type;

	GetEvalParseType(pos, &type);
	switch (type) {
		case EVAL_PARSE_PROGN:
			return compile_eval_progn(ptr, pos);

		case EVAL_PARSE_LOCALLY:
			return compile_eval_locally(ptr, pos);

		case EVAL_PARSE_SYMBOL_MACROLET:
			return compile_eval_symbol_macrolet(ptr, pos);

		case EVAL_PARSE_EVAL_WHEN:
			return compile_eval_eval_when(ptr, pos);

		default:
			return compile_eval_execute_(ptr, pos);
	}
}

static int compile_eval_scope_(Execute ptr, addr pos)
{
	addr control;
	LocalHold hold;

	push_control(ptr, &control);
	hold = LocalHold_local_push(ptr, pos);
	if (compile_eval_hold(ptr, pos) == 0)
		localhold_end(hold);
	return pop_control_(ptr, control);
}

static int compile_eval_call_(Execute ptr, addr pos)
{
	LocalHold hold;

	/* special variable */
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	/* init */
	init_scope_load_time_value(ptr);
	Return(begin_eval_stack_(ptr));
	free_eval_stack(ptr);
	/* parse */
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, pos);
	Return(eval_parse(ptr, &pos, pos, T));
	/* optimize parse */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr->local, pos, &pos, NULL));
	/* scope */
	localhold_set(hold, 0, pos);
	Return(compile_eval_hold(ptr, pos));
	/* free */
	localhold_end(hold);
	return 0;
}

int compile_eval(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_call_(ptr, pos);
	return pop_control_(ptr, control);
}

