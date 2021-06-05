#include "code_make.h"
#include "compile_eval.h"
#include "compile_file.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_execute.h"
#include "eval_object.h"
#include "eval_stack.h"
#include "eval_value.h"
#include "execute_values.h"
#include "hold.h"
#include "load_instance.h"
#include "load_time_value.h"
#include "optimize_parse.h"
#include "parse_function.h"
#include "parse_macro.h"
#include "parse_object.h"
#include "reader.h"
#include "step.h"
#include "scope.h"
#include "scope_declare.h"
#include "scope_object.h"
#include "stream.h"
#include "type_value.h"
#include "typedef.h"

/*
 *  begin, end
 */
static int begin_eval_(Execute ptr, addr *ret, addr toplevel)
{
	addr control;

	push_control(ptr, &control);
	/* initialize */
	init_parse_step(ptr);
	init_parse_environment(ptr);
	/* variables */
	push_toplevel_eval(ptr, toplevel);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	/* load-time-value */
	disable_load_time_value(ptr);
	/* scope */
	Return(begin_scope_(ptr));

	return Result(ret, control);
}


/*
 *  eval
 */
static int eval_execute_scope_(Execute ptr, LocalHold hold, addr pos)
{
	/* optimize parse */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr->local, pos, &pos, NULL));
	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));
	/* code generator */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);
	/* execute */
	localhold_set(hold, 0, pos);
	return runcode_control_(ptr, pos);
}

static int eval_execute_parse_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(parse_execute_toplevel_(ptr, &pos, pos));
	Return(eval_execute_scope_(ptr, hold, pos));
	localhold_end(hold);

	return 0;
}

static int eval_execute_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_execute_parse_(ptr, pos);
	return pop_control_(ptr, control);
}


/*
 *  interface
 */
int eval_execute_partial_(Execute ptr, addr pos)
{
	addr control;

	Return(begin_eval_(ptr, &control, Nil));
	(void)eval_execute_(ptr, pos);
	return pop_control_(ptr, control);
}

static int eval_result_partial_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 0, pos);
	Return(eval_execute_(ptr, pos));
	getresult_control(ptr, ret);
	localhold_set(hold, 1, *ret);

	return 0;
}

int eval_result_partial_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	Return(begin_eval_(ptr, &control, Nil));
	(void)eval_result_partial_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

static int eval_result_partial_form_call_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil); /* Don't run compile mode. */
	(void)eval_result_partial_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

int eval_result_partial_form_(Execute ptr, addr pos, addr *ret)
{
	addr control;

	Return(begin_eval_(ptr, &control, Nil));
	(void)eval_result_partial_form_call_(ptr, pos, ret);
	return pop_control_(ptr, control);
}

static int eval_result_macro_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 0, pos);
	Return(eval_execute_scope_(ptr, hold, pos));
	getresult_control(ptr, ret);
	localhold_set(hold, 1, *ret);

	return 0;
}

int eval_result_macro_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_push(hold, pos);
	push_control(ptr, &control);
	(void)eval_result_macro_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  eval-stream
 */
static int eval_toplevel_scope_(Execute ptr, addr pos, addr *ret);

static int eval_toplevel_runcode_(Execute ptr, addr pos, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	/* scope */
	Return(eval_scope_(ptr, &pos, pos));
	GetEvalScopeThe(pos, ret);
	/* code */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);
	/* execute */
	localhold_set(hold, 0, pos);
	Return(runcode_control_(ptr, pos));
	/* end */
	localhold_end(hold);
	return 0;
}

static int eval_toplevel_allcons_(Execute ptr, addr cons, addr *ret)
{
	addr expr;
	LocalHold hold;

	/* cons */
	hold = LocalHold_local_push(ptr, cons);
	expr = Nil;
	while (cons != Nil) {
		GetCons(cons, &expr, &cons);
		Return(eval_toplevel_scope_(ptr, expr, ret));
	}
	localhold_end(hold);

	if (expr == Nil)
		type_value_nil(ret);

	return 0;
}

static int eval_toplevel_progn_(Execute ptr, addr eval, addr *ret)
{
	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	return eval_toplevel_allcons_(ptr, eval, ret);
}

static int eval_toplevel_locally_(Execute ptr, addr eval, addr *ret)
{
	addr decl, cons, free, stack;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);

	Return(newstack_nil_(ptr, &stack));
	Return(apply_declare_(ptr, stack, decl, &free));
	Return(eval_toplevel_allcons_(ptr, eval, ret));
	return freestack_eval_(ptr, stack);
}

static int eval_toplevel_eval_when_(Execute ptr, addr eval, addr *ret)
{
	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	return eval_toplevel_allcons_(ptr, eval, ret);
}

static int eval_toplevel_scope_(Execute ptr, addr pos, addr *ret)
{
	EvalParse type;

	GetEvalParseType(pos, &type);
	switch (type) {
		case EVAL_PARSE_PROGN:
			return eval_toplevel_progn_(ptr, pos, ret);

		case EVAL_PARSE_LOCALLY:
			return eval_toplevel_locally_(ptr, pos, ret);

		case EVAL_PARSE_EVAL_WHEN:
			return eval_toplevel_eval_when_(ptr, pos, ret);

		default:
			return eval_toplevel_runcode_(ptr, pos, ret);
	}
}

static int eval_toplevel_call_(Execute ptr, addr pos)
{
	LocalHold hold;
	addr ignore;

	hold = LocalHold_array(ptr, 1);
	/* parse */
	localhold_set(hold, 0, pos);
	Return(parse_execute_toplevel_(ptr, &pos, pos));
	/* optimize */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr->local, pos, &pos, NULL));
	/* scope */
	Return(eval_toplevel_scope_(ptr, pos, &ignore));
	localhold_end(hold);

	return 0;
}

static int eval_toplevel_execute_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_toplevel_call_(ptr, pos);
	return pop_control_(ptr, control);
}

static int eval_toplevel_loop_(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(eval_toplevel_execute_(ptr, pos));
	}

	return 0;
}

int eval_stream_toplevel_(Execute ptr, addr stream)
{
	addr control;

	Return(begin_eval_(ptr, &control, T));
	(void)eval_toplevel_loop_(ptr, stream);
	return pop_control_(ptr, control);
}

