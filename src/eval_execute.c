#include "code_make.h"
#include "compile_eval.h"
#include "compile_file.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_execute.h"
#include "execute_values.h"
#include "hold.h"
#include "optimize_parse.h"
#include "parse.h"
#include "reader.h"
#include "scope.h"
#include "stream.h"
#include "typedef.h"

/*
 *  begin, end
 */
int begin_eval_(Execute ptr, addr *ret, addr toplevel)
{
	addr control;

	push_control(ptr, &control);
	begin_parse(ptr, toplevel);
	Return(begin_scope_(ptr));

	return Result(ret, control);
}

int begin_compile_(Execute ptr, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	Return(begin_compile_eval_(ptr));
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

static int eval_execute_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(eval_parse_(ptr, &pos, pos));
	Return(eval_execute_scope_(ptr, hold, pos));
	localhold_end(hold);

	return 0;
}

int eval_execute_partial(Execute ptr, addr pos)
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

int eval_result_partial(Execute ptr, addr pos, addr *ret)
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

int eval_result_macro(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	push_control(ptr, &control);
	(void)eval_result_macro_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  eval-stream
 */
static int eval_load_stream_loop_(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(eval_execute_(ptr, pos));
	}

	return 0;
}

int eval_load_stream_(Execute ptr, addr stream, addr toplevel)
{
	addr control;

	Return(begin_eval_(ptr, &control, toplevel));
	(void)eval_load_stream_loop_(ptr, stream);
	return pop_control_(ptr, control);
}

static int compile_load_stream_loop_(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(compile_execute_(ptr, pos));
	}

	return 0;
}

int compile_load_stream_(Execute ptr, addr stream)
{
	addr control;

	Return(begin_compile_(ptr, &control));
	(void)compile_load_stream_loop_(ptr, stream);
	return pop_control_(ptr, control);
}


/*
 *  eval
 */
int eval_stream_partial(Execute ptr, addr stream)
{
	return eval_load_stream_(ptr, stream, Nil);
}

int eval_stream_toplevel(Execute ptr, addr stream)
{
	return eval_load_stream_(ptr, stream, T);
}

