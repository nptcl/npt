#include "condition.h"
#include "control.h"
#include "eval.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "function.h"
#include "prompt.h"
#include "readtable.h"
#include "stream.h"
#include "stream_string.h"

/*
 *  restart abort
 */
static void eval_loop_abort_function(Execute ptr)
{
	setresult_control(ptr, Nil);
}

static void eval_loop_abort_report(Execute ptr, addr stream)
{
	fmts(stream, "Return to eval-loop.", NULL);
	setresult_control(ptr, Nil);
}

static void eval_loop_abort_test(Execute ptr, addr condition)
{
	setresult_control(ptr, T);
}

static void eval_main_restart_abort(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_ABORT, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, eval_loop_abort_function);
	setfunction_restart(pos, value);
	/* interactive */
	setinteractive_restart(pos, Nil);
	/* report */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, eval_loop_abort_report);
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, eval_loop_abort_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}

static void push_eval_main_restart_abort(Execute ptr, addr *ret)
{
	addr pos;

	eval_main_restart_abort(&pos);
	pushobject_restart_control(ptr, pos);
	*ret = pos;
}


/*
 *  eval-loop
 */
static void eval_loop_output(Execute ptr, addr stream, addr control)
{
	addr list;

	getvalues_list_control_local(ptr, &list);
	fmts(stream, "~&~{~S~%~}~&", list, NULL);
}

static int eval_loop_stream(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_close_control(ptr, &control);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);
	if (eval_execute(ptr, pos)) {
		return runcode_free_control(ptr, control);
	}
	else {
		eval_loop_output(ptr, stream, pos);
		return free_control(ptr, control);
	}
}

static int eval_loop_execute(Execute ptr, addr stream, int *ret)
{
	int result;
	addr pos;

	/* read */
	if (read_stream(ptr, stream, &result, &pos)) {
		return 1;
	}

	/* EOF */
	if (result) {
		fmts(stream, "~&", NULL);
		*ret = 1;
		return 0;
	}

	/* execute */
	if (eval_loop_stream(ptr, stream, pos)) {
		return 1;
	}

	/* normal */
	*ret = 0;
	return 0;
}

static int eval_loop_restart(Execute ptr, addr stream, int *ret)
{
	int check;
	addr control, restart;
	codejump jump;

	/* execute */
	push_restart_control(ptr, &control);
	begin_switch(ptr, &jump);
	check = 0;
	if (codejump_run_p(&jump)) {
		push_eval_main_restart_abort(ptr, &restart);
		check = eval_loop_execute(ptr, stream, ret);
	}
	end_switch(&jump);

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		*ret = 0;
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	if (check)
		return runcode_free_control(ptr, control);
	else
		return free_control(ptr, control);
}

int eval_main_loop(Execute ptr)
{
	int exit;
	addr stream;
	size_t index;

	index = getindex_prompt(ptr);
	terminal_io_stream(ptr, &stream);
	while (! getbreak_prompt(ptr)) {
		setindex_prompt(ptr, index);
		setshow_prompt(ptr, 1);
		if (eval_loop_restart(ptr, stream, &exit)) {
			terpri_stream(stream);
			return 1;
		}
		if (exit) {
			terpri_stream(stream);
			return 0;
		}
	}
	terpri_stream(stream);

	return 1;
}


/*
 *  eval_main_string
 */
static int evalcall_string_result(Execute ptr, addr eval)
{
	int check;
	addr stream, control;

	push_close_control(ptr, &control);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);
	open_input_string_stream(&stream, eval);
	check = eval_stream(ptr, stream);
	close_stream(stream);
	if (check)
		return runcode_free_control(ptr, control);
	else
		return free_control(ptr, control);
}

static int evalrestart_string(Execute ptr, addr eval, int *abort)
{
	int check;
	addr control, restart;
	codejump jump;

	/* execute */
	push_restart_control(ptr, &control);
	begin_switch(ptr, &jump);
	check = 0;
	*abort = 0;
	if (codejump_run_p(&jump)) {
		push_eval_main_restart_abort(ptr, &restart);
		check = evalcall_string_result(ptr, eval);
	}
	end_switch(&jump);

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		*abort = 1;
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	if (check)
		return runcode_free_control(ptr, control);
	else
		return free_control(ptr, control);
}

void eval_main_string(Execute ptr, addr eval, int *abort)
{
	if (evalrestart_string(ptr, eval, abort))
		fmte("Cannot catch a system signal.", NULL);
}



/*
 *  eval_main_load
 */
static int evalrestart_load(Execute ptr,
		addr file, int exists, int *result, int *abort)
{
	int check;
	addr control, restart;
	codejump jump;

	/* execute */
	push_restart_control(ptr, &control);
	begin_switch(ptr, &jump);
	check = 0;
	*result = 1;
	*abort = 0;
	if (codejump_run_p(&jump)) {
		push_eval_main_restart_abort(ptr, &restart);
		check = eval_load(ptr, result, file, Nil, Nil, exists, Unbound);
	}
	end_switch(&jump);

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		*abort = 1;
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	if (check)
		return runcode_free_control(ptr, control);
	else
		return free_control(ptr, control);
}

int eval_main_load(Execute ptr, addr file, int exists, int *abort)
{
	int result;
	if (evalrestart_load(ptr, file, exists, &result, abort))
		fmte("Cannot catch a system signal.", NULL);
	return (result == 0);
}

