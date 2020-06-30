#include "cons.h"
#include "condition.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "print_write.h"
#include "prompt.h"
#include "reader.h"
#include "restart.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"

/*
 *  restart abort
 */
static int function_eval_loop_abort_function(Execute ptr)
{
	setresult_control(ptr, Nil);
	return 0;
}

static int function_eval_loop_abort_report(Execute ptr, addr stream)
{
	Return(format_stream(ptr, stream, "Return to eval-loop.", NULL));
	setresult_control(ptr, Nil);
	return 0;
}

static int function_eval_loop_abort_test(Execute ptr, addr condition)
{
	setresult_control(ptr, T);
	return 0;
}

static void eval_main_restart_abort(addr *ret)
{
	addr pos, value;

	/* restart */
	GetConst(COMMON_ABORT, &pos);
	restart_heap(&pos, pos);
	/* function */
	compiled_heap(&value, Nil);
	setcompiled_empty(value, p_defun_eval_loop_abort_function);
	setfunction_restart(pos, value);
	/* interactive */
	setinteractive_restart(pos, Nil);
	/* report */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_defun_eval_loop_abort_report);
	setreport_restart(pos, value);
	/* test */
	compiled_heap(&value, Nil);
	setcompiled_var1(value, p_defun_eval_loop_abort_test);
	settest_restart(pos, value);
	/* escape */
	setescape_restart(pos, 1);  /* restart-case */
	/* result */
	*ret = pos;
}


/*
 *  eval-loop
 */
static void eval_loop_minus(Execute ptr, addr value)
{
	addr symbol;

	GetConst(COMMON_MINUS, &symbol);
	setspecial_local(ptr, symbol, value);
}

static void eval_loop_shift(Execute ptr, addr list)
{
	addr sym0, sym1, sym2, sym3, pos1, pos2, pos3;

	/* +, ++, +++ */
	GetConst(COMMON_MINUS, &sym0);
	GetConst(COMMON_PLUS,  &sym1);
	GetConst(COMMON_PLUS2, &sym2);
	GetConst(COMMON_PLUS3, &sym3);
	getspecial_local(ptr, sym0, &pos1);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, pos1);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);

	/* *, **, *** */
	GetConst(COMMON_ASTERISK,  &sym1);
	GetConst(COMMON_ASTERISK2, &sym2);
	GetConst(COMMON_ASTERISK3, &sym3);
	GetCar(list, &pos1);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, pos1);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);

	/* /, //, /// */
	GetConst(COMMON_SLASH,  &sym1);
	GetConst(COMMON_SLASH2, &sym2);
	GetConst(COMMON_SLASH3, &sym3);
	getspecial_local(ptr, sym1, &pos2);
	getspecial_local(ptr, sym2, &pos3);
	setspecial_local(ptr, sym1, list);
	setspecial_local(ptr, sym2, pos2);
	setspecial_local(ptr, sym3, pos3);
}

_g int eval_loop_output(Execute ptr, addr stream, addr control)
{
	addr list, pos;

	getvalues_list_control_heap(ptr, &list);
	eval_loop_shift(ptr, list);
	/* format_stream(ptr, stream, "~&~{~S~%~}~&", list, NULL); */
	fresh_line_stream(stream);
	while (list != Nil) {
		getcons(list, &pos, &list);
		Return(prin1_print(ptr, stream, pos));
		terpri_stream(stream);
	}
	fresh_line_stream(stream);
	force_output_stream(stream);

	return 0;
}

static int eval_loop_stream(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	eval_loop_minus(ptr, pos);
	Return(eval_execute_partial(ptr, pos));
	Return(eval_loop_output(ptr, stream, pos));
	return free_control_(ptr, control);
}

static void eval_loop_variable(Execute ptr)
{
	/* -
	 * +, ++, +++
	 * *, **, ***
	 * /, //, ///
	 */
	addr symbol;

	GetConst(COMMON_MINUS, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_PLUS3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_ASTERISK3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH2, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(COMMON_SLASH3, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

struct eval_loop_struct {
	eval_loop_calltype call;
	addr stream;
	int *ret;
};

static int eval_loop_execute(Execute ptr, void *voidp)
{
	int check, *ret;
	addr pos, stream;
	eval_loop_calltype call;
	struct eval_loop_struct *str;

	str = (struct eval_loop_struct *)voidp;
	stream = str->stream;
	call = str->call;
	ret = str->ret;

	/* read */
	Return(read_stream(ptr, stream, &check, &pos));

	/* EOF */
	if (check) {
		fresh_line_stream(stream);
		return Result(ret, 1);
	}

	/* calltype */
	Return((*call)(ptr, stream, pos, ret, &check));

	/* execute */
	if (check) {
		Return(eval_loop_stream(ptr, stream, pos));
	}

	/* normal */
	return 0;
}

static int eval_loop_restart(Execute ptr, addr stream,
		eval_loop_calltype call, int *ret)
{
	addr control, restart;
	struct eval_loop_struct str;

	push_new_control(ptr, &control);
	str.stream = stream;
	str.call = call;
	str.ret = ret;
	eval_main_restart_abort(&restart);
	Return(restart0_control(ptr, restart, eval_loop_execute, (void *)&str));

	return free_control_(ptr, control);
}

_g int eval_custom_loop(Execute ptr, addr stream, eval_loop_calltype call)
{
	int exit;
	size_t index;

	index = getindex_prompt(ptr);
	eval_loop_variable(ptr);
	while (! getbreak_prompt(ptr)) {
		setindex_prompt(ptr, index);
		setshow_prompt(ptr, 1);

		if (eval_loop_restart(ptr, stream, call, &exit)) {
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

static int eval_main_execute(Execute ptr, addr stream, addr pos, int *exit, int *exec)
{
	*exit = 0;
	*exec = 1;
	return 0;
}

_g int eval_main_loop_(Execute ptr)
{
	addr stream;
	terminal_io_stream(ptr, &stream);
	return eval_custom_loop(ptr, stream, eval_main_execute);
}


/*
 *  eval_main_string
 */
static int evalcall_string_result_(Execute ptr, addr eval)
{
	addr stream;

	open_input_string_stream(&stream, eval);
	push_close_stream(ptr, stream);
	return eval_stream_partial(ptr, stream);
}

_g int eval_main_string_(Execute ptr, addr eval)
{
	addr control, restart;

	push_new_control(ptr, &control);
	eval_main_restart_abort(&restart);
	Return(restart1_control(ptr, restart, evalcall_string_result_, eval));
	return free_control_(ptr, control);
}


/*
 *  eval_main_load
 */
struct eval_main_load_struct {
	addr file;
	int exists;
	int *ret;
};

static int eval_main_load_execute(Execute ptr, void *voidp)
{
	struct eval_main_load_struct *str;
	str = (struct eval_main_load_struct *)voidp;
	return eval_load(ptr, str->ret, str->file, Nil, Nil, str->exists, Unbound);
}

_g int eval_main_load_(Execute ptr, addr file, int exists, int *ret)
{
	int check;
	addr control, restart;
	struct eval_main_load_struct str;

	push_new_control(ptr, &control);
	str.file = file;
	str.exists = exists;
	str.ret = ret? ret: &check;
	eval_main_restart_abort(&restart);
	Return(restart0_control(ptr, restart, eval_main_load_execute, (void *)&str));

	return free_control_(ptr, control);
}


/*
 *  initialize
 */
_g void init_eval_main(void)
{
	SetPointerCall(defun, empty, eval_loop_abort_function);
	SetPointerCall(defun, var1, eval_loop_abort_report);
	SetPointerCall(defun, var1, eval_loop_abort_test);
}

