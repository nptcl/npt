#include "cons.h"
#include "condition.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "eval_load.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "print_write.h"
#include "prompt.h"
#include "prompt_for.h"
#include "reader.h"
#include "restart.h"
#include "restart_value.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  restart abort
 */
static void eval_main_restart_abort(addr *ret)
{
	abort_restart_char_heap(ret, "Return to eval-loop.");
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

int eval_loop_output(Execute ptr, addr stream)
{
	addr list, pos;

	getvalues_list_control_heap(ptr, &list);
	eval_loop_shift(ptr, list);
	/* format_stream(ptr, stream, "~&~{~S~%~}~&", list, NULL); */
	Return(fresh_line_stream_(stream, NULL));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(prin1_print(ptr, stream, pos));
		Return(terpri_stream_(stream));
	}
	Return(fresh_line_stream_(stream, NULL));
	return force_output_stream_(stream);
}

static int eval_loop_stream_call_(Execute ptr, addr stream, addr pos)
{
	eval_loop_minus(ptr, pos);
	Return(eval_execute_partial_(ptr, pos));
	return eval_loop_output(ptr, stream);
}

static int eval_loop_stream(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_loop_stream_call_(ptr, stream, pos);
	return pop_control_(ptr, control);
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
	int *ret, eof;
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
		str->eof = 1;
		Return(fresh_line_stream_(stream, NULL));
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
		eval_loop_calltype call, int *ret, int *eof)
{
	addr control, restart;
	struct eval_loop_struct str;

	push_control(ptr, &control);
	str.stream = stream;
	str.call = call;
	str.ret = ret;
	str.eof = 0;
	eval_main_restart_abort(&restart);
	(void)restart0_control(ptr, restart, eval_loop_execute, (void *)&str);
	*eof = str.eof;
	return pop_control_(ptr, control);
}

int eval_custom_loop_(Execute ptr, addr stream, eval_loop_calltype call, int *ret)
{
	int exit, eof;

	eval_loop_variable(ptr);
	exit = 0;
	for (;;) {
		Return(eval_loop_restart(ptr, stream, call, &exit, &eof));
		if (exit)
			break;
	}
	if (ret)
		*ret = eof;

	Return(terpri_stream_(stream));
	return finish_output_stream_(stream);
}

static int eval_main_execute(Execute ptr, addr stream, addr pos, int *exit, int *exec)
{
	*exit = 0;
	*exec = 1;
	return 0;
}

int eval_main_loop_(Execute ptr)
{
	addr stream, control;

	Return(terminal_io_stream_(ptr, &stream));
	push_control(ptr, &control);
	push_prompt_eval_loop(ptr);
	(void)eval_custom_loop_(ptr, stream, eval_main_execute, NULL);
	return pop_control_(ptr, control);
}

int eval_main_loop_toplevel_(Execute ptr)
{
#ifdef LISP_PROMPT_DISABLE
	return eval_main_loop_(ptr);
#else
	addr pos, io;
	unicode c;
	size_t size;

	/* loop */
	for (;;) {
		/* eval-loop */
		Return(eval_main_loop_(ptr));

		/* eval-loop-exit */
		GetConst(SYSTEM_EVAL_LOOP_EXIT, &pos);
		getspecial_local(ptr, pos, &pos);
		if (pos == Unbound || pos == Nil)
			break;

		/* prompt */
		strvect_char_heap(&pos, "Exit? ");
		Return(prompt_string_stream_(ptr, pos, 0, &pos));
		if (pos == Nil) {
			Return(terminal_io_stream_(ptr, &io));
			Return(terpri_stream_(io));
			Return(finish_output_stream_(io));
		}
		if (stringp(pos)) {
			string_length(pos, &size);
			if (size == 0)
				continue;
			Return(string_getc_(pos, 0, &c));
			if (toUpperUnicode(c) == 'Y')
				break;
		}
	}

	return 0;
#endif
}


/*
 *  eval_main_string
 */
static int evalcall_string_result_(Execute ptr, addr eval)
{
	addr stream;

	Return(open_input_string_stream_(&stream, eval));
	(void)eval_stream_toplevel_(ptr, stream);
	return close_stream_unwind_protect_(ptr, stream);
}

int eval_main_string_(Execute ptr, addr eval)
{
	addr control, restart;

	push_control(ptr, &control);
	eval_main_restart_abort(&restart);
	(void)restart1_control(ptr, restart, evalcall_string_result_, eval);
	return pop_control_(ptr, control);
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
	return eval_load_(ptr, str->ret, str->file, Nil, Nil, str->exists, Unbound);
}

int eval_main_load_(Execute ptr, addr file, int exists, int *ret)
{
	int check;
	addr control, restart;
	struct eval_main_load_struct str;

	push_control(ptr, &control);
	str.file = file;
	str.exists = exists;
	str.ret = ret? ret: &check;
	eval_main_restart_abort(&restart);
	(void)restart0_control(ptr, restart, eval_main_load_execute, (void *)&str);
	return pop_control_(ptr, control);
}

