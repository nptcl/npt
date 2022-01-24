#include "compile_eval.h"
#include "compile_file.h"
#include "condition.h"
#include "cons.h"
#include "control_execute.h"
#include "control_object.h"
#include "equal.h"
#include "eval_execute.h"
#include "eval_object.h"
#include "eval_stack.h"
#include "eval_value.h"
#include "execute_values.h"
#include "hold.h"
#include "load_instance.h"
#include "load_time_value.h"
#include "make.h"
#include "optimize_parse.h"
#include "parse_function.h"
#include "parse_macro.h"
#include "parse_object.h"
#include "print_write.h"
#include "reader.h"
#include "scope.h"
#include "scope_declare.h"
#include "scope_object.h"
#include "step.h"
#include "stream.h"
#include "stream_common.h"
#include "symbol.h"
#include "type_value.h"
#include "typedef.h"

/*
 *  begin, end
 */
static void begin_eval(Execute ptr, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	init_parse_step(ptr);
	init_parse_environment(ptr);
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	push_parse_declare(ptr, Nil);
	disable_load_time_value(ptr);
	*ret = control;
}


/*
 *  eval
 */
static int eval_execute_scope_(Execute ptr, LocalHold hold, addr pos)
{
	/* optimize */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));

	/* code */
	localhold_set(hold, 0, pos);
	Return(code_make_(ptr, &pos, pos));

	/* execute */
	localhold_set(hold, 0, pos);
	return runcode_control_(ptr, pos);
}

static int eval_execute_parse_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(parse_execute_(ptr, &pos, pos));
	Return(eval_execute_scope_(ptr, hold, pos));
	localhold_end(hold);

	return 0;
}

static int eval_execute_(Execute ptr, addr pos, addr compiler_macro)
{
	addr control;

	push_control(ptr, &control);
	push_enable_compiler_macro(ptr, compiler_macro);
	(void)eval_execute_parse_(ptr, pos);
	return pop_control_(ptr, control);
}


/*
 *  interface
 */
static int eval_result_partial_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 0, pos);
	Return(eval_execute_(ptr, pos, Nil));
	getresult_control(ptr, ret);
	localhold_set(hold, 1, *ret);

	return 0;
}

int eval_result_partial_(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	begin_eval(ptr, &control);
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

	begin_eval(ptr, &control);
	(void)eval_result_partial_form_call_(ptr, pos, ret);
	return pop_control_(ptr, control);
}

int eval_result_compile_(Execute ptr, addr pos, addr *ret)
{
	addr control;

	begin_eval(ptr, &control);
	set_eval_compile_mode(ptr, Nil); /* Don't run compile mode. */
	gchold_push_special(ptr, pos);
	if (eval_execute_(ptr, pos, T))
		goto escape;
	getresult_control(ptr, ret);
escape:
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
static int eval_toplevel_execute_(Execute ptr, addr pos);

/* progn */
static int eval_toplevel_progn_(Execute ptr, addr list)
{
	addr pos;
	LocalHold hold;

	/* (progn) -> nil */
	if (list == Nil)
		return eval_toplevel_execute_(ptr, Nil);

	hold = LocalHold_local_push(ptr, list);
	/* (progn ...) */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(eval_toplevel_execute_(ptr, pos));
	}
	localhold_end(hold);

	return 0;
}


/* locally */
static int eval_toplevel_locally_call_(Execute ptr, addr decl, addr body)
{
	addr list, control;

	if (decl == Nil)
		return eval_toplevel_progn_(ptr, body);

	/* addr *parse-declare* */
	Return(add_parse_declare_(ptr, decl, &list));
	push_control(ptr, &control);
	push_parse_declare(ptr, list);
	(void)eval_toplevel_progn_(ptr, body);
	return pop_control_(ptr, control);
}

static int eval_toplevel_locally_(Execute ptr, addr list)
{
	addr decl;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_push(hold, list);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);

	return 0;
}


/* eval-when */
static int eval_toplevel_eval_when_call_(Execute ptr, addr list)
{
	addr args, compile, load, exec;
	addr compile1, load1, exec1, ctime1;

	if (! consp_getcons(list, &args, &list))
		return fmte_("eval-when form must be (eval-when (...) . body).", NULL);

	/* arguments */
	Return(parse_eval_when_list_(args, &compile, &load, &exec));

	/* backup */
	Return(get_compile_toplevel_eval_(ptr, &compile1));
	Return(get_load_toplevel_eval_(ptr, &load1));
	Return(get_execute_eval_(ptr, &exec1));
	Return(get_compile_time_eval_(ptr, &ctime1));

	/* set variable */
	set_compile_toplevel_eval(ptr, compile);
	set_load_toplevel_eval(ptr, load);
	set_execute_eval(ptr, exec);

	/* discard */
	if (! parse_eval_when_process(ptr, compile, load, exec, T, ctime1))
		return eval_toplevel_execute_(ptr, Nil);

	/* execute */
	Return(eval_toplevel_progn_(ptr, list));

	/* rollback */
	set_compile_toplevel_eval(ptr, compile1);
	set_load_toplevel_eval(ptr, load1);
	set_execute_eval(ptr, exec1);
	set_compile_time_eval(ptr, ctime1);

	return 0;
}

static int eval_toplevel_eval_when_(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_toplevel_eval_when_call_(ptr, pos);
	return pop_control_(ptr, control);
}


/* macrolet */
static int eval_toplevel_macrolet_(Execute ptr, addr list)
{
	addr args, decl, rollback;
	LocalHold hold;

	if (! consp_getcons(list, &args, &list))
		return fmte_("macrolet form must be (macrolet args . body).", NULL);

	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));
	Return(parse_macrolet_args_(ptr, args));

	/* decl */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);

	/* body */
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);
	return rollback_envstack_(ptr, rollback);
}


/* symbol-macrolet */
static int eval_toplevel_symbol_macrolet_(Execute ptr, addr list)
{
	addr args, decl, rollback;
	LocalHold hold;

	if (! consp_getcons(list, &args, &list)) {
		return fmte_("symbol-macrolet form must be "
				"(symbol-macrolet args . body).", NULL);
	}

	/* local scope environment */
	Return(snapshot_envstack_(ptr, &rollback));

	/* decl */
	hold = LocalHold_local(ptr);
	Return(parse_declare_body_(ptr, list, &decl, &list));
	localhold_pushva(hold, decl, list, NULL);

	/* args */
	Return(parse_symbol_macrolet_args_(ptr, args, decl));

	/* body */
	Return(eval_toplevel_locally_call_(ptr, decl, list));
	localhold_end(hold);
	return rollback_envstack_(ptr, rollback);
}


/* value */
static int eval_execute_value_(Execute ptr, addr value)
{
	addr pos;
	LocalHold hold;

	/* parse */
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 1, value);
	Return(parse_execute_(ptr, &pos, value));

	/* optimize */
	localhold_set(hold, 0, pos);
	Return(optimize_parse_(ptr, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));

	/* code */
	localhold_set(hold, 0, pos);
	Return(code_make_(ptr, &pos, pos));

	/* close *parse-declare* */
	set_parse_declare(ptr, Nil);

	/* execute */
	localhold_set(hold, 0, pos);
	Return(runcode_control_(ptr, pos));
	localhold_end(hold);

	return 0;
}

static int eval_toplevel_value_(Execute ptr, addr pos)
{
	if (eval_compile_p(ptr))
		return compile_eval_value_(ptr, pos);
	else
		return eval_execute_value_(ptr, pos);
}


/* toplevel */
static int eval_toplevel_cons_(Execute ptr, addr cons)
{
	addr car, cdr, check;

	GetCons(cons, &car, &cdr);

	/* macro */
	Return(parse_macroexpand_(ptr, &check, cons));
	if (check != Unbound)
		return eval_toplevel_execute_(ptr, check);

	/* progn */
	GetConst(COMMON_PROGN, &check);
	if (car == check)
		return eval_toplevel_progn_(ptr, cdr);

	/* locally */
	GetConst(COMMON_LOCALLY, &check);
	if (car == check)
		return eval_toplevel_locally_(ptr, cdr);

	/* evan-when */
	GetConst(COMMON_EVAL_WHEN, &check);
	if (car == check)
		return eval_toplevel_eval_when_(ptr, cdr);

	/* macrolet */
	GetConst(COMMON_MACROLET, &check);
	if (car == check)
		return eval_toplevel_macrolet_(ptr, cdr);

	/* symbol-macrolet */
	GetConst(COMMON_SYMBOL_MACROLET, &check);
	if (car == check)
		return eval_toplevel_symbol_macrolet_(ptr, cdr);

	/* function */
	return eval_toplevel_value_(ptr, cons);
}

static int eval_toplevel_execute_call_(Execute ptr, addr pos)
{
	int check;
	addr value;

	if (consp(pos))
		return eval_toplevel_cons_(ptr, pos);

	if (! symbolp(pos))
		return eval_toplevel_value_(ptr, pos);

	/* symbol */
	Return(symbol_macrolet_envstack_p_(ptr, pos, &value, &check));
	if (check)
		return eval_toplevel_execute_(ptr, value);

	return eval_toplevel_value_(ptr, pos);
}

static int eval_toplevel_print_char_(Execute ptr, addr pos, addr stream, const char *str)
{
	int ignore;

	Return(fresh_line_stream_(stream, &ignore));
	Return(print_ascii_stream_(stream, str));
	Return(prin1_print_(ptr, stream, pos));
	Return(terpri_stream_(stream));

	return 0;
}

static int eval_toplevel_print_output_(Execute ptr, addr pos, int load, int compile)
{
	addr symbol, value, stream;

	/* (setq *print-level* 2) */
	GetConst(SPECIAL_PRINT_LEVEL, &symbol);
	fixnum_heap(&value, 2);
	pushspecial_control(ptr, symbol, value);

	/* (setq *print-length* 3) */
	GetConst(SPECIAL_PRINT_LENGTH, &symbol);
	fixnum_heap(&value, 3);
	pushspecial_control(ptr, symbol, value);

	/* (format *standard-output* "~&;; Load: ~S~%") */
	Return(standard_output_stream_(ptr, &stream));
	if (load) {
		Return(eval_toplevel_print_char_(ptr, pos, stream, ";; Load: "));
	}

	/* (format *standard-output* "~&;; Compile: ~S~%") */
	if (compile) {
		Return(eval_toplevel_print_char_(ptr, pos, stream, ";; Compile: "));
	}

	return 0;
}

static int eval_toplevel_print_(Execute ptr, addr pos)
{
	int check1, check2;
	addr control, symbol, value;

	/* load */
	GetConst(SPECIAL_LOAD_PRINT, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	check1 = (value != Nil);

	/* compile-file */
	GetConst(SPECIAL_COMPILE_PRINT, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	check2 = (value != Nil);

	/* print */
	if (check1 || check2) {
		push_control(ptr, &control);
		(void)eval_toplevel_print_output_(ptr, pos, check1, check2);
		return pop_control_(ptr, control);
	}
	return 0;
}

static int eval_toplevel_execute_(Execute ptr, addr pos)
{
	LocalHold hold;

	hold = LocalHold_local_push(ptr, pos);
	Return(eval_toplevel_print_(ptr, pos));
	Return(eval_toplevel_execute_call_(ptr, pos));
	localhold_end(hold);

	return 0;
}

int eval_toplevel_loop_(Execute ptr, addr stream)
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

	begin_eval(ptr, &control);
	(void)eval_toplevel_loop_(ptr, stream);
	return pop_control_(ptr, control);
}

int eval_execute_partial_(Execute ptr, addr pos)
{
	addr control;

	begin_eval(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_toplevel_execute_(ptr, pos);
	return pop_control_(ptr, control);
}

