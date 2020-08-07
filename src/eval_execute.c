#include "code_make.h"
#include "compile_eval.h"
#include "compile_file.h"
#include "compile_load.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "execute_values.h"
#include "file_open.h"
#include "files.h"
#include "hold.h"
#include "make_load_form.h"
#include "optimize_parse.h"
#include "parse.h"
#include "pathname_object.h"
#include "pathname.h"
#include "prompt.h"
#include "reader.h"
#include "scope.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  toplevel
 */
static void symbol_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_TOPLEVEL, ret);
}

_g int gettoplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

_g void settoplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

_g int toplevelp_eval(Execute ptr)
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

_g int get_compile_time_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

_g void set_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	pushspecial_control(ptr, symbol, value);
}

_g int compile_time_too_eval_(Execute ptr, int *ret)
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

_g int get_compile_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

_g void set_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

_g int compile_toplevel_p_eval_(Execute ptr, int *ret)
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

_g int get_load_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

_g void set_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

_g int load_toplevel_p_eval_(Execute ptr, int *ret)
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

_g int get_execute_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

_g void set_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

_g void push_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

_g int executep_eval_(Execute ptr, int *ret)
{
	addr value;
	Return(get_execute_eval_(ptr, &value));
	return Result(ret, value != Nil);
}


/*
 *  eval
 */
static int eval_execute_scope(Execute ptr, LocalHold hold, addr pos)
{
	/* optimize parse */
	localhold_set(hold, 0, pos);
	optimize_parse(ptr->local, &pos, pos);
	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope(ptr, &pos, pos));
	/* code generator */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);
	/* execute */
	localhold_set(hold, 0, pos);
	return runcode_control(ptr, pos);
}

static int eval_execute(Execute ptr, addr pos, addr toplevel)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(eval_parse(ptr, &pos, pos, toplevel));
	Return(eval_execute_scope(ptr, hold, pos));
	localhold_end(hold);

	return 0;
}

_g int eval_execute_partial(Execute ptr, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	Return(eval_execute(ptr, pos, Nil));
	return free_control_(ptr, control);
}

_g int eval_result_partial(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	Return(eval_execute(ptr, pos, Nil));
	getresult_control(ptr, &pos);
	localhold_set(hold, 0, pos);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

_g int eval_result_macro(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	push_new_control(ptr, &control);
	localhold_set(hold, 1, pos);
	Return(eval_execute_scope(ptr, hold, pos));
	getresult_control(ptr, &pos);
	localhold_set(hold, 2, pos);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}


/*
 *  eval
 */
static int eval_stream(Execute ptr, addr stream, addr toplevel)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(eval_execute(ptr, pos, toplevel));
	}

	return 0;
}

_g int eval_stream_partial(Execute ptr, addr stream)
{
	addr control;

	push_new_control(ptr, &control);
	Return(eval_stream(ptr, stream, Nil));
	return free_control_(ptr, control);
}

_g int eval_stream_toplevel(Execute ptr, addr stream)
{
	addr control;

	push_new_control(ptr, &control);
	Return(eval_stream(ptr, stream, T));
	return free_control_(ptr, control);
}

_g int eval_object(Execute ptr, addr eval, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil); /* Don't run compile mode. */

	gchold_push_local(ptr->local, eval);
	Return(eval_execute(ptr, eval, Nil));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  eval-load
 */
static int eval_load_fasl_p_(addr file, int *ret)
{
	int check;

	if (streamp(file))
		return Result(ret, 0);
	GetTypePathname(file, &file);
	if (! stringp(file))
		return Result(ret, 0);
	Return(string_equalp_char_(file, "fasl", &check));
	if (check)
		return Result(ret, 1);
	else
		return string_equalp_char_(file, "fas", ret);
}

static int eval_load_push(Execute ptr, addr file, int exist, int binary,
		int *ret, addr *rcontrol, addr *rstream)
{
	int closep;
	addr stream;

	/* stream */
	closep = 1;
	if (streamp(file)) {
		stream = file;
		closep = 0;
	}
	else {
		if (binary) {
			Return(open_input_binary_stream_(ptr, &stream, file));
		}
		else {
			Return(open_input_stream_(ptr, &stream, file));
		}
		if (stream == NULL) {
			if (exist)
				simple_file_error_stdarg(file, "Cannot open file ~S.", file, NULL);
			return Result(ret, 0);
		}
	}

	/* eval */
	push_new_control(ptr, rcontrol);
	gchold_push_local(ptr->local, stream);
	if (closep)
		push_close_stream(ptr, stream);
	*rstream = stream;
	return Result(ret, 1);
}

static int eval_load_fasl(Execute ptr, int *ret, addr file, int exist)
{
	int check;
	addr control, stream;

	Return(eval_load_push(ptr, file, exist, 1, &check, &control, &stream));
	if (! check)
		return Result(ret, 0);
	init_read_make_load_form(ptr);
	Return(eval_compile_load(ptr, stream));
	Return(free_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_lisp(Execute ptr, int *ret, addr file, int exist)
{
	int check;
	addr control, stream;

	Return(eval_load_push(ptr, file, exist, 0, &check, &control, &stream));
	if (! check)
		return Result(ret, 0);
	Return(eval_stream(ptr, stream, T));
	Return(free_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_check_type(Execute ptr, addr file, addr *ret)
{
	addr check;

	GetTypePathname(file, &check);
	if (! stringp(check))
		return Result(ret, file);

	/* *. */
	Return(probe_file_files_(ptr, &check, file));
	if (check != Nil)
		return Result(ret, file);
	copy_pathname_heap(&file, file);

	/* *.fasl */
	strvect_char_heap(&check, "fasl");
	SetTypePathname(file, check);
	Return(probe_file_files_(ptr, &check, file));
	if (check != Nil)
		return Result(ret, file);

	/* *.lisp */
	strvect_char_heap(&check, "lisp");
	SetTypePathname(file, check);
	Return(probe_file_files_(ptr, &check, file));
	if (check != Nil)
		return Result(ret, file);

	/* do nothing */
	return Result(ret, file);
}

static int eval_load_check(
		Execute ptr, addr file, addr verbose, addr print, addr external,
		constindex file_pathname,
		constindex file_truename,
		constindex file_verbose,
		constindex file_print,
		addr *ret)
{
	int check;
	addr symbol, pos, truename, value;

	/* wild-pathname-p */
	if (! streamp(file)) {
		Return(pathname_designer_heap_(ptr, file, &file));
		Return(wild_pathname_boolean_(file, Nil, &check));
		if (check ) {
			return call_simple_file_error_va_(ptr, file,
					"LOAD don't allow the wildcard filename ~S.", file, NULL);
		}
	}
	/* type */
	if (! streamp(file)) {
		Return(eval_load_check_type(ptr, file, &file));
	}
	/* load-pathname */
	GetConstant(file_pathname, &symbol);
	if (streamp(file)) {
		GetPathnameStream(file, &value);
		if (value != Nil) {
			Return(physical_pathname_heap_(ptr, file, &value));
			pushspecial_control(ptr, symbol, value);
		}
	}
	else {
		Return(physical_pathname_heap_(ptr, file, &file));
		pushspecial_control(ptr, symbol, file);
		value = file;
	}
	/* load-truename */
	if (value != Nil) {
		GetConstant(file_truename, &symbol);
		Return(truename_files_(ptr, value, &truename, 0));
		pushspecial_control(ptr, symbol, truename);
	}
	/* package */
	GetConst(SPECIAL_PACKAGE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* readtable */
	GetConst(SPECIAL_READTABLE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	pushspecial_control(ptr, symbol, pos);
	/* verbose */
	if (verbose != Unbound) {
		GetConstant(file_verbose, &symbol);
		pushspecial_control(ptr, symbol, verbose);
	}
	/* print */
	if (print != Unbound) {
		GetConstant(file_print, &symbol);
		pushspecial_control(ptr, symbol, print);
	}
	/* external-format */
	if (external != Unbound) {
		GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
		pushspecial_control(ptr, symbol, external);
	}
	/* result */
	return Result(ret, file);
}

static int eval_load_file(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	int check;

	Return(eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_TRUENAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			CONSTANT_SPECIAL_LOAD_PRINT,
			&file));
	Return(eval_load_fasl_p_(file, &check));
	if (check)
		return eval_load_fasl(ptr, ret, file, exist);
	else
		return eval_load_lisp(ptr, ret, file, exist);
}

_g int eval_load(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_new_control(ptr, &control);
	push_prompt_info(ptr);
	set_eval_compile_mode(ptr, Nil);
	Return(eval_load_file(ptr, ret, file, verbose, print, exist, external));
	return free_control_(ptr, control);
}


/*
 *  compile-file
 */
static int compile_load_stream(Execute ptr, addr stream)
{
	int check;
	addr pos;

	for (;;) {
		Return(read_stream(ptr, stream, &check, &pos));
		if (check)
			break;
		Return(compile_eval(ptr, pos));
	}

	return 0;
}

static int compile_load_lisp(Execute ptr, int *ret, addr file, int exist)
{
	int check;
	addr control, stream;

	Return(eval_load_push(ptr, file, exist, 0, &check, &control, &stream));
	if (! check)
		return Result(ret, 0);
	Return(compile_load_stream(ptr, stream));
	Return(free_control_(ptr, control));
	return Result(ret, 1);
}

static int compile_load_file(
		Execute ptr, addr file, addr verbose, addr print, addr external)
{
	int check;

	Return(eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			CONSTANT_SPECIAL_COMPILE_PRINT,
			&file));
	return compile_load_lisp(ptr, &check, file, 1);
}

_g int compile_load(Execute ptr, addr file, addr verbose, addr print, addr external)
{
	addr control;

	push_new_control(ptr, &control);
	push_prompt_info(ptr);
	Return(compile_load_file(ptr, file, verbose, print, external));
	return free_control_(ptr, control);
}

