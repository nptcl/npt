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
#include "file.h"
#include "files.h"
#include "hold.h"
#include "make_load_form.h"
#include "optimize_parse.h"
#include "parse.h"
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

_g void gettoplevel_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
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
	getspecialcheck_local(ptr, pos, &pos);
	return pos != Nil;
}


/*
 *  compile-time
 */
static void symbol_compile_time_eval(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TIME, ret);
}

_g void get_compile_time_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	getspecialcheck_local(ptr, symbol, ret);
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

_g int compile_time_too_eval(Execute ptr)
{
	addr value;
	get_compile_time_eval(ptr, &value);
	return value != Nil;
}


/*
 *  eval-when :compile-toplevel
 */
static void symbol_compile_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_COMPILE_TOPLEVEL, ret);
}

_g void get_compile_toplevel_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
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

_g int compile_toplevel_p_eval(Execute ptr)
{
	addr value;
	get_compile_toplevel_eval(ptr, &value);
	return value != Nil;
}


/*
 *  eval-when :load-toplevel
 */
static void symbol_load_toplevel_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_LOAD_TOPLEVEL, ret);
}

_g void get_load_toplevel_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
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

_g int load_toplevel_p_eval(Execute ptr)
{
	addr value;
	get_load_toplevel_eval(ptr, &value);
	return value != Nil;
}


/*
 *  eval-when :execute
 */
static void symbol_execute_eval(addr *ret)
{
	GetConst(SYSTEM_EVAL_EXECUTE, ret);
}

_g void get_execute_eval(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
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

_g int executep_eval(Execute ptr)
{
	addr value;
	get_execute_eval(ptr, &value);
	return value != Nil;
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

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	Return(eval_execute_scope(ptr, hold, pos));
	getresult_control(ptr, &pos);
	localhold_set(hold, 0, pos);
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
static int eval_load_fasl_p(addr file)
{
	if (streamp(file))
		return 0;
	GetPathname(file, PATHNAME_INDEX_TYPE, &file);
	return stringp(file) &&
		(string_equalp_char(file, "fasl") || string_equalp_char(file, "fas"));
}

static int eval_load_push(Execute ptr, addr file, int exist, int binary,
		int *ret, addr *rcontrol, addr *rstream)
{
	int closep, check;
	addr stream;

	/* stream */
	closep = 0;
	if (streamp(file)) {
		stream = file;
		closep = 1;
	}
	else {
		if (binary)
			check = open_input_binary_stream(ptr, &stream, file);
		else
			check = open_input_stream(ptr, &stream, file);
		if (check) {
			if (exist)
				simple_file_error_stdarg(file, "Cannot open file ~S.", file, NULL);
			return Result(ret, 0);
		}
	}

	/* eval */
	push_new_control(ptr, rcontrol);
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
	return Result(ret, 1);
}

static void eval_load_check_type(Execute ptr, addr file, addr *ret)
{
	addr check;

	GetPathname(file, PATHNAME_INDEX_TYPE, &check);
	if (! stringp(check)) {
		*ret = file;
		return;
	}

	/* *. */
	probe_file_files(ptr, &check, file);
	if (check != Nil) {
		*ret = file;
		return;
	}
	copy_pathname_heap(&file, file);

	/* *.fasl */
	strvect_char_heap(&check, "fasl");
	SetPathname(file, PATHNAME_INDEX_TYPE, check);
	probe_file_files(ptr, &check, file);
	if (check != Nil) {
		*ret = file;
		return;
	}

	/* *.lisp */
	strvect_char_heap(&check, "lisp");
	SetPathname(file, PATHNAME_INDEX_TYPE, check);
	probe_file_files(ptr, &check, file);
	if (check != Nil) {
		*ret = file;
		return;
	}

	/* do nothing */
	*ret = file;
}

static void eval_load_check(
		Execute ptr, addr file, addr verbose, addr print, addr external,
		constindex file_pathname,
		constindex file_truename,
		constindex file_verbose,
		constindex file_print,
		addr *ret)
{
	addr symbol, pos, truename, value;

	/* wild-pathname-p */
	if (! streamp(file)) {
		pathname_designer_heap(ptr, file, &file);
		if (wild_pathname_boolean(file, Nil))
			file_error(file);
	}
	/* type */
	if (! streamp(file))
		eval_load_check_type(ptr, file, &file);
	/* load-pathname */
	GetConstant(file_pathname, &symbol);
	if (streamp(file)) {
		GetPathnameStream(file, &value);
		if (value != Nil) {
			physical_pathname_heap(ptr, file, &value);
			pushspecial_control(ptr, symbol, value);
		}
	}
	else {
		physical_pathname_heap(ptr, file, &file);
		pushspecial_control(ptr, symbol, file);
		value = file;
	}
	/* load-truename */
	if (value != Nil) {
		GetConstant(file_truename, &symbol);
		truename_files(ptr, value, &truename, 0);
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
	*ret = file;
}

static int eval_load_file(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_TRUENAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			CONSTANT_SPECIAL_LOAD_PRINT,
			&file);
	if (eval_load_fasl_p(file))
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
	return Result(ret, 1);
}

static int compile_load_file(
		Execute ptr, addr file, addr verbose, addr print, addr external)
{
	int check;

	eval_load_check(ptr, file, verbose, print, external,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			CONSTANT_SPECIAL_COMPILE_PRINT,
			&file);
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

