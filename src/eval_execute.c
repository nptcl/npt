#include "code_make.h"
#include "compile_eval.h"
#include "compile_file.h"
#include "compile_load.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_execute.h"
#include "execute.h"
#include "execute_values.h"
#include "file_open.h"
#include "files.h"
#include "hashtable.h"
#include "hold.h"
#include "make_load_form.h"
#include "optimize_parse.h"
#include "parse.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
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

int gettoplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void settoplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int toplevelp_eval(Execute ptr)
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

int get_compile_time_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_time_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_time_eval(ptr, &symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_time_too_eval_(Execute ptr, int *ret)
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

int get_compile_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_compile_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_compile_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int compile_toplevel_p_eval_(Execute ptr, int *ret)
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

int get_load_toplevel_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_load_toplevel_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_load_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int load_toplevel_p_eval_(Execute ptr, int *ret)
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

int get_execute_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	setspecial_local(ptr, symbol, value);
}

void push_execute_eval(Execute ptr, addr value)
{
	addr symbol;
	symbol_execute_eval(&symbol);
	pushspecial_control(ptr, symbol, value);
}

int executep_eval_(Execute ptr, int *ret)
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
	Return(optimize_parse_(ptr->local, pos, &pos, NULL));
	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope(ptr, &pos, pos));
	/* code generator */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);
	/* execute */
	localhold_set(hold, 0, pos);
	return runcode_control_(ptr, pos);
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

int eval_execute_partial(Execute ptr, addr pos)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_execute(ptr, pos, Nil);
	return pop_control_(ptr, control);
}

static int eval_result_partial_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	Return(eval_execute(ptr, pos, Nil));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, pos);

	return 0;
}

int eval_result_partial(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)eval_result_partial_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

static int eval_result_macro_call_(Execute ptr, LocalHold hold, addr pos, addr *ret)
{
	localhold_set(hold, 1, pos);
	Return(eval_execute_scope(ptr, hold, pos));
	getresult_control(ptr, ret);
	localhold_set(hold, 2, pos);

	return 0;
}

int eval_result_macro(Execute ptr, addr pos, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	push_control(ptr, &control);
	eval_result_macro_call_(ptr, hold, pos, &pos);
	Return(pop_control_(ptr, control));
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

int eval_stream_partial(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_stream(ptr, stream, Nil);
	return pop_control_(ptr, control);
}

int eval_stream_toplevel(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_stream(ptr, stream, T);
	return pop_control_(ptr, control);
}

static int eval_object_call_(Execute ptr, LocalHold hold, addr eval, addr *ret)
{
	/* Don't run compile mode. */
	set_eval_compile_mode(ptr, Nil);

	/* execute */
	gchold_push_local(ptr->local, eval);
	Return(eval_execute(ptr, eval, Nil));
	getresult_control(ptr, ret);
	localhold_set(hold, 0, *ret);

	return 0;
}

int eval_object(Execute ptr, addr eval, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	eval_object_call_(ptr, hold, eval, ret);
	Return(pop_control_(ptr, control));
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

static int eval_load_open_file_(Execute ptr, addr *ret, addr file, int binary)
{
	if (binary)
		return open_input_binary_stream_(ptr, ret, file);
	else
		return open_input_stream_(ptr, ret, file);
}


static int eval_load_open_(Execute ptr, addr file, int exist, int binary,
		int *openp, int *closep, addr *ret)
{
	addr stream;

	/* stream */
	if (streamp(file)) {
		*openp = 1;
		*closep = 0;
		return Result(ret, file);
	}

	/* open pathname */
	Return(eval_load_open_file_(ptr, &stream, file, binary));
	if (stream != NULL) {
		*openp = 1;
		*closep = 1;
		return Result(ret, stream);
	}

	/* file-error */
	if (exist) {
		*ret = Nil;
		*openp = *closep = 0;
		return call_simple_file_error_va_(ptr, file,
				"Cannot open file ~S.", file, NULL);
	}

	/* file is not open */
	*openp = 0;
	*closep = 0;
	return Result(ret, Nil);
}

static void init_read_compile_gensym(Execute ptr)
{
	addr symbol, pos;

	GetConst(SYSTEM_COMPILE_GENSYM, &symbol);
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	pushspecial_control(ptr, symbol, pos);
}

static int eval_load_fasl_call_(Execute ptr, addr file, int closep)
{
	gchold_push_local(ptr->local, file);
	if (closep)
		push_close_stream(ptr, file);
	init_read_make_load_form(ptr);
	init_read_compile_gensym(ptr);
	return eval_compile_load(ptr, file);
}

static int eval_load_fasl_(Execute ptr, int *ret, addr file, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, exist, 1, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)eval_load_fasl_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_lisp_call_(Execute ptr, addr file, int closep)
{
	gchold_push_local(ptr->local, file);
	if (closep)
		push_close_stream(ptr, file);
	return eval_stream(ptr, file, T);
}

static int eval_load_lisp_(Execute ptr, int *ret, addr file, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)eval_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
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
		if (memory_stream_p(value)) {
			value = Nil;
			pushspecial_control(ptr, symbol, value);
		}
		else if (value != Nil) {
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
		return eval_load_fasl_(ptr, ret, file, exist);
	else
		return eval_load_lisp_(ptr, ret, file, exist);
}

int eval_load(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	push_prompt_info(ptr);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file(ptr, ret, file, verbose, print, exist, external);
	return pop_control_(ptr, control);
}

static int eval_load_file_switch_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external, int faslp)
{
	Return(eval_load_check(ptr, file, verbose, print, external,
				CONSTANT_SPECIAL_LOAD_PATHNAME,
				CONSTANT_SPECIAL_LOAD_TRUENAME,
				CONSTANT_SPECIAL_LOAD_VERBOSE,
				CONSTANT_SPECIAL_LOAD_PRINT,
				&file));
	if (faslp)
		return eval_load_fasl_(ptr, ret, file, exist);
	else
		return eval_load_lisp_(ptr, ret, file, exist);
}

int eval_load_force_lisp_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	push_prompt_info(ptr);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 0);
	return pop_control_(ptr, control);
}

int eval_load_force_fasl_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	push_prompt_info(ptr);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 1);
	return pop_control_(ptr, control);
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

static int compile_load_lisp_call_(Execute ptr, addr file, int closep)
{
	gchold_push_local(ptr->local, file);
	if (closep)
		push_close_stream(ptr, file);
	return compile_load_stream(ptr, file);
}

static int compile_load_lisp(Execute ptr, int *ret, addr file, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)compile_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
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

int compile_load(Execute ptr, addr file, addr verbose, addr print, addr external)
{
	addr control;

	push_control(ptr, &control);
	push_prompt_info(ptr);
	(void)compile_load_file(ptr, file, verbose, print, external);
	return pop_control_(ptr, control);
}

