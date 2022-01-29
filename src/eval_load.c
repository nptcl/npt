#include "compile_eval.h"
#include "compile_file.h"
#include "compile_load.h"
#include "condition_define.h"
#include "constant.h"
#include "control_object.h"
#include "eval_execute.h"
#include "eval_load.h"
#include "file_open.h"
#include "files.h"
#include "hashtable.h"
#include "hold.h"
#include "load_instance.h"
#include "pathname.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
#include "print_write.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  verbose
 */
static int load_file_verbose_(Execute ptr, addr file,
		constindex index1, constindex index2,
		const char *str1, const char *str2)
{
	int ignore;
	addr pos, verbose, stream;

	/* filename */
	GetConstant(index1, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound || pos == Nil)
		pos = file;

	/* verbose */
	GetConstant(index2, &verbose);
	getspecial_local(ptr, verbose, &verbose);
	if (verbose == Nil)
		return 0;
	if (verbose == Unbound)
		return 0;

	/* output */
	Return(standard_output_stream_(ptr, &stream));
	Return(fresh_line_stream_(stream, &ignore));
	Return(print_ascii_stream_(stream, str1));
	Return(prin1_print_(ptr, stream, pos));
	Return(print_ascii_stream_(stream, str2));
	Return(terpri_stream_(stream));
	Return(finish_output_stream_(stream));

	return 0;
}

static int compile_file_begin_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			";; Compiling file ", " ...");
}
static int compile_file_end_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
			CONSTANT_SPECIAL_COMPILE_VERBOSE,
			";; Wrote file ", ".");
}

static int load_common_begin_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			";; Loading file ", " ...");
}
static int load_common_end_(Execute ptr, addr file)
{
	return load_file_verbose_(ptr, file,
			CONSTANT_SPECIAL_LOAD_PATHNAME,
			CONSTANT_SPECIAL_LOAD_VERBOSE,
			";; Loaded file ", ".");
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

static int eval_load_open_file_(Execute ptr, addr *ret,
		addr file, addr external, int binary)
{
	if (binary)
		return open_input_binary_stream_(ptr, ret, file);
	else
		return open_input_stream_(ptr, ret, file, external);
}

static int eval_load_open_(Execute ptr,
		addr file, addr external, int exist, int binary,
		int *openp, int *closep, addr *ret)
{
	addr stream;

	/* stream */
	if (streamp(file) && (! memory_stream_p(file))) {
		*openp = 1;
		*closep = 0;
		return Result(ret, file);
	}

	/* open pathname */
	Return(eval_load_open_file_(ptr, &stream, file, external, binary));
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

static int eval_load_fasl_call_(Execute ptr, addr file, int closep)
{
	int escape;

	gchold_push_local(ptr->local, file);
	escape = eval_compile_load_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int eval_load_fasl_(Execute ptr, int *ret, addr file, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, Unbound, exist, 1, &openp, &closep, &file));
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
	int escape;

	gchold_push_local(ptr->local, file);
	escape = eval_stream_toplevel_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int eval_load_lisp_(Execute ptr, int *ret, addr file, addr external, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, external, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)eval_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int eval_load_check_nil_p(addr type)
{
	addr check;
	size_t size;

	/* nil */
	if (type == Nil)
		return 1;

	/* :unspecific */
	GetConst(KEYWORD_UNSPECIFIC, &check);
	if (type == check)
		return 1;

	/* "" */
	if (! stringp(type))
		return 0;
	string_length(type, &size);
	if (size == 0)
		return 1;

	/* otherwise */
	return 0;
}

static int eval_load_check_type_(Execute ptr, addr file, int loadp, addr *ret)
{
	addr pos;

	/* *. */
	Return(probe_file_files_(ptr, &pos, file));
	if (pos != Nil)
		return Result(ret, file);

	/* :type nil */
	GetTypePathname(file, &pos);
	if (! eval_load_check_nil_p(pos))
		return Result(ret, file);
	copy_pathname_heap(&file, file);

	/* *.fasl */
	if (! loadp) {
		strvect_char_heap(&pos, "fasl");
		SetTypePathname(file, pos);
		Return(probe_file_files_(ptr, &pos, file));
		if (pos != Nil)
			return Result(ret, file);
	}

	/* *.lisp */
	strvect_char_heap(&pos, "lisp");
	SetTypePathname(file, pos);
	Return(probe_file_files_(ptr, &pos, file));
	if (pos != Nil)
		return Result(ret, file);

	/* do nothing */
	return Result(ret, file);
}

static int eval_load_check_(
		Execute ptr, addr file, addr verbose, addr print, int loadp,
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
		Return(eval_load_check_type_(ptr, file, loadp, &file));
	}
	/* load-pathname */
	GetConstant(file_pathname, &symbol);
	if (streamp(file)) {
		GetPathnameStream(file, &value);
		if (memory_stream_p(value)) {
			value = Nil;
		}
		else if (value != Nil) {
			Return(physical_pathname_heap_(ptr, file, &value));
		}
	}
	else {
		Return(physical_pathname_heap_(ptr, file, &file));
		value = file;
	}
	pushspecial_control(ptr, symbol, value);
	/* load-truename */
	GetConstant(file_truename, &symbol);
	if (value != Nil) {
		Return(truename_files_(ptr, value, &truename, 0));
		pushspecial_control(ptr, symbol, truename);
	}
	else {
		pushspecial_control(ptr, symbol, Nil);
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

	/* result */
	return Result(ret, file);
}

static int eval_load_type_(Execute ptr, int *ret,
		addr file, addr external, int exist, int faslp)
{
	Return(load_common_begin_(ptr, file));
	if (faslp) {
		Return(eval_load_fasl_(ptr, ret, file, exist));
	}
	else {
		Return(eval_load_lisp_(ptr, ret, file, external, exist));
	}
	Return(load_common_end_(ptr, file));

	return 0;
}

static int eval_load_file_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external)
{
	int faslp;

	Return(eval_load_check_(ptr, file, verbose, print, 1,
				CONSTANT_SPECIAL_LOAD_PATHNAME,
				CONSTANT_SPECIAL_LOAD_TRUENAME,
				CONSTANT_SPECIAL_LOAD_VERBOSE,
				CONSTANT_SPECIAL_LOAD_PRINT,
				&file));
	Return(eval_load_fasl_p_(file, &faslp));
	return eval_load_type_(ptr, ret, file, external, exist, faslp);
}

int eval_load_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_(ptr, ret, file, verbose, print, exist, external);
	return pop_control_(ptr, control);
}

static int eval_load_file_switch_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist,
		addr external, int faslp)
{
	Return(eval_load_check_(ptr, file, verbose, print, 1,
				CONSTANT_SPECIAL_LOAD_PATHNAME,
				CONSTANT_SPECIAL_LOAD_TRUENAME,
				CONSTANT_SPECIAL_LOAD_VERBOSE,
				CONSTANT_SPECIAL_LOAD_PRINT,
				&file));
	return eval_load_type_(ptr, ret, file, external, exist, faslp);
}

int eval_load_force_lisp_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 0);
	return pop_control_(ptr, control);
}

int eval_load_force_fasl_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external)
{
	addr control;

	push_control(ptr, &control);
	set_eval_compile_mode(ptr, Nil);
	(void)eval_load_file_switch_(ptr, ret, file, verbose, print, exist, external, 1);
	return pop_control_(ptr, control);
}


/*
 *  compile-file
 */
static int compile_load_lisp_call_(Execute ptr, addr file, int closep)
{
	int escape;

	gchold_push_local(ptr->local, file);
	escape = compile_load_stream_(ptr, file);
	if (! closep)
		return escape;

	return close_stream_unwind_protect_(ptr, file);
}

static int compile_load_lisp_(Execute ptr, int *ret, addr file, addr external, int exist)
{
	int openp, closep;
	addr control;

	Return(eval_load_open_(ptr, file, external, exist, 0, &openp, &closep, &file));
	if (! openp)
		return Result(ret, 0);

	/* open */
	push_control(ptr, &control);
	(void)compile_load_lisp_call_(ptr, file, closep);
	Return(pop_control_(ptr, control));
	return Result(ret, 1);
}

static int compile_load_file_(Execute ptr,
		addr file, addr verbose, addr print, addr external)
{
	int ignore;
	addr input;

	Return(eval_load_check_(ptr, file, verbose, print, 0,
				CONSTANT_SPECIAL_COMPILE_FILE_PATHNAME,
				CONSTANT_SPECIAL_COMPILE_FILE_TRUENAME,
				CONSTANT_SPECIAL_COMPILE_VERBOSE,
				CONSTANT_SPECIAL_COMPILE_PRINT,
				&input));
	Return(compile_file_begin_(ptr, file));
	Return(compile_load_lisp_(ptr, &ignore, input, external, 1));
	Return(compile_file_end_(ptr, file));

	return 0;
}

int compile_load_(Execute ptr, addr file, addr verbose, addr print, addr external)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_load_file_(ptr, file, verbose, print, external);
	return pop_control_(ptr, control);
}

