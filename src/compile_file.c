#include "clos_class.h"
#include "compile.h"
#include "compile_file.h"
#include "compile_write.h"
#include "condition.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_load.h"
#include "eval_value.h"
#include "file_open.h"
#include "files.h"
#include "function.h"
#include "hashtable.h"
#include "hold.h"
#include "load_instance.h"
#include "pathname.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_init.h"
#include "stream_object.h"
#include "symbol.h"

void set_eval_compile_mode(Execute ptr, addr value)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	pushspecial_control(ptr, pos, value);
}

int eval_compile_p(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	getspecial_local(ptr, pos, &pos);
	return (pos != Nil) && (pos != Unbound);
}

int eval_compile_file_(Execute ptr, addr pos)
{
	addr stream;

	Check(GetType(pos) != LISPTYPE_CODE, "type error");
	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	Return(getspecialcheck_local_(ptr, stream, &stream));

	return faslwrite_value_(ptr, stream, pos);
}


/*
 *  handler-compile
 */
static int function_handler_compile(Execute ptr, addr condition)
{
	int check;
	addr value;

	/* warning */
	GetConst(CONDITION_WARNING, &value);
	Return(clos_subtype_p_(condition, value, &check));
	if (check) {
		GetConst(SYSTEM_COMPILE_WARNING, &value);
		setspecial_local(ptr, value, T);
	}

	/* style-warning */
	GetConst(CONDITION_STYLE_WARNING, &value);
	Return(clos_subtype_p_(condition, value, &check));
	if (check) {
		GetConst(SYSTEM_COMPILE_STYLE_WARNING, &value);
		setspecial_local(ptr, value, T);
	}

	return 0;
}

int handler_compile_(Execute ptr)
{
	addr pos, call;

	/* enable-compiler-macro */
	push_enable_compiler_macro(ptr, T);

	/* compile-warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	pushspecial_control(ptr, pos, Nil);

	/* compile-style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	pushspecial_control(ptr, pos, Nil);

	/* handler-bind */
	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_compile);
	return pushhandler_common_(ptr, pos, call, 0);
}


/*
 *  compile-file
 */
struct compile_file_struct {
	unsigned warnings_p : 1;
	unsigned failure_p : 1;
	Execute ptr;
	LocalHold hold;
	addr input_file, output_file;
	addr input, output, verbose, print, format;
	addr result;
};

static int compile_file_write_(Execute ptr, struct compile_file_struct *str)
{
	addr input, output, verbose, print, format;

	Check(! streamp(str->input), "input error");
	Check(! streamp(str->output), "output error");
	input = str->input;
	output = str->output;
	verbose = str->verbose;
	print = str->print;
	format = str->format;

	/* fasl */
	Return(faslwrite_header_(output));
	Return(compile_load_(ptr, input, verbose, print, format));
	Return(faslwrite_footer_(output));
	return finish_output_stream_(output);
}

static int compile_file_execute_(Execute ptr, struct compile_file_struct *str)
{
	addr output, symbol, pos;

	Check(! streamp(str->input), "input error");
	Check(! streamp(str->output), "output error");
	output = str->output;

	/* variable */
	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	pushspecial_control(ptr, symbol, output);
	set_eval_compile_mode(ptr, T);

	/* compile */
	Return(compile_file_write_(ptr, str));
	GetPathnameStream(output, &pos);
	if (memory_stream_p(pos)) {
		pos = Nil;
	}
	else {
		Return(truename_files_(ptr, output, &pos, 0));
	}
	str->result = pos;
	localhold_set(str->hold, 0, pos);

	return 0;
}

static int compile_file_stream_p(addr stream)
{
	return streamp(stream) && (! memory_stream_p(stream));
}

static int compile_file_output_call_(Execute ptr, struct compile_file_struct *str)
{
	addr stream, output;

	output = str->output_file;
	Return(open_output_binary_stream_(ptr, &stream, output, FileOutput_supersede));
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, output,
				"Cannot open the output file, ~S.", output, NULL);
	}
	str->output = stream;
	(void)compile_file_execute_(ptr, str);
	return close_stream_unwind_protect_(ptr, stream);
}

static int compile_file_output_(Execute ptr, struct compile_file_struct *str)
{
	addr output, control;

	output = str->output_file;
	if (compile_file_stream_p(output)) {
		str->output = output;
		return compile_file_execute_(ptr, str);
	}

	/* open output */
	push_control(ptr, &control);
	(void)compile_file_output_call_(ptr, str);
	return pop_control_(ptr, control);
}

static int compile_file_input_call_(Execute ptr, struct compile_file_struct *str)
{
	addr stream, input, format;

	input = str->input_file;
	format = str->format;
	Return(open_input_stream_(ptr, &stream, input, format));
	if (stream == NULL) {
		return call_simple_file_error_va_(ptr, input,
				"Cannot open the input file, ~S.", input, NULL);
	}
	str->input = stream;
	(void)compile_file_output_(ptr, str);
	return close_stream_unwind_protect_(ptr, stream);
}

static int compile_file_input_(Execute ptr, struct compile_file_struct *str)
{
	addr input, control;

	input = str->input_file;
	if (compile_file_stream_p(input)) {
		str->input = input;
		return compile_file_output_(ptr, str);
	}

	/* open input */
	push_control(ptr, &control);
	(void)compile_file_input_call_(ptr, str);
	return pop_control_(ptr, control);
}

static int compile_file_handler_(Execute ptr, struct compile_file_struct *str)
{
	int check1, check2;
	addr pos;

	Return(handler_compile_(ptr));
	Return(compile_file_input_(ptr, str));

	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	check1 = (pos != Nil);

	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	check2 = (pos != Nil);

	/* result */
	str->warnings_p = check1;
	str->failure_p = (check1 != 0) && (check2 == 0);

	return 0;
}

static int compile_file_call_(Execute ptr, struct compile_file_struct *str)
{
	addr control;
	LocalHold hold;

	hold = str->hold;
	localhold_set(hold, 1, str->input_file);
	localhold_set(hold, 2, str->output_file);
	localhold_set(hold, 3, str->input);
	localhold_set(hold, 4, str->output);
	push_control(ptr, &control);
	(void)compile_file_handler_(ptr, str);
	return pop_control_(ptr, control);
}

int compile_file_common_(Execute ptr, addr input, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr pos;
	LocalHold hold;
	struct compile_file_struct str;

	str.ptr = ptr;
	str.warnings_p = 0;
	str.failure_p = 0;
	str.input_file = input;
	str.input = Nil;
	str.output = Nil;
	str.result = Unbound;

	/* verbose */
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &pos))
		pos = Unbound;
	str.verbose = pos;

	/* print */
	if (GetKeyArgs(rest, KEYWORD_PRINT, &pos))
		pos = Unbound;
	str.print = pos;

	/* external-format */
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &pos))
		pos = Unbound;
	str.format = pos;

	/* output */
	Return(compile_file_pathname_common_(ptr, input, rest, &pos));
	str.output_file = pos;

	/* call */
	hold = LocalHold_array(ptr, 5);
	str.hold = hold;
	Return(compile_file_call_(ptr, &str));
	localhold_end(hold);

	/* result */
	Check(str.result == Unbound, "result error");
	*ret1 = str.result;
	*ret2 = str.warnings_p? T: Nil;
	*ret3 = str.failure_p? T: Nil;

	return 0;
}


/*
 *  initialize
 */
void init_compile_file(void)
{
	SetPointerCall(defun, var1, handler_compile);
}

