#include "clos_class.h"
#include "compile.h"
#include "compile_write.h"
#include "condition.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "file_open.h"
#include "files.h"
#include "function.h"
#include "hold.h"
#include "make_load_form.h"
#include "pathname.h"
#include "stream.h"
#include "stream_init.h"
#include "symbol.h"

_g void set_eval_compile_mode(Execute ptr, addr value)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	pushspecial_control(ptr, pos, value);
}

_g int eval_compile_p(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_CODE, &pos);
	getspecial_local(ptr, pos, &pos);
	return (pos != Nil) && (pos != Unbound);
}

_g int eval_compile_file(Execute ptr, addr pos)
{
	addr stream;

	Check(GetType(pos) != LISPTYPE_CODE, "type error");
	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	getspecialcheck_local(ptr, stream, &stream);

	return faslwrite_value(ptr, stream, pos);
}


/*
 *  compile-file
 */
static int compile_file_output(Execute ptr, addr input, addr output, addr rest)
{
	addr verbose, print, external;

	Check(! streamp(input), "type error");
	Check(! streamp(output), "type error");

	/* argument */
	if (GetKeyArgs(rest, KEYWORD_VERBOSE, &verbose))
		verbose = Unbound;
	if (GetKeyArgs(rest, KEYWORD_PRINT, &print))
		print = Unbound;
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &external))
		external = Unbound;

	/* write */
	faslwrite_header(output);
	Return(compile_load(ptr, input, verbose, print, external));
	faslwrite_footer(output);
	finish_output_stream(output);

	return 0;
}

static int compile_file_execute(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr symbol;

	Check(! streamp(input), "input error");
	Check(! streamp(output), "output error");

	/* variable */
	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	pushspecial_control(ptr, symbol, output);
	set_eval_compile_mode(ptr, T);

	/* compile */
	Return(compile_file_output(ptr, input, output, rest));
	Return(truename_files_(ptr, output, ret, 0));

	return 0;
}

static int compile_file_output_stream(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr control, stream;

	if (streamp(output))
		return compile_file_output_stream(ptr, input, output, rest, ret);

	/* open input */
	push_new_control(ptr, &control);
	Return(open_output_binary_stream_(ptr, &stream, output, FileOutput_supersede));
	if (stream == NULL)
		return fmte_("Cannot open the output file ~S.", output, NULL);
	push_close_stream(ptr, stream);
	Return(compile_file_execute(ptr, input, stream, rest, ret));
	return free_control_(ptr, control);
}

static int compile_file_input_stream(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr control, stream;

	if (streamp(input))
		return compile_file_output_stream(ptr, input, output, rest, ret);

	/* open input */
	push_new_control(ptr, &control);
	Return(open_input_stream_(ptr, &stream, input));
	if (stream == NULL)
		return fmte_("Cannot open the input file ~S.", input, NULL);
	push_close_stream(ptr, stream);
	Return(compile_file_output_stream(ptr, stream, output, rest, ret));
	return free_control_(ptr, control);
}

static int function_handler_compile(Execute ptr, addr condition)
{
	addr check;

	/* warning */
	GetConst(CONDITION_WARNING, &check);
	if (clos_subtype_p(condition, check)) {
		GetConst(SYSTEM_COMPILE_WARNING, &check);
		setspecial_local(ptr, check, T);
	}

	/* style-warning */
	GetConst(CONDITION_STYLE_WARNING, &check);
	if (clos_subtype_p(condition, check)) {
		GetConst(SYSTEM_COMPILE_STYLE_WARNING, &check);
		setspecial_local(ptr, check, T);
	}

	return 0;
}

_g void handler_compile(Execute ptr)
{
	addr pos, call;

	/* *compiler-macro* */
	GetConst(SYSTEM_COMPILER_MACRO, &pos);
	pushspecial_control(ptr, pos, T);

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
	pushhandler_common(ptr, pos, call, 0);
}

_g int compile_file_common(Execute ptr, addr input, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr output, control, pos;
	LocalHold hold;

	/* pathname-designer */
	compile_file_pathname_common(ptr, input, rest, &output);
	/* push control */
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	handler_compile(ptr);
	init_write_make_load_form(ptr);
	Return(compile_file_input_stream(ptr, input, output, rest, ret1));
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	getspecialcheck_local(ptr, pos, ret2);
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	getspecialcheck_local(ptr, pos, ret3);
	/* free */
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  initialize
 */
_g void init_compile_file(void)
{
	SetPointerCall(defun, var1, handler_compile);
}

