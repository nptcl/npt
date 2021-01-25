#include "clos_class.h"
#include "compile.h"
#include "compile_file.h"
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
#include "hashtable.h"
#include "hold.h"
#include "make_load_form.h"
#include "pathname.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_init.h"
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

int eval_compile_file(Execute ptr, addr pos)
{
	addr stream;

	Check(GetType(pos) != LISPTYPE_CODE, "type error");
	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	Return(getspecialcheck_local_(ptr, stream, &stream));

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
	Return(faslwrite_header_(output));
	Return(compile_load(ptr, input, verbose, print, external));
	Return(faslwrite_footer_(output));
	Return(finish_output_stream_(output));

	return 0;
}

static int compile_file_execute_(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr symbol, pos;

	Check(! streamp(input), "input error");
	Check(! streamp(output), "output error");

	/* variable */
	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	pushspecial_control(ptr, symbol, output);
	set_eval_compile_mode(ptr, T);

	/* gensym table */
	GetConst(SYSTEM_COMPILE_GENSYM, &symbol);
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	pushspecial_control(ptr, symbol, pos);

	/* gensym index */
	GetConst(SYSTEM_COMPILE_GENSYM_INDEX, &symbol);
	fixnum_heap(&pos, 0);
	pushspecial_control(ptr, symbol, pos);

	/* compile */
	Return(compile_file_output(ptr, input, output, rest));
	GetPathnameStream(output, &pos);
	if (memory_stream_p(pos))
		return Result(ret, Nil);
	Return(truename_files_(ptr, output, ret, 0));

	return 0;
}

static int compile_file_output_stream_call_(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr stream;

	Return(open_output_binary_stream_(ptr, &stream, output, FileOutput_supersede));
	if (stream == NULL)
		return fmte_("Cannot open the output file ~S.", output, NULL);
	push_close_stream(ptr, stream);
	return compile_file_execute_(ptr, input, stream, rest, ret);
}

static int compile_file_output_stream_(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr control;

	if (streamp(output) && (! memory_stream_p(output)))
		return compile_file_execute_(ptr, input, output, rest, ret);

	/* open input */
	push_control(ptr, &control);
	(void)compile_file_output_stream_call_(ptr, input, output, rest, ret);
	return pop_control_(ptr, control);
}

static int compile_file_input_stream_call_(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr stream;

	Return(open_input_stream_(ptr, &stream, input));
	if (stream == NULL)
		return fmte_("Cannot open the input file ~S.", input, NULL);
	push_close_stream(ptr, stream);
	return compile_file_output_stream_(ptr, stream, output, rest, ret);
}

static int compile_file_input_stream_(Execute ptr,
		addr input, addr output, addr rest, addr *ret)
{
	addr control;

	if (streamp(input) && (! memory_stream_p(input)))
		return compile_file_output_stream_(ptr, input, output, rest, ret);

	/* open input */
	push_control(ptr, &control);
	(void)compile_file_input_stream_call_(ptr, input, output, rest, ret);
	return pop_control_(ptr, control);
}

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
	return pushhandler_common_(ptr, pos, call, 0);
}

static int compile_file_common_call_(Execute ptr,
		LocalHold hold, addr input, addr output, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr pos;

	Return(handler_compile_(ptr));
	init_write_make_load_form(ptr);
	Return(compile_file_input_stream_(ptr, input, output, rest, ret1));
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, ret2));
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, ret3));

	return 0;
}

int compile_file_common(Execute ptr, addr input, addr rest,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr control, output, check;
	LocalHold hold;

	/* pathname-designer */
	compile_file_pathname_common(ptr, input, rest, &output);

	/* push control */
	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)compile_file_common_call_(ptr, hold, input, output, rest, ret1, ret2, &check);
	*ret3 = ((*ret2 != Nil) && (check == Nil))? T: Nil;
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  initialize
 */
void init_compile_file(void)
{
	SetPointerCall(defun, var1, handler_compile);
}

