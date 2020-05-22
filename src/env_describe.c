#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_main.h"
#include "env_describe.h"
#include "format.h"
#include "function.h"
#include "mop.h"
#include "print.h"
#include "print_write.h"
#include "stream_prompt.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  t
 */
static int method_describe_object_t(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Lisp Object: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  class
 */
static int method_describe_object_class(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Class: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  standard-object
 */
static int method_describe_object_standard_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Instance: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  structure-object
 */
static int method_describe_object_structure_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Structure: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  defmethod
 */
static void method_type_describe_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Stream);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_describe_object(Execute ptr, addr name, addr gen,
		pointer p, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p);
	method_type_describe_object(&type);
	settype_function(call, type);
	/* method */
	GetConstant(index, &pos);
	mop_argument_method_print_object(&pos, pos); /* print-object */
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  describe
 */
_g int describe_common(Execute ptr, addr object, addr stream)
{
	addr call;

	/* stream */
	output_stream_designer(ptr, stream, &stream);
	fresh_line_stream(stream);
	/* call */
	GetConst(COMMON_DESCRIBE_OBJECT, &call);
	getfunction_global(call, &call);
	return funcall_control(ptr, call, object, stream, NULL);
}


/*
 *  inspect
 */
static int exit_inspect_p(addr pos)
{
	if (! symbolp(pos))
		return 0;
	GetNameSymbol(pos, &pos);
	return string_equalp_char(pos, "Q")
		|| string_equalp_char(pos, "QUIT")
		|| string_equalp_char(pos, "E")
		|| string_equalp_char(pos, "EXIT");
}

static int help_inspect_p(addr pos)
{
	if (! symbolp(pos))
		return 0;
	GetNameSymbol(pos, &pos);
	return string_equalp_char(pos, "?")
		|| string_equalp_char(pos, "H")
		|| string_equalp_char(pos, "HELP");
}

static void help_inspect(Execute ptr, addr io)
{
	static const char *const message[] = {
		"Inspect help.",
		"---",
		"quit  Quit inspect.",
		"help  Output this message.",
		"---",
		NULL
	};
	int i;
	const char *str;

	for (i = 0; ; i++) {
		str = message[i];
		if (str == NULL)
			break;
		print_ascii_stream(io, str);
		terpri_stream(io);
		force_output_stream(io);
	}
}

static int eval_loop_inspect(Execute ptr, addr io, addr pos, int *exit, int *exec)
{
	if (exit_inspect_p(pos)) {
		*exit = 1;
		*exec = 0;
		return 0;
	}
	if (help_inspect_p(pos)) {
		*exit = 0;
		*exec = 0;
		help_inspect(ptr, io);
		return 0;
	}
	*exit = 0;
	*exec = 1;
	return 0;
}

_g int inspect_common(Execute ptr, addr object)
{
	addr io, symbol;

	terminal_io_stream(ptr, &io);
	Return(describe_common(ptr, object, io));
	/* *inspected */
	GetConst(SYSTEM_INSPECTED, &symbol);
	pushspecial_control(ptr, symbol, object);
	/* prompt */
	mode_prompt_stream(ptr, PromptStreamMode_Inspect);
	Return(eval_custom_loop(ptr, eval_loop_inspect));

	return 0;
}


/*
 *  defgeneric
 */
_g void init_environment_describe(void)
{
	SetPointerType(var4, method_describe_object_t);
	SetPointerType(var4, method_describe_object_class);
	SetPointerType(var4, method_describe_object_standard_object);
	SetPointerType(var4, method_describe_object_structure_object);
}

#define DefMethod_DescribeObject(ptr, name, gen, p, c) { \
	defmethod_describe_object((ptr), (name), (gen), \
			p_method_describe_object_##p, CONSTANT_CLOS_##c); \
}
static void build_describe_object_method(Execute ptr, addr name, addr gen)
{
	DefMethod_DescribeObject(ptr, name, gen, t, T);
	DefMethod_DescribeObject(ptr, name, gen, class, CLASS);
	DefMethod_DescribeObject(ptr, name, gen, standard_object, STANDARD_OBJECT);
	DefMethod_DescribeObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
}

_g void build_environment_describe(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_DESCRIBE_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	build_describe_object_method(ptr, name, gen);
	common_method_finalize(gen);
}

