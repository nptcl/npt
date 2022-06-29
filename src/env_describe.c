#include "callname.h"
#include "clos.h"
#include "clos_defgeneric.h"
#include "clos_method.h"
#include "clos_type.h"
#include "closget_class.h"
#include "closget_method.h"
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
#include "prompt.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  t
 */
static int method_describe_object_t_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Lisp Object: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  class
 */
static int method_describe_object_class_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Class: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  standard-object
 */
static int method_describe_object_standard_object_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Instance: ~S~%", pos, NULL));
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  structure-object
 */
static int method_describe_object_structure_object_(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	Return(format_stream_(ptr, stream, "Structure: ~S~%", pos, NULL));
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

static int defmethod_describe_object_(Execute ptr, addr name, addr gen,
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
	Return(method_instance_lambda_(ptr, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  describe
 */
int describe_common_(Execute ptr, addr object, addr stream)
{
	addr call;

	/* stream */
	Return(output_stream_designator_(ptr, stream, &stream));
	Return(fresh_line_stream_(stream, NULL));
	/* call */
	GetConst(COMMON_DESCRIBE_OBJECT, &call);
	Return(getfunction_global_(call, &call));
	return funcall_control_(ptr, call, object, stream, NULL);
}


/*
 *  inspect
 */
static int exit_inspect_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "Q", "QUIT", "E" "EXIT", NULL);
}

static int help_inspect_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "?", "H", "HELP", NULL);
}

static int help_inspect_(Execute ptr, addr io)
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
		Return(print_ascii_stream_(io, str));
		Return(terpri_stream_(io));
		Return(force_output_stream_(io));
	}

	return 0;
}

static int eval_loop_inspect_(Execute ptr, addr io, addr pos, int *exit, int *exec)
{
	int check;

	Return(exit_inspect_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		return 0;
	}
	Return(help_inspect_p_(pos, &check));
	if (check) {
		*exit = 0;
		*exec = 0;
		return help_inspect_(ptr, io);
	}
	*exit = 0;
	*exec = 1;
	return 0;
}

static int inspect_common_call_(Execute ptr, addr object)
{
	addr io, symbol;

	Return(terminal_io_stream_(ptr, &io));
	Return(describe_common_(ptr, object, io));
	/* *inspected* */
	GetConst(SYSTEM_INSPECTED, &symbol);
	pushspecial_control(ptr, symbol, object);
	/* prompt */
	Return(eval_custom_loop_(ptr, io, eval_loop_inspect_, NULL));

	return 0;
}

int inspect_common_(Execute ptr, addr object)
{
	addr control, prompt;

	push_control(ptr, &control);
	strvect_char_heap(&prompt, "Inspect> ");
	push_prompt(ptr, prompt, prompt_inspect);
	(void)inspect_common_call_(ptr, object);
	return pop_control_(ptr, control);
}


/*
 *  defgeneric
 */
void init_environment_describe(void)
{
	SetPointerType_(var4, method_describe_object_t);
	SetPointerType_(var4, method_describe_object_class);
	SetPointerType_(var4, method_describe_object_standard_object);
	SetPointerType_(var4, method_describe_object_structure_object);
}

#define DefMethod_DescribeObject(ptr, name, gen, p, c) { \
	Return(defmethod_describe_object_((ptr), (name), (gen), \
				p_method_describe_object_##p, CONSTANT_CLOS_##c)); \
}
static int build_describe_object_method_(Execute ptr, addr name, addr gen)
{
	DefMethod_DescribeObject(ptr, name, gen, t, T);
	DefMethod_DescribeObject(ptr, name, gen, class, CLASS);
	DefMethod_DescribeObject(ptr, name, gen, standard_object, STANDARD_OBJECT);
	DefMethod_DescribeObject(ptr, name, gen, structure_object, STRUCTURE_OBJECT);
	return 0;
}

static int build_environment_describe_call_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_DESCRIBE_OBJECT, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(ptr, &gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(build_describe_object_method_(ptr, name, gen));
	return common_method_finalize_(ptr, gen);
}

void build_environment_describe(Execute ptr)
{
	Error(build_environment_describe_call_(ptr));
}

