#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "env_describe.h"
#include "format.h"
#include "function.h"
#include "mop.h"
#include "print.h"
#include "print_write.h"
#include "stream.h"
#include "symbol.h"
#include "type_table.h"


/*
 *  t
 */
static void method_describe_object_t(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Lisp Object: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
}


/*
 *  class
 */
static void method_describe_object_class(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Class: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
}


/*
 *  standard-object
 */
static void method_describe_object_standard_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Instance: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
}


/*
 *  structure-object
 */
static void method_describe_object_structure_object(Execute ptr,
		addr method, addr next, addr pos, addr stream)
{
	format_stream(ptr, stream, "Structure: ~S~%", pos, NULL);
	setresult_control(ptr, Nil);
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
	getfunctioncheck_local(ptr, call, &call);
	return funcall_control(ptr, call, object, stream, NULL);
}


/*
 *  inspect
 */
_g int inspect_common(Execute ptr, addr object)
{
	addr stream;

	terminal_io_stream(ptr, &stream);
	Return1(describe_common(ptr, object, stream));
	/* prompt */
	fmte("TODO", NULL);

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

