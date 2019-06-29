/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "clos.h"
#include "clos_combination.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
#include "function.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  make-method-lambda
 ***********************************************************************/
static void method_make_method_lambda_std(Execute ptr,
		addr method, addr next, addr gen, addr mclass, addr list, addr env)
{
	method_make_method_lambda(list, env, &list);
	setresult_control(ptr, list);
}

static void method_type_make_method_lambda_std(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var4(&args, args, args, values, type);
	typeargs_method(args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void method_argument_make_method_lambda_std(addr *ret)
{
	addr pos, list, type1, type2, type3;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, STANDARD_METHOD);
	ArgumentMethod_var(&type3, T);
	list_heap(&list, type1, type2, type3, type3, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_make_method_lambda_std(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var6(call, p_method_make_method_lambda_std);
	method_type_make_method_lambda_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_make_method_lambda_std(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_make_method_lambda(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_MAKE_METHOD_LAMBDA, &symbol);
	mop_argument_generic_var4(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	export_mop(symbol);
	/* no-method */
	defmethod_make_method_lambda_std(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
_g void init_mop_protocols(void)
{
	SetPointerType(var6, method_make_method_lambda_std);
}

_g void build_mop_protocols(Execute ptr)
{
	defgeneric_make_method_lambda(ptr);
}

