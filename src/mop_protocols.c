/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_defgeneric.h"
#include "clos_method.h"
#include "closget_method.h"
#include "condition.h"
#include "cons.h"
#include "control_operator.h"
#include "function.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "mop_common.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  make-method-lambda
 ***********************************************************************/
static int method_make_method_lambda_std(Execute ptr,
		addr method, addr next, addr gen, addr mclass, addr list, addr env)
{
	method_make_method_lambda(list, env, &list);
	setresult_control(ptr, list);
	return 0;
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

static int defmethod_make_method_lambda_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var6(call, p_method_make_method_lambda_std);
	method_type_make_method_lambda_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_make_method_lambda_std(&pos);
	Return(method_instance_lambda_(ptr, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_method_lambda_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_MAKE_METHOD_LAMBDA, &symbol);
	mop_argument_generic_var4(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(ptr, &gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* no-method */
	Return(defmethod_make_method_lambda_std_(ptr, name, gen));
	return common_method_finalize_(ptr, gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
void init_mop_protocols(void)
{
	SetPointerType(var6, method_make_method_lambda_std);
}

int build_mop_protocols_(Execute ptr)
{
	Return(defgeneric_make_method_lambda_(ptr));

	return 0;
}

