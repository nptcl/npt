/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "clos_combination.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
#include "function.h"
#include "lambda.h"
#include "mop.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  no-applicable-method
 ***********************************************************************/
static void defgeneric_no_applicable_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_APPLICABLE_METHOD, &symbol);
	mop_argument_generic_var1rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  no-next-method
 ***********************************************************************/
static void defgeneric_no_next_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_NEXT_METHOD, &symbol);
	mop_argument_generic_var2rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  ensure-generic-function-using-class
 ***********************************************************************/
static void method_ensure_generic_function_struct(struct generic_argument *str,
		Execute ptr, addr clos, addr name, addr rest)
{
	addr order, decl, doc, env, gen, lambda, method, comb;

	/* arguments */
	if (getkeyargs(rest, KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &order))
		order = Nil;
	if (getkeyargs(rest, KEYWORD_DECLARE, &decl))
		decl = Nil;
	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	if (getkeyargs(rest, KEYWORD_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (getkeyargs(rest, KEYWORD_METHOD_COMBINATION, &comb))
		comb = Nil;
	if (getkeyargs(rest, KEYWORD_GENERIC_FUNCTION_CLASS, &gen))
		GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gen);
	if (getkeyargs(rest, KEYWORD_METHOD_CLASS, &method))
		GetConst(CLOS_STANDARD_METHOD, &method);

	/* parse */
	if (! callnamep(name))
		parse_callname_error(&name, name);
	if (! argumentp(lambda))
		argument_generic_heap(ptr->local, &lambda, lambda);

	/* generic-addr */
	str->ptr = ptr;
	str->env = env;
	str->name = name;
	str->lambda = lambda;
	str->generic = gen;
	str->method = method;
	str->combination = comb;
	str->order = order;
	str->declare = decl;
	str->doc = doc;
}

static void method_ensure_generic_function_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	struct generic_argument str;

	method_ensure_generic_function_struct(&str, ptr, clos, name, rest);
	if (generic_change(&str, &name)) return;
	setresult_control(ptr, name);
}

static void method_type_ensure_generic_function_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_ensure_generic_function_class(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4dynamic(call, method_ensure_generic_function_class);
	method_type_ensure_generic_function_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_class(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void method_ensure_generic_function_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	struct generic_argument str;

	method_ensure_generic_function_struct(&str, ptr, clos, name, rest);
	if (generic_add(&str, &name)) return;
	setresult_control(ptr, name);
}

static void method_type_ensure_generic_function_null(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Null);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_null(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, NULL);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_ensure_generic_function_null(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4dynamic(call, method_ensure_generic_function_null);
	method_type_ensure_generic_function_null(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_null(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_ensure_generic_function_using_class(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &symbol);
	mop_argument_generic_var2rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_ensure_generic_function_class(ptr, name, gen);
	defmethod_ensure_generic_function_null(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  ensure-method
 ***********************************************************************/
/* `(defun ensure-method (name
 *      &key lambda-list qualifiers specializers function) ...)
 *      -> method
 *    name           function-name
 *    lambda-list    list
 *    qualifiers     list
 *    specializers   list
 *    function       function
 */
static void function_ensure_method(Execute ptr, addr name, addr rest)
{
	addr lambda, qua, spec, call;

	/* arguments */
	if (getkeyargs(rest, CLOSKEY_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (getkeyargs(rest, CLOSKEY_QUALIFIERS, &qua))
		qua = Nil;
	if (getkeyargs(rest, CLOSKEY_SPECIALIZERS, &spec))
		spec = Nil;
	if (getkeyargs(rest, CLOSKEY_FUNCTION, &call))
		fmte("Invalid ensure-method argument :function ~S.", call, NULL);

	/* add method */
	ensure_method_common(ptr, &name, name, lambda, qua, spec, call);
	setresult_control(ptr, name);
}

static void type_ensure_method(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	keytypetable(CONSTANT_CLOSKEY_LAMBDA_LIST, TypeTable_T, &key1);
	keytypetable(CONSTANT_CLOSKEY_QUALIFIERS, TypeTable_List, &key2);
	keytypetable(CONSTANT_CLOSKEY_SPECIALIZERS, TypeTable_List, &key3);
	keytypetable(CONSTANT_CLOSKEY_FUNCTION, TypeTable_Function, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, FunctionName);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_ensure_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  flet-method-p
 ***********************************************************************/
/* (defun clos::flet-method-p (var) ...) -> boolean */
static void function_flet_method_p(Execute ptr, addr var)
{
	setbool_control(ptr, var != Nil);
}

static void defun_flet_method_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_METHOD_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_flet_method_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  flet-next-method
 ***********************************************************************/
/* (defun clos::flet-next-method (method next args rest) ...) -> t */
static void function_flet_next_method(Execute ptr,
		addr method, addr next, addr args, addr rest)
{
	addr call;
	LocalRoot local;
	LocalStack stack;

	getcons(next, &method, &next);
	stdget_method_function(method, &call);
	if (rest == Nil)
		rest = args;
	/* call method */
	local = ptr->local;
	push_local(local, &stack);
	lista_local(local, &rest, method, next, rest, NULL);
	if (apply_control(ptr, call, rest)) return;
	rollback_local(local, stack);
}

static void type_flet_next_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_flet_next_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var4(pos, function_flet_next_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_flet_next_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-short
 ***********************************************************************/
static void function_ensure_method_combination_short(Execute ptr, addr var, addr rest)
{
	addr doc, ident, oper;

	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &ident))
		ident = Nil;
	if (getkeyargs(rest, KEYWORD_OPERATION, &oper))
		oper = var;
	ensure_define_combination_short_common(var, doc, ident, oper);
	setresult_control(ptr, var);
}

static void type_ensure_method_combination_short(addr *ret)
{
	addr args, values, key, key1, key2, key3;

	/* key */
	KeyTypeTable(&key1, DOCUMENTATION, String);
	KeyTypeTable(&key2, IDENTITY_WITH_ONE_ARGUMENT, T);
	KeyTypeTable(&key3, OPERATOR, Symbol);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_short(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_ensure_method_combination_short);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_short(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-long
 ***********************************************************************/
static void function_ensure_method_combination_long(Execute ptr,
		addr name, addr lambda, addr spec, addr rest)
{
	addr args, gen, doc, form;

	if (getkeyargs(rest, KEYWORD_ARGUMENTS, &args))
		args = Nil;
	if (getkeyargs(rest, KEYWORD_GENERIC_FUNCTION, &gen))
		GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gen);
	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, CLOSKEY_FORM, &form))
		form = Nil;
	ensure_define_combination_long_common(name, lambda, spec, args, gen, doc, form);
	setresult_control(ptr, name);
}

static void type_ensure_method_combination_long(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	KeyTypeTable(&key1, ARGUMENTS, T);
	KeyTypeTable(&key2, GENERIC_FUNCTION, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	keytypetable(CONSTANT_CLOSKEY_FORM, TypeTable_T, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var3key(&args, args, values, values, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_long(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_LONG, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_ensure_method_combination_long);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_long(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
void intern_mop_generic(Execute ptr)
{
	/* defclass */
	defgeneric_no_applicable_method_mop(ptr);
	defgeneric_no_next_method_mop(ptr);
	/* defgeneric */
	defgeneric_ensure_generic_function_using_class(ptr);
	defun_ensure_method();
	defun_flet_method_p();
	defun_flet_next_method();
	/* define-method-combination */
	defun_ensure_define_combination_short();
	defun_ensure_define_combination_long();
}

