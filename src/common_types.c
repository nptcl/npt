/*
 *  ANSI COMMON LISP: 4. Types and Classes
 */
#include "cons.h"
#include "common_header.h"
#include "eval_declare.h"
#include "lambda.h"
#include "type_subtypep.h"
#include "type_typep.h"
#include "type_parse.h"

/* (defmacro deftype (name lambda &body body) ...) -> name
 *   name    symbol
 *   lambda  deftype-lambda-list
 *   body    body
 */
static void function_deftype(Execute ptr, addr right, addr env)
{
	addr eval, name, args, decl, doc;

	/* (deftype . right) */
	getcdr(right, &right);
	if (right == Nil)
		fmte("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		fmte("Invalid deftype form.", NULL);

	/* name */
	getcons(right, &name, &right);
	if (! symbolp(name))
		fmte("deftype name ~S must be a symbol.", name, NULL);
	if (right == Nil)
		fmte("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		fmte("Invalid deftype form.", NULL);

	/* args */
	getcons(right, &args, &right);
	if (! IsList(right))
		fmte("Invalid deftype form.", NULL);

	/* parse */
	lambda_macro(ptr->local, &args, args, Nil);
	declare_body_documentation(right, &doc, &decl, &right);

	/* (eval::deftype name args decl doc body) */
	GetConst(SYSTEM_DEFTYPE, &eval);
	list_heap(&eval, eval, name, args, decl, doc, right, NULL);
	setresult_control(ptr, eval);
}

static void defmacro_deftype(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFTYPE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_deftype);
	SetMacroCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_MacroFunction);
	settype_function(pos, type);
}


/* (defun subtypep (type1 type2 &optional env) ...) -> value, valid
 *  type1  (or symbol list)  ;; type-specifier
 *  type2  (or symbol list)  ;; type-specifier
 *  env    (or environment null)
 *  value  boolean
 *  valid  boolean
 */
static void function_subtypep(Execute ptr, addr type1, addr type2, addr env)
{
	int result, invalid;

	if (env == Unbound) env = Nil;
	/* TODO: environment parameter */
	result = subtypep_clang(type1, type2, &invalid);
	setvalues_va_control(ptr, (result? T: Nil), (invalid? T: Nil), NULL);
}

static void type_subtypep(addr *ret)
{
	addr arg, values, type, env;

	GetCallType(&type, TypeSpec);
	GetCallType(&env, EnvironmentNull);
	var2opt1_argtype(&arg, type, type, env);
	GetCallType(&values, Boolean);
	values2_valuestype(&values, values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_subtypep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBTYPEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, function_subtypep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subtypep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun typep (object type &optional env) ...) -> boolean
 *   type  type-specifier
 */
static void function_typep(Execute ptr, addr var, addr type, addr env)
{
	if (env == Unbound) env = Nil;
	/* TODO: environment parameter */
	setbool_control(ptr, typep_throw(ptr->local, var, type));
}

static void type_typep(addr *ret)
{
	addr arg, values, type, env;

	GetCallType(&arg, T);
	GetCallType(&type, TypeSpec);
	GetCallType(&env, EnvironmentNull);
	var2opt1_argtype(&arg, arg, type, env);
	GetCallType(&values, Values_Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_typep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, function_typep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_typep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
void intern_common_types(void)
{
	/* defun_coerce(); */
	defmacro_deftype();
	defun_subtypep();
	/* defun_type_of(); */
	defun_typep();
	/* defun_type_error_datum(); */
	/* defun_type_error_expected_type(); */
}

