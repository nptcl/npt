/*
 *  ANSI COMMON LISP: 4. Types and Classes
 */
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "eval_declare.h"
#include "lambda.h"
#include "symbol.h"
#include "type_coerce.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_typep.h"
#include "type_value.h"

/* (defun coerce (object type) ...) -> result
 *   object  t
 *   type    type-spec
 *   result  t
 */
static void function_coerce(Execute ptr, addr pos, addr type)
{
	if (coerce_common(ptr, pos, type, &pos)) return;
	setresult_control(ptr, pos);
}

static void type_coerce(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, TypeSpec);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_coerce(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COERCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_coerce);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_coerce(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro deftype (name lambda &body body) ...) -> name
 *   name    symbol
 *   lambda  deftype-lambda-list
 *   body    body
 */
static void function_deftype(Execute ptr, addr form, addr env)
{
	addr right, eval, name, args, decl, doc;

	/* (deftype . form) */
	getcdr(form, &right);
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
	lambda_deftype(ptr->local, &args, args, Nil);
	if (declare_body_documentation(ptr, env, right, &doc, &decl, &right))
		return;

	/* (eval::deftype name args decl doc body) */
	GetConst(SYSTEM_DEFTYPE, &eval);
	list_heap(&form, eval, name, args, decl, doc, right, NULL);
	setresult_control(ptr, form);
}

static void defmacro_deftype(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFTYPE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftype);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun subtypep (type1 type2 &optional env) ...) -> value, valid
 *  type1  (or symbol list)  ;; type-specifier
 *  type2  (or symbol list)  ;; type-specifier
 *  env    (or environment null)
 *  value  boolean
 *  valid  boolean
 */
static void function_subtypep(Execute ptr, addr x, addr y, addr env)
{
	if (env == Unbound) env = Nil;
	if (subtypep_common(ptr, x, y, env, &x, &y)) return;
	setvalues_control(ptr, x, y, NULL);
}

static void type_subtypep(addr *ret)
{
	addr arg, values, type, env;

	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&arg, type, type, env);
	GetTypeTable(&values, Boolean);
	typevalues_values2(&values, values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_subtypep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBTYPEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_subtypep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subtypep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-of (object) ...) -> type-spec */
static void function_type_of(Execute ptr, addr pos)
{
	type_value(&pos, pos);
	type_object(&pos, pos);
	setresult_control(ptr, pos);
}

static void type_type_of(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(arg, values, ret);
}

static void defun_type_of(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_OF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_of);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_of(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun typep (object type &optional env) ...) -> boolean
 *   type  type-specifier
 */
static void function_typep(Execute ptr, addr x, addr y, addr env)
{
	int check;

	if (env == Unbound) env = Nil;
	if (parse_type(ptr, &y, y, env)) return;
	if (typep_clang(ptr, x, y, &check)) return;
	setbool_control(ptr, check);
}

static void type_typep(addr *ret)
{
	addr arg, values, type, env;

	GetTypeTable(&arg, T);
	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&arg, arg, type, env);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_typep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_typep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_typep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-datum (condition) -> object */
static void function_type_error_datum(Execute ptr, addr pos)
{
	type_error_datum(pos, &pos);
	setresult_control(ptr, pos);
}

static void type_type_error_datum(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, TypeError);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_type_error_datum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_DATUM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_error_datum);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_error_datum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-expected-type (condition) -> type-spec */
static void function_type_error_expected_type(Execute ptr, addr pos)
{
	type_error_expected(pos, &pos);
	setresult_control(ptr, pos);
}

static void type_type_error_expected_type(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, TypeError);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_type_error_expected_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_EXPECTED_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_error_expected_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_error_expected_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_types(void)
{
	SetPointerCall(defun,     var2,      coerce);
	SetPointerCall(defmacro,  macro,     deftype);
	SetPointerCall(defun,     var2opt1,  subtypep);
	SetPointerCall(defun,     var1,      type_of);
	SetPointerCall(defun,     var2opt1,  typep);
	SetPointerCall(defun,     var1,      type_error_datum);
	SetPointerCall(defun,     var1,      type_error_expected_type);
}

_g void build_common_types(void)
{
	defun_coerce();
	defmacro_deftype();
	defun_subtypep();
	defun_type_of();
	defun_typep();
	defun_type_error_datum();
	defun_type_error_expected_type();
}

