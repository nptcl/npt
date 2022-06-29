/*
 *  ANSI COMMON LISP: 4. Types and Classes
 */
#include "call_types.h"
#include "common_header.h"
#include "type_coerce.h"
#include "type_deftype.h"

/* (defun coerce (object type) ...) -> result
 *   object  t
 *   type    type-spec
 *   result  t
 */
static int function_coerce(Execute ptr, addr pos, addr type)
{
	Return(coerce_common_(ptr, pos, type, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_coerce(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, TypeSpec);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_coerce(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COERCE, &symbol);
	compiled_system(&pos, symbol);
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
static int function_deftype(Execute ptr, addr form, addr env)
{
	Return(deftype_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_deftype(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFTYPE, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_subtypep(Execute ptr, addr x, addr y, addr env)
{
	Return(subtypep_common_(ptr, x, y, env, &x, &y));
	setvalues_control(ptr, x, y, NULL);
	return 0;
}

static void type_subtypep(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&args, type, type, env);
	GetTypeTable(&values, Boolean);
	typevalues_values2(&values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBTYPEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_subtypep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subtypep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-of (object) ...) -> type-spec */
static int function_type_of(Execute ptr, addr pos)
{
	Return(type_of_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_of(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(args, values, ret);
}

static void defun_type_of(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_OF, &symbol);
	compiled_system(&pos, symbol);
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
static int function_typep(Execute ptr, addr x, addr y, addr env)
{
	Return(typep_common_(ptr, x, y, env, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_typep(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&args, T);
	GetTypeTable(&type, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	typeargs_var2opt1(&args, args, type, env);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_typep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_typep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_typep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-datum (condition) -> object */
static int function_type_error_datum(Execute ptr, addr pos)
{
	Return(type_error_datum_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_error_datum(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_type_error_datum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_DATUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_type_error_datum);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_type_error_datum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun type-error-expected-type (condition) -> type-spec */
static int function_type_error_expected_type(Execute ptr, addr pos)
{
	Return(type_error_expected_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_type_error_expected_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeError);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_type_error_expected_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TYPE_ERROR_EXPECTED_TYPE, &symbol);
	compiled_system(&pos, symbol);
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
void init_common_types(void)
{
	SetPointerCall(defun,     var2,      coerce);
	SetPointerCall(defmacro,  macro,     deftype);
	SetPointerCall(defun,     var2opt1,  subtypep);
	SetPointerCall(defun,     var1,      type_of);
	SetPointerCall(defun,     var2opt1,  typep);
	SetPointerCall(defun,     var1,      type_error_datum);
	SetPointerCall(defun,     var1,      type_error_expected_type);
}

void build_common_types(void)
{
	defun_coerce();
	defmacro_deftype();
	defun_subtypep();
	defun_type_of();
	defun_typep();
	defun_type_error_datum();
	defun_type_error_expected_type();
}

