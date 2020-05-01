/*
 *  ANSI COMMON LISP: 3. Evaluation and Compilation
 */
#include "call_eval.h"
#include "common_header.h"
#include "eval_declare.h"

/* (defmacro lambda (&whole right &rest args)  ...) */
static int function_lambda(Execute ptr, addr form, addr env)
{
	lambda_common(form, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_lambda(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_LAMBDA, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_lambda);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun compile (name &optional definition) ...)
 *     -> function, warnings-p, failure-p
 *   name        (or function-name null)
 *   definition  (or cons function)
 *   function    (or function-name function)
 *   warnings-p  boolean
 *   failure-p   boolean
 */
static int function_compile(Execute ptr, addr var, addr opt)
{
	addr x, y, z;
	Return(compile_common(ptr, var, opt, &x, &y, &z));
	setvalues_control(ptr, x, y, z, NULL);
	return 0;
}

static void type_compile(addr *ret)
{
	addr args, values, x, y, type1, type2;

	/* args */
	GetTypeTable(&type1, FunctionName);
	GetTypeTable(&type2, Null);
	type2or_heap(type1, type2, &x);
	GetTypeTable(&type1, Cons);
	GetTypeTable(&type2, Function);
	type2or_heap(type1, type2, &y);
	typeargs_var1opt1(&args, x, y);
	/* values */
	GetTypeTable(&type1, FunctionName);
	GetTypeTable(&type2, Function);
	type2or_heap(type1, type2, &x);
	GetTypeTable(&y, Boolean);
	typevalues_values3(&values, x, y, y);
	/* result */
	type_compiled_heap(args, values, ret);
}

static void defun_compile(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_compile);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eval (form) ...) -> result */
static int function_eval(Execute ptr, addr var)
{
	return eval_common(ptr, var);
}

static void type_eval(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_eval(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_eval);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_eval(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* special-operator eval-when */
static void defspecial_eval_when(void)
{
	DefineSpecialOperator(COMMON_EVAL_WHEN);
}


/* special-operator load-time-value */
static void defspecial_load_time_value(void)
{
	DefineSpecialOperator(COMMON_LOAD_TIME_VALUE);
}


/* special-operator quote */
static void defspecial_quote(void)
{
	DefineSpecialOperator(COMMON_QUOTE);
}


/* (defun compiler-macro-function (name &optional env) ...)
 *     -> function
 *   name      function-name
 *   env       environment
 *   function  (or function null)
 */
static int function_compiler_macro_function(Execute ptr, addr var, addr env)
{
	compiler_macro_function_common(var, env, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_compiler_macro_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	GetTypeTable(&values, Environment);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, FunctionNull);
	type_compiled_heap(args, values, ret);
}

static void defun_compiler_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILER_MACRO_FUNCTION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_compiler_macro_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compiler_macro_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf compiler-macro-function) (value name &optional env) ...)
 *     -> function
 *   values    function
 *   name      function-name
 *   env       environment
 *   function  (or function null)
 */
static int function_setf_compiler_macro_function(
		Execute ptr, addr value, addr var, addr env)
{
	setf_compiler_macro_function_common(value, var, env);
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_compiler_macro_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, Function);
	GetTypeTable(&args, FunctionName);
	GetTypeTable(&values, Environment);
	typeargs_var2opt1(&args, type, args, values);
	GetTypeValues(&values, FunctionNull);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_compiler_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILER_MACRO_FUNCTION, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_setf_compiler_macro_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_compiler_macro_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defmacro define-compiler-macro (name lambda &body form) ...) -> name */
static int function_define_compiler_macro(Execute ptr, addr form, addr env)
{
	define_compiler_macro_common(ptr, form, env, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_compiler_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_COMPILER_MACRO, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_compiler_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defmacro (name args &body body) ...) */
static int function_defmacro(Execute ptr, addr form, addr env)
{
	Return(defmacro_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defmacro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFMACRO, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defmacro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun macro-function (symbol &optional environment) ...) -> function) */
static int function_macro_function(Execute ptr, addr symbol, addr env)
{
	macro_function_common(symbol, env, &symbol);
	setresult_control(ptr, symbol);
	return 0;
}

static void type_macro_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACRO_FUNCTION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macro_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_macro_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf macro-function) (value symbol &optional env) ...) -> function
 *   The function accept the environment object, but ignored.
 *   If there is a non-nil environemnt argument,
 *     sbcl  -> error.
 *     clisp -> update global macro.
 *     ccl   -> update global macro.
 */
static int function_setf_macro_function(Execute ptr, addr value, addr symbol, addr env)
{
	setmacro_symbol(symbol, value);
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_macro_function(addr *ret)
{
	addr args, values, type, env;

	GetTypeTable(&args, Function);
	GetTypeTable(&type, Symbol);
	GetTypeTable(&env, Environment);
	typeargs_var2opt1(&args, args, type, env);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACRO_FUNCTION, &symbol);
	compiled_setf_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_setf_macro_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_macro_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun macroexpand (form &optional env) ...) -> expansion, expansion-p */
static int function_macroexpand(Execute ptr, addr form, addr env)
{
	Return(macroexpand_common(ptr, form, env, &form, &env));
	setvalues_control(ptr, form, env, NULL);
	return 0;
}

static void defun_macroexpand(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACROEXPAND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macroexpand);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroExpand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun macroexpand-1 (form &optional env) ...) -> expansion, expansion-p */
static int function_macroexpand_1(Execute ptr, addr form, addr env)
{
	Return(macroexpand_1_common(ptr, form, env, &form, &env));
	setvalues_control(ptr, form, env, NULL);
	return 0;
}

static void defun_macroexpand_1(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACROEXPAND_1, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_macroexpand_1);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroExpand);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro define-symbol-macro (symbol expansion) ...) -> symbol */
static int function_define_symbol_macro(Execute ptr, addr form, addr env)
{
	Return(define_symbol_macro_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_symbol_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_SYMBOL_MACRO, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_symbol_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator symbol-macrolet */
static void defspecial_symbol_macrolet(void)
{
	DefineSpecialOperator(COMMON_SYMBOL_MACROLET);
}


/* (defun proclaim (declaration) ...) -> null */
static int function_proclaim(Execute ptr, addr var)
{
	Return(proclaim_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_proclaim(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_proclaim(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROCLAIM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_proclaim);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_proclaim(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro declaim (&rest declarations)  ...) */
static int function_declaim(Execute ptr, addr form, addr env)
{
	Return(declaim_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_declaim(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DECLAIM, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_declaim);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator locally */
static void defspecial_locally(void)
{
	DefineSpecialOperator(COMMON_LOCALLY);
}


/* special-operator the */
static void defspecial_the(void)
{
	DefineSpecialOperator(COMMON_THE);
}


/* (defun special-operator-p (symbol) ...) -> boolean */
static int function_special_operator_p(Execute ptr, addr var)
{
	setbool_control(ptr, get_special_operator(var));
	return 0;
}

static void defun_special_operator_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SPECIAL_OPERATOR_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_special_operator_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Symbol_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun constantp (form &optional env) ...) -> boolean
 *   form  object
 *   env   (or environment null)
 */
static int function_constantp(Execute ptr, addr var, addr opt)
{
	Return(constantp_common(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_constantp(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&args, args, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_constantp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSTANTP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_constantp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_constantp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_eval(void)
{
	SetPointerCall(defmacro,  macro,     lambda);
	SetPointerCall(defun,     var1opt1,  compile);
	SetPointerCall(defun,     var1,      eval);
	SetPointerCall(defun,     var1opt1,  compiler_macro_function);
	SetPointerCall(defun,     var2opt1,  setf_compiler_macro_function);
	SetPointerCall(defmacro,  macro,     define_compiler_macro);
	SetPointerCall(defmacro,  macro,     defmacro);
	SetPointerCall(defun,     var1opt1,  macro_function);
	SetPointerCall(defun,     var2opt1,  setf_macro_function);
	SetPointerCall(defun,     var1opt1,  macroexpand);
	SetPointerCall(defun,     var1opt1,  macroexpand_1);
	SetPointerCall(defmacro,  macro,     define_symbol_macro);
	SetPointerCall(defun,     var1,      proclaim);
	SetPointerCall(defmacro,  macro,     declaim);
	SetPointerCall(defun,     var1,      special_operator_p);
	SetPointerCall(defun,     var1opt1,  constantp);
}

_g void build_common_eval(void)
{
	defmacro_lambda();
	defun_compile();
	defun_eval();
	defspecial_eval_when();
	defspecial_load_time_value();
	defspecial_quote();
	defun_compiler_macro_function();
	defun_setf_compiler_macro_function();
	defmacro_define_compiler_macro();
	defmacro_defmacro();
	defun_macro_function();
	defun_setf_macro_function();
	defun_macroexpand();
	defun_macroexpand_1();
	defmacro_define_symbol_macro();
	defspecial_symbol_macrolet();
	defun_proclaim();
	defmacro_declaim();
	defspecial_locally();
	defspecial_the();
	defun_special_operator_p();
	defun_constantp();
}

