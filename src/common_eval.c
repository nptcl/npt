/*
 *  ANSI COMMON LISP: 3. Evaluation and Compilation
 */
#include "common_header.h"
#include "cons.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "lambda.h"
#include "type_parse.h"

/* (defmacro lambda (&whole right &rest args)  ...) */
static void function_lambda(Execute ptr, addr form, addr env)
{
	addr symbol;

	GetConst(COMMON_FUNCTION, &symbol);
	list_heap(&form, symbol, form, NULL);
	setresult_control(ptr, form);
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


/* (defun eval (form) ...) -> result */
static void function_eval(Execute ptr, addr var)
{
	int check;
	addr control;

	/* push */
	push_return_control(ptr, &control);
	/* code */
	push_toplevel_eval(ptr, Nil);
	push_evalwhen_eval(ptr);
	hide_lexical_control(ptr);
	check = eval_execute(ptr, var);
	(void)free_check_control(ptr, control, check);
}

static void type_eval(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
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


/* (defmacro defmacro (name args &body body) ...) */
static void function_defmacro(Execute ptr, addr right, addr env)
{
	addr eval, name, args, decl, doc;

	/* (defmacro . right) */
	getcdr(right, &right);
	if (right == Nil)
		fmte("defmacro form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		fmte("Invalid defmacro form.", NULL);

	/* name */
	getcons(right, &name, &right);
	if (! symbolp(name))
		fmte("defmacro name ~S must be a symbol.", name, NULL);
	if (right == Nil)
		fmte("defmacro form must have at least a name and body.", NULL);
	if (! consp(right))
		fmte("Invalid defmacro form.", NULL);

	/* args */
	getcons(right, &args, &right);
	if (! IsList(right))
		fmte("Invalid defmacro form.", NULL);

	/* parse */
	check_function_variable(name);
	lambda_macro(ptr->local, &args, args, Nil);
	if (declare_body_documentation(ptr, env, right, &doc, &decl, &right))
		return;

	/* (eval::defmacro name args decl doc body) */
	GetConst(SYSTEM_DEFMACRO, &eval);
	list_heap(&eval, eval, name, args, decl, doc, right, NULL);
	setresult_control(ptr, eval);
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
static void function_macro_function(Execute ptr, addr symbol, addr env)
{
	int check;
	if (env == Unbound) env = Nil;
	check = findmacro_environment(symbol, env, &symbol);
	setresult_control(ptr, check? symbol: Nil);
}

static void type_macro_function(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Symbol);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&arg, arg, type);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
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
static void function_setf_macro_function(Execute ptr,
		addr value, addr symbol, addr env)
{
	setmacro_symbol(symbol, value);
	setresult_control(ptr, value);
}

static void type_setf_macro_function(addr *ret)
{
	addr arg, values, type, env;

	GetTypeTable(&arg, Function);
	GetTypeTable(&type, Symbol);
	GetTypeTable(&env, Environment);
	typeargs_var2opt1(&arg, arg, type, env);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_macro_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACRO_FUNCTION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_setf_macro_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_macro_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun macroexpand (form &optional env) ...) -> expansion, expansion-p */
static void function_macroexpand(Execute ptr, addr form, addr env)
{
	int check;

	if (env == Unbound) env = Nil;
	if (macroexpand(&form, form, env, &check))
		return;
	if (check)
		setvalues_control(ptr, form, T, NULL);
	else
		setvalues_control(ptr, Nil, Nil, NULL);
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
static void function_macroexpand_1(Execute ptr, addr form, addr env)
{
	int check;

	if (env == Unbound) env = Nil;
	if (macroexpand1(&form, form, env, &check))
		return;
	if (check)
		setvalues_control(ptr, form, T, NULL);
	else
		setvalues_control(ptr, Nil, Nil, NULL);
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
static void function_define_symbol_macro(Execute ptr, addr form, addr env)
{
	addr cons, symbol, expansion;

	getcdr(form, &cons);
	if (! consp(cons)) goto error;
	GetCons(cons, &symbol, &cons);
	if (! consp(cons)) goto error;
	GetCons(cons, &expansion, &cons);
	if (cons != Nil) goto error;
	if (! symbolp(symbol))
		fmte("The argument ~S must be a symbol.", NULL);
	/* (lisp-system::define-symbol-macro symbol expansion) */
	GetConst(SYSTEM_DEFINE_SYMBOL_MACRO, &form);
	list_heap(&cons, form, symbol, expansion, NULL);
	setresult_control(ptr, cons);
	return;

error:
	fmte("define-symbol-macro argument ~S "
			"must be a (symbol expansion) form.", form, NULL);
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


/* (defmacro declaim (&rest declarations)  ...) */
static void function_declaim(Execute ptr, addr form, addr env)
{
	addr symbol;

	GetConst(SYSTEM_DECLAIM, &symbol);
	getcdr(form, &form); /* (declaim . form) */
	if (parse_declaim_heap(ptr, Nil, form, &form)) return;
	conscar_heap(&form, form);
	cons_heap(&form, symbol, form);
	setresult_control(ptr, form);
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
static void function_special_operator_p(Execute ptr, addr var)
{
	setbool_control(ptr, get_special_operator(var));
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
static void function_constantp(Execute ptr, addr var, addr opt)
{
	int check;

	if (opt == Unbound) opt = Nil;
	if (eval_constantp(var, opt, &check))
		return;
	setbool_control(ptr, check);
}

static void type_constantp(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, T);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var1opt1(&arg, arg, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
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
void init_common_eval(void)
{
	SetPointerCall(defmacro,  macro,     lambda);
	SetPointerCall(defun,     var1,      eval);
	SetPointerCall(defmacro,  macro,     defmacro);
	SetPointerCall(defun,     var1opt1,  macro_function);
	SetPointerCall(defun,     var2opt1,  setf_macro_function);
	SetPointerCall(defun,     var2opt1,  setf_macro_function);
	SetPointerCall(defun,     var1opt1,  macroexpand);
	SetPointerCall(defun,     var1opt1,  macroexpand_1);
	SetPointerCall(defmacro,  macro,     define_symbol_macro);
	SetPointerCall(defmacro,  macro,     declaim);
	SetPointerCall(defun,     var1,      special_operator_p);
	SetPointerCall(defun,     var1opt1,  constantp);
}

void build_common_eval(void)
{
	defmacro_lambda();
	defun_eval();
	defspecial_eval_when();
	defspecial_load_time_value();
	defspecial_quote();
	defmacro_defmacro();
	defun_macro_function();
	defun_setf_macro_function();
	defun_macroexpand();
	defun_macroexpand_1();
	defmacro_define_symbol_macro();
	defspecial_symbol_macrolet();
	defmacro_declaim();
	defspecial_locally();
	defspecial_the();
	defun_special_operator_p();
	defun_constantp();
}

