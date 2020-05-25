#include "callname.h"
#include "compile.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "eval.h"
#include "function.h"
#include "hold.h"
#include "lambda.h"
#include "parse.h"
#include "parse_macro.h"
#include "symbol.h"

/*
 *  lambda
 */
_g void lambda_common(addr form, addr *ret)
{
	addr symbol;
	GetConst(COMMON_FUNCTION, &symbol);
	list_heap(ret, symbol, form, NULL);
}


/*
 *  eval
 */
_g int eval_common(Execute ptr, addr var)
{
	addr control;

	push_new_control(ptr, &control);
	push_toplevel_eval(ptr, Nil);
	push_evalwhen_eval(ptr);
	Return(eval_execute(ptr, var));
	return free_control_(ptr, control);
}


/*
 *  compiler-macro-function
 */
static void compiler_macro_function_symbol(addr var, addr env, addr *ret)
{
	if (env != Unbound && find_environment(var, env, &env)) {
		/* compiler-macro-function is shadowed */
		*ret = Nil;
		return;
	}
	GetCallName(var, &var);
	get_compiler_macro_symbol(var, ret);
}

static void compiler_macro_function_setf(addr var, addr env, addr *ret)
{
	if (env != Unbound) {
		fmte("Don't use environment argument ~S "
				"in COMPILER-MACRO-FUNCTION setf-form.", env, NULL);
		*ret = Nil;
		return;
	}
	GetCallName(var, &var);
	get_setf_compiler_macro_symbol(var, ret);
}

_g void compiler_macro_function_common(addr var, addr env, addr *ret)
{
	parse_callname_error(&var, var);
	if (symbolp_callname(var))
		compiler_macro_function_symbol(var, env, ret);
	else
		compiler_macro_function_setf(var, env, ret);
}

static void setf_compiler_macro_function_symbol(addr var, addr env, addr value)
{
	if (env != Unbound && find_environment(var, env, &env)) {
		/* compiler-macro-function is shadowed */
		fmte("COMPILER-MACRO-FUNCTION ~S is shadowed in the environment.", var, NULL);
		return;
	}
	GetCallName(var, &var);
	set_compiler_macro_symbol(var, value);
}

static void setf_compiler_macro_function_setf(addr var, addr env, addr value)
{
	if (env != Unbound) {
		fmte("Don't use environment argument ~S "
				"in COMPILER-MACRO-FUNCTION setf-form.", env, NULL);
		return;
	}
	GetCallName(var, &var);
	set_setf_compiler_macro_symbol(var, value);
}

_g void setf_compiler_macro_function_common(addr value, addr var, addr env)
{
	if (! callnamep(var))
		parse_callname_error(&var, var);
	if (symbolp_callname(var))
		setf_compiler_macro_function_symbol(var, env, value);
	else
		setf_compiler_macro_function_setf(var, env, value);
}


/*
 *  define-compiler-macro
 */
_g void define_compiler_macro_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;

	/* (define-compiler-macro . form) */
	getcdr(form, &right);
	if (right == Nil)
		fmte("define-compiler-macro form must have at least a name and body.", NULL);
	if (! consp(right))
		fmte("Invalid define-compiler-macro form.", NULL);

	/* name */
	getcons(right, &name, &right);
	parse_callname_error(&name, name);
	if (right == Nil)
		fmte("define-compiler-macro form must have at least a name and body.", NULL);
	if (! consp(right))
		fmte("Invalid define-compiler-macro form.", NULL);

	/* args */
	getcons(right, &args, &right);
	if (! IsList(right))
		fmte("Invalid define-compiler-macro form.", NULL);

	/* parse */
	lambda_macro(ptr->local, &args, args, Nil);
	if (declare_body_documentation(ptr, env, right, &doc, &decl, &right))
		return;

	/* (eval::define-compiler-macro name args decl doc body) */
	GetConst(SYSTEM_DEFINE_COMPILER_MACRO, &eval);
	list_heap(ret, eval, name, args, decl, doc, right, NULL);
}

_g void set_define_compiler_macro(addr callname, addr value)
{
	setf_compiler_macro_function_common(value, callname, Unbound);
}


/*
 *  compile
 */
static void compile_variable(Execute ptr, addr var, addr opt, addr *ret)
{
	addr call, check;

	parse_callname_error(&call, var);
	getglobal_callname(call, &check);
	if (check == Unbound) {
		if (! symbolp_callname(call))
			goto unbound;
		getmacro_symbol(var, &check);
		if (check == Unbound)
			goto unbound;
	}
	fmtw("This implementation cannot compile a function.", NULL);
	*ret = var;
	return;

unbound:
	fmte("The function ~S is unbound.", var, NULL);
	*ret = Nil;
}

static int compile_lambda_p(addr opt)
{
	addr check;

	if (! consp(opt))
		return 0;
	GetCar(opt, &opt);
	GetConst(COMMON_LAMBDA, &check);
	return check == opt;
}

static int compile_lambda(Execute ptr, addr opt, addr *ret)
{
	if (functionp(opt)) {
		fmtw("This implementation cannot compile a function.", NULL);
		*ret = opt;
		return 0;
	}
	if (compile_lambda_p(opt)) {
		fmtw("This implementation cannot compile a function.", NULL);
		Return(eval_object(ptr, opt, &opt));
		*ret = opt;
		return 0;
	}

	fmte("The second argument ~S must be a lambda expression.", opt, NULL);
	*ret = Nil;
	return 0;
}

static int compile_symbol(Execute ptr, addr var, addr opt, addr *ret)
{
	addr call;
	LocalHold hold;

	parse_callname_error(&call, var);
	if (functionp(opt)) {
		fmtw("This implementation cannot compile a function.", NULL);
		setglobal_callname(call, opt);
		*ret = var;
		return 0;
	}
	if (compile_lambda_p(opt)) {
		fmtw("This implementation cannot compile a function.", NULL);
		hold = LocalHold_local(ptr);
		localhold_pushva_force(hold, call, opt, NULL);
		Return(eval_object(ptr, opt, &opt));
		localhold_end(hold);
		setglobal_callname(call, opt);
		*ret = var;
		return 0;
	}

	fmte("The second argument ~S must be a lambda expression.", opt, NULL);
	*ret = Nil;
	return 0;
}

static int compile_execute(Execute ptr, addr var, addr opt, addr *ret)
{
	if (opt == Unbound) {
		compile_variable(ptr, var, opt, ret);
		return 0;
	}
	if (var == Nil) {
		Return(compile_lambda(ptr, opt, ret));
		return 0;
	}
	if (function_name_p(var)) {
		Return(compile_symbol(ptr, var, opt, ret));
		return 0;
	}

	/* error */
	fmte("The first argument ~S in COMPILE must be a function-name.", var, NULL);
	*ret = Nil;
	return 0;
}

_g int compile_common(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	handler_compile(ptr);
	Return(compile_execute(ptr, var, opt, ret1));
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &var);
	getspecialcheck_local(ptr, var, ret2);
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &var);
	getspecialcheck_local(ptr, var, ret3);
	/* free */
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}


/*
 *  defmacro
 */
_g int defmacro_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr eval, name, args, decl, doc;

	/* (defmacro . form) */
	Return_getcdr(form, &form);
	if (form == Nil)
		return fmte_("defmacro form must have at least a name and body.", NULL);
	if (GetType(form) != LISPTYPE_CONS)
		return fmte_("Invalid defmacro form.", NULL);

	/* name */
	Return_getcons(form, &name, &form);
	if (! symbolp(name))
		return fmte_("defmacro name ~S must be a symbol.", name, NULL);
	if (form == Nil)
		return fmte_("defmacro form must have at least a name and body.", NULL);
	if (! consp(form))
		return fmte_("Invalid defmacro form.", NULL);

	/* args */
	Return_getcons(form, &args, &form);
	if (! IsList(form))
		return fmte_("Invalid defmacro form.", NULL);

	/* parse */
	check_function_variable(name);
	lambda_macro(ptr->local, &args, args, Nil);
	Return(declare_body_documentation(ptr, env, form, &doc, &decl, &form));

	/* (eval::defmacro name args decl doc body) */
	GetConst(SYSTEM_DEFMACRO, &eval);
	list_heap(ret, eval, name, args, decl, doc, form, NULL);

	return 0;
}


/*
 *  macro-function
 */
_g void macro_function_common(addr symbol, addr env, addr *ret)
{
	int check;

	if (env == Unbound)
		env = Nil;
	check = find_environment(symbol, env, &symbol);
	*ret = check? symbol: Nil;
}


/*
 *  macroexpand
 */
_g int macroexpand_common(Execute ptr, addr form, addr env, addr *ret, addr *sec)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(macroexpand(ptr, &form, form, env, &check));
	if (check) {
		*ret = form;
		*sec = T;
	}
	else {
		*ret = Nil;
		*sec = Nil;
	}

	return 0;
}


/*
 *  macroexpand_1
 */
_g int macroexpand_1_common(Execute ptr, addr form, addr env, addr *ret, addr *sec)
{
	int check;

	if (env == Unbound)
		env = Nil;
	Return(macroexpand1(ptr, &form, form, env, &check));
	if (check) {
		*ret = form;
		*sec = T;
	}
	else {
		*ret = Nil;
		*sec = Nil;
	}

	return 0;
}


/*
 *  define-symbol-macro
 */
_g int define_symbol_macro_common(addr form, addr env, addr *ret)
{
	addr cons, symbol, expansion;

	Return_getcdr(form, &cons);
	if (! consp(cons))
		goto error;
	GetCons(cons, &symbol, &cons);
	if (! consp(cons))
		goto error;
	GetCons(cons, &expansion, &cons);
	if (cons != Nil)
		goto error;
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol.", NULL);
	/* (lisp-system::define-symbol-macro symbol expansion) */
	GetConst(SYSTEM_DEFINE_SYMBOL_MACRO, &form);
	list_heap(ret, form, symbol, expansion, NULL);
	return 0;

error:
	return fmte_("define-symbol-macro argument ~S "
			"must be a (symbol expansion) form.", form, NULL);
}


/*
 *  declaim
 */
_g int declaim_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr symbol;

	GetConst(SYSTEM_DECLAIM, &symbol);
	Return_getcdr(form, &form); /* (declaim . form) */
	Return(parse_declaim_heap(ptr, Nil, form, &form));
	conscar_heap(&form, form);
	cons_heap(ret, symbol, form);

	return 0;
}


/*
 *  constantp
 */
_g int constantp_common(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	Return(eval_constantp(ptr, var, opt, &check));
	*ret = check? T: Nil;

	return 0;
}

