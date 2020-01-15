#include "compile.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "eval_common.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "function.h"
#include "gc.h"
#include "lambda.h"
#include "symbol.h"

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
	if (symbol_callname_p(var))
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
	if (symbol_callname_p(var))
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
	getfunction_callname_local(ptr, call, &check);
	if (check == Unbound) {
		if (! symbol_callname_p(call))
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
		Return1(eval_object(ptr, opt, &opt));
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
		setfunction_callname_global(call, opt);
		*ret = var;
		return 0;
	}
	if (compile_lambda_p(opt)) {
		fmtw("This implementation cannot compile a function.", NULL);
		hold = LocalHold_local(ptr);
		localhold_pushva_force(hold, call, opt, NULL);
		Return1(eval_object(ptr, opt, &opt));
		localhold_end(hold);
		setfunction_callname_global(call, opt);
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
		Return1(compile_lambda(ptr, opt, ret));
		return 0;
	}
	if (function_name_p(var)) {
		Return1(compile_symbol(ptr, var, opt, ret));
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
	push_close_control(ptr, &control);
	handler_compile(ptr);
	if (compile_execute(ptr, var, opt, ret1))
		return free_check_control(ptr, control, 1);
	localhold_set(hold, 0, *ret1);
	/* warning */
	GetConst(SYSTEM_COMPILE_WARNING, &var);
	getlexicalcheck_local(ptr, var, ret2);
	/* style-warning */
	GetConst(SYSTEM_COMPILE_STYLE_WARNING, &var);
	getlexicalcheck_local(ptr, var, ret3);
	/* free */
	Return1(free_check_control(ptr, control, 0));
	localhold_end(hold);

	return 0;
}

