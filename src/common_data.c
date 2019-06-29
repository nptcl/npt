/*
 *  ANSI COMMON LISP: 5. Data and Control Flow
 */
#include "array.h"
#include "clos_class.h"
#include "code.h"
#include "common_header.h"
#include "cons.h"
#include "equal.h"
#include "eval_declare.h"
#include "eval_parse.h"
#include "hashtable.h"
#include "lambda.h"
#include "sequence.h"
#include "setf.h"
#include "strtype.h"
#include "type_parse.h"

/* (defun apply (call arg &rest args) ...) -> value
 *   call   (or function symbol)  ;; function-designer
 *   arg    t
 *   args   t
 *   value  (values &rest t)
 */
static void checkfunction(Execute ptr, addr call, addr *ret)
{
	if (GetType(call) == LISPTYPE_SYMBOL) {
		getfunctioncheck_local(ptr, call, &call);
		if (macro_function_p(call))
			fmte("Cannot call the macro-function ~S.", call, NULL);
	}
	if (! funcallp(call))
		fmte("The argument ~S is not executable.", call, NULL);
	*ret = call;
}

static void function_apply(Execute ptr, addr call, addr arg, addr args)
{
	LocalRoot local;
	LocalStack stack;

	checkfunction(ptr, call, &call);
	local = ptr->local;
	push_local(local, &stack);
	lista_local_safe(local, &args, arg, args);
	if (apply_control(ptr, call, args)) return;
	rollback_local(local, stack);
}

static void type_apply(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, FunctionDesigner);
	GetTypeTable(&type, T);
	typeargs_var2rest(&arg, arg, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_apply(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPLY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_apply);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apply(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro defun (name lambda-list &rest body) ...) */
static void function_defun(Execute ptr, addr right, addr env)
{
	addr eval, name, args, decl, doc;

	/* (defun . right) */
	getcdr(right, &right);
	if (right == Nil)
		fmte("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		fmte("Invalid defun form.", NULL);

	/* name */
	getcons(right, &name, &right);
	if (parse_callname_heap(&name, name))
		fmte("defun name ~S must be a symbol or (setf name) form.", name, NULL);
	if (right == Nil)
		fmte("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		fmte("Invalid defun form.", NULL);

	/* args */
	getcons(right, &args, &right);
	if (! IsList(args))
		fmte("defun argument ~S don't allow dotted list.", args, NULL);
	if (! IsList(right))
		fmte("Invalid defun form.", NULL);

	/* parse */
	check_function_variable(name);
	lambda_ordinary(ptr->local, &args, args);
	if (declare_body_documentation(ptr, env, right, &doc, &decl, &right))
		return;

	/* (eval::defun name args decl doc body) */
	GetConst(SYSTEM_DEFUN, &eval);
	list_heap(&eval, eval, name, args, decl, doc, right, NULL);
	setresult_control(ptr, eval);
}

static void defmacro_defun(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFUN, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defun);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun fdefinition (name) ...) -> function
 *   name  (or symbol (cons (eql setf) (cons symbol null))) -> function-name
 */
static void function_fdefinition(Execute ptr, addr name)
{
	addr call;

	parse_callname_error(&call, name);
	getfunction_callname_global(call, &call);
	if (call == Unbound)
		undefined_function(name);
	setresult_control(ptr, call);
}

static void type_fdefinition(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, FunctionName);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
}

static void defun_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_fdefinition);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fdefinition(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf fdefinition) (function name) ...) -> function
 *   name   function-name
 */
static void function_setf_fdefinition(Execute ptr, addr value, addr name)
{
	addr call;

	parse_callname_error(&call, name);
	setfunction_callname_global(call, value);
	setresult_control(ptr, value);
}

static void type_setf_fdefinition(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Function);
	GetTypeTable(&values, FunctionName);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_fdefinition);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_fdefinition(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun fbound (name) ...) -> boolean
 *   name  function-name
 */
static void function_fboundp(Execute ptr, addr name)
{
	addr call;

	parse_callname_error(&call, name);
	getfunction_callname_global(call, &call);
	setbool_control(ptr, call != Unbound);
}

static void type_fboundp(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, FunctionName);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_fboundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FBOUNDP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_fboundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fboundp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fmakunbound (name) ...) -> name
 *   name  function-name
 */
static void function_fmakunbound(Execute ptr, addr name)
{
	addr call;

	parse_callname_error(&call, name);
	setfunction_callname_global(call, Unbound);
	setresult_control(ptr, name);
}

static void type_fmakunbound(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, FunctionName);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, FunctionName);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_fmakunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FMAKUNBOUND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_fmakunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fmakunbound(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* special-operator flet */
static void defspecial_flet(void)
{
	DefineSpecialOperator(COMMON_FLET);
}


/* special-operator labels */
static void defspecial_labels(void)
{
	DefineSpecialOperator(COMMON_LABELS);
}


/* special-operator macrolet */
static void defspecial_macrolet(void)
{
	DefineSpecialOperator(COMMON_MACROLET);
}


/* (defun funcall (function &rest args) ...) -> value
 *   function  (or function symbol)  ;; function-designer
 *   args      t
 *   value     (values &rest t)
 */
static void function_funcall(Execute ptr, addr call, addr args)
{
	checkfunction(ptr, call, &call);
	(void)apply_control(ptr, call, args);
}

static void type_funcall(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, FunctionDesigner);
	GetTypeTable(&type, T);
	typeargs_var1rest(&arg, arg, type);
	typevalues_rest(&values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_funcall(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCALL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_funcall);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_funcall(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* special-operator function */
static void defspecial_function(void)
{
	DefineSpecialOperator(COMMON_FUNCTION);
}


/* (defun function-lambda-expression (function) ...)
 *   -> lambda-expression, closure-p, name
 *   lambda-expression  t
 *   closure-p          boolean
 *   name               function-name
 */
static int function_closure_p(addr var)
{
	addr check;

	GetClosureValueFunction(var, &check);
	if (check != Nil) return 1;
	GetClosureFunctionFunction(var, &check);
	if (check != Nil) return 1;
	GetClosureTagbodyFunction(var, &check);
	if (check != Nil) return 1;
	GetClosureBlockFunction(var, &check);
	if (check != Nil) return 1;

	return 0;
}

static void function_function_lambda_expression(Execute ptr, addr var)
{
	addr pos1, pos2, pos3;

	/* lambda-expression */
	getlambda_expression_function(var, &pos1);
	/* closure-p */
	pos2 = function_closure_p(var)? T: Nil;
	/* name */
	GetNameFunction(var, &pos3);
	if (GetType(pos3) == LISPTYPE_CALLNAME)
		name_callname_heap(pos3, &pos3);
	/* result */
	setvalues_control(ptr, pos1, pos2, pos3, NULL);
}

static void type_function_lambda_expression(addr *ret)
{
	addr arg, values, type1, type2, type3, null;

	GetTypeTable(&arg, Function);
	typeargs_var1(&arg, arg);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Boolean);
	GetTypeTable(&type3, FunctionName);
	GetTypeTable(&null, Null);
	type2or_heap(type3, null, &type3);
	typevalues_values3(&values, type1, type2, type3);
	type_compiled_heap(arg, values, ret);
}

static void defun_function_lambda_expression(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTION_LAMBDA_EXPRESSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_function_lambda_expression);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_function_lambda_expression(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun functionp (object) ...) -> boolean */
static void function_functionp(Execute ptr, addr var)
{
	setbool_control(ptr, funcallp(var));
}

static void defun_functionp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTIONP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_functionp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun compiled-function-p (object) ...) -> boolean */
static void function_compiled_function_p(Execute ptr, addr var)
{
	setbool_control(ptr, compiled_function_p(var));
}

static void defun_compiled_function_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILED_FUNCTION_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_compiled_function_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant call-arguments-limit FIXNUM-MAX) */
static void defconstant_call_arguments_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_CALL_ARGUMENTS_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant lambda-list-keywords
 *   '(&optional &rest &body &key &allow-other-keys &aux
 *     &whole &environment))
 */
static void defconstant_lambda_list_keywords(void)
{
	int i;
	addr list, pos;
	constindex index;
	static const constindex lambda_list_keywords[] = {
		CONSTANT_AMPERSAND_WHOLE,
		CONSTANT_AMPERSAND_OPTIONAL,
		CONSTANT_AMPERSAND_REST,
		CONSTANT_AMPERSAND_BODY,
		CONSTANT_AMPERSAND_KEY,
		CONSTANT_AMPERSAND_ALLOW,
		CONSTANT_AMPERSAND_AUX,
		CONSTANT_AMPERSAND_ENVIRONMENT,
		CONSTANT_EMPTY
	};

	list = Nil;
	for (i = 0; ; i++) {
		index = lambda_list_keywords[i];
		if (index == CONSTANT_EMPTY) break;
		GetConstant(index, &pos);
		cons_heap(&list, pos, list);
	}
	GetConst(COMMON_LAMBDA_LIST_KEYWORDS, &pos);
	defconstant_symbol(pos, list);
}


/* (defconstant lambda-parameters-limit FIXNUM-MAX) */
static void defconstant_lambda_parameters_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_LAMBDA_PARAMETERS_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defmacro defconstant (name value &optional documentation) ...) -> name
 *    name           symbol
 *    value          t
 *    documentation  string
 */
static void function_defconstant(Execute ptr, addr form, addr env)
{
	/* (lisp-system::defconstant symbol value doc) */
	addr args, symbol, value, doc, quote;

	getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &symbol, &args);
	if (! symbolp(symbol))
		fmte("The defconstant argument ~S must be a symbol.", symbol, NULL);
	if (! consp(args))
		goto error;
	GetCons(args, &value, &args);
	if (args == Nil) {
		doc = Nil;
	}
	else {
		if (! consp(args))
			goto error;
		GetCons(args, &doc, &args);
		if (! stringp(doc))
			fmte("The defconstant argument ~S must be a string.", doc, NULL);
		if (args != Nil)
			goto error;
	}
	GetConst(SYSTEM_DEFCONSTANT, &args);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&doc, quote, doc, NULL);
	list_heap(&args, args, symbol, value, doc, NULL);
	setresult_control(ptr, args);
	return;

error:
	fmte("The defconstant argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}

static void defmacro_defconstant(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCONSTANT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defconstant);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defparameter (name value &optional documentation) ...) -> name
 *    name           symbol
 *    value          t
 *    documentation  string
 */
static void expand_defparameter(Execute ptr, addr symbol, addr value, addr doc)
{
	/* `(progn (declaim (special ,symbol))
	 *         (setf (symbol-value ',symbol) ,value)
	 *         ,(when doc
	 *            `(lisp-system::setdoc-variable ',symbol ',doc))
	 *         ',symbol)
	 */
	addr progn, declaim, special, setf, symbolv, setdoc, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_SETF, &setf);
	GetConst(COMMON_SYMBOL_VALUE, &symbolv);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&symbolv, symbolv, symbol, NULL);
	list_heap(&setf, setf, symbolv, value, NULL);
	if (doc == Nil) {
		list_heap(&progn, progn, declaim, setf, symbol, NULL);
	}
	else {
		GetConst(SYSTEM_SETDOC_VARIABLE, &setdoc);
		list_heap(&doc, quote, doc, NULL);
		list_heap(&setdoc, setdoc, symbol, doc, NULL);
		list_heap(&progn, progn, declaim, setf, setdoc, symbol, NULL);
	}
	setresult_control(ptr, progn);
}

static void function_defparameter(Execute ptr, addr form, addr env)
{
	/* (lisp-system::defparameter symbol value doc) */
	addr args, symbol, value, doc;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &symbol, &args);
	if (! symbolp(symbol))
		fmte("The defparameter argument ~S must be a symbol.", symbol, NULL);
	if (! consp(args)) goto error;
	GetCons(args, &value, &args);
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp(args)) goto error;
	GetCons(args, &doc, &args);
	if (! stringp(doc))
		fmte("The defparameter argument ~S must be a string.", doc, NULL);
	if (args != Nil) goto error;
expand:
	expand_defparameter(ptr, symbol, value, doc);
	return;

error:
	fmte("The defparameter argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}

static void defmacro_defparameter(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPARAMETER, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defparameter);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defvar (symbol &optional value document) ...) -> symbol */
static void expand_defvar_novalue(Execute ptr, addr symbol)
{
	/* `(progn (declaim (special ,symbol))
	 *         ',symbol)
	 */
	addr progn, declaim, special, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&progn, progn, declaim, symbol, NULL);
	setresult_control(ptr, progn);
}

static void expand_defvar(Execute ptr, addr symbol, addr value, addr doc)
{
	/* `(progn (declaim (special ,symbol))
	 *         `(unless (boundp ',symbol)
	 *           (setf (symbol-value ',symbol) ,value))
	 *         ,(when doc
	 *           `(lisp-system::setdoc-variable ',symbol ',doc))
	 *         ',symbol)
	 */
	addr progn, declaim, special, unless, boundp, setf, symbolv, setdoc, quote;

	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_DECLAIM, &declaim);
	GetConst(COMMON_SPECIAL, &special);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_BOUNDP, &boundp);
	GetConst(COMMON_SETF, &setf);
	GetConst(COMMON_SYMBOL_VALUE, &symbolv);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&special, special, symbol, NULL);
	list_heap(&declaim, declaim, special, NULL);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&symbolv, symbolv, symbol, NULL);
	list_heap(&setf, setf, symbolv, value, NULL);
	list_heap(&boundp, boundp, symbol, NULL);
	list_heap(&unless, unless, boundp, setf, NULL);
	if (doc == Nil) {
		list_heap(&progn, progn, declaim, unless, symbol, NULL);
	}
	else {
		GetConst(SYSTEM_SETDOC_VARIABLE, &setdoc);
		list_heap(&doc, quote, doc, NULL);
		list_heap(&setdoc, setdoc, symbol, doc, NULL);
		list_heap(&progn, progn, declaim, unless, setdoc, symbol, NULL);
	}
	setresult_control(ptr, progn);
}

static void function_defvar(Execute ptr, addr form, addr env)
{
	addr args, symbol, value, doc;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &symbol, &args);
	if (args == Nil) {
		value = Unbound;
		doc = Nil;
		goto expand;
	}
	if (! consp(args)) goto error;
	GetCons(args, &value, &args);
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp(args)) goto error;
	GetCons(args, &doc, &args);
	if (args != Nil) goto error;
expand:
	if (value == Unbound)
		expand_defvar_novalue(ptr, symbol);
	else
		expand_defvar(ptr, symbol, value, doc);
	return;

error:
	fmte("The defvar argument ~S must be a "
			"(symbol &optional value documentation) form.", form, NULL);
}

static void defmacro_defvar(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFVAR, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defvar);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro destructuring-bind (lambda expr &body body) ...) -> t */
static void check_destructuring_bind(addr pos)
{
	getenvironment_macro_lambda(pos, &pos);
	if (pos != Nil)
		fmte("destructuring-bind don't accept &environment parameter ~S.", pos, NULL);
}

static void function_destructuring_bind(Execute ptr, addr form, addr env)
{
	addr args, lambda, expr, decl, eval;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &lambda, &args);
	if (! consp(args)) goto error;
	GetCons(args, &expr, &args);
	/* parse */
	lambda_macro(ptr->local, &lambda, lambda, Nil);
	check_destructuring_bind(lambda);
	if (declare_body(ptr, env, args, &decl, &args))
		return;
	/* (eval::destructuring-bind lambda expr decl args) */
	GetConst(SYSTEM_DESTRUCTURING_BIND, &eval);
	list_heap(&eval, eval, lambda, expr, decl, args, NULL);
	setresult_control(ptr, eval);
	return;

error:
	fmte("destructuring-bind argument ~S must be a "
			"(lambda-list expr &body body) form.", form, NULL);
}

static void defmacro_destructuring_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DESTRUCTURING_BIND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_destructuring_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator let */
static void defspecial_let(void)
{
	DefineSpecialOperator(COMMON_LET);
}


/* special-operator let* */
static void defspecial_leta(void)
{
	DefineSpecialOperator(COMMON_LETA);
}


/* special-operator progv */
static void defspecial_progv(void)
{
	DefineSpecialOperator(COMMON_PROGV);
}


/* special-operator setq */
static void defspecial_setq(void)
{
	DefineSpecialOperator(COMMON_SETQ);
}


/* (defmacro psetq (&rest args) ...) -> value */
static void constant_psetq(Execute ptr, addr form, addr env,
		constindex setq_constant, constindex psetq_constant)
{
	addr args, root, var, value, gensym, cons, setq, let;

	getcdr(form, &form);
	GetConstant(setq_constant, &setq);
	args = root = Nil;
	while (form != Nil) {
		if (! consp(form)) {
			GetConstant(psetq_constant, &setq);
			fmte("~A argument ~S don't allow dotted list.", setq, form, NULL);
		}
		GetCons(form, &var, &form);
		if (! consp(form))
			fmte("After variable ~S must be a cons, but ~S.", var, form, NULL);
		GetCons(form, &value, &form);
		make_gensym(ptr, &gensym);
		/* let argument */
		list_heap(&cons, gensym, value, NULL);
		cons_heap(&args, cons, args);
		/* body */
		list_heap(&cons, setq, var, gensym, NULL);
		cons_heap(&root, cons, root);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&root, root);
	/* let form */
	GetConst(COMMON_LET, &let);
	lista_heap(&let, let, args, root, NULL);
	setresult_control(ptr, let);
}

static void function_psetq(Execute ptr, addr form, addr env)
{
	constant_psetq(ptr, form, env, CONSTANT_COMMON_SETQ, CONSTANT_COMMON_PSETQ);
}

static void defmacro_psetq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETQ, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_psetq);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator block */
static void defspecial_block(void)
{
	DefineSpecialOperator(COMMON_BLOCK);
}


/* special-operator catch */
static void defspecial_catch(void)
{
	DefineSpecialOperator(COMMON_CATCH);
}


/* special-operator go */
static void defspecial_go(void)
{
	DefineSpecialOperator(COMMON_GO);
}


/* special-operator return-from */
static void defspecial_return_from(void)
{
	DefineSpecialOperator(COMMON_RETURN_FROM);
}


/* (defmacro return (&optional value) ...) */
static void function_return(Execute ptr, addr form, addr env)
{
	addr args, value, return_from;

	getcdr(form, &args);
	if (args == Nil) {
		value = Nil;
		goto expand;
	}
	if (! consp(args)) goto error;
	GetCons(args, &value, &args);
	if (args != Nil) goto error;
expand:
	GetConst(COMMON_RETURN_FROM, &return_from);
	list_heap(&value, return_from, Nil, value, NULL);
	setresult_control(ptr, value);
	return;

error:
	fmte("RETURN argument ~S must be a (&optional value) form.", form, NULL);
}

static void defmacro_return(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RETURN, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_return);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator tagbody */
static void defspecial_tagbody(void)
{
	DefineSpecialOperator(COMMON_TAGBODY);
}


/* special-operator throw */
static void defspecial_throw(void)
{
	DefineSpecialOperator(COMMON_THROW);
}


/* special-operator unwind-protect */
static void defspecial_unwind_protect(void)
{
	DefineSpecialOperator(COMMON_UNWIND_PROTECT);
}


/* (defun not (object) ...) -> boolean */
static void function_not(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
}

static void defun_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eq (x y) ...) -> boolean */
static void function_eq(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eq(x, y));
}

static void defun_eq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQ, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_eq);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eql (x y) ...) -> boolean */
static void function_eql(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eql(x, y));
}

static void defun_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal (x y) ...) -> boolean */
static void function_equal(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, equal(x, y));
}

static void defun_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalp (x y) ...) -> boolean */
static void function_equalp(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, equalp(x, y));
}

static void defun_equalp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUALP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun identity (object) ...) -> object */
static void function_identity(Execute ptr, addr var)
{
	setresult_control(ptr, var);
}

static void type_identity(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_identity(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IDENTITY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_identity);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_identity(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complement (function) ...) -> function */
static void function_lambda_complement(Execute ptr, addr rest)
{
	addr pos;

	getdata_control(ptr, &pos);
	if (callclang_apply(ptr, &pos, pos, rest)) return;
	setbool_control(ptr, pos == Nil);
}

static void function_complement(Execute ptr, addr var)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_lambda_complement);
	SetDataFunction(pos, var);
	setresult_control(ptr, pos);
}

static void type_complement(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Function);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
}

static void defun_complement(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEMENT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_complement);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_complement(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun constantly (value) ...) -> function */
static void function_lambda_constantly(Execute ptr)
{
	addr pos;
	getdata_control(ptr, &pos);
	setresult_control(ptr, pos);
}

static void function_constantly(Execute ptr, addr var)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_any(pos, p_defun_lambda_constantly);
	SetDataFunction(pos, var);
	setresult_control(ptr, pos);
}

static void type_constantly(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Function);
	type_compiled_heap(arg, values, ret);
}

static void defun_constantly(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSTANTLY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_constantly);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_constantly(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun every (call &rest sequence+) ...) -> boolean */
static int result_every(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto result_true;
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil) goto result_true;
		if (consp(pos)) {
			getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			if (length_sequence(pos, 1) <= size) goto result_true;
			getelt_sequence(NULL, pos, size, &car);
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;
	if (pos == Nil) goto result_false;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto result_true;
			if (consp(cdr)) {
				getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				if (length_sequence(cdr, 1) <= size) goto result_true;
				getelt_sequence(NULL, cdr, size, &car);
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		if (pos == Nil) goto result_false;
	}

result_true:
	rollback_local(local, stack);
	*ret = T;
	return 0;

result_false:
	rollback_local(local, stack);
	*ret = Nil;
	return 0;
}

static void function_every(Execute ptr, addr call, addr rest)
{
	if (result_every(ptr, call, rest, &rest)) return;
	setresult_control(ptr, rest);
}

static void defun_every(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVERY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_every);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun some (call &rest sequence+) ...) -> result */
static int result_some(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto result_false;
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil) goto result_false;
		if (consp(pos)) {
			getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			if (length_sequence(pos, 1) <= size) goto result_false;
			getelt_sequence(NULL, pos, size, &car);
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return 1;
	if (pos != Nil) goto result;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto result_false;
			if (consp(cdr)) {
				getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				if (length_sequence(cdr, 1) <= size) goto result_false;
				getelt_sequence(NULL, cdr, size, &car);
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return 1;
		if (pos != Nil) goto result;
	}

result:
	rollback_local(local, stack);
	*ret = pos;
	return 0;

result_false:
	rollback_local(local, stack);
	*ret = Nil;
	return 0;
}

static void function_some(Execute ptr, addr call, addr rest)
{
	if (result_some(ptr, call, rest, &rest)) return;
	setresult_control(ptr, rest);
}

static void type_some(addr *ret)
{
	addr arg, values, call, sequence;

	GetTypeTable(&call, FunctionDesigner);
	GetTypeTable(&sequence, Sequence);
	typeargs_var2rest(&arg, call, sequence, sequence);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_some(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_some);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_some(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notany (call &rest sequence+) ...) -> boolean */
static void function_notany(Execute ptr, addr call, addr rest)
{
	/* (notany predicate sequence*) ==  (not (some predicate sequence*)) */
	if (result_some(ptr, call, rest, &rest)) return;
	setbool_control(ptr, (rest == Nil));
}

static void defun_notany(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTANY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notany);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notevery (call &rest sequence+) ...) -> boolean */
static void function_notevery(Execute ptr, addr call, addr rest)
{
	/* (notevery predicate sequence*) ==  (not (every predicate sequence*)) */
	if (result_every(ptr, call, rest, &rest)) return;
	setbool_control(ptr, (rest == Nil));
}

static void defun_notevery(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTEVERY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notevery);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro and (&rest form) ...) */
static void function_and(Execute ptr, addr form, addr env)
{
	addr expr, when, andv;

	getcdr(form, &form);

	/* (and) */
	if (form == Nil) {
		setresult_control(ptr, T);
		return;
	}

	/* (and expr) */
	if (singlep(form)) {
		GetCar(form, &form);
		setresult_control(ptr, form);
		return;
	}

	/* (and expr . tail) -> (when expr (and . tail)) */
	GetCons(form, &expr, &form);
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_AND, &andv);
	cons_heap(&andv, andv, form);
	list_heap(&form, when, expr, andv, NULL);
	setresult_control(ptr, form);
}

static void defmacro_and(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_AND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_and);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro cond (&rest clause) ...) */
static void function_cond(Execute ptr, addr form, addr env)
{
	addr expr, tail, ifsym, progn, cond;

	getcdr(form, &form);

	/* (cond) */
	if (form == Nil) {
		setresult_control(ptr, Nil);
		return;
	}

	/* (cond clause ...) */
	/* (cond (expr . tail) . form)
	 *   `(if ,expr (progn ,@tail) (cond ,@form))
	 */
	if (! consp(form))
		fmte("The cond form ~S must be a cons.", form, NULL);
	GetCons(form, &expr, &form);
	if (! consp(expr))
		fmte("The cond clause ~S must be a cons.", expr, NULL);
	GetCons(expr, &expr, &tail);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_COND, &cond);
	cons_heap(&form, cond, form);
	cons_heap(&tail, progn, tail);
	list_heap(&form, ifsym, expr, tail, form, NULL);
	setresult_control(ptr, form);
}

static void defmacro_cond(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_COND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_cond);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator if */
static void defspecial_if(void)
{
	DefineSpecialOperator(COMMON_IF);
}


/* (defmacro or (&rest form) ...) */
static void function_or(Execute ptr, addr form, addr env)
{
	addr gensym, let, ifsym, orv, expr;

	getcdr(form, &form);

	/* (or) */
	if (form == Nil) {
		setresult_control(ptr, Nil);
		return;
	}

	/* (or expr) */
	if (singlep(form)) {
		GetCar(form, &form);
		setresult_control(ptr, form);
		return;
	}

	/* (or expr . form) ->
	 *   (let ((#:g expr))
	 *     (if #:g #:g (or . form))) */
	if (! consp(form))
		fmte("The or form ~S must be a cons.", NULL);
	GetCons(form, &expr, &form);
	make_gensym(ptr, &gensym);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_OR, &orv);
	list_heap(&expr, gensym, expr, NULL);
	conscar_heap(&expr, expr);
	cons_heap(&form, orv, form);
	list_heap(&form, ifsym, gensym, gensym, form, NULL);
	list_heap(&form, let, expr, form, NULL);
	setresult_control(ptr, form);
}

static void defmacro_or(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_OR, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_or);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro when (expr &body body) ...) -> object */
static void function_when(Execute ptr, addr form, addr env)
{
	addr args, expr, ifsym, cons;

	getcdr(form, &args);
	if (! consp(args))
		fmte("The when ~S must be a (when test . body) form.", form, NULL);
	GetCons(args, &expr, &args);
	/* `(if ,expr (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_IF, &ifsym);
	list_heap(&args, ifsym, expr, cons, NULL);
	setresult_control(ptr, args);
}

static void defmacro_when(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WHEN, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_when);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro unless (expr &body body) ...) -> object */
static void function_unless(Execute ptr, addr form, addr env)
{
	addr args, notv, expr, ifsym, cons;

	getcdr(form, &args);
	if (! consp(args))
		fmte("The unless ~S must be a (unless test . body) form.", form, NULL);
	GetCons(args, &expr, &args);
	/* `(if (not ,expr) (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_NOT, &notv);
	list_heap(&expr, notv, expr, NULL);
	GetConst(COMMON_IF, &ifsym);
	list_heap(&args, ifsym, expr, cons, NULL);
	setresult_control(ptr, args);
}

static void defmacro_unless(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_UNLESS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_unless);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro case key &rest args) -> result */
static void function_case(Execute ptr, addr form, addr env)
{
	int lastp;
	addr key, args, list, test, body, g, root;
	addr let, cond, eql, member, quote, otherwise, declare, ignorable;

	/* (let ((g key))
	 *   (declare (ignorable g))
	 *   (cond ((eql g 'test1) . body1)
	 *         ((member g '(test2)) . body2)
	 *         (t . otherwise)))
	 */
	getcdr(form, &form);
	if (! consp(form))
		fmte("CASE argument must be (key &rest clauses) form.", form, NULL);
	GetCons(form, &key, &args);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	make_gensym(ptr, &g);

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			fmte("CASE clauses ~S must be list type.", args, NULL);
		if (lastp)
			fmte("CASE clauses ~S don't appear after otherwise clause.", args, NULL);
		GetCons(args, &test, &args);
		if (! consp(test))
			fmte("CASE clauses ~S must be list type.", test, NULL);
		GetCons(test, &test, &body);
		if (test == T || test == otherwise) {
			cons_heap(&list, T, body);
			lastp = 1;
		}
		else {
			list_heap(&list, quote, test, NULL);
			list_heap(&list, consp(test)? member: eql, g, list, NULL);
			cons_heap(&list, list, body);
		}
		cons_heap(&root, list, root);
	}
	/* otherwise */
	if (lastp == 0) {
		list_heap(&list, T, Nil, NULL);
		cons_heap(&root, list, root);
	}
	/* cond form */
	nreverse_list_unsafe(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&list, let, list, declare, root, NULL);
	setresult_control(ptr, list);
}

static void defmacro_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ecase (key &rest args) -> result */
static void function_ecase(Execute ptr, addr form, addr env)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, eql, member, quote, error, type, a;

	/* (let ((g key))
	 *   (cond ((eql g 'test1) . body1)
	 *         ((member g '(test2)) . body2)
	 *         (t . (type-error g (member ...)))))
	 */
	getcdr(form, &form);
	if (! consp(form))
		fmte("ECASE argument must be (key &rest clauses) form.", form, NULL);
	GetCons(form, &key, &args);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ECASE_ERROR, &error);
	make_gensym(ptr, &g);

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			fmte("ECASE clauses ~S must be list type.", args, NULL);
		GetCons(args, &test, &args);
		if (! consp(test))
			fmte("ECASE clauses ~S must be list type.", test, NULL);
		GetCons(test, &test, &body);
		list_heap(&list, quote, test, NULL);
		if (consp(test)) {
			list_heap(&list, member, g, list, NULL);
			while (test != Nil) {
				getcons(test, &a, &test);
				cons_heap(&type, a, type);
			}
		}
		else {
			list_heap(&list, eql, g, list, NULL);
			cons_heap(&type, test, type);
		}
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}
	/* error */
	nreverse_list_unsafe(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse_list_unsafe(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&list, let, list, root, NULL);
	setresult_control(ptr, list);
}

static void defmacro_ecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ECASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ccase (keyplace &rest args) -> result */
static void function_ccase(Execute ptr, addr form, addr env)
{
	fmte("TODO", NULL);
}

static void defmacro_ccase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CCASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ccase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro typecase (key &rest clauses) ...) -> result */
static void function_typecase(Execute ptr, addr form, addr env)
{
	int lastp;
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, otherwise, declare, ignorable;

	/* (let ((g key))
	 *   (declare (ignorable g))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . otherwise)))
	 */
	getcdr(form, &form);
	if (! consp(form))
		fmte("TYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetCons(form, &key, &args);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	make_gensym(ptr, &g);

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			fmte("TYPECASE clauses ~S must be list type.", args, NULL);
		if (lastp) {
			fmte("TYPECASE clauses ~S don't "
					"appear after otherwise clause.", args, NULL);
		}
		GetCons(args, &test, &args);
		if (! consp(test))
			fmte("TYPECASE clauses ~S must be list type.", test, NULL);
		GetCons(test, &test, &body);
		if (test == T || test == otherwise) {
			cons_heap(&list, T, body);
			lastp = 1;
		}
		else {
			list_heap(&list, quote, test, NULL);
			list_heap(&list, typep, g, list, NULL);
			cons_heap(&list, list, body);
		}
		cons_heap(&root, list, root);
	}
	/* otherwise */
	if (lastp == 0) {
		list_heap(&list, T, Nil, NULL);
		cons_heap(&root, list, root);
	}
	/* cond form */
	nreverse_list_unsafe(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&list, let, list, declare, root, NULL);
	setresult_control(ptr, list);
}

static void defmacro_typecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TYPECASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_typecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro etypecase (key &rest clauses) ...) -> result */
static void function_etypecase(Execute ptr, addr form, addr env)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, error, type;

	/* (let ((g key))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . (type-error g (or ...)))))
	 */
	getcdr(form, &form);
	if (! consp(form))
		fmte("ETYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetCons(form, &key, &args);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ETYPECASE_ERROR, &error);
	make_gensym(ptr, &g);

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			fmte("ETYPECASE clauses ~S must be list type.", args, NULL);
		GetCons(args, &test, &args);
		if (! consp(test))
			fmte("ETYPECASE clauses ~S must be list type.", test, NULL);
		GetCons(test, &test, &body);
		list_heap(&list, quote, test, NULL);
		list_heap(&list, typep, g, list, NULL);
		cons_heap(&type, test, type);
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}
	/* error */
	nreverse_list_unsafe(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse_list_unsafe(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&list, let, list, root, NULL);
	setresult_control(ptr, list);
}

static void defmacro_etypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ETYPECASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_etypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ctypecase (key &rest clauses) ...) -> result */
static void function_ctypecase(Execute ptr, addr form, addr env)
{
	fmte("TODO", NULL);
}

static void defmacro_ctypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CTYPECASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ctypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro multiple-value-bind (vars expr &body body) ...) -> t */
static void function_multiple_value_bind(Execute ptr, addr form, addr env)
{
	addr list, pos, vars, expr, doc, decl;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	getcons(form, &vars, &form);
	for (list = vars; list != Nil; ) {
		getcons(list, &pos, &list);
		check_variable(pos);
	}
	if (! consp(form)) goto error;
	getcons(form, &expr, &form);
	/* extract */
	if (declare_body_documentation(ptr, env, form, &doc, &decl, &form))
		return;
	GetConst(SYSTEM_MULTIPLE_VALUE_BIND, &pos);
	list_heap(&pos, pos, vars, expr, decl, doc, form, NULL);
	setresult_control(ptr, pos);
	return;

error:
	fmte("The multiple-value-bind argument must be a "
			"((vars*) expr &body body) form.", NULL);
}

static void defmacro_multiple_value_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator multiple-value-call */
static void defspecial_multiple_value_call(void)
{
	DefineSpecialOperator(COMMON_MULTIPLE_VALUE_CALL);
}


/* (defmacro multiple-value-list (form) ...) */
static void function_multiple_value_list(Execute ptr, addr form, addr env)
{
	addr args, expr, symbol, func, list;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &expr, &args);
	if (args != Nil) goto error;

	/* `(multiple-value-call #'list ,expr) */
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &symbol);
	GetConst(COMMON_FUNCTION, &func);
	GetConst(COMMON_LIST, &list);
	list_heap(&list, func, list, NULL);
	list_heap(&expr, symbol, list, expr, NULL);
	setresult_control(ptr, expr);
	return;

error:
	fmte("The multiple-value-list argument ~S must be a single list.", form, NULL);
}

static void defmacro_multiple_value_list(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_LIST, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_list);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator multiple-value-prog1 */
static void defspecial_multiple_value_prog1(void)
{
	DefineSpecialOperator(COMMON_MULTIPLE_VALUE_PROG1);
}


/* (defmacro multiple-value-setq (vars form) ...) */
static void function_multiple_value_setq(Execute ptr, addr form, addr env)
{
	addr args, vars, expr, values, setf;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &vars, &args);
	if (! consp(args)) goto error;
	GetCons(args, &expr, &args);
	if (args != Nil) goto error;

	/* `(values (setf (values ,@vars) ,expr)) */
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_SETF, &setf);
	cons_heap(&vars, values, vars);
	list_heap(&vars, setf, vars, expr, NULL);
	list_heap(&vars, values, vars, NULL);
	setresult_control(ptr, vars);
	return;

error:
	fmte("The multiple-value-setq arguments ~S must be a (vars form).", form, NULL);
}

static void defmacro_multiple_value_setq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_setq);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values (&rest object) ...) -> * */
static void function_values(Execute ptr, addr rest)
{
	setvalues_list_control(ptr, rest);
}

static void defun_values(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_values);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander values (&rest args &environment env) ...) */
static void define_setf_expander_values(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_VALUES, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_values);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values-list (list) ...) -> * */
static void function_values_list(Execute ptr, addr list)
{
	setvalues_list_control(ptr, list);
}

static void type_values_list(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	GetTypeTable(&values, Asterisk);
	typeargs_var1(&arg, arg);
	type_compiled_heap(arg, values, ret);
}

static void defun_values_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_values_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_values_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant multiple-value-limit FIXNUM_MAX) */
static void defconstant_multiple_values_limit(void)
{
	addr symbol, value;

	GetConst(COMMON_MULTIPLE_VALUES_LIMIT, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defmacro nth-value (index form) ...) -> object
 *   index  (integer 0 *)
 */
static void function_nth_value(Execute ptr, addr form, addr env)
{
	addr args, nth, expr, nth_value;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &nth, &args);
	if (! consp(args)) goto error;
	GetCons(args, &expr, &args);
	if (args != Nil) goto error;
	GetConst(SYSTEM_NTH_VALUE, &nth_value);
	list_heap(&args, nth_value, nth, expr, NULL);
	setresult_control(ptr, args);
	return;

error:
	fmte("NTH-VALUE argument ~S must be (nth expr) form.", form, NULL);
}

static void defmacro_nth_value(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_NTH_VALUE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_nth_value);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog ([var] declaration* tagbody*) ...) -> result */
static void function_prog_constant(Execute ptr, addr form,
		constindex prog_constant,
		constindex let_constant)
{
	/*  `(block nil
	 *    (let ,var
	 *      ,@decl
	 *      (tagbody ,@body)))
	 */
	addr var, decl, root, pos;
	addr let, block, tagbody;

	/* argument */
	getcdr(form, &form);
	if (! consp(form)) {
		GetConstant(prog_constant, &var);
		fmte("~A argument ~S must be ([var] &rest body) form.", var, form, NULL);
	}
	GetCons(form, &var, &form);
	declare_body_form(form, &decl, &form);

	/* expand */
	GetConstant(let_constant, &let);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	/* (tagbody ...) */
	cons_heap(&form, tagbody, form);
	/* (let ...) */
	conscar_heap(&root, let);
	cons_heap(&root, var, root);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&root, pos, root);
	}
	cons_heap(&root, form, root);
	nreverse_list_unsafe(&root, root);
	/* (block ...) */
	list_heap(&root, block, Nil, root, NULL);
	setresult_control(ptr, root);
}

static void function_prog(Execute ptr, addr form, addr env)
{
	function_prog_constant(ptr, form, CONSTANT_COMMON_PROG, CONSTANT_COMMON_LET);
}

static void defmacro_prog(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog* ([var] declaration* tagbody*) ...) -> result */
static void function_proga(Execute ptr, addr form, addr env)
{
	function_prog_constant(ptr, form, CONSTANT_COMMON_PROGA, CONSTANT_COMMON_LETA);
}

static void defmacro_proga(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROGA, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_proga);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog1 (form1 &body form) ...) -> form1 */
static void function_prog1(Execute ptr, addr form, addr env)
{
	addr expr, g, let, root;

	getcdr(form, &form);
	if (! consp(form))
		fmte("PROG1 arguemnt ~S must be (form1 &body body) form.", form, NULL);
	GetCons(form, &expr, &form);
	if (form == Nil) {
		setresult_control(ptr, expr);
		return;
	}
	/* `(let ((,g ,expr)) ,@form ,g) */
	make_gensym(ptr, &g);
	GetConst(COMMON_LET, &let);
	list_heap(&expr, g, expr, NULL);
	conscar_heap(&expr, expr);
	conscar_heap(&root, let);
	cons_heap(&root, expr, root);
	while (form != Nil) {
		if (! consp(form))
			fmte("PROG1 argument ~S don't accept a dotted list.", form, NULL);
		GetCons(form, &expr, &form);
		cons_heap(&root, expr, root);
	}
	cons_heap(&root, g, root);
	nreverse_list_unsafe(&root, root);
	setresult_control(ptr, root);
}

static void defmacro_prog1(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG1, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog1);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog2 (form1 form2 &body form) ...) -> form2 */
static void function_prog2(Execute ptr, addr form, addr env)
{
	addr expr, progn, prog1;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &expr, &form);
	if (! consp(form)) goto error;
	/* `(progn ,expr (prog1 ,@form)) */
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_PROG1, &prog1);
	cons_heap(&prog1, prog1, form);
	list_heap(&progn, progn, expr, prog1, NULL);
	setresult_control(ptr, progn);
	return;

error:
	fmte("PROG2 arguemnt ~S must be (form1 form2 &body body) form.", form, NULL);
}

static void defmacro_prog2(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG2, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog2);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* special-operator progn */
static void defspecial_progn(void)
{
	DefineSpecialOperator(COMMON_PROGN);
}


/* (defmacro define-modify-macro (name args call &optional doc) ...) -> name */
static int find_lambda_ampersand(addr key, addr lambda, addr *ret)
{
	addr args, check;

	for (args = lambda; args != Nil; ) {
		if (! consp(args)) goto error;
		GetCons(args, &check, &args);
		if (check == key) {
			if (! consp(args)) goto error;
			GetCar(args, ret);
			return 1;
		}
	}
	return 0;

error:
	fmte("Invaild macro-lambda-list ~S.", lambda, NULL);
	return 0;
}

static void expand_define_modify_macro(LocalRoot local, addr *ret,
		addr name, addr args, addr call, addr doc)
{
	addr place, env, whole, key;
	addr defmacro, mvbind, expansion, quote, let, mapcar, function;
	addr list, lista, cddr, cons, values, declare, ignorable;
	addr a, b, g, w, r, qmvbind, list1, list2, list3;

	/* place */
	make_symbolchar(&place, "PLACE");
	cons_heap(&args, place, args);
	/* &environment */
	GetConst(AMPERSAND_ENVIRONMENT, &key);
	if (! find_lambda_ampersand(key, args, &env)) {
		make_symbolchar(&env, "ENV");
		lista_heap(&args, key, env, args, NULL);
	}
	/* &whole */
	GetConst(AMPERSAND_WHOLE, &key);
	if (! find_lambda_ampersand(key, args, &whole)) {
		make_symbolchar(&whole, "WHOLE");
		lista_heap(&args, key, whole, args, NULL);
	}

	/* expand
	 *
	 *  (defmacro name (&whole whole &environment env var &rest args)
	 *    (declare (ignorable ...))
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion var env)
	 *      `(let ,(mapcar #'list a b)
	 *        (multiple-value-bind ,g (call ,r ,@(cddr whole))
	 *          ,w
	 *          (values ,@g)))))
	 *
	 *  (defmacro name (&whole whole &environment env var &rest args)
	 *    (declare (ignorable ...))
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion var env)
	 *      (list (quote let) (mapcar (function list) a b)
	 *        (list (quote multiple-value-bind) g
	 *          (list* (quote call) r (cddr whole)) w
	 *          (cons (quote values) g)))))
	 */
	GetConst(COMMON_DEFMACRO, &defmacro);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_GET_SETF_EXPANSION, &expansion);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_FUNCTION, &function);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LISTA, &lista);
	GetConst(COMMON_CDDR, &cddr);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	make_symbolchar(&a, "A");
	make_symbolchar(&b, "B");
	make_symbolchar(&g, "G");
	make_symbolchar(&w, "W");
	make_symbolchar(&r, "R");
	allsymbol_macro_lambda_heap(local, &list1, args);
	cons_heap(&ignorable, ignorable, list1);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&values, quote, values, NULL);
	list_heap(&cons, cons, values, g, NULL);
	list_heap(&cddr, cddr, whole, NULL);
	list_heap(&call, quote, call, NULL);
	list_heap(&lista, lista, call, r, cddr, NULL);
	list_heap(&qmvbind, quote, mvbind, NULL);
	list_heap(&list2, list, qmvbind, g, lista, w, cons, NULL);
	list_heap(&function, function, list, NULL);
	list_heap(&mapcar, mapcar, function, a, b, NULL);
	list_heap(&let, quote, let, NULL);
	list_heap(&list1, list, let, mapcar, list2, NULL);
	list_heap(&expansion, expansion, place, env, NULL);
	list_heap(&list3, a, b, g, w, r, NULL);
	list_heap(&mvbind, mvbind, list3, expansion, list1, NULL);
	list_heap(ret, defmacro, name, args, declare, mvbind, NULL);
}

static void function_define_modify_macro(Execute ptr, addr form, addr env)
{
	addr args, name, lambda, call, doc;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &name, &args);
	if (! consp(args)) goto error;
	GetCons(args, &lambda, &args);
	if (! consp(args)) goto error;
	GetCons(args, &call, &args);
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	GetCons(args, &doc, &args);
	if (args != Nil) goto error;
	if (! stringp(doc)) {
		fmte("DEFINE-MODIFY-MACRO documentation ~S "
				"must be a string type.", doc, NULL);
	}
expand:
	expand_define_modify_macro(ptr->local, &form, name, lambda, call, doc);
	setresult_control(ptr, form);
	return;

error:
	fmte("DEFINE-MODIFY-MACRO argument ~S must be "
			"(name lambda-list functionn &optional documentation) "
			"form.", form, NULL);
}

static void defmacro_define_modify_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_MODIFY_MACRO, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_modify_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defsetf (access &rest args) -> access */
static void function_defsetf_short(Execute ptr,
		addr access, addr update, addr doc)
{
	/* `(define-setf-expander ,access (&environment env &rest args)
	 *   (system::defsetf-short ',access ',update, args env))
	 */
	addr define, defsetf, rest, args, envi, env, quote, value;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(SYSTEM_DEFSETF_SHORT, &defsetf);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(AMPERSAND_ENVIRONMENT, &envi);
	GetConst(COMMON_QUOTE, &quote);
	make_symbolchar(&args, "ARGS");
	make_symbolchar(&env, "ENV");

	list_heap(&update, quote, update, NULL);
	list_heap(&value, quote, access, NULL);
	list_heap(&defsetf, defsetf, value, update, args, env, NULL);
	list_heap(&rest, envi, env, rest, args, NULL);
	if (doc == Nil)
		list_heap(&define, define, access, rest, defsetf, NULL);
	else
		list_heap(&define, define, access, rest, doc, defsetf, NULL);
	setresult_control(ptr, define);
}

static void function_defsetf_long(Execute ptr,
		addr access, addr lambda, addr store, addr body)
{
	/* `(define-setf-expander ,access (&environment env &rest args)
	 *   (system::defsetf-long ,access ,lambda ,store ,body args env))
	 */
	addr define, defsetf, rest, args, envi, env, quote, value;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(SYSTEM_DEFSETF_LONG, &defsetf);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(AMPERSAND_ENVIRONMENT, &envi);
	GetConst(COMMON_QUOTE, &quote);
	make_symbolchar(&args, "ARGS");
	make_symbolchar(&env, "ENV");

	list_heap(&lambda, quote, lambda, NULL);
	list_heap(&store, quote, store, NULL);
	list_heap(&body, quote, body, NULL);
	list_heap(&value, quote, access, NULL);
	list_heap(&defsetf, defsetf, value, lambda, store, body, args, env, NULL);
	list_heap(&rest, envi, env, rest, args, NULL);
	list_heap(&define, define, access, rest, defsetf, NULL);
	setresult_control(ptr, define);
}

static void function_defsetf(Execute ptr, addr form, addr env)
{
	addr args, arg1, arg2, arg3;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &arg1, &args);
	if (! consp(args)) goto error;
	GetCons(args, &arg2, &args);
	if (listp(arg2)) {
		/* long form */
		if (! consp(args))
			fmte("Invalid defsetf long form ~S.", form, NULL);
		GetCons(args, &arg3, &args);
		if (! listp(arg3))
			fmte("defsetf argument ~S must be a list type.", arg3, NULL);
		function_defsetf_long(ptr, arg1, arg2, arg3, args);
	}
	else if (args == Nil) {
		/* short form */
		function_defsetf_short(ptr, arg1, arg2, Nil);
	}
	else {
		/* short form, documentation */
		if (! consp(args))
			fmte("Invalid defsetf short form ~S.", form, NULL);
		GetCons(args, &arg3, &args);
		if (args != Nil)
			fmte("Invalid defsetf short form ~S.", form, NULL);
		if (! stringp(arg3))
			fmte("defsetf documentation ~S must be a string type.", arg3, NULL);
		function_defsetf_short(ptr, arg1, arg2, arg3);
	}
	return;

error:
	fmte("Invalid defsetf form ~S.", form, NULL);
}

static void defmacro_defsetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSETF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defsetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-setf-expander (access &rest args) ...) -> access */
static void function_define_setf_expander(Execute ptr, addr form, addr env)
{
	/* `(system::define-setf-expander
	 *     ',access
	 *     (system::macro-lambda ,@args))
	 */
	addr access, args, define, lambda, quote;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &access, &args);
	if (! consp(args)) goto error;
	/* expand */
	GetConst(SYSTEM_DEFINE_SETF_EXPANDER, &define);
	GetConst(SYSTEM_MACRO_LAMBDA, &lambda);
	GetConst(COMMON_QUOTE, &quote);
	cons_heap(&lambda, lambda, args);
	list_heap(&access, quote, access, NULL);
	list_heap(&define, define, access, lambda, NULL);
	setresult_control(ptr, define);
	return;

error:
	fmte("DEFINE-SETF-EXPANDER argument ~S "
			"must be (access lambda-list &rest body) form.", form, NULL);
}

static void defmacro_define_setf_expander(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_setf_expander);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun get-setf-expansion (place &optional env) ...) -> (list list list t t) */
static void function_get_setf_expansion(Execute ptr, addr place, addr env)
{
	addr a, b, g, w, r;

	if (env == Unbound)
		env = Nil;
	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	setvalues_control(ptr, a, b, g, w, r, NULL);
}

static void type_get_setf_expansion(addr *ret)
{
	addr arg, values, type, list, env;

	GetTypeTable(&type, T);
	GetTypeTable(&env, Environment);
	GetTypeTable(&list, T);
	typeargs_var1opt1(&arg, type, env);
	typevalues_values5(&values, list, list, list, type, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_get_setf_expansion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_SETF_EXPANSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_get_setf_expansion);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_setf_expansion(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro setf (&rest pair) ...) -> t */
static void setf_single(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2)
	 *        (g value))
	 *   (declare (ignorable g1 g2))
	 *     writer
	 *     g)
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &a, &list1);
		getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	GetCar(store, &store);
	list_heap(&a, store, value, NULL);
	cons_heap(&args, a, args);
	nreverse_list_unsafe(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, writer, store, NULL);
}

static void setf_multiple(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2))
	 *   (declare (ignorable g1 g2))
	 *   (multiple-value-bind (g ...) value
	 *     writer
	 *     (values g1 ...)))
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable, bind, values;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &a, &list1);
		getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	nreverse_list_unsafe(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* values */
	GetConst(COMMON_VALUES, &values);
	cons_heap(&values, values, store);
	/* multiple-value-bind */
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &bind);
	list_heap(&bind, bind, store, value, writer, values, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, bind, NULL);
}

static void setf_setform(Execute ptr, addr *ret, addr key, addr value, addr env)
{
	addr var1, var2, store, reader, writer;

	if (get_setf_expansion(ptr, key, env, &var1, &var2, &store, &writer, &reader))
		return;
	if (singlep(store))
		setf_single(ret, value, var1, var2, store, writer, reader);
	else
		setf_multiple(ret, value, var1, var2, store, writer, reader);
}

static void function_setf(Execute ptr, addr form, addr env)
{
	addr key, value, root, progn;

	getcdr(form, &form);
	if (form == Nil) {
		setresult_control(ptr, Nil);
		return;
	}
	for (root = Nil; form != Nil; ) {
		if (! consp(form)) goto error;
		GetCons(form, &key, &form);
		if (! consp(form)) goto error;
		GetCons(form, &value, &form);
		setf_setform(ptr, &key, key, value, env);
		cons_heap(&root, key, root);
	}
	nreverse_list_unsafe(&root, root);
	/* (progn ...) */
	GetConst(COMMON_PROGN, &progn);
	cons_heap(&root, progn, root);
	setresult_control(ptr, root);
	return;

error:
	fmte("The setf form ~S must be a place value form.", form, NULL);
}

static void defmacro_setf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SETF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro psetf (&rest args) ...) -> value */
static void function_psetf(Execute ptr, addr form, addr env)
{
	constant_psetq(ptr, form, env, CONSTANT_COMMON_SETF, CONSTANT_COMMON_PSETF);
}

static void defmacro_psetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_psetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro shiftf (place+ newvalue) ...) -> oldvalue */
static void push_mapcar_list2(addr *ret, addr a, addr b, addr root)
{
	addr x, y;

	while (a != Nil) {
		getcons(a, &x, &a);
		getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	*ret = root;
}

static void declare_ignorable(addr *ret, addr list)
{
	addr declare, ignorable;

	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(ret, declare, ignorable, NULL);
}

static void mvbind_declare_ignorable(addr *ret, addr g, addr r, addr body)
{
	addr mvbind, declare;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	declare_ignorable(&declare, g);
	lista_heap(ret, mvbind, g, r, declare, body, NULL);
}

static void function_shiftf(Execute ptr, addr form, addr env)
{
	addr args, root, pos, let, declare, prog1;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;

	/* (shiftf x0 x1 x2 value)
	 * (let* (...)
	 *   (multiple-value-prog1 r0
	 *     (multiple-value-bind (g0) r1
	 *     (declare (ignorable g0))
	 *       (multiple-value-bind (g1) r2
	 *       (declare (ignorable g1))
	 *         (multiple-value-bind (g2) value
	 *         (declare (ignorable g2))
	 *           w0 w1 w2)))))
	 */
	getcdr(form, &form);
	if (! consp(form)) goto error;
	getcons(form, &pos, &args);
	if (! consp(args)) goto error;

	/* push */
	if (get_setf_expansion(ptr, pos, env, &a, &b, &g, &w, &r0))
		return;
	alist = glist = wlist = rlist = Nil;
	push_mapcar_list2(&alist, a, b, alist);
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	for (;;) {
		getcons(args, &pos, &args);
		if (args == Nil) {
			break;
		}
		if (get_setf_expansion(ptr, pos, env, &a, &b, &g, &w, &r))
			return;
		push_mapcar_list2(&alist, a, b, alist);
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
	}

	/* last expand */
	nreverse_list_unsafe(&wlist, wlist);
	GetCons(glist, &g, &glist);
	mvbind_declare_ignorable(&root, g, pos, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		mvbind_declare_ignorable(&root, g, r, root);
	}

	/* multiple-value-prog1 */
	GetConst(COMMON_MULTIPLE_VALUE_PROG1, &prog1);
	list_heap(&root, prog1, r0, root, NULL);

	/* let expand */
	if (alist != Nil) {
		nreverse_list_unsafe(&alist, alist);
		GetConst(COMMON_LETA, &let);
		declare_ignorable(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	setresult_control(ptr, root);
	return;

error:
	fmte("SHIFT argument ~S must be (place ... value) form.", form, NULL);
}

static void defmacro_shiftf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SHIFTF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_shiftf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro rotatef (&rest place) ...) -> nil) */
static void function_rotatef(Execute ptr, addr form, addr env)
{
	addr args, root, pos, let, declare;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;

	getcdr(form, &form);
	if (form == Nil) {
		setresult_control(ptr, Nil);
		return;
	}
	if (! consp(form)) goto error;
	getcons(form, &pos, &args);
	if (args == Nil) {
		/* (progn pos nil) */
		GetConst(COMMON_PROGN, &root);
		list_heap(&root, root, pos, Nil, NULL);
		setresult_control(ptr, root);
		return;
	}
	if (! consp(args)) goto error;

	/* push */
	if (get_setf_expansion(ptr, pos, env, &a, &b, &g, &w, &r0))
		return;
	alist = glist = wlist = rlist = Nil;
	push_mapcar_list2(&alist, a, b, alist);
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	while (args != Nil) {
		getcons(args, &pos, &args);
		if (get_setf_expansion(ptr, pos, env, &a, &b, &g, &w, &r))
			return;
		push_mapcar_list2(&alist, a, b, alist);
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
	}

	/* last expand */
	cons_heap(&wlist, Nil, wlist);
	nreverse_list_unsafe(&wlist, wlist);
	GetCons(glist, &g, &glist);
	mvbind_declare_ignorable(&root, g, r0, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		mvbind_declare_ignorable(&root, g, r, root);
	}

	/* let expand */
	if (alist != Nil) {
		nreverse_list_unsafe(&alist, alist);
		GetConst(COMMON_LETA, &let);
		declare_ignorable(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	setresult_control(ptr, root);
	return;

error:
	fmte("ROTATEF argument ~S don't accept a dotted list.", form, NULL);
}

static void defmacro_rotatef(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ROTATEF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_rotatef);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
_g void init_common_data(void)
{
	SetPointerCall(defun,     var2dynamic,  apply);
	SetPointerCall(defmacro,  macro,        defun);
	SetPointerCall(defun,     var1,         fdefinition);
	SetPointerCall(defun,     var2,         setf_fdefinition);
	SetPointerCall(defun,     var1,         fboundp);
	SetPointerCall(defun,     var1,         fmakunbound);
	SetPointerCall(defun,     var1dynamic,  funcall);
	SetPointerCall(defun,     var1,         function_lambda_expression);
	SetPointerCall(defun,     var1,         functionp);
	SetPointerCall(defun,     var1,         compiled_function_p);
	SetPointerCall(defmacro,  macro,        defconstant);
	SetPointerCall(defmacro,  macro,        defparameter);
	SetPointerCall(defmacro,  macro,        defvar);
	SetPointerCall(defmacro,  macro,        destructuring_bind);
	SetPointerCall(defmacro,  macro,        psetq);
	SetPointerCall(defmacro,  macro,        return);
	SetPointerCall(defun,     var1,         not);
	SetPointerCall(defun,     var2,         eq);
	SetPointerCall(defun,     var2,         eql);
	SetPointerCall(defun,     var2,         equal);
	SetPointerCall(defun,     var2,         equalp);
	SetPointerCall(defun,     var1,         identity);
	SetPointerCall(defun,     dynamic,      lambda_complement);
	SetPointerCall(defun,     var1,         complement);
	SetPointerCall(defun,     any,          lambda_constantly);
	SetPointerCall(defun,     var1,         constantly);
	SetPointerCall(defun,     var1rest,     every);
	SetPointerCall(defun,     var1rest,     some);
	SetPointerCall(defun,     var1rest,     notany);
	SetPointerCall(defun,     var1rest,     notevery);
	SetPointerCall(defmacro,  macro,        and);
	SetPointerCall(defmacro,  macro,        cond);
	SetPointerCall(defmacro,  macro,        or);
	SetPointerCall(defmacro,  macro,        when);
	SetPointerCall(defmacro,  macro,        unless);
	SetPointerCall(defmacro,  macro,        case);
	SetPointerCall(defmacro,  macro,        ecase);
	SetPointerCall(defmacro,  macro,        ccase);
	SetPointerCall(defmacro,  macro,        typecase);
	SetPointerCall(defmacro,  macro,        etypecase);
	SetPointerCall(defmacro,  macro,        ctypecase);
	SetPointerCall(defmacro,  macro,        multiple_value_bind);
	SetPointerCall(defmacro,  macro,        multiple_value_list);
	SetPointerCall(defmacro,  macro,        multiple_value_setq);
	SetPointerCall(defun,     dynamic,      values);
	SetPointerCall(defmacro,  macro,        setf_values);
	SetPointerCall(defun,     var1,         values_list);
	SetPointerCall(defmacro,  macro,        nth_value);
	SetPointerCall(defmacro,  macro,        prog);
	SetPointerCall(defmacro,  macro,        proga);
	SetPointerCall(defmacro,  macro,        prog1);
	SetPointerCall(defmacro,  macro,        prog2);
	SetPointerCall(defmacro,  macro,        define_modify_macro);
	SetPointerCall(defmacro,  macro,        defsetf);
	SetPointerCall(defmacro,  macro,        define_setf_expander);
	SetPointerCall(defun,     var1opt1,     get_setf_expansion);
	SetPointerCall(defmacro,  macro,        setf);
	SetPointerCall(defmacro,  macro,        psetf);
	SetPointerCall(defmacro,  macro,        shiftf);
	SetPointerCall(defmacro,  macro,        rotatef);
}

_g void build_common_data(void)
{
	defun_apply();
	defmacro_defun();
	defun_fdefinition();
	defun_setf_fdefinition();
	defun_fboundp();
	defun_fmakunbound();
	defspecial_flet();
	defspecial_labels();
	defspecial_macrolet();
	defun_funcall();
	defspecial_function();
	defun_function_lambda_expression();
	defun_functionp();
	defun_compiled_function_p();
	defconstant_call_arguments_limit();
	defconstant_lambda_list_keywords();
	defconstant_lambda_parameters_limit();
	defmacro_defconstant();
	defmacro_defparameter();
	defmacro_defvar();
	defmacro_destructuring_bind();
	defspecial_let();
	defspecial_leta();
	defspecial_progv();
	defspecial_setq();
	defmacro_psetq();
	defspecial_block();
	defspecial_catch();
	defspecial_go();
	defspecial_return_from();
	defmacro_return();
	defspecial_tagbody();
	defspecial_throw();
	defspecial_unwind_protect();
	defun_not();
	defun_eq();
	defun_eql();
	defun_equal();
	defun_equalp();
	defun_identity();
	defun_complement();
	defun_constantly();
	defun_every();
	defun_some();
	defun_notany();
	defun_notevery();
	defmacro_and();
	defmacro_cond();
	defspecial_if();
	defmacro_or();
	defmacro_when();
	defmacro_unless();
	defmacro_case();
	defmacro_ecase();
	defmacro_ccase();
	defmacro_typecase();
	defmacro_etypecase();
	defmacro_ctypecase();
	defmacro_multiple_value_bind();
	defspecial_multiple_value_call();
	defmacro_multiple_value_list();
	defspecial_multiple_value_prog1();
	defmacro_multiple_value_setq();
	defun_values();
	define_setf_expander_values();
	defun_values_list();
	defconstant_multiple_values_limit();
	defmacro_nth_value();
	defmacro_prog();
	defmacro_proga();
	defmacro_prog1();
	defmacro_prog2();
	defspecial_progn();
	defmacro_define_modify_macro();
	defmacro_defsetf();
	defmacro_define_setf_expander();
	defun_get_setf_expansion();
	defmacro_setf();
	defmacro_psetf();
	defmacro_shiftf();
	defmacro_rotatef();
}

