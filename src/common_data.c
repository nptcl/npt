/*
 *  ANSI COMMON LISP: 5. Data and Control Flow
 */
#include "call_data.h"
#include "clos_class.h"
#include "common_header.h"
#include "control_execute.h"
#include "control_object.h"
#include "equal.h"
#include "function.h"
#include "setf.h"

/* (defun apply (call pos &rest args) ...) -> value
 *   call   (or function symbol)  ;; function-designer
 *   pos    t
 *   args   t
 *   value  (values &rest t)
 */
static int check_data_function(addr call, addr *ret)
{
	int check;

	if (GetType(call) == LISPTYPE_SYMBOL) {
		Return(getfunction_global_(call, &call));
		if (macro_function_p(call))
			return fmte_("Cannot call the macro-function ~S.", call, NULL);
	}
	Return(funcallp_(call, &check));
	if (! check)
		return fmte_("The argument ~S is not executable.", call, NULL);

	return Result(ret, call);
}

static int function_apply(Execute ptr, addr call, addr pos, addr args)
{
	Return(check_data_function(call, &call));
	return apply_common_(ptr, call, pos, args);
}

static void type_apply(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_apply(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPLY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_apply);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apply(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander apply (place indicator &optional default) ...)
 *   place      t
 *   default    t   ;; default nil
 *   value      t
 */
static void define_setf_expander_apply(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_APPLY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_apply);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defun (name lambda-list &rest body) ...) */
static int function_defun(Execute ptr, addr right, addr env)
{
	Return(defun_common(ptr, right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_defun(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFUN, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defun);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun fdefinition (name) ...) -> function
 *   name  (or symbol (cons (eql setf) (cons symbol null))) -> function-name
 */
static int function_fdefinition(Execute ptr, addr name)
{
	Return(fdefinition_common(ptr, name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_fdefinition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_system(&pos, symbol);
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
static int function_setf_fdefinition(Execute ptr, addr value, addr name)
{
	Return(setf_fdefinition_common(value, name));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_fdefinition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Function);
	GetTypeTable(&values, FunctionName);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_fdefinition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FDEFINITION, &symbol);
	compiled_setf_system(&pos, symbol);
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
static int function_fboundp(Execute ptr, addr name)
{
	int check;

	Return(fboundp_common_(name, &check));
	setbool_control(ptr, check);

	return 0;
}

static void type_fboundp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_fboundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FBOUNDP, &symbol);
	compiled_system(&pos, symbol);
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
static int function_fmakunbound(Execute ptr, addr name)
{
	Return(fmakunbound_common(name));
	setresult_control(ptr, name);
	return 0;
}

static void type_fmakunbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeTable(&values, FunctionName);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_fmakunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FMAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
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
static int function_funcall(Execute ptr, addr call, addr args)
{
	Return(check_data_function(call, &call));
	return funcall_common_(ptr, call, args);
}

static void type_funcall(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, FunctionDesigner);
	GetTypeTable(&type, T);
	typeargs_var1rest(&args, args, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defvar_macroexpand_hook(addr value)
{
	addr symbol;
	GetConst(SPECIAL_MACROEXPAND_HOOK, &symbol);
	SetValueSymbol(symbol, value);
}

static void defun_funcall(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCALL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_funcall);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_funcall(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	/* (defvar *macroexpand-hook* #'funcall) */
	defvar_macroexpand_hook(pos);
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
static int function_function_lambda_expression(Execute ptr, addr var)
{
	addr pos1, pos2, pos3;

	function_lambda_expression_common(var, &pos1, &pos2, &pos3);
	setvalues_control(ptr, pos1, pos2, pos3, NULL);
	return 0;
}

static void type_function_lambda_expression(addr *ret)
{
	addr args, values, type1, type2, type3, null;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeTable(&type1, T);
	GetTypeTable(&type2, Boolean);
	GetTypeTable(&type3, FunctionName);
	GetTypeTable(&null, Null);
	type2or_heap(type3, null, &type3);
	typevalues_values3(&values, type1, type2, type3);
	type_compiled_heap(args, values, ret);
}

static void defun_function_lambda_expression(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTION_LAMBDA_EXPRESSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_function_lambda_expression);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_function_lambda_expression(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun functionp (object) ...) -> boolean */
static int function_functionp(Execute ptr, addr var)
{
	int check;

	Return(funcallp_(var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_functionp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FUNCTIONP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_functionp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun compiled-function-p (object) ...) -> boolean */
static int function_compiled_function_p(Execute ptr, addr var)
{
	setbool_control(ptr, compiled_function_p(var));
	return 0;
}

static void defun_compiled_function_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILED_FUNCTION_P, &symbol);
	compiled_system(&pos, symbol);
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
	addr list, pos;

	lambda_list_keywords_common(&list);
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
static int function_defconstant(Execute ptr, addr form, addr env)
{
	Return(defconstant_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defconstant(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFCONSTANT, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_defparameter(Execute ptr, addr form, addr env)
{
	Return(defparameter_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defparameter(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPARAMETER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defparameter);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defvar (symbol &optional value document) ...) -> symbol */
static int function_defvar(Execute ptr, addr form, addr env)
{
	Return(defvar_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defvar(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFVAR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defvar);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro destructuring-bind (lambda expr &body body) ...) -> t */
static int function_destructuring_bind(Execute ptr, addr form, addr env)
{
	Return(destructuring_bind_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_destructuring_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DESTRUCTURING_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_psetq(Execute ptr, addr form, addr env)
{
	Return(psetq_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_psetq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETQ, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_return(Execute ptr, addr form, addr env)
{
	Return(return_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_return(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RETURN, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_not(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
}

static void defun_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eq (x y) ...) -> boolean */
static int function_eq(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eq_function(x, y));
	return 0;
}

static void defun_eq(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQ, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_eq);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eql (x y) ...) -> boolean */
static int function_eql(Execute ptr, addr x, addr y)
{
	setbool_control(ptr, eql_function(x, y));
	return 0;
}

static void defun_eql(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_eql);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal (x y) ...) -> boolean */
static int function_equal(Execute ptr, addr x, addr y)
{
	int check;

	Return(equal_function_(x, y, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalp (x y) ...) -> boolean */
static int function_equalp(Execute ptr, addr x, addr y)
{
	int check;

	Return(equalp_function_(x, y, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equalp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EQUALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun identity (object) ...) -> object */
static int function_identity(Execute ptr, addr var)
{
	setresult_control(ptr, var);
	return 0;
}

static void type_identity(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_identity(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IDENTITY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_identity);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_identity(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complement (function) ...) -> function */
static int function_lambda_complement(Execute ptr, addr rest)
{
	addr pos;

	getdata_control(ptr, &pos);
	Return(callclang_apply(ptr, &pos, pos, rest));
	setbool_control(ptr, pos == Nil);

	return 0;
}

static int function_complement(Execute ptr, addr var)
{
	complement_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_complement(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_complement(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEMENT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_complement);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_complement(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun constantly (value) ...) -> function */
static int function_lambda_constantly(Execute ptr)
{
	addr pos;

	getdata_control(ptr, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static int function_constantly(Execute ptr, addr var)
{
	constantly_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_constantly(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_constantly(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSTANTLY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_constantly);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_constantly(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun every (call &rest sequence+) ...) -> boolean */
static int function_every(Execute ptr, addr call, addr rest)
{
	Return(every_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_every(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVERY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_every);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun some (call &rest sequence+) ...) -> result */
static int function_some(Execute ptr, addr call, addr rest)
{
	Return(some_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_some(addr *ret)
{
	addr args, values, call, sequence;

	GetTypeTable(&call, FunctionDesigner);
	GetTypeTable(&sequence, Sequence);
	typeargs_var2rest(&args, call, sequence, sequence);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_some(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_some);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_some(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notany (call &rest sequence+) ...) -> boolean */
static int function_notany(Execute ptr, addr call, addr rest)
{
	/* (notany predicate sequence*) ==  (not (some predicate sequence*)) */
	Return(notany_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_notany(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTANY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notany);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun notevery (call &rest sequence+) ...) -> boolean */
static int function_notevery(Execute ptr, addr call, addr rest)
{
	Return(notevery_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_notevery(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NOTEVERY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_notevery);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Every);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro and (&rest form) ...) */
static int function_and(Execute ptr, addr form, addr env)
{
	Return(and_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_and(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_AND, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_and);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro cond (&rest clause) ...) */
static int function_cond(Execute ptr, addr form, addr env)
{
	Return(cond_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_cond(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_COND, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_or(Execute ptr, addr form, addr env)
{
	Return(or_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_or(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_OR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_or);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro when (expr &body body) ...) -> object */
static int function_when(Execute ptr, addr form, addr env)
{
	Return(when_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_when(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WHEN, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_when);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro unless (expr &body body) ...) -> object */
static int function_unless(Execute ptr, addr form, addr env)
{
	Return(unless_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_unless(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_UNLESS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_unless);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro case key &rest args) -> result */
static int function_case(Execute ptr, addr form, addr env)
{
	Return(case_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ecase (key &rest args) -> result */
static int function_ecase(Execute ptr, addr form, addr env)
{
	Return(ecase_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ccase (keyplace &rest args) -> result */
static int function_ccase(Execute ptr, addr form, addr env)
{
	Return(ccase_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ccase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CCASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ccase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro typecase (key &rest clauses) ...) -> result */
static int function_typecase(Execute ptr, addr form, addr env)
{
	Return(typecase_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_typecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_typecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro etypecase (key &rest clauses) ...) -> result */
static int function_etypecase(Execute ptr, addr form, addr env)
{
	Return(etypecase_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_etypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ETYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_etypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ctypecase (key &rest clauses) ...) -> result */
static int function_ctypecase(Execute ptr, addr form, addr env)
{
	Return(ctypecase_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ctypecase(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CTYPECASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ctypecase);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro multiple-value-bind (vars expr &body body) ...) -> t */
static int function_multiple_value_bind(Execute ptr, addr form, addr env)
{
	Return(multiple_value_bind_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_multiple_value_list(Execute ptr, addr form, addr env)
{
	Return(multiple_value_list_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_list(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_LIST, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_multiple_value_setq(Execute ptr, addr form, addr env)
{
	Return(multiple_value_setq_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_multiple_value_setq(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_multiple_value_setq);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values (&rest object) ...) -> * */
static int function_values(Execute ptr, addr rest)
{
	setvalues_list_control(ptr, rest);
	return 0;
}

static void defun_values(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES, &symbol);
	compiled_system(&pos, symbol);
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
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_values);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun values-list (list) ...) -> * */
static int function_values_list(Execute ptr, addr list)
{
	setvalues_list_control(ptr, list);
	return 0;
}

static void type_values_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, Asterisk);
	typeargs_var1(&args, args);
	type_compiled_heap(args, values, ret);
}

static void defun_values_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_VALUES_LIST, &symbol);
	compiled_system(&pos, symbol);
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
static int function_nth_value(Execute ptr, addr form, addr env)
{
	Return(nth_value_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_nth_value(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_NTH_VALUE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_nth_value);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog ([var] declaration* tagbody*) ...) -> result */
static int function_prog(Execute ptr, addr form, addr env)
{
	Return(prog_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog* ([var] declaration* tagbody*) ...) -> result */
static int function_proga(Execute ptr, addr form, addr env)
{
	Return(proga_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_proga(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROGA, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_proga);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog1 (form1 &body form) ...) -> form1 */
static int function_prog1(Execute ptr, addr form, addr env)
{
	Return(prog1_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog1(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG1, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_prog1);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro prog2 (form1 form2 &body form) ...) -> form2 */
static int function_prog2(Execute ptr, addr form, addr env)
{
	Return(prog2_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_prog2(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PROG2, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_define_modify_macro(Execute ptr, addr form, addr env)
{
	Return(define_modify_macro_common(ptr->local, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_modify_macro(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_MODIFY_MACRO, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_modify_macro);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro defsetf (access &rest args) -> access */
static int function_defsetf(Execute ptr, addr form, addr env)
{
	Return(defsetf_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defsetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFSETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defsetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-setf-expander (access &rest args) ...) -> access */
static int function_define_setf_expander(Execute ptr, addr form, addr env)
{
	Return(define_setf_expander_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_setf_expander(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_setf_expander);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun get-setf-expansion (place &optional env) ...) -> (list list list t t) */
static int function_get_setf_expansion(Execute ptr, addr place, addr env)
{
	addr a, b, g, w, r;

	if (env == Unbound)
		env = Nil;
	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}

static void type_get_setf_expansion(addr *ret)
{
	addr args, values, type, list, env;

	GetTypeTable(&type, T);
	GetTypeTable(&env, EnvironmentNull);
	GetTypeTable(&list, T);
	typeargs_var1opt1(&args, type, env);
	typevalues_values5(&values, list, list, list, type, type);
	type_compiled_heap(args, values, ret);
}

static void defun_get_setf_expansion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_SETF_EXPANSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_get_setf_expansion);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_setf_expansion(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro setf (&rest pair) ...) -> t */
static int function_setf(Execute ptr, addr form, addr env)
{
	Return(setf_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_setf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro psetf (&rest args) ...) -> value */
static int function_psetf(Execute ptr, addr form, addr env)
{
	Return(psetf_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_psetf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PSETF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_psetf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro shiftf (place+ newvalue) ...) -> oldvalue */
static int function_shiftf(Execute ptr, addr form, addr env)
{
	Return(shiftf_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_shiftf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_SHIFTF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_shiftf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro rotatef (&rest place) ...) -> nil) */
static int function_rotatef(Execute ptr, addr form, addr env)
{
	Return(rotatef_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_rotatef(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ROTATEF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_rotatef);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
void init_common_data(void)
{
	SetPointerCall(defun,     var2dynamic,  apply);
	SetPointerCall(defmacro,  macro,        setf_apply);
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

void build_common_data(void)
{
	defun_apply();
	define_setf_expander_apply();
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

