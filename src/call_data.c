#include "call_data.h"
#include "callname.h"
#include "clos_instance.h"
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "declare.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "lambda.h"
#include "local.h"
#include "parse.h"
#include "print.h"
#include "print_write.h"
#include "restart_value.h"
#include "sequence.h"
#include "setf.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  apply
 */
static int check_data_function_(Execute ptr, addr call, addr *ret)
{
	int check;

	if (GetType(call) == LISPTYPE_SYMBOL) {
		Return(getfunction_global_(call, &call));
		if (macro_function_p(call))
			return fmte_("Cannot call the macro-function ~S.", call, NULL);
	}
	Return(funcallp_(ptr, call, &check));
	if (! check)
		return fmte_("The argument ~S is not executable.", call, NULL);

	return Result(ret, call);
}

int apply_common_(Execute ptr, addr call, addr arg, addr args)
{
	Return(check_data_function_(ptr, call, &call));
	Return(lista_safe_local_(ptr->local, &args, arg, args));
	return apply_named_control_(ptr, call, args);
}


/*
 *  defun
 */
int defun_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;
	LocalHold hold;

	/* (defun . right) */
	Return_getcdr(form, &right);
	if (right == Nil)
		return fmte_("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		return fmte_("Invalid defun form.", NULL);

	/* name */
	hold = LocalHold_local(ptr);
	Return_getcons(right, &name, &right);
	if (parse_callname_heap(&name, name))
		return fmte_("defun name ~S must be a symbol or (setf name) form.", name, NULL);
	localhold_push(hold, name);
	if (right == Nil)
		return fmte_("defun form must have at least a name and body.", NULL);
	if (GetType(right) != LISPTYPE_CONS)
		return fmte_("Invalid defun form.", NULL);

	/* args */
	Return_getcons(right, &args, &right);
	if (! IsList(args))
		return fmte_("defun argument ~S don't allow dotted list.", args, NULL);
	if (! IsList(right))
		return fmte_("Invalid defun form.", NULL);

	/* parse */
	Return(check_function_variable_(name));
	Return(lambda_ordinary_(ptr->local, &args, args));
	localhold_push(hold, args);
	Return(declare_body_documentation_(ptr, env, right, &doc, &decl, &right));

	/* (eval::defun name args decl doc body) */
	GetConst(SYSTEM_DEFUN, &eval);
	list_heap(ret, eval, name, args, decl, doc, right, form, NULL);
	localhold_end(hold);

	return 0;
}


/*
 *  fdefinition
 */
int fdefinition_common_(Execute ptr, addr name, addr *ret)
{
	Return(parse_callname_error_(&name, name));
	return fdefinition_restart_(ptr, name, ret);
}


/*
 *  (setf fdefinition)
 */
static int setf_setf_fdefinition_common_(addr value, addr symbol)
{
	addr type;

	if (macro_function_p(value)) {
		GetTypeTable(&type, Function);
		return call_type_error_va_(NULL, value, type,
				"The function ~S must be a funcallable function"
				" rather than a macro function.", value, NULL);
	}

	Return(alldelete_function_(symbol));
	return setsetf_symbol_(symbol, value);
}

static int funcall_setf_fdefinition_common_(addr value, addr symbol)
{
	Return(alldelete_function_(symbol));
	if (macro_function_p(value))
		return setmacro_symbol_(symbol, value);
	else
		return setfunction_symbol_(symbol, value);
}

int setf_fdefinition_common_(addr value, addr name)
{
	CallNameType type;

	Return(parse_callname_error_(&name, name));
	GetCallNameType(name, &type);
	GetCallName(name, &name);
	if (type == CALLNAME_SETF)
		return setf_setf_fdefinition_common_(value, name);
	else
		return funcall_setf_fdefinition_common_(value, name);
}


/*
 *  fboundp
 */
int fboundp_common_(addr name, int *ret)
{
	addr check;
	CallNameType type;

	/* callname */
	Return(parse_callname_error_(&name, name));

	/* function check */
	getglobal_callname(name, &check);
	if (check != Unbound)
		return Result(ret, 1);

	/* setf */
	GetCallNameType(name, &type);
	if (type != CALLNAME_SYMBOL)
		return Result(ret, 0);

	/* macro check */
	GetCallName(name, &name);
	getmacro_symbol(name, &check);
	if (check != Unbound)
		return Result(ret, 1);

	/* special operator */
	return Result(ret, get_special_operator(name));
}


/*
 *  fmakunbound
 */
int fmakunbound_common_(addr name)
{
	addr check;
	CallNameType type;

	/* callname */
	Return(parse_callname_error_(&name, name));

	/* remove function */
	Return(remtype_global_callname_(name));
	Return(setglobal_callname_(name, Unbound));

	/* setf */
	GetCallNameType(name, &type);
	if (type != CALLNAME_SYMBOL)
		return 0;

	/* remove macro */
	GetCallName(name, &check);
	remmacro_symbol(check);

	return 0;
}


/*
 *  funcall
 */
int funcall_common_(Execute ptr, addr call, addr args)
{
	Return(check_data_function_(ptr, call, &call));
	return apply_named_control_(ptr, call, args);
}


/*
 *  function-lambda-expression
 */
static int function_closure_p(addr var)
{
	GetDataFunction(var, &var);
	return var != Nil;
}

void function_lambda_expression_common(addr var, addr *ret1, addr *ret2, addr *ret3)
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
	*ret1 = pos1;
	*ret2 = pos2;
	*ret3 = pos3;
}


/*
 *  lambda-list-keywords
 */
void lambda_list_keywords_common(addr *ret)
{
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
	int i;
	addr list, pos;
	constindex index;

	list = Nil;
	for (i = 0; ; i++) {
		index = lambda_list_keywords[i];
		if (index == CONSTANT_EMPTY)
			break;
		GetConstant(index, &pos);
		cons_heap(&list, pos, list);
	}
	*ret = list;
}


/*
 *  defconstant
 */
int defconstant_common_(addr form, addr env, addr *ret)
{
	/* (lisp-system::defconstant symbol value doc) */
	addr args, symbol, value, doc, quote;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (! symbolp(symbol))
		return fmte_("The defconstant argument ~S must be a symbol.", symbol, NULL);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
	}
	else {
		if (! consp_getcons(args, &doc, &args))
			goto error;
		if (! stringp(doc))
			return fmte_("The defconstant argument ~S must be a string.", doc, NULL);
		if (args != Nil)
			goto error;
	}
	GetConst(SYSTEM_DEFCONSTANT, &args);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, symbol, NULL);
	list_heap(&doc, quote, doc, NULL);
	list_heap(ret, args, symbol, value, doc, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The defconstant argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}


/*
 *  defparameter
 */
static void defparameter_expand_common(addr symbol, addr value, addr doc, addr *ret)
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
	*ret = progn;
}

int defparameter_common_(addr form, addr env, addr *ret)
{
	/* (lisp-system::defparameter symbol value doc) */
	addr args, symbol, value, doc;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (! symbolp(symbol))
		return fmte_("The defparameter argument ~S must be a symbol.", symbol, NULL);
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &doc, &args))
		goto error;
	if (! stringp(doc))
		return fmte_("The defparameter argument ~S must be a string.", doc, NULL);
	if (args != Nil)
		goto error;
expand:
	defparameter_expand_common(symbol, value, doc, ret);
	return 0;

error:
	*ret = Nil;
	return fmte_("The defparameter argument ~S must be a "
			"(symbol value &optional documentation) form.", form, NULL);
}


/*
 *  defvar
 */
static void defvar_novalue_common(addr symbol, addr *ret)
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
	*ret = progn;
}

static void defvar_expand_common(addr symbol, addr value, addr doc, addr *ret)
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
	*ret = progn;
}

int defvar_common_(addr form, addr env, addr *ret)
{
	addr args, symbol, value, doc;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &symbol, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &doc, &args))
		goto error;
	if (! stringp(doc))
		return fmte_("The defvar argument ~S must be a string.", doc, NULL);
	if (args != Nil)
		goto error;
expand:
	if (value == Unbound)
		defvar_novalue_common(symbol, &form);
	else
		defvar_expand_common(symbol, value, doc, &form);
	return Result(ret, form);

error:
	*ret = Nil;
	return fmte_("The defvar argument ~S must be a "
			"(symbol &optional value documentation) form.", form, NULL);
}


/*
 *  destructuring-bind
 */
static int destructuring_bind_check_common_(addr pos)
{
	getenvironment_macro_lambda(pos, &pos);
	if (pos != Nil) {
		return fmte_("destructuring-bind don't accept "
				"&environment parameter ~S.", pos, NULL);
	}

	return 0;
}

int destructuring_bind_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, lambda, expr, decl, eval;
	LocalHold hold;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	/* parse */
	if (! listp(lambda)) {
		return fmte_("destructuring-bind argument ~S "
				"must be a list type.", lambda, NULL);
	}

	Return(lambda_macro_(ptr->local, &lambda, lambda, Nil));
	Return(destructuring_bind_check_common_(lambda));
	hold = LocalHold_local_push(ptr, lambda);
	Return(declare_body_(ptr, env, args, &decl, &args));
	localhold_end(hold);
	/* (eval::destructuring-bind lambda expr decl args) */
	GetConst(SYSTEM_DESTRUCTURING_BIND, &eval);
	list_heap(ret, eval, lambda, expr, decl, args, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("destructuring-bind argument ~S must be a "
			"(lambda-list expr &body body) form.", form, NULL);
}


/*
 *  psetq
 */
static int psetq_common_constant_(Execute ptr, addr form, addr env, addr *ret,
		constindex setq_constant,
		constindex psetq_constant)
{
	addr args, root, var, value, gensym, cons, setq, let;

	Return_getcdr(form, &form);
	GetConstant(setq_constant, &setq);
	args = root = Nil;
	while (form != Nil) {
		if (! consp_getcons(form, &var, &form)) {
			GetConstant(psetq_constant, &setq);
			return fmte_("~A argument ~S don't allow dotted list.", setq, form, NULL);
		}
		if (! consp_getcons(form, &value, &form))
			return fmte_("After variable ~S must be a cons, but ~S.", var, form, NULL);
		Return(make_gensym_(ptr, &gensym));
		/* let argument */
		list_heap(&cons, gensym, value, NULL);
		cons_heap(&args, cons, args);
		/* body */
		list_heap(&cons, setq, var, gensym, NULL);
		cons_heap(&root, cons, root);
	}
	/* nil */
	cons_heap(&root, Nil, root);
	/* let form */
	nreverse(&args, args);
	nreverse(&root, root);
	GetConst(COMMON_LET, &let);
	lista_heap(ret, let, args, root, NULL);

	return 0;
}

int psetq_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return psetq_common_constant_(ptr, form, env, ret,
			CONSTANT_COMMON_SETQ,
			CONSTANT_COMMON_PSETQ);
}


/*
 *  psetf
 */
int psetf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	return psetq_common_constant_(ptr, form, env, ret,
			CONSTANT_COMMON_SETF,
			CONSTANT_COMMON_PSETF);
}


/*
 *  return
 */
int return_common_(addr form, addr env, addr *ret)
{
	addr args, value, return_from;

	Return_getcdr(form, &args);
	if (args == Nil) {
		value = Nil;
		goto expand;
	}
	if (! consp_getcons(args, &value, &args))
		goto error;
	if (args != Nil)
		goto error;
expand:
	GetConst(COMMON_RETURN_FROM, &return_from);
	list_heap(ret, return_from, Nil, value, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("RETURN argument ~S must be a (&optional value) form.", form, NULL);
}


/*
 *  complement
 */
void complement_common(addr var, addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_dynamic(pos, p_defun_lambda_complement);
	SetDataFunction(pos, var);
	*ret = pos;
}


/*
 *  constantly
 */
void constantly_common(addr var, addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_any(pos, p_defun_lambda_constantly);
	SetDataFunction(pos, var);
	*ret = pos;
}


/*
 *  every
 */
int every_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size, len;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil)
		return fmte_("Too few arguments.", NULL);

	/* second */
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil)
			goto result_true;
		if (consp(pos)) {
			Return_getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			Return(length_sequence_(pos, 1, &len));
			if (len <= size)
				goto result_true;
			Return(getelt_sequence_(NULL, pos, size, &car));
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	Return(apply1_control_(ptr, &pos, call, args));
	if (pos == Nil)
		goto result_false;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto result_true;
			if (consp(cdr)) {
				Return_getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				Return(length_sequence_(cdr, 1, &len));
				if (len <= size)
					goto result_true;
				Return(getelt_sequence_(NULL, cdr, size, &car));
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		Return(apply1_control_(ptr, &pos, call, args));
		if (pos == Nil)
			goto result_false;
	}

result_true:
	rollback_local(local, stack);
	return Result(ret, T);

result_false:
	rollback_local(local, stack);
	return Result(ret, Nil);
}


/*
 *  some
 */
int some_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	addr pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;
	size_t size, len;

	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil)
		return fmte_("Too few arguments.", NULL);

	/* second */
	args = next = Nil;
	size = 0;
	while (rest != Nil) {
		GetCons(rest, &pos, &rest);
		if (pos == Nil)
			goto result_false;
		if (consp(pos)) {
			Return_getcons(pos, &car, &cdr);
			cons_local(local, &args, car, args);
			cons_local(local, &next, cdr, next);
		}
		else {
			Return(length_sequence_(pos, 1, &len));
			if (len <= size)
				goto result_false;
			Return(getelt_sequence_(NULL, pos, size, &car));
			cons_local(local, &args, car, args);
			cons_local(local, &next, pos, next);
		}
	}
	nreverse(&args, args);
	nreverse(&rest, next);
	if (apply1_control_(ptr, &pos, call, args))
		return 1;
	if (pos != Nil)
		goto result;

	/* second */
	for (size = 1; ; size++) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil)
				goto result_false;
			if (consp(cdr)) {
				Return_getcons(cdr, &car, &cdr);
				SetCar(temp1, car);
				SetCar(temp2, cdr);
			}
			else {
				Return(length_sequence_(cdr, 1, &len));
				if (len <= size)
					goto result_false;
				Return(getelt_sequence_(NULL, cdr, size, &car));
				SetCar(temp1, car);
			}
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (apply1_control_(ptr, &pos, call, args))
			return 1;
		if (pos != Nil)
			goto result;
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


/*
 *  notevery
 */
int notevery_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	/* (notevery predicate sequence*) ==  (not (every predicate sequence*)) */
	Return(every_common_(ptr, call, rest, &rest));
	return Result(ret, (rest == Nil)? T: Nil);
}


/*
 *  notany
 */
int notany_common_(Execute ptr, addr call, addr rest, addr *ret)
{
	/* (notany predicate sequence*) ==  (not (some predicate sequence*)) */
	Return(some_common_(ptr, call, rest, &rest));
	return Result(ret, (rest == Nil)? T: Nil);
}


/*
 *  and
 */
int and_common_(addr form, addr env, addr *ret)
{
	addr expr, when, andv;

	Return_getcdr(form, &form);

	/* (and) */
	if (form == Nil)
		return Result(ret, T);

	/* (and expr) */
	if (singlep(form)) {
		GetCar(form, ret);
		return 0;
	}

	/* (and expr . tail) -> (when expr (and . tail)) */
	GetCons(form, &expr, &form);
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_AND, &andv);
	cons_heap(&andv, andv, form);
	list_heap(ret, when, expr, andv, NULL);

	return 0;
}


/*
 *  cond
 */
int cond_common_(addr form, addr env, addr *ret)
{
	addr expr, tail, ifsym, progn, cond;

	Return_getcdr(form, &form);

	/* (cond) */
	if (form == Nil)
		return Result(ret, Nil);

	/* (cond clause ...) */
	if (! consp_getcons(form, &expr, &form))
		return fmte_("The cond form ~S must be a cons.", form, NULL);
	if (! consp_getcons(expr, &expr, &tail))
		return fmte_("The cond clause ~S must be a cons.", expr, NULL);
	if (tail == Nil) {
		/* (cond (expr) . form)
		 *   `(or ,expr (cond ,$form)))
		 */
		GetConst(COMMON_OR, &ifsym);
		GetConst(COMMON_COND, &cond);
		cons_heap(&form, cond, form);
		list_heap(ret, ifsym, expr, form, NULL);
	}
	else {
		/* (cond (expr . tail) . form)
		 *   `(if ,expr (progn ,@tail) (cond ,@form))
		 */
		GetConst(COMMON_IF, &ifsym);
		GetConst(COMMON_PROGN, &progn);
		GetConst(COMMON_COND, &cond);
		cons_heap(&form, cond, form);
		cons_heap(&tail, progn, tail);
		list_heap(ret, ifsym, expr, tail, form, NULL);
	}

	return 0;
}


/*
 *  or
 */
int or_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr gensym, let, ifsym, orv, expr;

	Return_getcdr(form, &form);

	/* (or) */
	if (form == Nil)
		return Result(ret, Nil);

	/* (or expr) */
	if (singlep(form)) {
		GetCar(form, ret);
		return 0;
	}

	/* (or expr . form) ->
	 *   (let ((#:g expr))
	 *     (if #:g #:g (or . form))) */
	if (! consp_getcons(form, &expr, &form))
		return fmte_("The or form ~S must be a cons.", NULL);
	Return(make_gensym_(ptr, &gensym));
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_OR, &orv);
	list_heap(&expr, gensym, expr, NULL);
	conscar_heap(&expr, expr);
	cons_heap(&form, orv, form);
	list_heap(&form, ifsym, gensym, gensym, form, NULL);
	list_heap(ret, let, expr, form, NULL);

	return 0;
}


/*
 *  when
 */
int when_common_(addr form, addr env, addr *ret)
{
	addr args, expr, ifsym, cons;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		return fmte_("The when ~S must be a (when test . body) form.", form, NULL);
	/* `(if ,expr (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_IF, &ifsym);
	list_heap(ret, ifsym, expr, cons, NULL);

	return 0;
}


/*
 *  unless
 */
int unless_common_(addr form, addr env, addr *ret)
{
	addr args, notv, expr, ifsym, cons;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		return fmte_("The unless ~S must be a (unless test . body) form.", form, NULL);
	/* `(if (not ,expr) (progn ,@body)) */
	GetConst(COMMON_PROGN, &cons);
	cons_heap(&cons, cons, args);
	GetConst(COMMON_NOT, &notv);
	list_heap(&expr, notv, expr, NULL);
	GetConst(COMMON_IF, &ifsym);
	list_heap(ret, ifsym, expr, cons, NULL);

	return 0;
}


/*
 *  case
 */
int case_common_(Execute ptr, addr form, addr env, addr *ret)
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
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("CASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	Return(make_gensym_(ptr, &g));

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			return fmte_("CASE clauses ~S must be list type.", args, NULL);
		if (lastp) {
			return fmte_("CASE clauses ~S "
					"don't appear after otherwise clause.", args, NULL);
		}
		GetCons(args, &test, &args);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		if (test == T || test == otherwise) {
			cons_heap(&list, T, body);
			lastp = 1;
		}
		else {
			list_heap(&list, quote, test, NULL);
			list_heap(&list, listp(test)? member: eql, g, list, NULL);
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
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, list, declare, root, NULL);

	return 0;
}


/*
 *  ecase
 */
int ecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, eql, member, quote, error, type, a;

	/* (let ((g key))
	 *   (cond ((eql g 'test1) . body1)
	 *         ((member g '(test2)) . body2)
	 *         (t . (type-error g (member ...)))))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("ECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ECASE_ERROR, &error);
	Return(make_gensym_(ptr, &g));

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("ECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("ECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		if (listp(test)) {
			list_heap(&list, member, g, list, NULL);
			while (test != Nil) {
				Return_getcons(test, &a, &test);
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
	nreverse(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(ret, let, list, root, NULL);

	return 0;
}


/*
 *  ccase
 */
static int ccase_comma_common_(Execute ptr, addr stream, addr x, int *first)
{
	if (*first) {
		*first = 0;
	}
	else {
		Return(print_ascii_stream_(stream, ", "));
	}
	return princ_print_(ptr, stream, x);
}

static int ccase_string_common_(Execute ptr,
		addr *ret, addr *rtype, addr place, addr args)
{
	int first;
	addr stream, pos, x, list;
	LocalHold hold;

	/* member */
	GetConst(COMMON_MEMBER, &list);
	conscar_heap(&list, list);
	/* stream */
	open_output_string_stream(&stream, 0);
	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, stream);
	localhold_set(hold, 1, list);
	Return(format_stream_(ptr, stream, "The value of ~A, ~~A, is not ", place, NULL));
	/* loop */
	for (first = 1; args != Nil; ) {
		Return_getcons(args, &pos, &args);
		Return_getcar(pos, &pos);
		if (listp(pos)) {
			while (pos != Nil) {
				Return_getcons(pos, &x, &pos);
				Return(ccase_comma_common_(ptr, stream, x, &first));
				cons_heap(&list, x, list);
				localhold_set(hold, 1, list);
			}
		}
		else {
			Return(ccase_comma_common_(ptr, stream, pos, &first));
			cons_heap(&list, pos, list);
			localhold_set(hold, 1, list);
		}
	}
	localhold_end(hold);

	Return(write_char_stream_(stream, '.'));
	Return(string_stream_heap_(stream, ret));
	Return(close_stream_(stream, NULL));
	nreverse(rtype, list);

	return 0;
}

static int ccase_cond_common_(addr g, addr str3, addr type, addr args, addr *ret)
{
	/*  (cond ((eql g 'test1) . body1)
	 *        ((member g 'test2) . body2)
	 *        ...
	 *        (t (error
	 *             (make-condition 'simple-type-error
	 *               :datum g
	 *               :expected-type '(member ...)
	 *               :format-control "The g of xx, ~A, is not xx"
	 *               :format-arguments (list g)))))
	 */
	addr invoke, make, simple, datum, expect, control, arguments, quote, list;
	addr root, test, body, member, eql, a, cond;

	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_SIMPLE_TYPE_ERROR, &simple);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_MEMBER, &member);
	GetConst(COMMON_EQL, &eql);
	GetConst(COMMON_COND, &cond);

	/* (error ...) */
	list_heap(&list, list, g, NULL);
	list_heap(&type, quote, type, NULL);
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple,
			datum, g, expect, type, control, str3, arguments, list, NULL);
	list_heap(&invoke, invoke, make, NULL);

	/* cond */
	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("CCASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CCASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		if (listp(test)) {
			list_heap(&list, member, g, list, NULL);
			while (test != Nil) {
				Return_getcons(test, &a, &test);
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
	list_heap(&invoke, T, invoke, NULL);
	cons_heap(&root, invoke, root);
	nreverse(&root, root);
	cons_heap(ret, cond, root);

	return 0;
}

static int ccase_expand_common_(Execute ptr,
		addr env, addr *ret, addr place, addr args)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (block result
	 *     (tagbody
	 *       loop
	 *       (restart-bind
	 *         ((store-value
	 *            (lambda (v) (setq g v value v) w (go loop))
	 *            :report-function
	 *              (lambda (s)
	 *                (princ "Retry ccase with new value xx." s))
	 *            :interactive-function
	 *              (lambda ()
	 *                (list (eval (prompt-for t "Input xx> "))))))
	 *         (return-from result ...)))))
	 */
	addr a, b, g, r, w, v, s, str1, str2, str3;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, cond, quote;
	addr x, y, root, type, block, retfrom, result;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);

	Return_getcar(g, &g);
	Return(format_string_(ptr, &str1,
				"Retry ccase with new value ~A.", place, NULL));
	localhold_push(hold, str1);
	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);
	Return(ccase_string_common_(ptr, &str3, &type, place, args));
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	make_symbolchar(&result, "RESULT");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_RETURN_FROM, &retfrom);
	/* expand */
	Return(ccase_cond_common_(value, str3, type, args, &cond));
	list_heap(&cond, retfrom, result, cond, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, cond, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	list_heap(&tagbody, block, result, tagbody, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int ccase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, x;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &x, &args))
		goto error;
	Return(ccase_expand_common_(ptr, env, ret, x, args));
	localhold_end(hold);
	return 0;

error:
	*ret = Nil;
	return fmte_("CCASE arguments ~S must be (place &rest args) form.", form, NULL);
}


/*
 *  typecase
 */
int typecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	int lastp;
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, otherwise, declare, ignorable;

	/* (let ((g key))
	 *   (declare (ignorable g))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . otherwise)))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("TYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_OTHERWISE, &otherwise);
	Return(make_gensym_(ptr, &g));

	lastp = 0;
	for (root = Nil; args != Nil; ) {
		if (! consp(args))
			return fmte_("TYPECASE clauses ~S must be list type.", args, NULL);
		if (lastp) {
			return fmte_("TYPECASE clauses ~S don't "
					"appear after otherwise clause.", args, NULL);
		}
		GetCons(args, &test, &args);
		if (! consp_getcons(test, &test, &body))
			return fmte_("TYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
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
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(&ignorable, ignorable, g, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(ret, let, list, declare, root, NULL);

	return 0;
}


/*
 *  etypecase
 */
int etypecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, args, list, test, body, g, root;
	addr let, cond, typep, quote, error, type;

	/* (let ((g key))
	 *   (cond ((typep g 'test1) . body1)
	 *         (t . (type-error g (or ...)))))
	 */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &key, &args))
		return fmte_("ETYPECASE argument must be (key &rest clauses) form.", form, NULL);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_COND, &cond);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_ETYPECASE_ERROR, &error);
	Return(make_gensym_(ptr, &g));

	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("ETYPECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("ETYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		list_heap(&list, typep, g, list, NULL);
		cons_heap(&type, test, type);
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}
	/* error */
	nreverse(&type, type);
	list_heap(&type, quote, type, NULL);
	list_heap(&list, error, g, type, NULL);
	list_heap(&list, T, list, NULL);
	cons_heap(&root, list, root);
	/* cond form */
	nreverse(&root, root);
	cons_heap(&root, cond, root);
	list_heap(&list, g, key, NULL);
	conscar_heap(&list, list);
	list_heap(ret, let, list, root, NULL);

	return 0;
}


/*
 *  ctypecase
 */
static int ctypecase_string_common_(Execute ptr, addr *ret, addr args)
{
	addr list, pos;

	GetConst(COMMON_OR, &list);
	conscar_heap(&list, list);
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return_getcar(pos, &pos);
		cons_heap(&list, pos, list);
	}
	nreverse(ret, list);

	return 0;
}

static int ctypecase_cond_common_(addr g, addr type, addr args, addr *ret)
{
	/*  (cond ((typep g 'type1) . body1)
	 *        ...
	 *        (t (error
	 *             (make-condition 'type-error
	 *               :datum g
	 *               :expected-type 'type))))
	 */
	addr invoke, make, type_error, datum, expect, quote, typep, cond;
	addr root, test, body, list;

	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_TYPE_ERROR, &type_error);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_COND, &cond);

	/* (error ...) */
	list_heap(&type, quote, type, NULL);
	list_heap(&type_error, quote, type_error, NULL);
	list_heap(&make, make, type_error, datum, g, expect, type, NULL);
	list_heap(&invoke, invoke, make, NULL);

	/* cond */
	type = Nil;
	for (root = Nil; args != Nil; ) {
		if (! consp_getcons(args, &test, &args))
			return fmte_("CTYPECASE clauses ~S must be list type.", args, NULL);
		if (! consp_getcons(test, &test, &body))
			return fmte_("CTYPECASE clauses ~S must be list type.", test, NULL);
		if (body == Nil)
			consnil_heap(&body);
		list_heap(&list, quote, test, NULL);
		list_heap(&list, typep, g, list, NULL);
		cons_heap(&type, test, type);
		cons_heap(&list, list, body);
		cons_heap(&root, list, root);
	}

	/* error */
	list_heap(&invoke, T, invoke, NULL);
	cons_heap(&root, invoke, root);
	nreverse(&root, root);
	cons_heap(ret, cond, root);

	return 0;
}

static int ctypecase_expand_common_(Execute ptr,
		addr env, addr *ret, addr place, addr args)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (block result
	 *     (tagbody
	 *       loop
	 *       (restart-bind
	 *         ((store-value
	 *            (lambda (v) (setq g v value v) w (go loop))
	 *            :report-function
	 *              (lambda (s)
	 *                (princ "Retry ctypecase with new value xx." s))
	 *            :interactive-function
	 *              (lambda ()
	 *                (list (eval (prompt-for t "Input xx> "))))))
	 *         (return-from result ...)))))
	 */
	addr a, b, g, r, w, v, s, str1, str2;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, cond;
	addr x, y, root, type, block, retfrom, result;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);

	Return_getcar(g, &g);
	Return(format_string_(ptr, &str1,
				"Retry ctypecase with new value ~A.", place, NULL));
	localhold_push(hold, str1);
	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);
	Return(ctypecase_string_common_(ptr, &type, args));
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	make_symbolchar(&result, "RESULT");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_RETURN_FROM, &retfrom);

	/* expand */
	Return(ctypecase_cond_common_(value, type, args, &cond));
	list_heap(&cond, retfrom, result, cond, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, cond, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	list_heap(&tagbody, block, result, tagbody, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int ctypecase_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, x;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &x, &args))
		goto error;
	Return(ctypecase_expand_common_(ptr, env, ret, x, args));
	localhold_end(hold);
	return 0;

error:
	*ret = Nil;
	return fmte_("CTYPECASE arguments ~S must be (place &rest args) form.", form, NULL);
}


/*
 *  multiple-value-bind
 */
int multiple_value_bind_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr body, list, pos, vars, expr, doc, decl;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, form, env, NULL);
	/* argument */
	Return_getcdr(form, &body);
	if (! consp_getcons(body, &vars, &body))
		goto error;
	for (list = vars; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(check_variable_(pos));
	}
	if (! consp_getcons(body, &expr, &body))
		goto error;
	/* extract */
	Return(declare_body_documentation_(ptr, env, body, &doc, &decl, &body));
	localhold_end(hold);
	GetConst(SYSTEM_MULTIPLE_VALUE_BIND, &pos);
	list_heap(ret, pos, vars, expr, decl, doc, body, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-bind argument must be a "
			"((vars*) expr &body body) form.", NULL);
}


/*
 *  multiple-value-list
 */
int multiple_value_list_common_(addr form, addr env, addr *ret)
{
	addr args, expr, symbol, func, list;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(multiple-value-call #'list ,expr) */
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &symbol);
	GetConst(COMMON_FUNCTION, &func);
	GetConst(COMMON_LIST, &list);
	list_heap(&list, func, list, NULL);
	list_heap(ret, symbol, list, expr, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-list argument ~S "
			"must be a single list.", form, NULL);
}


/*
 *  multiple-value-setq
 */
int multiple_value_setq_common_(addr form, addr env, addr *ret)
{
	addr args, vars, expr, values, setf;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &vars, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(values (setf (values ,@vars) ,expr)) */
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_SETF, &setf);
	cons_heap(&vars, values, vars);
	list_heap(&vars, setf, vars, expr, NULL);
	list_heap(ret, values, vars, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("The multiple-value-setq arguments ~S "
			"must be a (vars form).", form, NULL);
}


/*
 *  nth-value
 */
int nth_value_common_(addr form, addr env, addr *ret)
{
	addr list, args, nth, expr, nth_value;

	Return_getcdr(form, &list);
	if (! consp_getcons(list, &nth, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;
	GetConst(SYSTEM_NTH_VALUE, &nth_value);
	list_heap(ret, nth_value, nth, expr, form, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("NTH-VALUE argument ~S must be (nth expr) form.", form, NULL);
}


/*
 *  prog
 */
static int function_prog_constant_(addr form, addr *ret,
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
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &var, &form)) {
		GetConstant(prog_constant, &var);
		return fmte_("~A argument ~S must be ([var] &rest body) form.", var, form, NULL);
	}
	Return(declare_body_form_(form, &decl, &form));

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
	nreverse(&root, root);
	/* (block ...) */
	list_heap(ret, block, Nil, root, NULL);

	return 0;
}

int prog_common_(addr form, addr env, addr *ret)
{
	return function_prog_constant_(form, ret,
			CONSTANT_COMMON_PROG,
			CONSTANT_COMMON_LET);
}


/*
 *  prog*
 */
int proga_common_(addr form, addr env, addr *ret)
{
	return function_prog_constant_(form, ret,
			CONSTANT_COMMON_PROGA,
			CONSTANT_COMMON_LETA);
}


/*
 *  prog1
 */
int prog1_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr expr, g, let, root;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		return fmte_("PROG1 arguemnt ~S must be (form1 &body body) form.", form, NULL);
	if (form == Nil)
		return Result(ret, expr);
	/* `(let ((,g ,expr)) ,@form ,g) */
	Return(make_gensym_(ptr, &g));
	GetConst(COMMON_LET, &let);
	list_heap(&expr, g, expr, NULL);
	conscar_heap(&expr, expr);
	conscar_heap(&root, let);
	cons_heap(&root, expr, root);
	while (form != Nil) {
		if (! consp_getcons(form, &expr, &form))
			return fmte_("PROG1 argument ~S don't accept a dotted list.", form, NULL);
		cons_heap(&root, expr, root);
	}
	cons_heap(&root, g, root);
	nreverse(ret, root);

	return 0;
}


/*
 *  prog2
 */
int prog2_common_(addr form, addr env, addr *ret)
{
	addr expr, progn, prog1;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		goto error;
	if (! consp(form))
		goto error;
	/* `(progn ,expr (prog1 ,@form)) */
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_PROG1, &prog1);
	cons_heap(&prog1, prog1, form);
	list_heap(ret, progn, expr, prog1, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("PROG2 arguemnt ~S "
			"must be (form1 form2 &body body) form.", form, NULL);
}


/*
 *  define-modify-macro
 */
static int define_modify_macro_expand_(LocalRoot local, addr *ret,
		addr name, addr args, addr call, addr doc)
{
	addr place, env, key, declare, ignore, append;
	addr defmacro, mvbind, expansion, quote, let, mapcar, function;
	addr list, lista, vars, x, cons, values;
	addr a, b, g, w, r, qmvbind, list1, list2, list3, rest;

	/* define-modify-macro lambda list */
	Return(define_modify_macro_heap_(local, &list1, &rest, args));
	GetConst(AMPERSAND_ENVIRONMENT, &key);
	make_symbolchar(&env, "ENV");
	make_symbolchar(&place, "PLACE");
	lista_heap(&args, key, env, place, args, NULL);

	/* expand
	 *
	 * `(defmacro name (&environment env place ,args)
	 *    ,doc
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion ',place env)
	 *      `(let ,(mapcar #'list a b)
	 *         (declare (ignorable ,@a ,@b))
	 *         (multiple-value-bind ,g (call ,r ,v1 ,v2 ,v3 ... ,@rest)
	 *           ,w
	 *           (values ,@g)))))
	 *
	 *  (defmacro name (&environment env place args...)
	 *    doc
	 *    (multiple-value-bind (a b g w r) (get-setf-expansion (quote place) env)
	 *      (list (quote let) (mapcar (function list) a b)
	 *        (list (quote multiple-value-bind) g
	 *          (list (quote declare) (append (list (quote ignorable)) a))
	 *          (list* (quote call) r v1 v2 v3 ... rest)
	 *          w
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
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignore);
	GetConst(COMMON_APPEND, &append);
	make_symbolchar(&a, "A");
	make_symbolchar(&b, "B");
	make_symbolchar(&g, "G");
	make_symbolchar(&w, "W");
	make_symbolchar(&r, "R");
	list_heap(&values, quote, values, NULL);
	list_heap(&cons, cons, values, g, NULL);
	list_heap(&call, quote, call, NULL);

	if (rest == Nil) {
		/* (list 'call r v1 v2 o1 o2) */
		conscar_heap(&vars, list);
		cons_heap(&vars, call, vars);
		cons_heap(&vars, r, vars);
		while (list1 != Nil) {
			GetCons(list1, &x, &list1);
			cons_heap(&vars, x, vars);
		}
		nreverse(&vars, vars);
	}
	else {
		/* (list* 'call r v1 v2 o1 o2 rest) */
		conscar_heap(&vars, lista);
		cons_heap(&vars, call, vars);
		cons_heap(&vars, r, vars);
		while (list1 != Nil) {
			GetCons(list1, &x, &list1);
			cons_heap(&vars, x, vars);
		}
		cons_heap(&vars, rest, vars);
		nreverse(&vars, vars);
	}

	list_heap(&qmvbind, quote, mvbind, NULL);
	list_heap(&list2, list, qmvbind, g, vars, w, cons, NULL);
	list_heap(&function, function, list, NULL);
	list_heap(&mapcar, mapcar, function, a, b, NULL);
	list_heap(&let, quote, let, NULL);
	list_heap(&list1, list, let, mapcar, list2, NULL);
	list_heap(&expansion, expansion, place, env, NULL);
	list_heap(&list3, a, b, g, w, r, NULL);

	list_heap(&declare, quote, declare, NULL);
	list_heap(&ignore, quote, ignore, NULL);
	list_heap(&ignore, list, ignore, NULL);
	list_heap(&append, append, ignore, a, NULL);
	list_heap(&declare, list, declare, append, NULL);

	list_heap(&mvbind, mvbind, list3, expansion, declare, list1, NULL);
	list_heap(ret, defmacro, name, args, doc, mvbind, NULL);

	return 0;
}

int define_modify_macro_common_(LocalRoot local, addr form, addr env, addr *ret)
{
	addr args, name, lambda, call, doc;

	CheckLocal(local);
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &name, &args))
		goto error;
	if (! consp_getcons(args, &lambda, &args))
		goto error;
	if (! consp_getcons(args, &call, &args))
		goto error;
	if (args == Nil) {
		doc = Nil;
		goto expand;
	}
	GetCons(args, &doc, &args);
	if (args != Nil)
		goto error;
	if (! stringp(doc)) {
		return fmte_("DEFINE-MODIFY-MACRO documentation ~S "
				"must be a string type.", doc, NULL);
	}
expand:
	return define_modify_macro_expand_(local, ret, name, lambda, call, doc);

error:
	*ret = Nil;
	return fmte_("DEFINE-MODIFY-MACRO argument ~S must be "
			"(name lambda-list functionn &optional documentation) "
			"form.", form, NULL);
}


/*
 *  defsetf-short
 */
static int defsetf_short_common_(Execute ptr, addr name, addr call, addr doc, addr *ret)
{
	/*  (defmacro defsetf-short (name call &optional doc)
	 *    (let ((args (gensym))
	 *          (a (make-symbol "A"))
	 *          (g (make-symbol "G")))
	 *      `(define-setf-expander ,name (&rest ,args)
	 *         ,doc
	 *         (let ((,a (mapcar (lambda (,a)
	 *                             (declare (ignore ,a))
	 *                             (gensym))
	 *                     ,args)))
	 *           (values
	 *             ,a
	 *             ,args
	 *             (list ',g)
	 *             (append (list ',call) ,a (list ',g))
	 *             (append (list ',name) ,a))))))
	 */
	addr args, a, g, w, r, x;
	addr define, rest, let, mapcar, lambda, declare, ignore;
	addr gensym, values, append, list, quote;

	Return(make_gensym_(ptr, &args));
	make_symbolchar(&a, "A");
	make_symbolchar(&g, "G");

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(COMMON_GENSYM, &gensym);
	GetConst(COMMON_VALUES, &values);
	GetConst(COMMON_APPEND, &append);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);

	list_heap(&g, quote, g, NULL);
	list_heap(&g, list, g, NULL);
	list_heap(&w, quote, call, NULL);
	list_heap(&w, list, w, NULL);
	list_heap(&w, append, w, a, g, NULL);
	list_heap(&r, quote, name, NULL);
	list_heap(&r, list, r, NULL);
	list_heap(&r, append, r, a, NULL);
	list_heap(&values, values, a, args, g, w, r, NULL);
	list_heap(&ignore, ignore, a, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&x, a, NULL);
	list_heap(&gensym, gensym, NULL);
	list_heap(&lambda, lambda, x, declare, gensym, NULL);
	list_heap(&mapcar, mapcar, lambda, args, NULL);
	list_heap(&a, a, mapcar, NULL);
	list_heap(&a, a, NULL);
	list_heap(&let, let, a, values, NULL);
	list_heap(&rest, rest, args, NULL);
	list_heap(ret, define, name, rest, doc, let, NULL);

	return 0;
}


/*
 *  defsetf-long
 */
static int defsetf_long_var_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g;

	while (list != Nil) {
		GetCons(list, &x, &list);
		Return(make_gensym_(ptr, &g));
		cons_heap(args, g, *args);
		cons_heap(a, x, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_opt_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g, h, var, init, sup;

	if (list == Nil)
		return 0;
	GetConst(AMPERSAND_OPTIONAL, &x);
	cons_heap(args, x, *args);

	while (list != Nil) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &init, &sup, NULL);

		/* (var init sup) */
		if (sup != Nil) {
			Return(make_gensym_(ptr, &g));
			Return(make_gensym_(ptr, &h));
			list_heap(&x, g, init, h, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			cons_heap(a, sup, *a);
			cons_heap(b, h, *b);
			continue;
		}

		/* (var init) */
		if (init != Nil) {
			Return(make_gensym_(ptr, &g));
			list_heap(&x, g, init, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			continue;
		}

		/* var */
		Return(make_gensym_(ptr, &g));
		cons_heap(args, g, *args);
		cons_heap(a, var, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_rest_(Execute ptr, addr rest, addr *args, addr *a, addr *b)
{
	addr x, g;

	if (rest == Nil)
		return 0;
	GetConst(AMPERSAND_REST, &x);
	cons_heap(args, x, *args);

	Return(make_gensym_(ptr, &g));
	cons_heap(args, g, *args);
	cons_heap(a, rest, *a);
	cons_heap(b, g, *b);

	return 0;
}

static int defsetf_long_key_(Execute ptr, addr list, addr *args, addr *a, addr *b)
{
	addr x, g, h, var, name, init, sup;

	if (list == Nil)
		return 0;
	GetConst(AMPERSAND_KEY, &x);
	cons_heap(args, x, *args);

	while (list != Nil) {
		GetCons(list, &x, &list);
		List_bind(x, &var, &name, &init, &sup, NULL);

		/* (var init sup) */
		if (sup != Nil) {
			Return(make_gensym_(ptr, &g));
			Return(make_gensym_(ptr, &h));
			list_heap(&name, name, g, NULL);
			list_heap(&x, name, init, h, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			cons_heap(a, sup, *a);
			cons_heap(b, h, *b);
			continue;
		}

		/* (var init) */
		if (init != Nil) {
			Return(make_gensym_(ptr, &g));
			list_heap(&name, name, g, NULL);
			list_heap(&x, name, init, NULL);
			cons_heap(args, x, *args);
			cons_heap(a, var, *a);
			cons_heap(b, g, *b);
			continue;
		}

		/* var */
		Return(make_gensym_(ptr, &g));
		list_heap(&name, name, g, NULL);
		cons_heap(args, name, *args);
		cons_heap(a, var, *a);
		cons_heap(b, g, *b);
	}

	return 0;
}

static int defsetf_long_lambda_(Execute ptr,
		addr lambda, addr *rargs, addr *ra, addr *rb, addr *renv)
{
	addr var, opt, rest, key, allow;
	addr args, a, b;

	Return(lambda_defsetf_(ptr->local, &lambda, lambda));
	List_bind(lambda, &var, &opt, &rest, &key, &allow, renv, NULL);
	args = a = b = Nil;

	Return(defsetf_long_var_(ptr, var, &args, &a, &b));
	Return(defsetf_long_opt_(ptr, opt, &args, &a, &b));
	Return(defsetf_long_rest_(ptr, rest, &args, &a, &b));
	Return(defsetf_long_key_(ptr, key, &args, &a, &b));

	nreverse(rargs, args);
	nreverse(ra, a);
	nreverse(rb, b);

	return 0;
}

static int defsetf_long_store_(Execute ptr, addr store, addr *ret)
{
	addr list, g;

	list = Nil;
	while (store != Nil) {
		GetCdr(store, &store);
		Return(make_gensym_(ptr, &g));
		cons_heap(&list, g, list);
	}
	nreverse(ret, list);

	return 0;
}

static void defsetf_long_values_g(addr v3, addr *ret)
{
	addr quote;

	GetConst(COMMON_QUOTE, &quote);
	list_heap(ret, quote, v3, NULL);
}

static void defsetf_long_values_r(addr name, addr a, addr v3, addr *ret)
{
	/* r: (list* ',name 'g3 'g4 a) */
	addr lista, quote, root, x;

	GetConst(COMMON_LISTA, &lista);
	GetConst(COMMON_QUOTE, &quote);

	conscar_heap(&root, lista);
	list_heap(&x, quote, name, NULL);
	cons_heap(&root, x, root);
	while (v3 != Nil) {
		GetCons(v3, &x, &v3);
		list_heap(&x, quote, x, NULL);
		cons_heap(&root, x, root);
	}
	cons_heap(&root, a, root);
	nreverse(ret, root);
}

static void defsetf_long_values_w(addr v1, addr v2, addr v3, addr store,
		addr args, addr decl, addr body, addr a, addr *ret)
{
	addr declare, lambda, let, quote, list, cons;
	addr root, x, y;

	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_CONS, &cons);

	/* lambda-list */
	root = Nil;
	while (v1 != Nil) {
		GetCons(v1, &x, &v1);
		GetCons(v2, &y, &v2);
		list_heap(&y, quote, y, NULL);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}

	/* store */
	while (store != Nil) {
		GetCons(store, &x, &store);
		GetCons(v3, &y, &v3);
		list_heap(&y, quote, y, NULL);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	nreverse(&root, root);

	/* lambda */
	list_heap(&lambda, quote, lambda, NULL);
	list_heap(&args, quote, args, NULL);
	lista_heap(&lambda, list, lambda, args, body, NULL);
	list_heap(&body, cons, lambda, a, NULL);

	/* let */
	cons_heap(&declare, declare, decl);
	list_heap(ret, let, root, declare, body, NULL);
}

static int defsetf_long_common_(Execute ptr, addr name,
		addr args, addr store, addr body, addr *ret)
{
	/* `(define-setf-expander ,name (&rest ,b &environment env)
	 *    ,doc
	 *    (let ((,a (mapcar (lambda (,a)
	 *                        (declare (ignore ,a))
	 *                        (gensym))
	 *                      ,b)))
	 *      (values ,a
	 *              ,b
	 *              '(g3 g4 ...)
	 *              (let ((v1 'g1)
	 *                    (v2 'g2)
	 *                    ...
	 *                    (store1 'g3)
	 *                    (store2 'g4)
	 *                    ...)
	 *                (declare ,@decl)
	 *                `((lambda ,[args]
	 *                    ,@body)
	 *                  ,@a))
	 *              (list* ',name 'g3 'g4 a))))
	 */
	addr define, let, mapcar, lambda, declare, ignore, gensym, values;
	addr arest, aenv, doc, decl, a, b, g, w, r, x;
	addr v1, v2, v3, env;

	GetConst(COMMON_DEFINE_SETF_EXPANDER, &define);
	GetConst(AMPERSAND_REST, &arest);
	GetConst(AMPERSAND_ENVIRONMENT, &aenv);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAPCAR, &mapcar);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(COMMON_GENSYM, &gensym);
	GetConst(COMMON_VALUES, &values);

	Return(make_gensym_(ptr, &a));
	Return(make_gensym_(ptr, &b));
	Return(split_decl_body_doc_(body, &doc, &decl, &body));

	/* values */
	Return(defsetf_long_lambda_(ptr, args, &args, &v1, &v2, &env));
	Return(defsetf_long_store_(ptr, store, &v3));
	defsetf_long_values_g(v3, &g);
	defsetf_long_values_w(v1, v2, v3, store, args, decl, body, a, &w);
	defsetf_long_values_r(name, a, v3, &r);
	list_heap(&values, values, a, b, g, w, r, NULL);
	/* let */
	list_heap(&gensym, gensym, NULL);
	list_heap(&ignore, ignore, a, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&x, a, NULL);
	list_heap(&x, lambda, x, declare, gensym, NULL);
	list_heap(&x, mapcar, x, b, NULL);
	list_heap(&x, a, x, NULL);
	list_heap(&x, x, NULL);
	list_heap(&let, let, x, values, NULL);
	if (env == Nil)
		list_heap(&x, arest, b, NULL);
	else
		list_heap(&x, arest, b, aenv, env, NULL);
	list_heap(ret, define, name, x, doc, let, NULL);

	return 0;
}

int defsetf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, arg1, arg2, arg3;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &arg1, &args))
		goto error;
	if (! consp_getcons(args, &arg2, &args))
		goto error;
	if (listp(arg2)) {
		/* long form */
		if (! consp_getcons(args, &arg3, &args))
			return fmte_("Invalid defsetf long form ~S.", form, NULL);
		if (! listp(arg3))
			return fmte_("defsetf argument ~S must be a list type.", arg3, NULL);
		Return(defsetf_long_common_(ptr, arg1, arg2, arg3, args, &form));
	}
	else if (args == Nil) {
		/* short form */
		Return(defsetf_short_common_(ptr, arg1, arg2, Nil, &form));
	}
	else {
		/* short form, documentation */
		if (! consp_getcons(args, &arg3, &args))
			return fmte_("Invalid defsetf short form ~S.", form, NULL);
		if (args != Nil)
			return fmte_("Invalid defsetf short form ~S.", form, NULL);
		if (! stringp(arg3))
			return fmte_("defsetf documentation ~S must be a string type.", arg3, NULL);
		Return(defsetf_short_common_(ptr, arg1, arg2, arg3, &form));
	}
	return Result(ret, form);

error:
	*ret = Nil;
	return fmte_("Invalid defsetf form ~S.", form, NULL);
}


/*
 *  define-setf-expander
 */
int define_setf_expander_common_(addr form, addr env, addr *ret)
{
	/* `(system::define-setf-expander
	 *     ',access
	 *     (system::macro-lambda ,@args))
	 */
	addr access, args, define, lambda, quote;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &access, &args))
		goto error;
	if (! consp(args))
		goto error;
	/* expand */
	GetConst(SYSTEM_DEFINE_SETF_EXPANDER, &define);
	GetConst(SYSTEM_MACRO_LAMBDA, &lambda);
	GetConst(COMMON_QUOTE, &quote);
	cons_heap(&lambda, lambda, args);
	list_heap(&access, quote, access, NULL);
	list_heap(ret, define, access, lambda, NULL);
	return 0;

error:
	*ret = Nil;
	return fmte_("DEFINE-SETF-EXPANDER argument ~S "
			"must be (access lambda-list &rest body) form.", form, NULL);
}


/*
 *  setf
 */
static int setf_single_common_(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2)
	 *        (g value))
	 *   (declare (ignorable g1 g2))
	 *     writer)
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &a, &list1);
		Return_getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	GetCar(store, &store);
	list_heap(&a, store, value, NULL);
	cons_heap(&args, a, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, writer, NULL);

	return 0;
}

static int setf_multiple_common_(addr *ret, addr value,
		addr var1, addr var2, addr store, addr writer, addr reader)
{
	/* (let* ((g1 a1)
	 *        (g2 a2))
	 *   (declare (ignorable g1 g2))
	 *   (multiple-value-bind (g ...) value
	 *     writer
	 *     (values g1 ...)))
	 */
	addr list1, list2, args, a, b, leta, declare, ignorable, bind;

	list1 = var1;
	list2 = var2;
	args = Nil;
	while (list1 != Nil) {
		Return_getcons(list1, &a, &list1);
		Return_getcons(list2, &b, &list2);
		list_heap(&a, a, b, NULL);
		cons_heap(&args, a, args);
	}
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, var1);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* multiple-value-bind */
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &bind);
	list_heap(&bind, bind, store, value, writer, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, bind, NULL);

	return 0;
}

static int setf_expr_common_(Execute ptr, addr *ret, addr key, addr value, addr env)
{
	addr a, b, g, w, r;

	Return(get_setf_expansion_(ptr, key, env, &a, &b, &g, &w, &r));
	if (singlep(g))
		return setf_single_common_(ret, value, a, b, g, w, r);
	else
		return setf_multiple_common_(ret, value, a, b, g, w, r);
}

int setf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr key, value, root, progn;
	LocalHold hold;

	Return_getcdr(form, &form);
	if (form == Nil)
		return Result(ret, Nil);

	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, form);
	localhold_set(hold, 1, env);
	for (root = Nil; form != Nil; ) {
		if (! consp_getcons(form, &key, &form))
			goto error;
		if (! consp_getcons(form, &value, &form))
			goto error;
		Return(setf_expr_common_(ptr, &key, key, value, env));
		cons_heap(&root, key, root);
		localhold_set(hold, 2, root);
	}
	localhold_end(hold);
	nreverse(&root, root);

	/* (progn ...) */
	GetConst(COMMON_PROGN, &progn);
	cons_heap(ret, progn, root);
	return 0;

error:
	*ret = Nil;
	return fmte_("The setf form ~S must be a place value form.", form, NULL);
}


/*
 *  shiftf
 */
static int shiftf_list2_common_(addr *ret, addr a, addr b, addr root)
{
	addr x, y;

	while (a != Nil) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}

	return Result(ret, root);
}

static void shiftf_ignorable_common(addr *ret, addr list)
{
	addr declare, ignorable;

	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(ret, declare, ignorable, NULL);
}

static void shiftf_mvbind_common(addr *ret, addr g, addr r, addr body)
{
	addr mvbind, declare;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	shiftf_ignorable_common(&declare, g);
	lista_heap(ret, mvbind, g, r, declare, body, NULL);
}

int shiftf_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, root, pos, let, declare, prog1;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;
	LocalHold hold;

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
	hold = LocalHold_array(ptr, 4);
	localhold_pushva_force(hold, form, env, NULL);

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &pos, &args))
		goto error;
	if (! consp(args))
		goto error;

	/* push */
	Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r0));
	alist = glist = wlist = rlist = Nil;
	Return(shiftf_list2_common_(&alist, a, b, alist));
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	localhold_set(hold, 0, alist);
	localhold_set(hold, 1, glist);
	localhold_set(hold, 2, wlist);
	for (;;) {
		Return_getcons(args, &pos, &args);
		if (args == Nil)
			break;
		Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r));
		Return(shiftf_list2_common_(&alist, a, b, alist));
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
		localhold_set(hold, 0, alist);
		localhold_set(hold, 1, glist);
		localhold_set(hold, 2, wlist);
		localhold_set(hold, 3, rlist);
	}
	localhold_end(hold);

	/* last expand */
	nreverse(&wlist, wlist);
	GetCons(glist, &g, &glist);
	shiftf_mvbind_common(&root, g, pos, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		shiftf_mvbind_common(&root, g, r, root);
	}

	/* multiple-value-prog1 */
	GetConst(COMMON_MULTIPLE_VALUE_PROG1, &prog1);
	list_heap(&root, prog1, r0, root, NULL);

	/* let expand */
	if (alist != Nil) {
		nreverse(&alist, alist);
		GetConst(COMMON_LETA, &let);
		shiftf_ignorable_common(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	return Result(ret, root);

error:
	*ret = Nil;
	return fmte_("SHIFT argument ~S must be (place ... value) form.", form, NULL);
}


/*
 *  rotatef
 */
int rotatef_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, root, pos, let, declare;
	addr a, b, g, w, r, r0, alist, glist, wlist, rlist;
	LocalHold hold;

	hold = LocalHold_array(ptr, 4);
	localhold_pushva_force(hold, form, env, NULL);

	Return_getcdr(form, &form);
	if (form == Nil) {
		localhold_end(hold);
		return Result(ret, Nil);
	}
	if (! consp_getcons(form, &pos, &args))
		goto error;
	if (args == Nil) {
		/* (progn pos nil) */
		GetConst(COMMON_PROGN, &root);
		list_heap(ret, root, pos, Nil, NULL);
		localhold_end(hold);
		return 0;
	}
	if (! consp(args))
		goto error;

	/* push */
	Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r0));
	alist = glist = wlist = rlist = Nil;
	Return(shiftf_list2_common_(&alist, a, b, alist));
	cons_heap(&glist, g, glist);
	cons_heap(&wlist, w, wlist);
	localhold_set(hold, 0, alist);
	localhold_set(hold, 1, glist);
	localhold_set(hold, 2, wlist);
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		Return(get_setf_expansion_(ptr, pos, env, &a, &b, &g, &w, &r));
		Return(shiftf_list2_common_(&alist, a, b, alist));
		cons_heap(&glist, g, glist);
		cons_heap(&wlist, w, wlist);
		cons_heap(&rlist, r, rlist);
		localhold_set(hold, 0, alist);
		localhold_set(hold, 1, glist);
		localhold_set(hold, 2, wlist);
		localhold_set(hold, 3, rlist);
	}
	localhold_end(hold);

	/* last expand */
	cons_heap(&wlist, Nil, wlist);
	nreverse(&wlist, wlist);
	GetCons(glist, &g, &glist);
	shiftf_mvbind_common(&root, g, r0, wlist);

	/* loop expand */
	while (glist != Nil) {
		GetCons(glist, &g, &glist);
		GetCons(rlist, &r, &rlist);
		list_heap(&root, root, NULL);
		shiftf_mvbind_common(&root, g, r, root);
	}

	/* let expand */
	if (alist != Nil) {
		nreverse(&alist, alist);
		GetConst(COMMON_LETA, &let);
		shiftf_ignorable_common(&declare, a);
		list_heap(&root, let, alist, declare, root, NULL);
	}

	/* result */
	return Result(ret, root);

error:
	*ret = Nil;
	return fmte_("ROTATEF argument ~S don't accept a dotted list.", form, NULL);
}

