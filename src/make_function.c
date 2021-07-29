#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "eval_object.h"
#include "eval_table.h"
#include "function.h"
#include "make.h"
#include "make_function.h"
#include "make_queue.h"
#include "make_value.h"
#include "scope_object.h"
#include "symbol.h"
#include "type.h"
#include "typedef.h"

int code_allcons_(CodeMake ptr, addr cons, addr escape)
{
	int make_label_p, escape_p;
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* escape */
	make_label_p = (escape == NULL);
	if (make_label_p)
		code_queue_make_label(ptr, &escape);

	/* butlast */
	escape_p = ptr->escape;
	code_queue_remmode(ptr, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;

		code_escape_clear(ptr);
		Return(code_make_execute_(ptr, pos));
		if (code_escape_get(ptr)) {
			code_jump_escape(ptr, escape);
			escape_p = 1;
		}
	}
	code_queue_rollback(ptr, &mode);

	/* last */
	code_escape_clear(ptr);
	Return(code_make_execute_(ptr, pos));
	if (code_escape_get(ptr)) {
		code_jump_escape(ptr, escape);
		escape_p = 1;
	}

	/* escape */
	if (make_label_p)
		code_queue_push_label(ptr, escape);

	/* result */
	ptr->escape = escape_p;
	return 0;
}

int code_allcons_set_(CodeMake ptr, addr cons, addr escape)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_allcons_(ptr, cons, escape));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_allcons_rem_(CodeMake ptr, addr cons, addr escape)
{
	int make_label_p, escape_p;
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil)
		return 0;

	/* escape */
	make_label_p = (escape == NULL);
	if (make_label_p)
		code_queue_make_label(ptr, &escape);

	/* cons */
	escape_p = ptr->escape;
	code_queue_remmode(ptr, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;

		code_escape_clear(ptr);
		Return(code_make_execute_(ptr, pos));
		if (code_escape_get(ptr)) {
			code_jump_escape(ptr, escape);
			escape_p = 1;
		}
	}

	/* last */
	code_escape_clear(ptr);
	Return(code_make_execute_(ptr, pos));
	if (code_escape_get(ptr)) {
		code_jump_escape(ptr, escape);
		escape_p = 1;
	}
	code_queue_rollback(ptr, &mode);

	/* escape */
	if (make_label_p)
		code_queue_push_label(ptr, escape);

	/* result */
	ptr->escape = escape_p;
	return 0;
}


/* function */
static void code_function_object(CodeMake ptr, addr pos)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

static void code_function_global(CodeMake ptr, addr pos)
{
	int symbolp;
	constindex index;
	addr symbol;

	getname_tablefunction(pos, &pos);
	symbolp = symbolp_callname(pos);
	GetCallName(pos, &symbol);

	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			index = ConstantCode(symbolp, FUNCTION_SET, SETF_SET);
			code_queue_cons(ptr, index, symbol);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModePush:
			index = ConstantCode(symbolp, FUNCTION_PUSH, SETF_PUSH);
			code_queue_cons(ptr, index, symbol);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

static void code_function_lexical(CodeMake ptr, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, LEXICAL_SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, LEXICAL_PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

int code_make_function_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	if (functionp(scope))
		code_function_object(ptr, scope);
	else if (getglobalp_tablefunction(scope))
		code_function_global(ptr, scope);
	else
		code_function_lexical(ptr, scope);

	return 0;
}


/* lambda */
static void code_ordinary_bind_value(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	code_make_type_value(ptr, pos, escape);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, LETA_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static void code_ordinary_bind_var(CodeMake ptr, addr list, addr escape)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		code_jump_escape_wake(ptr, escape);
		code_ordinary_bind_value(ptr, pos, escape);
	}
}

static int code_ordinary_bind_init_(CodeMake ptr,
		addr var, addr init, addr svar, addr escape)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	code_ordinary_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_ordinary_bind_value(ptr, svar, escape);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_jump_escape_wake(ptr, escape);

	code_ordinary_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_ordinary_bind_value(ptr, svar, escape);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_ordinary_bind_opt_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		code_jump_escape_wake(ptr, escape);
		Return(code_ordinary_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static void code_ordinary_bind_rest(CodeMake ptr, addr pos, addr escape)
{
	if (pos != Nil) {
		CodeQueue_single(ptr, REST);
		code_ordinary_bind_value(ptr, pos, escape);
	}
}

static int code_ordinary_bind_key_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		code_jump_escape_wake(ptr, escape);
		Return(code_ordinary_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static void code_ordinary_bind_allow(CodeMake ptr,
		addr rest, addr list, addr allow, addr escape)
{
	addr name, keys;

	if (rest != Nil || allow != Nil)
		return;
	/* check */
	keys = Nil;
	while (list != Nil) {
		GetCons(list, &name, &list);
		GetCdr(name, &name); /* var */
		GetCar(name, &name); /* name */
		cons_heap(&keys, name, keys);
	}
	if (keys != Nil) {
		CodeQueue_cons(ptr, ALLOW_OTHER_KEYS, keys);
		code_jump_escape_wake(ptr, escape);
	}
}

static int code_ordinary_bind_aux_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_jump_escape_wake(ptr, escape);
		code_ordinary_bind_value(ptr, var, escape);
	}

	return 0;
}


/*
 *  lambda
 */
static void code_lambda_lexical(CodeMake ptr, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Lexical, &list);
	if (list != Nil) {
		CodeQueue_cons(ptr, LAMBDA_LEXICAL, list);
	}
}

static int code_lambda_args_(CodeMake ptr, addr scope, addr escape)
{
	addr list, var, opt, rest, key, allow, aux;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	code_ordinary_bind_var(ptr, var, escape);
	Return(code_ordinary_bind_opt_(ptr, opt, escape));
	code_ordinary_bind_rest(ptr, rest, escape);
	if (rest == Nil && key == Nil) {
		CodeQueue_cons(ptr, REST_NULL, allow);
		code_jump_escape_wake(ptr, escape);
	}
	Return(code_ordinary_bind_key_(ptr, key, escape));
	code_ordinary_bind_allow(ptr, rest, key, allow, escape);
	Return(code_ordinary_bind_aux_(ptr, aux, escape));

	return 0;
}

static int code_lambda_body_(CodeMake ptr, addr scope, addr escape)
{
	addr list;
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	return code_allcons_set_(ptr, list, escape);
}

/* closure */
static int code_lambda_closure_table_(addr pos, addr *ret)
{
	enum EvalTable type;
	addr x, y, z;
	size_t src, dst;

	CheckTableTable(pos);
	type = gettype_evaltable(pos);
	get_evaltable(pos, &pos);
	switch (type) {
		case EvalTable_Value:
			src = getclosure_tablevalue(pos); /* from */
			dst = getlexical_tablevalue(pos); /* to */
			break;

		case EvalTable_Function:
			src = getclosure_tablefunction(pos); /* from */
			dst = getlexical_tablefunction(pos); /* to */
			break;

		case EvalTable_TagBody:
			src = getclosure_tabletagbody(pos); /* from */
			dst = getlexical_tabletagbody(pos); /* to */
			break;

		case EvalTable_Block:
			src = getclosure_tableblock(pos); /* from */
			dst = getlexical_tableblock(pos); /* to */
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid eval-table type.", NULL);
	}

	/* result */
	fixnum_heap(&x, (fixnum)type);
	index_heap(&y, src);
	index_heap(&z, dst);
	list_heap(ret, x, y, z, NULL);

	return 0;
}

static int code_lambda_closure_list_(addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(code_lambda_closure_table_(pos, &pos));
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static void code_lambda_self(addr scope, addr *ret)
{
	addr call, pos, x, y, z;

	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Table, &pos);
	if ((call != Nil) && (pos != Nil) && getreference_tablefunction(pos)) {
		fixnum_heap(&x, EvalTable_Self);
		index_heap(&y, 0);
		index_heap(&z, getlexical_tablefunction(pos));
		list_heap(ret, x, y, z, NULL);
	}
	else {
		*ret = Nil;
	}
}

static int code_lambda_closure_(addr scope, addr *ret)
{
	addr list, pos;

	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &list);
	Return(code_lambda_closure_list_(list, &list));

	/* self */
	code_lambda_self(scope, &pos);
	if (pos != Nil)
		cons_heap(&list, pos, list);

	/* result */
	return Result(ret, list);
}

static int code_lambda_info_(CodeMake ptr, addr scope)
{
	addr pos;

	/* name */
	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_NAME, pos);

	/* type */
	GetEvalScopeIndex(scope, EvalLambda_The, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_TYPE, pos);

	/* documentation */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_DOC, pos);

	/* form */
	GetEvalScopeIndex(scope, EvalLambda_Form, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_FORM, pos);

	/* defun */
	GetEvalScopeIndex(scope, EvalLambda_Defun, &pos);
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_DEFUN, pos);

	/* closure */
	Return(code_lambda_closure_(scope, &pos));
	if (pos != Nil)
		CodeQueue_cons(ptr, LAMBDA_CLOSURE, pos);

	return 0;
}

static int code_lambda_function_(CodeMake ptr, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* begin */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_cons(ptr, LAMBDA, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

static int code_make_lambda_cache_p_(addr scope, int *ret)
{
	OptimizeType value;
	struct scope_struct *str;
	addr pos;

	/* closure */
	Return(code_lambda_closure_(scope, &pos));
	if (pos != Nil)
		return Result(ret, 0);

	/* optimize */
	str = StructEvalScope(scope);
	value = str->optimize[EVAL_OPTIMIZE_SPEED];
	return Result(ret, value < 0 || 1 <= value);
}

int code_make_lambda_(CodeMake ptr, addr scope)
{
	int check;
	addr gensym, label;
	modeswitch mode;

	/* rem mode */
	if (code_queue_remp(ptr))
		return 0;

	/* closure check */
	Return(code_make_lambda_cache_p_(scope, &check));
	if (! check)
		return code_lambda_function_(ptr, scope);

	/* cache */
	make_symbolchar(&gensym, "LAMBDA-CACHE");

	/* code */
	code_queue_make_label(ptr, &label);
	code_queue_setmode(ptr, &mode);
	CodeQueue_double(ptr, LAMBDA_CACHE, label, gensym);
	Return(code_lambda_function_(ptr, scope));
	CodeQueue_cons(ptr, LAMBDA_CACHE_SET, gensym);
	code_queue_push_label(ptr, label);
	code_queue_rollback(ptr, &mode);
	code_queue_ifpush(ptr);

	return 0;
}


/* defun */
static int code_lambda_set_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_lambda_function_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_defun_(CodeMake ptr, addr scope)
{
	addr escape;

	code_queue_make_label(ptr, &escape);
	Return(code_lambda_set_(ptr, scope));
	CodeQueue_single(ptr, DEFUN);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}


/* macro-lambda */
static void code_macro_special(CodeMake, addr);
static void code_macro_special_value(CodeMake ptr, addr pos)
{
	if (pos == Nil)
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_cons(ptr, MACRO_SPECIAL, pos);
	}
}

static void code_macro_special_var(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (consp(pos)) {
			code_macro_special(ptr, pos);
			continue;
		}
		code_macro_special_value(ptr, pos);
	}
}

static void code_macro_special_car(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		code_macro_special_value(ptr, pos);
	}
}

static void code_macro_special_rest(CodeMake ptr, addr pos)
{
	/* (var . &rest) */
	if (pos != Nil) {
		GetCar(pos, &pos);
		code_macro_special_value(ptr, pos);
	}
}

static void code_macro_special(CodeMake ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_special_value(ptr, env);
	code_macro_special_value(ptr, whole);
	code_macro_special_var(ptr, var);
	code_macro_special_car(ptr, opt);
	code_macro_special_rest(ptr, rest);
	code_macro_special_car(ptr, key);
	code_macro_special_car(ptr, aux);
}

static int code_macro_bind_(CodeMake, addr, addr);
static void code_macro_bind_value(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	code_make_type_value(ptr, pos, escape);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, SETQ_SPECIAL, value);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static int code_macro_bind_push_(CodeMake ptr, addr pos, addr escape)
{
	fixnum id;
	addr finish;

	/* begin */
	code_queue_make_label(ptr, &finish);
	code_make_begin_call(ptr, &id);

	/* bind */
	Return(code_macro_bind_(ptr, pos, escape));

	/* end */
	code_queue_push_label(ptr, finish);
	code_make_end(ptr, id);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

static int code_macro_bind_var_(CodeMake ptr, addr list, addr escape)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		code_jump_escape_wake(ptr, escape);

		if (consp(pos)) {
			Return(code_macro_bind_push_(ptr, pos, escape));
		}
		else {
			code_macro_bind_value(ptr, pos, escape);
		}
	}

	return 0;
}

static int code_macro_bind_init_(CodeMake ptr,
		addr var, addr init, addr svar, addr escape)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	code_macro_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_macro_bind_value(ptr, svar, escape);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_jump_escape_wake(ptr, escape);

	code_macro_bind_value(ptr, var, escape);
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_macro_bind_value(ptr, svar, escape);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_macro_bind_opt_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		code_jump_escape_wake(ptr, escape);
		Return(code_macro_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static void code_macro_bind_rest(CodeMake ptr, addr list, addr escape)
{
	if (list != Nil) {
		GetCar(list, &list);
		CodeQueue_single(ptr, REST);
		code_macro_bind_value(ptr, list, escape);
	}
}

static int code_macro_bind_key_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		code_jump_escape_wake(ptr, escape);
		Return(code_macro_bind_init_(ptr, var, init, svar, escape));
	}

	return 0;
}

static int code_macro_bind_aux_(CodeMake ptr, addr list, addr escape)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_jump_escape_wake(ptr, escape);
		code_macro_bind_value(ptr, var, escape);
	}

	return 0;
}

static int code_macro_bind_list_(CodeMake ptr, addr args, addr escape)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(code_macro_bind_var_(ptr, var, escape));
	Return(code_macro_bind_opt_(ptr, opt, escape));
	if (rest == Nil && key == Nil) {
		CodeQueue_cons(ptr, REST_NULL, allow);
		code_jump_escape_wake(ptr, escape);
	}
	code_macro_bind_rest(ptr, rest, escape);
	Return(code_macro_bind_key_(ptr, key, escape));
	code_ordinary_bind_allow(ptr, rest, key, allow, escape);
	Return(code_macro_bind_aux_(ptr, aux, escape));

	return 0;
}

static void code_macro_bind_whole(CodeMake ptr, addr pos, addr escape)
{
	if (pos != Nil) {
		code_macro_bind_value(ptr, pos, escape);
	}
	CodeQueue_single(ptr, WHOLE);
}

static int code_macro_bind_(CodeMake ptr, addr args, addr escape)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_whole(ptr, whole, escape);
	Return(code_macro_bind_list_(ptr, args, escape));

	return 0;
}

static void code_macro_env(CodeMake ptr, addr env, addr escape)
{
	if (env != Nil) {
		CodeQueue_single(ptr, MACRO_ENV);
		code_jump_escape_wake(ptr, escape);
		code_macro_bind_value(ptr, env, escape);
	}
}

static void code_macro_whole(CodeMake ptr, addr whole, addr escape)
{
	CodeQueue_single(ptr, POP);
	code_jump_escape_wake(ptr, escape);

	if (whole != Nil) {
		code_macro_bind_value(ptr, whole, escape);
	}
	CodeQueue_single(ptr, MACRO_WHOLE);
	code_jump_escape_wake(ptr, escape);
}

static int code_macro_args_(CodeMake ptr, addr scope, addr escape)
{
	addr list, var, opt, rest, key, allow, aux, whole, env;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	code_macro_special(ptr, list);

	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_env(ptr, env, escape);
	code_macro_whole(ptr, whole, escape);
	Return(code_macro_bind_list_(ptr, list, escape));

	return 0;
}

static int code_macro_function_(CodeMake ptr, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* begin */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_macro_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_cons(ptr, MACRO, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

int code_make_macro_lambda_(CodeMake ptr, addr scope)
{
	return code_macro_function_(ptr, scope);
}


/* defmacro */
int code_make_defmacro_(CodeMake ptr, addr scope)
{
	addr name, lambda, escape;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);

	/* macdro-lambda */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, lambda));
	code_jump_escape_wake(ptr, escape);

	/* defmacro */
	CodeQueue_cons(ptr, DEFMACRO, name);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);

	/* result */
	code_queue_push_label(ptr, escape);
	return 0;
}


/* deftype */
int code_make_deftype_(CodeMake ptr, addr scope)
{
	addr call, doc, escape;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFTYPE, call, doc);
	code_queue_make_label(ptr, &escape);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}


/* define-compiler-macro */
int code_make_define_compiler_macro_(CodeMake ptr, addr scope)
{
	addr call, doc, escape;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFINE_COMPILER_MACRO, call, doc);
	code_queue_make_label(ptr, &escape);
	code_jump_escape_wake(ptr, escape);
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}


/* destructuring-bind */
static int code_bind_args_(CodeMake ptr, addr list, addr escape)
{
	code_macro_special(ptr, list);
	return code_macro_bind_(ptr, list, escape);
}

static int code_make_bind_execute_(CodeMake ptr, addr scope, addr escape)
{
	addr expr, args, body, free, pos;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);
	GetEvalScopeIndex(scope, 3, &body);
	GetEvalScopeIndex(scope, 4, &free);

	GetConst(COMMON_DESTRUCTURING_BIND, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_bind_args_(ptr, args, escape));
	Return(code_allcons_set_(ptr, body, escape));

	return 0;
}

int code_make_destructuring_bind_(CodeMake ptr, addr scope)
{
	addr escape, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_bind_execute_(ptr, scope, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}


/* flet */
static void code_make_type_function(CodeMake ptr, addr pos, addr escape)
{
	addr type;

	if (getcheck_tablefunction(pos)) {
		gettype_tablefunction(pos, &type);
		if ((! type_astert_p(type)) && (! type_function_aster_p(type))) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
			code_jump_escape_wake(ptr, escape);
		}
	}
}

static int code_make_flet_args_(CodeMake ptr, addr args, addr escape)
{
	addr pos, value;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &value);

		Return(code_lambda_set_(ptr, value));
		code_make_type_function(ptr, pos, escape);
		index_heap(&value, getlexical_tablefunction(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}

	return 0;
}

int code_make_flet_(CodeMake ptr, addr scope)
{
	addr args, body, free, escape;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_make_flet_args_(ptr, args, escape));
	Return(code_allcons_(ptr, body, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}


/* labels */
static int code_lambda_labels_(CodeMake ptr, addr index, addr scope)
{
	addr pos, escape;
	fixnum id;

	/* code_lambda_function_ */
	code_queue_push_code(ptr);
	code_make_begin_call(ptr, &id);
	code_queue_make_label(ptr, &escape);

	/* callname */
	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	/* body */
	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope, escape));
	Return(code_lambda_body_(ptr, scope, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_pop(ptr, &pos);

	/* result */
	CodeQueue_double(ptr, LABELS_LAMBDA, index, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	return 0;
}

static int code_make_labels_args_(CodeMake ptr, addr args, addr escape)
{
	addr pos, scope, index;
	modeswitch mode;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &scope);

		/* labels arguments */
		index_heap(&index, getlexical_tablefunction(pos));
		code_queue_setmode(ptr, &mode);
		Return(code_lambda_labels_(ptr, index, scope));
		code_queue_rollback(ptr, &mode);
		/* type check */
		code_make_type_function(ptr, pos, escape);
	}

	return 0;
}

static void code_make_labels_lexical(CodeMake ptr, addr args)
{
	addr pos, list, index;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCar(pos, &pos);

		index_heap(&index, getlexical_tablefunction(pos));
		cons_heap(&list, index, list);
	}
	CodeQueue_cons(ptr, LABELS_MAKE, list);
}

int code_make_labels_(CodeMake ptr, addr scope)
{
	addr args, body, free, escape;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	code_make_labels_lexical(ptr, args);
	Return(code_make_labels_args_(ptr, args, escape));
	Return(code_allcons_(ptr, body, escape));
	code_queue_push_label(ptr, escape);

	return 0;
}

