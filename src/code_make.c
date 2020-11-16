#include "callname.h"
#include "code_make.h"
#include "code_queue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "declare.h"
#include "eval_object.h"
#include "eval_table.h"
#include "function.h"
#include "optimize_common.h"
#include "scope_object.h"
#include "strvect.h"
#include "symbol.h"
#include "type.h"

/* nil */
static void code_make_nil(LocalRoot local, addr code, addr ignore)
{
	code_make_single(local, code,
			CONSTANT_CODE_NIL_SET,
			CONSTANT_CODE_NIL_PUSH);
}


/* t */
static void code_make_t(LocalRoot local, addr code, addr ignore)
{
	code_make_single(local, code,
			CONSTANT_CODE_T_SET,
			CONSTANT_CODE_T_PUSH);
}


/* value */
static void code_make_value(LocalRoot local, addr code, addr scope)
{
	CheckTypeCodeQueue(code);
	GetEvalScopeValue(scope, &scope);
	code_make_object(local, code, scope);
}


/* symbol */
static int code_symbol_special_p(addr pos)
{
	return getspecialp_tablevalue(pos) || getglobalp_tablevalue(pos);
}

static void code_symbol_set(LocalRoot local, addr code, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(local, code, SPECIAL_SET, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(local, code, LEXICAL_SET, pos);
	}
}

static void code_symbol_push(LocalRoot local, addr code, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(local, code, SPECIAL_PUSH, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(local, code, LEXICAL_PUSH, pos);
	}
}

static void code_symbol_remove(LocalRoot local, addr code, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(local, code, SPECIAL_REM, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(local, code, LEXICAL_REM, pos);
	}
}

static void code_make_symbol(LocalRoot local, addr code, addr scope)
{
	addr symbol, table;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol)) {
		code_make_value(local, code, scope);
		return;
	}

	/* symbol */
	GetEvalScopeIndex(scope, 0, &table);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			code_symbol_set(local, code, symbol, table);
			break;

		case CodeQueue_ModePush:
			code_symbol_push(local, code, symbol, table);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_symbol_remove(local, code, symbol, table);
			break;
	}
}


/* declaim */
static void code_declaim_special(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(local, code, DECLAIM_SPECIAL, pos);
	}
}

static void code_declaim_type_value(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_value_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(local, code, DECLAIM_TYPE_VALUE, key, value);
	}
}

static void code_declaim_type_function(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_function_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(local, code, DECLAIM_TYPE_FUNCTION, key, value);
	}
}

static void code_declaim_inline(LocalRoot local, addr code, addr cons)
{
	addr key, value, check1, check2;

	getall_inline_declare(cons, &cons);
	GetConst(COMMON_INLINE, &check1);
	GetConst(COMMON_NOTINLINE, &check2);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (value == check1)
			CodeQueue_cons(local, code, DECLAIM_INLINE, key);
		if (value == check2)
			CodeQueue_cons(local, code, DECLAIM_NOTINLINE, key);
	}
}

static void code_declaim_optimize(LocalRoot local, addr code, addr declare)
{
	addr pos;
	OptimizeType optimize;

	/* compilation-speed */
	optimize = get_optimize_compilation_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(local, code, DECLAIM_COMPILATION, pos);
	}
	/* debug */
	optimize = get_optimize_debug_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(local, code, DECLAIM_DEBUG, pos);
	}
	/* safety */
	optimize = get_optimize_safety_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(local, code, DECLAIM_SAFETY, pos);
	}
	/* space */
	optimize = get_optimize_space_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(local, code, DECLAIM_SPACE, pos);
	}
	/* speed */
	optimize = get_optimize_speed_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(local, code, DECLAIM_SPEED, pos);
	}
}

static void code_declaim_declaration(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_declaration_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(local, code, DECLAIM_DECLARATION, pos);
	}
}

static void code_make_declaim(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_declaim_special(local, code, scope);
	code_declaim_type_value(local, code, scope);
	code_declaim_type_function(local, code, scope);
	code_declaim_inline(local, code, scope);
	code_declaim_optimize(local, code, scope);
	code_declaim_declaration(local, code, scope);
	code_make_nil(local, code, NULL);
}


/*
 *  lexical
 */
static void code_make_lexical(LocalRoot local, addr code, addr scope)
{
	addr list, pos;

	GetEvalScopeIndex(scope, 0, &list);
	GetEvalScopeValue(scope, &pos);
	if (list == Nil) {
		code_make_execute(local, code, pos);
	}
	else {
		code_queue_push_new(local, code);
		CodeQueue_cons(local, code, LEXICAL, list);
		code_make_execute(local, code, pos);
		code_queue_pop(local, code, &pos);
		code_make_execute_control(local, code, pos);
	}
}


/*
 *  progn
 */
static void code_allcons(LocalRoot local, addr code, addr cons)
{
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil) {
		code_make_nil(local, code, NULL);
		return;
	}

	/* butlast */
	code_queue_remmode(code, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;
		code_make_execute(local, code, pos);
	}
	code_queue_rollback(code, &mode);

	/* last */
	code_make_execute(local, code, pos);
}

static void code_allcons_set(LocalRoot local, addr code, addr cons)
{
	modeswitch mode;

	code_queue_setmode(code, &mode);
	code_allcons(local, code, cons);
	code_queue_rollback(code, &mode);
}

static void code_allcons_rem(LocalRoot local, addr code, addr cons)
{
	addr pos;
	modeswitch mode;

	code_queue_remmode(code, &mode);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		code_make_execute(local, code, pos);
	}
	code_queue_rollback(code, &mode);
}

static void code_make_progn(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_allcons(local, code, scope);
}


/* let */
static void code_make_free_value(LocalRoot local, addr code, addr pos, addr type)
{
	if (type_astert_p(type))
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(local, code, TYPE_SPECIAL, pos, type);
	}
	else if (getglobalp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(local, code, TYPE_GLOBAL, pos, type);
	}
	else {
		/* lexical */
		index_heap(&pos, getlexical_tablevalue(pos));
		CodeQueue_double(local, code, TYPE_LEXICAL, pos, type);
	}
}

static void code_make_free_function(LocalRoot local, addr code, addr pos, addr type)
{
	addr symbol;

	if (type_astert_p(type) || type_function_aster_p(type))
		return;
	if (! getglobalp_tablevalue(pos)) {
		/* lexical */
		index_heap(&pos, getlexical_tablefunction(pos));
		CodeQueue_double(local, code, TYPE_LEXICAL, pos, type);
		return;
	}

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_double(local, code, TYPE_FUNCTION, symbol, type);
	}
	else {
		CodeQueue_double(local, code, TYPE_SETF, symbol, type);
	}
}

static void code_make_free(LocalRoot local, addr code, addr list)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			code_make_free_value(local, code, pos, type);
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			code_make_free_function(local, code, pos, type);
			continue;
		}
		Abort("type error");
	}
}

static void code_make_type_value(LocalRoot local, addr code, addr pos)
{
	addr type;

	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &type);
		if (! type_astert_p(type)) {
			CodeQueue_cons(local, code, TYPE_RESULT, type);
		}
	}
}

static void code_make_let_args(LocalRoot local, addr code, addr args)
{
	addr list, pos, value, index;

	/* value */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		code_make_execute_set(local, code, value);
		code_make_type_value(local, code, pos);
		index_heap(&value, getlet_tablevalue(pos));
		CodeQueue_cons(local, code, SETQ_LEXICAL, value);
	}

	/* bind */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);

		index_heap(&index, getlet_tablevalue(pos));
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_double(local, code, LET_SPECIAL, index, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_double(local, code, LET_LEXICAL, index, value);
		}
	}
}

static void code_make_let_execute(LocalRoot local, addr code, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_make_free(local, code, free);
	code_make_let_args(local, code, args);
	code_allcons(local, code, body);
}

static void code_make_let(LocalRoot local, addr code, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 4, &pos); /* allocate */
	if (pos == Nil) {
		code_make_let_execute(local, code, scope);
		return;
	}

	code_queue_setmode(code, &mode);
	code_queue_push_new(local, code);
	code_make_let_execute(local, code, scope);
	code_queue_pop(local, code, &pos);
	code_queue_rollback(code, &mode);
	code_make_execute_control(local, code, pos);
}


/* let* */
static void code_make_leta_args(LocalRoot local, addr code, addr list)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		code_make_execute_set(local, code, value);
		code_make_type_value(local, code, pos);
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_cons(local, code, LETA_SPECIAL, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(local, code, SETQ_LEXICAL, value);
		}
	}
}

static void code_make_leta_execute(LocalRoot local, addr code, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_make_free(local, code, free);
	code_make_leta_args(local, code, args);
	code_allcons(local, code, body);
}

static void code_make_leta(LocalRoot local, addr code, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 4, &pos); /* allocate */
	if (pos == Nil) {
		code_make_leta_execute(local, code, scope);
		return;
	}

	code_queue_setmode(code, &mode);
	code_queue_push_new(local, code);
	code_make_leta_execute(local, code, scope);
	code_queue_pop(local, code, &pos);
	code_queue_rollback(code, &mode);
	code_make_execute_control(local, code, pos);
}


/* setq */
static void code_setq_loop(LocalRoot local, addr code, addr list)
{
	addr pos, value, symbol;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);
		code_make_execute_set(local, code, value);

		getname_tablevalue(pos, &symbol);
		code_make_type_value(local, code, pos);
		if (getspecialp_tablevalue(pos)) {
			CodeQueue_cons(local, code, SETQ_SPECIAL, symbol);
		}
		else if (getglobalp_tablevalue(pos)) {
			CodeQueue_cons(local, code, SETQ_GLOBAL, symbol);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(local, code, SETQ_LEXICAL, value);
		}
	}
}

static void code_make_setq(LocalRoot local, addr code, addr scope)
{
	addr list;

	/* nil */
	GetEvalScopeValue(scope, &list);
	if (list == Nil) {
		code_make_nil(local, code, NULL);
		return;
	}

	/* setq */
	code_setq_loop(local, code, list);
	code_queue_ifpush(local, code);
}


/* function */
static void code_function_object(LocalRoot local, addr code, addr pos)
{
	CheckTypeCodeQueue(code);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

static void code_function_global(LocalRoot local, addr code, addr pos)
{
	int symbolp;
	constindex index;
	addr symbol;

	getname_tablefunction(pos, &pos);
	symbolp = symbolp_callname(pos);
	GetCallName(pos, &symbol);

	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			index = ConstantCode(symbolp, FUNCTION_SET, SETF_SET);
			code_queue_cons(local, code, index, symbol);
			break;

		case CodeQueue_ModePush:
			index = ConstantCode(symbolp, FUNCTION_PUSH, SETF_PUSH);
			code_queue_cons(local, code, index, symbol);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

static void code_function_lexical(LocalRoot local, addr code, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, LEXICAL_SET, pos);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, LEXICAL_PUSH, pos);
			break;

		case CodeQueue_ModeRemove:
		default:
			return;
	}
}

static void code_make_function(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	if (functionp(scope))
		code_function_object(local, code, scope);
	else if (getglobalp_tablefunction(scope))
		code_function_global(local, code, scope);
	else
		code_function_lexical(local, code, scope);
}


/* lambda */
static void ordinary_bind_value(LocalRoot local, addr code, addr pos)
{
	addr value;

	code_make_type_value(local, code, pos);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(local, code, LETA_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(local, code, SETQ_LEXICAL, value);
	}
}

static void ordinary_bind_var(LocalRoot local, addr code, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(local, code, POP);
		ordinary_bind_value(local, code, pos);
	}
}

static void ordinary_bind_init(LocalRoot local, addr code,
		addr var, addr init, addr svar)
{
	addr label, finish;

	/* label */
	code_queue_make_label(local, code, &label);
	code_queue_make_label(local, code, &finish);

	/* if-exists */
	code_queue_if_unbound(local, code, label);
	ordinary_bind_value(local, code, var);
	if (svar != Nil) {
		CodeQueue_single(local, code, T_SET);
		ordinary_bind_value(local, code, svar);
	}
	code_queue_goto(local, code, finish);

	/* if-does-not-exist */
	code_queue_push_label(local, code, label);
	code_make_execute_set(local, code, init);
	ordinary_bind_value(local, code, var);
	if (svar != Nil) {
		CodeQueue_single(local, code, NIL_SET);
		ordinary_bind_value(local, code, svar);
	}

	/* finish */
	code_queue_push_label(local, code, finish);
}

static void ordinary_bind_opt(LocalRoot local, addr code, addr list)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(local, code, POP_UNBOUND);
		ordinary_bind_init(local, code, var, init, svar);
	}
}

static void ordinary_bind_rest(LocalRoot local, addr code, addr pos)
{
	if (pos != Nil) {
		CodeQueue_single(local, code, REST);
		ordinary_bind_value(local, code, pos);
	}
}

static void ordinary_bind_key(LocalRoot local, addr code, addr list)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(local, code, GETF, name);
		ordinary_bind_init(local, code, var, init, svar);
	}
}

static void ordinary_bind_allow(LocalRoot local, addr code,
		addr rest, addr list, addr allow)
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
	if (keys != Nil)
		CodeQueue_cons(local, code, ALLOW_OTHER_KEYS, keys);
}

static void ordinary_bind_aux(LocalRoot local, addr code, addr list)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		code_make_execute_set(local, code, init);
		ordinary_bind_value(local, code, var);
	}
}


/*
 *  lambda
 */
static void code_lambda_lexical(LocalRoot local, addr code, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Lexical, &list);
	if (list != Nil) {
		CodeQueue_cons(local, code, LAMBDA_LEXICAL, list);
	}
}

static void code_lambda_args(LocalRoot local, addr code, addr scope)
{
	addr list, var, opt, rest, key, allow, aux;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	ordinary_bind_var(local, code, var);
	ordinary_bind_opt(local, code, opt);
	ordinary_bind_rest(local, code, rest);
	if (rest == Nil && key == Nil)
		CodeQueue_single(local, code, REST_NULL);
	ordinary_bind_key(local, code, key);
	ordinary_bind_allow(local, code, rest, key, allow);
	ordinary_bind_aux(local, code, aux);
}

static void code_lambda_body(LocalRoot local, addr code, addr scope)
{
	addr list;
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	code_allcons_set(local, code, list);
}

/* closure */
static void code_lambda_closure_table(addr pos, addr *ret)
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
			Abort("Invalid eval-table type.");
			*ret = 0;
			return;
	}

	/* result */
	fixnum_heap(&x, (fixnum)type);
	index_heap(&y, src);
	index_heap(&z, dst);
	list_heap(ret, x, y, z, NULL);
}

static void code_lambda_closure_list(addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		code_lambda_closure_table(pos, &pos);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
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

static void code_lambda_closure(addr scope, addr *ret)
{
	addr list, pos;

	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &list);
	code_lambda_closure_list(list, &list);

	/* self */
	code_lambda_self(scope, &pos);
	if (pos != Nil)
		cons_heap(&list, pos, list);

	/* result */
	*ret = list;
}

static void code_lambda_info(LocalRoot local, addr code, addr scope)
{
	addr pos;

	/* name */
	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_NAME, pos);

	/* type */
	GetEvalScopeIndex(scope, EvalLambda_The, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_TYPE, pos);

	/* documentation */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_DOC, pos);

	/* form */
	GetEvalScopeIndex(scope, EvalLambda_Form, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_FORM, pos);

	/* defun */
	GetEvalScopeIndex(scope, EvalLambda_Defun, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_DEFUN, pos);

	/* closure */
	code_lambda_closure(scope, &pos);
	if (pos != Nil)
		CodeQueue_cons(local, code, LAMBDA_CLOSURE, pos);
}

static void code_lambda_function(LocalRoot local, addr code, addr scope)
{
	addr pos;

	code_queue_push_simple(local, code);
	code_lambda_lexical(local, code, scope);
	code_lambda_args(local, code, scope);
	code_lambda_body(local, code, scope);
	code_queue_pop(local, code, &pos);
	CodeQueue_cons(local, code, LAMBDA, pos);
	code_lambda_info(local, code, scope);
	code_queue_ifpush(local, code);
}

static int code_make_lambda_cache_p(addr scope)
{
	OptimizeType value;
	struct scope_struct *str;
	addr pos;

	/* closure */
	code_lambda_closure(scope, &pos);
	if (pos != Nil)
		return 0;

	/* optimize */
	str = StructEvalScope(scope);
	value = str->optimize[EVAL_OPTIMIZE_SPEED];
	return value < 0 || 1 <= value;
}

static void code_make_lambda(LocalRoot local, addr code, addr scope)
{
	addr gensym, name, label;
	modeswitch mode;

	/* rem mode */
	if (code_queue_remp(code))
		return;

	/* closure check */
	if (! code_make_lambda_cache_p(scope)) {
		code_lambda_function(local, code, scope);
		return;
	}

	/* cache */
	symbol_heap(&gensym);
	strvect_char_heap(&name, "LAMBDA-CACHE");
	SetNameSymbol(gensym, name);

	/* code */
	code_queue_make_label(local, code, &label);
	code_queue_setmode(code, &mode);
	CodeQueue_double(local, code, LAMBDA_CACHE, label, gensym);
	code_lambda_function(local, code, scope);
	CodeQueue_cons(local, code, LAMBDA_CACHE_SET, gensym);
	code_queue_push_label(local, code, label);
	code_queue_rollback(code, &mode);
	code_queue_ifpush(local, code);
}


/* defun */
static void code_lambda_set(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_setmode(code, &mode);
	code_lambda_function(local, code, scope);
	code_queue_rollback(code, &mode);
}

static void code_make_defun(LocalRoot local, addr code, addr scope)
{
	code_lambda_set(local, code, scope);
	CodeQueue_single(local, code, DEFUN);
	code_queue_ifpush(local, code);
}


/* macro-lambda */
static void code_macro_special(LocalRoot, addr, addr);
static void code_macro_special_value(LocalRoot local, addr code, addr pos)
{
	if (pos == Nil)
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_cons(local, code, MACRO_SPECIAL, pos);
	}
}

static void code_macro_special_var(LocalRoot local, addr code, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (consp(pos)) {
			code_macro_special(local, code, pos);
			continue;
		}
		code_macro_special_value(local, code, pos);
	}
}

static void code_macro_special_car(LocalRoot local, addr code, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		code_macro_special_value(local, code, pos);
	}
}

static void code_macro_special_rest(LocalRoot local, addr code, addr pos)
{
	/* (var . &rest) */
	if (pos != Nil) {
		GetCar(pos, &pos);
		code_macro_special_value(local, code, pos);
	}
}

static void code_macro_special(LocalRoot local, addr code, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_special_value(local, code, env);
	code_macro_special_value(local, code, whole);
	code_macro_special_var(local, code, var);
	code_macro_special_car(local, code, opt);
	code_macro_special_rest(local, code, rest);
	code_macro_special_car(local, code, key);
	code_macro_special_car(local, code, aux);
}

static void code_macro_bind(LocalRoot, addr, addr);
static void code_macro_bind_value(LocalRoot local, addr code, addr pos)
{
	addr value;

	code_make_type_value(local, code, pos);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(local, code, SETQ_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(local, code, SETQ_LEXICAL, value);
	}
}

static void code_macro_bind_var(LocalRoot local, addr code, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(local, code, POP);
		if (consp(pos)) {
			code_queue_push_new(local, code);
			code_macro_bind(local, code, pos);
			code_queue_pop(local, code, &pos);
			code_make_execute_control(local, code, pos);
		}
		else {
			code_macro_bind_value(local, code, pos);
		}
	}
}

static void code_macro_bind_init(LocalRoot local, addr code,
		addr var, addr init, addr svar)
{
	addr label, finish;

	/* label */
	code_queue_make_label(local, code, &label);
	code_queue_make_label(local, code, &finish);

	/* if-exists */
	code_queue_if_unbound(local, code, label);
	code_macro_bind_value(local, code, var);
	if (svar != Nil) {
		CodeQueue_single(local, code, T_SET);
		code_macro_bind_value(local, code, svar);
	}
	code_queue_goto(local, code, finish);

	/* if-does-not-exist */
	code_queue_push_label(local, code, label);
	code_make_execute_set(local, code, init);
	code_macro_bind_value(local, code, var);
	if (svar != Nil) {
		CodeQueue_single(local, code, NIL_SET);
		code_macro_bind_value(local, code, svar);
	}

	/* finish */
	code_queue_push_label(local, code, finish);
}

static void code_macro_bind_opt(LocalRoot local, addr code, addr list)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(local, code, POP_UNBOUND);
		code_macro_bind_init(local, code, var, init, svar);
	}
}

static void code_macro_bind_rest(LocalRoot local, addr code, addr list)
{
	if (list != Nil) {
		GetCar(list, &list);
		CodeQueue_single(local, code, REST);
		code_macro_bind_value(local, code, list);
	}
}

static void code_macro_bind_key(LocalRoot local, addr code, addr list)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(local, code, GETF, name);
		code_macro_bind_init(local, code, var, init, svar);
	}
}

static void code_macro_bind_aux(LocalRoot local, addr code, addr list)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		code_make_execute_set(local, code, init);
		code_macro_bind_value(local, code, var);
	}
}

static void code_macro_bind_list(LocalRoot local, addr code, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_var(local, code, var);
	code_macro_bind_opt(local, code, opt);
	if (rest == Nil && key == Nil)
		CodeQueue_single(local, code, REST_NULL);
	code_macro_bind_rest(local, code, rest);
	code_macro_bind_key(local, code, key);
	ordinary_bind_allow(local, code, rest, key, allow);
	code_macro_bind_aux(local, code, aux);
}

static void code_macro_bind_whole(LocalRoot local, addr code, addr pos)
{
	if (pos != Nil) {
		code_macro_bind_value(local, code, pos);
	}
	CodeQueue_single(local, code, WHOLE);
}

static void code_macro_bind(LocalRoot local, addr code, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_whole(local, code, whole);
	code_macro_bind_list(local, code, args);
}

static void code_macro_env(LocalRoot local, addr code, addr env)
{
	if (env != Nil) {
		CodeQueue_single(local, code, MACRO_ENV);
		code_macro_bind_value(local, code, env);
	}
}

static void code_macro_whole(LocalRoot local, addr code, addr whole)
{
	CodeQueue_single(local, code, POP);
	if (whole != Nil) {
		code_macro_bind_value(local, code, whole);
	}
	CodeQueue_single(local, code, MACRO_WHOLE);
}

static void code_macro_args(LocalRoot local, addr code, addr scope)
{
	addr list, var, opt, rest, key, allow, aux, whole, env;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	code_macro_special(local, code, list);

	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_env(local, code, env);
	code_macro_whole(local, code, whole);
	code_macro_bind_list(local, code, list);
}

static void code_macro_function(LocalRoot local, addr code, addr scope)
{
	addr pos;

	code_queue_push_simple(local, code);
	code_lambda_lexical(local, code, scope);
	code_macro_args(local, code, scope);
	code_lambda_body(local, code, scope);
	code_queue_pop(local, code, &pos);
	CodeQueue_cons(local, code, MACRO, pos);
	code_lambda_info(local, code, scope);
	code_queue_ifpush(local, code);
}

static void code_make_macro_lambda(LocalRoot local, addr code, addr scope)
{
	code_macro_function(local, code, scope);
}


/* defmacro */
static void code_make_defmacro(LocalRoot local, addr code, addr scope)
{
	addr name, lambda;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);

	code_make_execute_set(local, code, lambda);
	CodeQueue_cons(local, code, DEFMACRO, name);
	code_queue_ifpush(local, code);
}


/* deftype */
static void code_make_deftype(LocalRoot local, addr code, addr scope)
{
	addr call, doc;

	code_macro_function(local, code, scope);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(local, code, DEFTYPE, call, doc);
	code_queue_ifpush(local, code);
}


/* define-compiler-macro */
static void code_make_define_compiler_macro(LocalRoot local, addr code, addr scope)
{
	addr call, doc;

	code_macro_function(local, code, scope);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(local, code, DEFINE_COMPILER_MACRO, call, doc);
	code_queue_ifpush(local, code);
}


/* destructuring-bind */
static void code_bind_args(LocalRoot local, addr code, addr list)
{
	code_macro_special(local, code, list);
	code_macro_bind(local, code, list);
}

static void code_make_bind_execute(LocalRoot local, addr code, addr scope)
{
	addr expr, args, body, free;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);
	GetEvalScopeIndex(scope, 3, &body);
	GetEvalScopeIndex(scope, 4, &free);

	code_make_execute_set(local, code, expr);
	code_make_free(local, code, free);
	code_bind_args(local, code, args);
	code_allcons(local, code, body);
}

static void code_make_destructuring_bind(LocalRoot local, addr code, addr scope)
{
	addr pos;
	modeswitch mode;

	code_queue_setmode(code, &mode);
	code_queue_push_new(local, code);
	code_make_bind_execute(local, code, scope);
	code_queue_pop(local, code, &pos);
	code_queue_rollback(code, &mode);
	code_make_execute_control(local, code, pos);
}


/* define-symbol-macro */
static void code_make_define_symbol_macro(LocalRoot local, addr code, addr scope)
{
	addr symbol, lambda, body;

	GetEvalScopeIndex(scope, 0, &symbol);
	GetEvalScopeIndex(scope, 1, &lambda);
	GetEvalScopeIndex(scope, 2, &body);
	code_queue_list(local, code,
			CONSTANT_CODE_DEFINE_SYMBOL_MACRO,
			symbol, lambda, body, NULL);
	code_queue_ifpush(local, code);
}


/* flet */
static void code_make_type_function(LocalRoot local, addr code, addr pos)
{
	addr type;

	if (getcheck_tablefunction(pos)) {
		gettype_tablefunction(pos, &type);
		if ((! type_astert_p(type)) && (! type_function_aster_p(type))) {
			CodeQueue_cons(local, code, TYPE_RESULT, type);
		}
	}
}

static void code_make_flet_args(LocalRoot local, addr code, addr args)
{
	addr pos, value;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &value);

		code_lambda_set(local, code, value);
		code_make_type_function(local, code, pos);
		index_heap(&value, getlexical_tablefunction(pos));
		CodeQueue_cons(local, code, SETQ_LEXICAL, value);
	}
}

static void code_make_flet(LocalRoot local, addr code, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	code_make_free(local, code, free);
	code_make_flet_args(local, code, args);
	code_allcons(local, code, body);
}


/* labels */
static void code_make_labels(LocalRoot local, addr code, addr scope)
{
	code_make_flet(local, code, scope);
}


/* values */
static void code_values_set(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		CodeQueue_single(local, code, VALUES_NIL);
		return;
	}

	/* list */
	code_queue_push_new(local, code);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		code_make_execute_push(local, code, pos);
	}
	CodeQueue_single(local, code, VALUES_SET);
	code_queue_pop(local, code, &pos);
	CodeQueue_cons(local, code, EXECUTE_CONTROL_SET, pos);
}

static void code_values_push(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		code_make_nil(local, code, NULL);
		return;
	}

	/* list */
	GetCons(cons, &pos, &cons);
	code_make_execute_push(local, code, pos);
	code_allcons_rem(local, code, cons);
}

static void code_make_values(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			code_values_set(local, code, scope);
			break;

		case CodeQueue_ModePush:
			code_values_push(local, code, scope);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_allcons(local, code, scope);
			break;
	}
}


/* the */
static void code_make_the(LocalRoot local, addr code, addr scope)
{
	addr form, type, check;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &check);
	if (check == Nil) {
		code_make_execute(local, code, form);
		return;
	}

	GetEvalScopeThe(scope, &type);
	if (code_queue_pushp(code)) {
		code_make_execute_push(local, code, form);
		CodeQueue_cons(local, code, THE_PUSH, type);
	}
	else {
		code_make_execute_set(local, code, form);
		CodeQueue_cons(local, code, THE_SET, type);
	}
}


/* eval-when */
static void code_make_eval_when(LocalRoot local, addr code, addr scope)
{
	addr cons, compile, load, exec, toplevel, mode;

	GetEvalScopeIndex(scope, 0, &cons);
	GetEvalScopeIndex(scope, 1, &compile);
	GetEvalScopeIndex(scope, 2, &load);
	GetEvalScopeIndex(scope, 3, &exec);
	GetEvalScopeIndex(scope, 4, &toplevel);
	GetEvalScopeIndex(scope, 5, &mode);

	/* cons */
	code_allcons(local, code, cons);
}


/* locally */
static void code_make_locally(LocalRoot local, addr code, addr scope)
{
	addr cons, free;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);
	code_make_free(local, code, free);
	code_allcons(local, code, cons);
}


/* if */
static int code_if_not_p(addr scope)
{
	addr call, check1, check2;

	if (RefEvalScopeType(scope) != EVAL_PARSE_CALL)
		return 0;
	/* args */
	GetEvalScopeIndex(scope, 1, &call);
	if (! singlep(call))
		return 0;
	/* (not x) or (null x) */
	GetEvalScopeIndex(scope, 0, &call);
	if (RefEvalScopeType(scope) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	if (! symbolp_callname(call))
		return 0;
	GetCallName(call, &call);
	GetConst(COMMON_NOT, &check1);
	GetConst(COMMON_NULL, &check2);

	return call == check1 || call == check2;
}

static void code_if_true(LocalRoot local, addr code, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if expr then else) */
	code_queue_make_label(local, code, &label_else);
	code_queue_make_label(local, code, &label_end);
	code_queue_if_nil(local, code, label_else);
	code_make_execute(local, code, then);
	code_queue_goto(local, code, label_end);
	code_queue_push_label(local, code, label_else);
	if (check)
		code_make_nil(local, code, NULL);
	else
		code_make_execute(local, code, last);
	code_queue_push_label(local, code, label_end);
}

static void code_if_false(LocalRoot local, addr code, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if (not expr) then else) */
	code_queue_make_label(local, code, &label_else);
	code_queue_make_label(local, code, &label_end);
	code_queue_if_t(local, code, label_else);
	code_make_execute(local, code, then);
	code_queue_goto(local, code, label_end);
	code_queue_push_label(local, code, label_else);
	if (check)
		code_make_nil(local, code, NULL);
	else
		code_make_execute(local, code, last);
	code_queue_push_label(local, code, label_end);
}

static void code_make_if(LocalRoot local, addr code, addr scope)
{
	addr expr, then, last;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	code_make_execute_set(local, code, expr);
	if (code_if_not_p(expr))
		code_if_false(local, code, then, last);
	else
		code_if_true(local, code, then, last);
}


/* unwind-protect */
static void code_make_unwind_protect(LocalRoot local, addr code, addr scope)
{
	addr form, cons;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);
	/* cleanup */
	code_queue_push_new(local, code);
	code_allcons_rem(local, code, cons);
	code_queue_pop(local, code, &cons);
	/* protect */
	code_queue_push_new(local, code);
	CodeQueue_cons(local, code, UNWIND_PROTECT, cons);
	code_make_execute_set(local, code, form);
	code_queue_pop(local, code, &form);
	/* set code */
	code_make_execute_control(local, code, form);
}


/* tagbody */
static void code_tagbody_rem(LocalRoot local, addr code, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (RefEvalScopeType(pos) != EVAL_PARSE_TAG)
			code_make_execute_rem(local, code, pos);
	}
}

static void code_tagbody_cons(LocalRoot local, addr code, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (RefEvalScopeType(pos) == EVAL_PARSE_TAG) {
			GetEvalScopeValue(pos, &pos);
			CodeQueue_cons(local, code, TAG, pos);
		}
		else {
			code_make_execute_rem(local, code, pos);
		}
	}
}

static void code_tagbody_body(LocalRoot local, addr code, addr tag, addr cons)
{
	modeswitch mode;

	/* body */
	CodeQueue_cons(local, code, TAGINFO, tag);
	code_tagbody_cons(local, code, cons);
	/* return */
	code_queue_setmode(code, &mode);
	code_make_nil(local, code, NULL);
	code_queue_rollback(code, &mode);
}

static void code_make_tagbody(LocalRoot local, addr code, addr scope)
{
	addr tag, cons;

	/*  code: tagbody
	 *    (code::taginfo tag1 tag2 ...)
	 *    ,@progn...
	 *    nil
	 */
	GetEvalScopeIndex(scope, 0, &tag);
	GetEvalScopeIndex(scope, 1, &cons);
	if (tag == Nil) {
		code_tagbody_rem(local, code, cons);
		code_make_nil(local, code, NULL);
		return;
	}
	/* body */
	code_queue_push_new(local, code);
	code_tagbody_body(local, code, tag, cons);
	code_queue_pop(local, code, &cons);
	/* execute */
	code_make_execute_control(local, code, cons);
}


/* go */
static void code_make_go(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	index_heap(&scope, getlexical_tabletagbody(scope));
	CodeQueue_cons(local, code, GO, scope);
}


/* block */
static void code_make_block(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	if (name == Nil) {
		code_allcons(local, code, cons);
		return;
	}
	code_queue_push_new(local, code);
	CodeQueue_cons(local, code, BLOCKINFO, name);
	code_allcons_set(local, code, cons);
	code_queue_pop(local, code, &cons);
	code_make_execute_control(local, code, cons);
}


/* return-from */
static void code_make_return_from(LocalRoot local, addr code, addr scope)
{
	addr pos, form;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &form);
	/* form */
	code_make_execute_set(local, code, form);
	/* name */
	index_heap(&pos, getlexical_tableblock(pos));
	CodeQueue_cons(local, code, RETURN_FROM, pos);
}


/* catch */
static void code_make_catch(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	code_queue_push_new(local, code);
	code_make_execute_set(local, code, name);
	CodeQueue_single(local, code, CATCH);
	code_allcons_set(local, code, cons);
	code_queue_pop(local, code, &cons);
	code_make_execute_control(local, code, cons);
}


/* throw */
static void code_make_throw(LocalRoot local, addr code, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	code_queue_push_new(local, code);
	code_make_execute_push(local, code, name);
	code_make_execute_set(local, code, form);
	CodeQueue_single(local, code, THROW);
	code_queue_pop(local, code, &form);
	CodeQueue_cons(local, code, EXECUTE_CONTROL_SET, form);
}


/* multiple-value-bind */
static void code_make_multiple_value_bind_index(
		LocalRoot local, addr code, size_t i, addr pos)
{
	addr index, value;

	index_heap(&index, i);
	/* type */
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_double(local, code, BIND1_TYPE, index, value);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_double(local, code, BIND1_SPECIAL, index, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_double(local, code, BIND1_LEXICAL, index, value);
	}
}

static void code_make_multiple_value_bind_list(LocalRoot local, addr code, addr pos)
{
	addr value;

	/* type */
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(local, code, BIND2_TYPE, value);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(local, code, BIND2_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(local, code, BIND2_LEXICAL, value);
	}
}

static void code_make_multiple_value_bind_args(LocalRoot local, addr code, addr list)
{
	addr pos;
	size_t i;

	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);

		if (i < EXECUTE_VALUES)
			code_make_multiple_value_bind_index(local, code, i, pos);
		else
			code_make_multiple_value_bind_list(local, code, pos);
	}
}

static void code_make_multiple_value_bind_execute(LocalRoot local, addr code, addr scope)
{
	addr args, expr, cons, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);

	code_make_execute_set(local, code, expr);
	code_make_free(local, code, free);
	code_make_multiple_value_bind_args(local, code, args);
	code_allcons(local, code, cons);
}

static void code_make_multiple_value_bind(LocalRoot local, addr code, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 6, &pos); /* allocate */
	if (pos == Nil) {
		code_make_multiple_value_bind_execute(local, code, scope);
		return;
	}

	code_queue_setmode(code, &mode);
	code_queue_push_new(local, code);
	code_make_multiple_value_bind_execute(local, code, scope);
	code_queue_pop(local, code, &pos);
	code_queue_rollback(code, &mode);
	code_make_execute_control(local, code, pos);
}


/* multiple-value-call */
static void code_make_multiple_value_call(LocalRoot local, addr code, addr scope)
{
	addr call, args, pos;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	/* call */
	code_make_execute_push(local, code, call);
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		code_make_execute_set(local, code, pos);
		CodeQueue_single(local, code, PUSH_VALUES);
	}
	/* call */
	CodeQueue_single(local, code, FUNCALL);
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}


/* multiple-value-prog1 */
static void code_make_multiple_value_prog1(LocalRoot local, addr code, addr scope)
{
	addr expr, cons;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &cons);

	/* expr */
	code_make_execute(local, code, expr);
	/* cons */
	code_queue_push_new(local, code);
	code_allcons_rem(local, code, cons);
	code_queue_pop(local, code, &cons);
	CodeQueue_cons(local, code, EXECUTE_CONTROL_SAVE, cons);
}


/* nth-value */
static void code_make_nth_value(LocalRoot local, addr code, addr scope)
{
	addr nth, expr, pos;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);
	code_queue_push_new(local, code);
	code_make_execute_push(local, code, nth);
	code_make_execute_set(local, code, expr);
	CodeQueue_single(local, code, NTH_VALUE);
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}


/* progv */
static void code_make_progv(LocalRoot local, addr code, addr scope)
{
	addr symbols, values, body, pos;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);
	code_queue_push_new(local, code);
	code_make_execute_push(local, code, symbols);
	code_make_execute_push(local, code, values);
	CodeQueue_single(local, code, PROGV);
	code_allcons_set(local, code, body);
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}


/* load-time-value */
static void code_make_load_time_value_bind(LocalRoot local, addr code, addr scope)
{
	addr expr, value;

	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &value);
	code_make_execute_set(local, code, expr);
	CodeQueue_cons(local, code, LOAD_TIME_VALUE_BIND, value);
}

static void code_make_load_time_value_init(LocalRoot local, addr code, addr scope)
{
	addr init, value, pos;

	GetEvalScopeIndex(scope, 3, &init);
	GetEvalScopeIndex(scope, 4, &value);
	if (init == Nil)
		return;

	/* call (lambda . argument) */
	code_queue_push_new(local, code);
	code_make_execute_set(local, code, init);
	CodeQueue_cons(local, code, LOAD_TIME_VALUE_INIT, value);
	CodeQueue_single(local, code, CALL_RESULT);
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}

static void code_make_load_time_value_body(LocalRoot local, addr code, addr scope)
{
	addr check, expr, list, loop, pos;

	GetEvalScopeIndex(scope, 0, &check);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 2, &list);
	Check(check == Nil, "check error");

	code_queue_push_new(local, code);
	/* bind */
	for (loop = list; loop != Nil; ) {
		GetCons(loop, &pos, &loop);
		code_make_load_time_value_bind(local, code, pos);
	}
	/* init */
	for (loop = list; loop != Nil; ) {
		GetCons(loop, &pos, &loop);
		code_make_load_time_value_init(local, code, pos);
	}
	/* body */
	code_make_execute(local, code, expr);
	/* result */
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}

static void code_make_load_time_value_expr(LocalRoot local, addr code, addr scope)
{
	addr check, value;

	GetEvalScopeIndex(scope, 0, &check);
	GetEvalScopeIndex(scope, 4, &value);
	Check(check != Nil, "check error");

	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, LOAD_TIME_VALUE_SET, value);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, LOAD_TIME_VALUE_PUSH, value);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

static void code_make_load_time_value(LocalRoot local, addr code, addr scope)
{
	addr check;

	GetEvalScopeIndex(scope, 0, &check);
	if (check != Nil)
		code_make_load_time_value_body(local, code, scope);
	else
		code_make_load_time_value_expr(local, code, scope);
}


/*
 *  step
 */
static void code_make_step(LocalRoot local, addr code, addr scope)
{
	addr expr, value;

	/* scope */
	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &value);

	/* code */
	code_queue_push_new(local, code);
	code_make_execute(local, code, expr);
	code_queue_pop(local, code, &expr);
	CodeQueue_double(local, code, STEP, expr, value);
	code_queue_ifpush(local, code);
}


/*
 *  specialized call
 */
static int code_make_specialize_common_p(addr call)
{
	addr common;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	if (RefCallNameType(call) != CALLNAME_SYMBOL)
		return 0;
	GetCallName(call, &call);
	GetPackageSymbol(call, &call);
	GetConst(PACKAGE_COMMON_LISP, &common);

	return call == common;
}

static int code_make_specialize_symbol_p(addr call, constindex index)
{
	addr left, right;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetConstant(index, &left);
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	GetCallName(call, &right);
	return RefCallNameType(call) == CALLNAME_SYMBOL && left == right;
}

static void code_make_specialize_allcons(LocalRoot local, addr code, addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		code_make_execute_set(local, code, pos);
	}
}

static int code_make_specialize_handler(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	code_make_specialize_allcons(local, code, args);
	code_queue_pop(local, code, &args);
	code_make_execute_control(local, code, args);

	return 1;
}

static int code_make_specialize_restart(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	code_make_specialize_allcons(local, code, args);
	code_queue_pop(local, code, &args);
	code_make_execute_control(local, code, args);

	return 1;
}

static int code_make_specialize_push_return(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	code_make_specialize_allcons(local, code, args);
	code_queue_pop(local, code, &args);
	code_make_execute_control(local, code, args);

	return 1;
}

static int code_make_specialize_type(
		LocalRoot local, addr code, addr scope, constindex type)
{
	addr args, pos;

	GetEvalScopeIndex(scope, 1, &args);
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		code_make_execute_push(local, code, pos);
	}
	code_queue_single(local, code, type);

	return 1;
}

static int code_make_specialize_handler_bind(LocalRoot local, addr code, addr scope)
{
	return code_make_specialize_type(local, code, scope, CONSTANT_CODE_HANDLER_BIND);
}

static int code_make_specialize_handler_case(LocalRoot local, addr code, addr scope)
{
	return code_make_specialize_type(local, code, scope, CONSTANT_CODE_HANDLER_CASE);
}

static int code_make_specialize_restart_bind(LocalRoot local, addr code, addr scope)
{
	return code_make_specialize_type(local, code, scope, CONSTANT_CODE_RESTART_BIND);
}

static int code_make_specialize_restart_case(LocalRoot local, addr code, addr scope)
{
	return code_make_specialize_type(local, code, scope, CONSTANT_CODE_RESTART_CASE);
}

static int code_make_specialize(LocalRoot local, addr code, addr scope)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call);

	/* common-lisp */
	if (code_make_specialize_common_p(call))
		return optimize_common(local, code, scope);

	/* lisp-system::handler */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER))
		return code_make_specialize_handler(local, code, scope);

	/* lisp-system::restart */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART))
		return code_make_specialize_restart(local, code, scope);

	/* lisp-system::push-return */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_PUSH_RETURN))
		return code_make_specialize_push_return(local, code, scope);

	/* lisp-system::handler-bind */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER_BIND))
		return code_make_specialize_handler_bind(local, code, scope);

	/* lisp-system::handler-case */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER_CASE))
		return code_make_specialize_handler_case(local, code, scope);

	/* lisp-system::restart-bind */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART_BIND))
		return code_make_specialize_restart_bind(local, code, scope);

	/* lisp-system::restart-case */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART_CASE))
		return code_make_specialize_restart_case(local, code, scope);

	/* lisp-system::optimize-check */
	if (code_make_specialize_symbol_p(call, CONSTANT_SYSTEM_OPTIMIZE_CHECK))
		return optimize_check_code(local, code, scope);

	return 0;
}


/*
 *  call
 */
static void code_make_call_args_push(LocalRoot local, addr code, addr pos)
{
	addr value;

	getvalue_tablecall(pos, &value);
	code_make_execute_push(local, code, value);
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(local, code, CALL_TYPE, value);
		}
	}
}

static void code_make_call_args_type(LocalRoot local, addr code, addr args)
{
	addr pos;

	while (consp_getcons(args, &pos, &args))
		code_make_call_args_push(local, code, pos);
}

static void code_make_call_args_count(LocalRoot local, addr code,
		addr list, addr args, addr *ret)
{
	addr pos;

	while (list != Nil) {
		GetCdr(list, &list);
		if (! consp_getcons(args, &pos, &args))
			break;
		code_make_call_args_push(local, code, pos);
	}
	*ret = args;
}

static int code_make_call_type(addr pos, addr *ret)
{
	GetEvalScopeThe(pos, &pos);
	if (! type_function_p(pos))
		return 1;
	GetArrayType(pos, 0, &pos); /* args */
	if (type_asterisk_p(pos))
		return 1;
	*ret = pos;
	return 0;
}

static void code_make_call_args_value(LocalRoot local, addr code, addr pos, addr key)
{
	addr value;
	modeswitch mode;

	getvalue_tablecall(pos, &value);

	/* setmode */
	code_queue_setmode(code, &mode);
	code_make_execute(local, code, value);
	code_queue_rollback(code, &mode);

	/* type check */
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(local, code, TYPE_RESULT, value);
		}
	}

	/* key */
	CodeQueue_cons(local, code, CALL_KEY, key);

	/* push */
	CodeQueue_single(local, code, PUSH_RESULT);
}

static void code_make_call_args_key(LocalRoot local, addr code, addr key, addr args)
{
	int kv;
	addr pos;

	for (kv = 0; args != Nil; kv = ! kv) {
		GetCons(args, &pos, &args);
		if (kv == 0) {
			code_make_call_args_push(local, code, pos);
		}
		else {
			code_make_call_args_value(local, code, pos, key);
		}
	}
}

static void code_make_call_args(LocalRoot local, addr code, addr first, addr args)
{
	addr var, opt, key;

	/* type */
	if (code_make_call_type(first, &first)) {
		code_make_call_args_type(local, code, args);
		return;
	}
	GetArrayA2(first, 0, &var);
	GetArrayA2(first, 1, &opt);
	GetArrayA2(first, 3, &key);

	/* var, &optional */
	code_make_call_args_count(local, code, var, args, &args);
	code_make_call_args_count(local, code, opt, args, &args);
	if (! consp(args))
		return;

	/* not key */
	if (! consp(key)) {
		code_make_call_args_type(local, code, args);
		return;
	}

	/* key */
	code_make_call_args_key(local, code, key, args);
}

static void code_make_call_global(LocalRoot local, addr code, addr pos)
{
	addr symbol;

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_cons(local, code, CALL_FUNCTION, symbol);
	}
	else {
		CodeQueue_cons(local, code, CALL_SETF, symbol);
	}
}

static void code_make_call_lexical(LocalRoot local, addr code, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	CodeQueue_cons(local, code, CALL_LEXICAL, pos);
}

static void code_make_call_function(LocalRoot local, addr code, addr table)
{
	if (getglobalp_tablefunction(table))
		code_make_call_global(local, code, table);
	else
		code_make_call_lexical(local, code, table);
}

static void code_make_call_first(LocalRoot local, addr code, addr pos)
{
	addr table;

	if (RefEvalScopeType(pos) == EVAL_PARSE_FUNCTION) {
		GetEvalScopeValue(pos, &table);
		code_make_call_function(local, code, table);
	}
	else {
		code_make_execute_set(local, code, pos);
		CodeQueue_single(local, code, CALL_RESULT);
	}
}

static void code_make_call(LocalRoot local, addr code, addr scope)
{
	addr first, args, pos;

	if (code_make_specialize(local, code, scope))
		return;
	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	/* args -> first */
	code_make_call_args(local, code, first, args);
	code_make_call_first(local, code, first);
	/* execute */
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);
}


/*
 *  code
 */
typedef void (*code_make_calltype)(LocalRoot, addr, addr);
static code_make_calltype CodeMakeTable[EVAL_PARSE_SIZE];

void code_make_execute(LocalRoot local, addr code, addr scope)
{
	EvalParse type;
	code_make_calltype call;

	Check(! eval_scope_p(scope), "type error");
	GetEvalScopeType(scope, &type);
	call = CodeMakeTable[type];
	if (call == NULL)
		Abort("Invalid scope type.");
	(*call)(local, code, scope);
}

void code_make(LocalRoot local, addr *ret, addr scope)
{
	addr code;
	LocalStack stack;

	push_local(local, &stack);
	code_queue_local(local, &code);
	code_make_execute_set(local, code, scope);
	code_queue_pop(local, code, ret);
	rollback_local(local, stack);
}

void init_code_make(void)
{
	CodeMakeTable[EVAL_PARSE_NIL] = code_make_nil;
	CodeMakeTable[EVAL_PARSE_T] = code_make_t;
	CodeMakeTable[EVAL_PARSE_TYPE] = code_make_value;
	CodeMakeTable[EVAL_PARSE_CLOS] = code_make_value;
	CodeMakeTable[EVAL_PARSE_INTEGER] = code_make_value;
	CodeMakeTable[EVAL_PARSE_RATIONAL] = code_make_value;
	CodeMakeTable[EVAL_PARSE_COMPLEX] = code_make_value;
	CodeMakeTable[EVAL_PARSE_CHARACTER] = code_make_value;
	CodeMakeTable[EVAL_PARSE_ARRAY] = code_make_value;
	CodeMakeTable[EVAL_PARSE_VECTOR] = code_make_value;
	CodeMakeTable[EVAL_PARSE_BITVECTOR] = code_make_value;
	CodeMakeTable[EVAL_PARSE_STRING] = code_make_value;
	CodeMakeTable[EVAL_PARSE_SYMBOL] = code_make_symbol;
	CodeMakeTable[EVAL_PARSE_FLOAT] = code_make_value;
	CodeMakeTable[EVAL_PARSE_DECLAIM] = code_make_declaim;
	CodeMakeTable[EVAL_PARSE_RANDOM_STATE] = code_make_value;
	CodeMakeTable[EVAL_PARSE_PATHNAME] = code_make_value;
	CodeMakeTable[EVAL_PARSE_ENVIRONMENT] = code_make_value;
	CodeMakeTable[EVAL_PARSE_LEXICAL] = code_make_lexical;
	CodeMakeTable[EVAL_PARSE_PROGN] = code_make_progn;
	CodeMakeTable[EVAL_PARSE_LET] = code_make_let;
	CodeMakeTable[EVAL_PARSE_LETA] = code_make_leta;
	CodeMakeTable[EVAL_PARSE_SETQ] = code_make_setq;
	CodeMakeTable[EVAL_PARSE_DEFUN] = code_make_defun;
	CodeMakeTable[EVAL_PARSE_DEFMACRO] = code_make_defmacro;
	CodeMakeTable[EVAL_PARSE_MACRO_LAMBDA] = code_make_macro_lambda;
	CodeMakeTable[EVAL_PARSE_DEFTYPE] = code_make_deftype;
	CodeMakeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = code_make_define_compiler_macro;
	CodeMakeTable[EVAL_PARSE_DESTRUCTURING_BIND] = code_make_destructuring_bind;
	CodeMakeTable[EVAL_PARSE_DEFINE_SYMBOL_MACRO] = code_make_define_symbol_macro;
	CodeMakeTable[EVAL_PARSE_QUOTE] = code_make_value;
	CodeMakeTable[EVAL_PARSE_FUNCTION] = code_make_function;
	CodeMakeTable[EVAL_PARSE_LAMBDA] = code_make_lambda;
	CodeMakeTable[EVAL_PARSE_IF] = code_make_if;
	CodeMakeTable[EVAL_PARSE_UNWIND_PROTECT] = code_make_unwind_protect;
	CodeMakeTable[EVAL_PARSE_TAGBODY] = code_make_tagbody;
	CodeMakeTable[EVAL_PARSE_GO] = code_make_go;
	CodeMakeTable[EVAL_PARSE_BLOCK] = code_make_block;
	CodeMakeTable[EVAL_PARSE_RETURN_FROM] = code_make_return_from;
	CodeMakeTable[EVAL_PARSE_CATCH] = code_make_catch;
	CodeMakeTable[EVAL_PARSE_THROW] = code_make_throw;
	CodeMakeTable[EVAL_PARSE_FLET] = code_make_flet;
	CodeMakeTable[EVAL_PARSE_LABELS] = code_make_labels;
	CodeMakeTable[EVAL_PARSE_THE] = code_make_the;
	CodeMakeTable[EVAL_PARSE_EVAL_WHEN] = code_make_eval_when;
	CodeMakeTable[EVAL_PARSE_VALUES] = code_make_values;
	CodeMakeTable[EVAL_PARSE_LOCALLY] = code_make_locally;
	CodeMakeTable[EVAL_PARSE_CALL] = code_make_call;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = code_make_multiple_value_bind;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = code_make_multiple_value_call;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = code_make_multiple_value_prog1;
	CodeMakeTable[EVAL_PARSE_NTH_VALUE] = code_make_nth_value;
	CodeMakeTable[EVAL_PARSE_PROGV] = code_make_progv;
	CodeMakeTable[EVAL_PARSE_LOAD_TIME_VALUE] = code_make_load_time_value;
	CodeMakeTable[EVAL_PARSE_STEP] = code_make_step;
}

