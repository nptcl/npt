#include "callname.h"
#include "compile_file.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "declare.h"
#include "eval_object.h"
#include "eval_table.h"
#include "function.h"
#include "load_instance.h"
#include "load_object.h"
#include "load_time_value.h"
#include "make.h"
#include "make_queue.h"
#include "make_value.h"
#include "scope_object.h"
#include "strvect.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "symbol.h"
#include "type.h"

/* nil */
int code_make_nil_(CodeMake ptr, addr ignore)
{
	code_make_single(ptr, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
	return 0;
}


/* t */
int code_make_t_(CodeMake ptr, addr ignore)
{
	code_make_single(ptr, CONSTANT_CODE_T_SET, CONSTANT_CODE_T_PUSH);
	return 0;
}


/* value */
int code_make_value_(CodeMake ptr, addr scope)
{
	CheckTypeCodeQueue(ptr->code);
	GetEvalScopeValue(scope, &scope);
	code_make_object(ptr, scope);

	return 0;
}


/* symbol */
static int code_symbol_special_p(addr pos)
{
	return getspecialp_tablevalue(pos) || getglobalp_tablevalue(pos);
}

static void code_symbol_set(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_SET, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_SET, pos);
	}
}

static void code_symbol_push(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_PUSH, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_PUSH, pos);
	}
}

static void code_symbol_remove(CodeMake ptr, addr pos, addr table)
{
	if (code_symbol_special_p(table)) {
		CodeQueue_cons(ptr, SPECIAL_REM, pos);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_REM, pos);
	}
}

int code_make_symbol_(CodeMake ptr, addr scope)
{
	addr symbol, table;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol))
		return code_make_value_(ptr, scope);

	/* symbol */
	GetEvalScopeIndex(scope, 0, &table);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			code_symbol_set(ptr, symbol, table);
			break;

		case CodeQueue_ModePush:
			code_symbol_push(ptr, symbol, table);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_symbol_remove(ptr, symbol, table);
			break;
	}

	return 0;
}


/* declaim */
static void code_declaim_special(CodeMake ptr, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(ptr, DECLAIM_SPECIAL, pos);
	}
}

static void code_declaim_type_value(CodeMake ptr, addr cons)
{
	addr key, value;

	getall_type_value_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(ptr, DECLAIM_TYPE_VALUE, key, value);
	}
}

static void code_declaim_type_function(CodeMake ptr, addr cons)
{
	addr key, value;

	getall_type_function_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		CodeQueue_double(ptr, DECLAIM_TYPE_FUNCTION, key, value);
	}
}

static void code_declaim_inline(CodeMake ptr, addr cons)
{
	addr key, value, check1, check2;

	getall_inline_declare(cons, &cons);
	GetConst(COMMON_INLINE, &check1);
	GetConst(COMMON_NOTINLINE, &check2);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (value == check1)
			CodeQueue_cons(ptr, DECLAIM_INLINE, key);
		if (value == check2)
			CodeQueue_cons(ptr, DECLAIM_NOTINLINE, key);
	}
}

static void code_declaim_optimize(CodeMake ptr, addr declare)
{
	addr pos;
	OptimizeType optimize;

	/* compilation-speed */
	optimize = get_optimize_compilation_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_COMPILATION, pos);
	}
	/* debug */
	optimize = get_optimize_debug_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_DEBUG, pos);
	}
	/* safety */
	optimize = get_optimize_safety_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SAFETY, pos);
	}
	/* space */
	optimize = get_optimize_space_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SPACE, pos);
	}
	/* speed */
	optimize = get_optimize_speed_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		CodeQueue_cons(ptr, DECLAIM_SPEED, pos);
	}
}

static void code_declaim_declaration(CodeMake ptr, addr cons)
{
	addr pos;

	getall_declaration_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(ptr, DECLAIM_DECLARATION, pos);
	}
}

int code_make_declaim_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_declaim_special(ptr, scope);
	code_declaim_type_value(ptr, scope);
	code_declaim_type_function(ptr, scope);
	code_declaim_inline(ptr, scope);
	code_declaim_optimize(ptr, scope);
	code_declaim_declaration(ptr, scope);
	return code_make_nil_(ptr, NULL);
}


/*
 *  lexical
 */
int code_make_lexical_(CodeMake ptr, addr scope)
{
	addr list, pos;

	GetEvalScopeIndex(scope, 0, &list);
	GetEvalScopeValue(scope, &pos);
	if (list == Nil) {
		Return(code_make_execute_(ptr, pos));
	}
	else {
		code_queue_push_new(ptr);
		CodeQueue_cons(ptr, LEXICAL, list);
		Return(code_make_execute_(ptr, pos));
		code_queue_pop(ptr, &pos);
		code_make_execute_control(ptr, pos);
	}

	return 0;
}


/*
 *  progn
 */
static int code_allcons_(CodeMake ptr, addr cons)
{
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* butlast */
	code_queue_remmode(ptr, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil)
			break;
		Return(code_make_execute_(ptr, pos));
	}
	code_queue_rollback(ptr, &mode);

	/* last */
	return code_make_execute_(ptr, pos);
}

static int code_allcons_set_(CodeMake ptr, addr cons)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_allcons_(ptr, cons));
	code_queue_rollback(ptr, &mode);

	return 0;
}

static int code_allcons_rem_(CodeMake ptr, addr cons)
{
	addr pos;
	modeswitch mode;

	code_queue_remmode(ptr, &mode);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Return(code_make_execute_(ptr, pos));
	}
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_progn_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	return code_allcons_(ptr, scope);
}


/* let */
static void code_make_free_value(CodeMake ptr, addr pos, addr type)
{
	if (type_astert_p(type))
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_SPECIAL, pos, type);
	}
	else if (getglobalp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_GLOBAL, pos, type);
	}
	else {
		/* lexical */
		index_heap(&pos, getlexical_tablevalue(pos));
		CodeQueue_double(ptr, TYPE_LEXICAL, pos, type);
	}
}

static void code_make_free_function(CodeMake ptr, addr pos, addr type)
{
	addr symbol;

	if (type_astert_p(type) || type_function_aster_p(type))
		return;
	if (! getglobalp_tablefunction(pos)) {
		/* lexical */
		index_heap(&pos, getlexical_tablefunction(pos));
		CodeQueue_double(ptr, TYPE_LEXICAL, pos, type);
		return;
	}

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_double(ptr, TYPE_FUNCTION, symbol, type);
	}
	else {
		CodeQueue_double(ptr, TYPE_SETF, symbol, type);
	}
}

static int code_make_free_(CodeMake ptr, addr list)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			code_make_free_value(ptr, pos, type);
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			code_make_free_function(ptr, pos, type);
			continue;
		}

		return fmte_("Invalid type object in code-make.", NULL);
	}

	return 0;
}

static void code_make_type_value(CodeMake ptr, addr pos)
{
	addr type;

	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &type);
		if (! type_astert_p(type)) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
		}
	}
}

static int code_make_let_args_(CodeMake ptr, addr args)
{
	addr list, pos, value, index;

	/* value */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_make_type_value(ptr, pos);
		index_heap(&value, getlet_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}

	/* bind */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);

		index_heap(&index, getlet_tablevalue(pos));
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_double(ptr, LET_SPECIAL, index, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_double(ptr, LET_LEXICAL, index, value);
		}
	}

	return 0;
}

static int code_make_let_execute_(CodeMake ptr, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	Return(code_make_free_(ptr, free));
	Return(code_make_let_args_(ptr, args));
	Return(code_allcons_(ptr, body));

	return 0;
}

int code_make_let_(CodeMake ptr, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 4, &pos); /* allocate */
	if (pos == Nil)
		return code_make_let_execute_(ptr, scope);

	code_queue_setmode(ptr, &mode);
	code_queue_push_new(ptr);
	Return(code_make_let_execute_(ptr, scope));
	code_queue_pop(ptr, &pos);
	code_queue_rollback(ptr, &mode);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* let* */
static int code_make_leta_args_(CodeMake ptr, addr list)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_make_type_value(ptr, pos);
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &value);
			CodeQueue_cons(ptr, LETA_SPECIAL, value);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(ptr, SETQ_LEXICAL, value);
		}
	}

	return 0;
}

static int code_make_leta_execute_(CodeMake ptr, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	Return(code_make_free_(ptr, free));
	Return(code_make_leta_args_(ptr, args));
	Return(code_allcons_(ptr, body));

	return 0;
}

int code_make_leta_(CodeMake ptr, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 4, &pos); /* allocate */
	if (pos == Nil)
		return code_make_leta_execute_(ptr, scope);

	code_queue_setmode(ptr, &mode);
	code_queue_push_new(ptr);
	Return(code_make_leta_execute_(ptr, scope));
	code_queue_pop(ptr, &pos);
	code_queue_rollback(ptr, &mode);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* setq */
static int code_setq_loop_(CodeMake ptr, addr list)
{
	addr pos, value, symbol;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);
		Return(code_make_execute_set_(ptr, value));

		getname_tablevalue(pos, &symbol);
		code_make_type_value(ptr, pos);
		if (getspecialp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_SPECIAL, symbol);
		}
		else if (getglobalp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_GLOBAL, symbol);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(ptr, SETQ_LEXICAL, value);
		}
	}

	return 0;
}

int code_make_setq_(CodeMake ptr, addr scope)
{
	addr list;

	/* nil */
	GetEvalScopeValue(scope, &list);
	if (list == Nil)
		return code_make_nil_(ptr, NULL);

	/* setq */
	Return(code_setq_loop_(ptr, list));
	code_queue_ifpush(ptr);

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
			break;

		case CodeQueue_ModePush:
			index = ConstantCode(symbolp, FUNCTION_PUSH, SETF_PUSH);
			code_queue_cons(ptr, index, symbol);
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
static void code_ordinary_bind_value(CodeMake ptr, addr pos)
{
	addr value;

	code_make_type_value(ptr, pos);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, LETA_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static void code_ordinary_bind_var(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		code_ordinary_bind_value(ptr, pos);
	}
}

static int code_ordinary_bind_init_(CodeMake ptr, addr var, addr init, addr svar)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	code_ordinary_bind_value(ptr, var);
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_ordinary_bind_value(ptr, svar);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_ordinary_bind_value(ptr, var);
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_ordinary_bind_value(ptr, svar);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_ordinary_bind_opt_(CodeMake ptr, addr list)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		Return(code_ordinary_bind_init_(ptr, var, init, svar));
	}

	return 0;
}

static void code_ordinary_bind_rest(CodeMake ptr, addr pos)
{
	if (pos != Nil) {
		CodeQueue_single(ptr, REST);
		code_ordinary_bind_value(ptr, pos);
	}
}

static int code_ordinary_bind_key_(CodeMake ptr, addr list)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		Return(code_ordinary_bind_init_(ptr, var, init, svar));
	}

	return 0;
}

static void code_ordinary_bind_allow(CodeMake ptr, addr rest, addr list, addr allow)
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
		CodeQueue_cons(ptr, ALLOW_OTHER_KEYS, keys);
}

static int code_ordinary_bind_aux_(CodeMake ptr, addr list)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_ordinary_bind_value(ptr, var);
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

static int code_lambda_args_(CodeMake ptr, addr scope)
{
	addr list, var, opt, rest, key, allow, aux;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	code_ordinary_bind_var(ptr, var);
	Return(code_ordinary_bind_opt_(ptr, opt));
	code_ordinary_bind_rest(ptr, rest);
	if (rest == Nil && key == Nil)
		CodeQueue_cons(ptr, REST_NULL, allow);
	Return(code_ordinary_bind_key_(ptr, key));
	code_ordinary_bind_allow(ptr, rest, key, allow);
	Return(code_ordinary_bind_aux_(ptr, aux));

	return 0;
}

static int code_lambda_body_(CodeMake ptr, addr scope)
{
	addr list;
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	return code_allcons_set_(ptr, list);
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
	addr pos;

	code_queue_push_simple(ptr);
	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope));
	Return(code_lambda_body_(ptr, scope));
	code_queue_pop(ptr, &pos);
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
	Return(code_lambda_set_(ptr, scope));
	CodeQueue_single(ptr, DEFUN);
	code_queue_ifpush(ptr);

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

static int code_macro_bind_(CodeMake, addr);
static void code_macro_bind_value(CodeMake ptr, addr pos)
{
	addr value;

	code_make_type_value(ptr, pos);
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, SETQ_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}
}

static int code_macro_bind_var_(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CodeQueue_single(ptr, POP);
		if (consp(pos)) {
			code_queue_push_new(ptr);
			Return(code_macro_bind_(ptr, pos));
			code_queue_pop(ptr, &pos);
			code_make_execute_control(ptr, pos);
		}
		else {
			code_macro_bind_value(ptr, pos);
		}
	}

	return 0;
}

static int code_macro_bind_init_(CodeMake ptr, addr var, addr init, addr svar)
{
	addr label, finish;

	/* label */
	code_queue_make_label(ptr, &label);
	code_queue_make_label(ptr, &finish);

	/* if-exists */
	code_queue_if_unbound(ptr, label);
	code_macro_bind_value(ptr, var);
	if (svar != Nil) {
		CodeQueue_single(ptr, T_SET);
		code_macro_bind_value(ptr, svar);
	}
	code_queue_goto(ptr, finish);

	/* if-does-not-exist */
	code_queue_push_label(ptr, label);
	Return(code_make_execute_set_(ptr, init));
	code_macro_bind_value(ptr, var);
	if (svar != Nil) {
		CodeQueue_single(ptr, NIL_SET);
		code_macro_bind_value(ptr, svar);
	}

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_macro_bind_opt_(CodeMake ptr, addr list)
{
	addr pos, var, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &svar, NULL);
		CodeQueue_single(ptr, POP_UNBOUND);
		Return(code_macro_bind_init_(ptr, var, init, svar));
	}

	return 0;
}

static void code_macro_bind_rest(CodeMake ptr, addr list)
{
	if (list != Nil) {
		GetCar(list, &list);
		CodeQueue_single(ptr, REST);
		code_macro_bind_value(ptr, list);
	}
}

static int code_macro_bind_key_(CodeMake ptr, addr list)
{
	addr pos, var, name, init, svar;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &name, &init, &svar, NULL);
		CodeQueue_cons(ptr, GETF, name);
		Return(code_macro_bind_init_(ptr, var, init, svar));
	}

	return 0;
}

static int code_macro_bind_aux_(CodeMake ptr, addr list)
{
	addr pos, var, init;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, NULL);
		Return(code_make_execute_set_(ptr, init));
		code_macro_bind_value(ptr, var);
	}

	return 0;
}

static int code_macro_bind_list_(CodeMake ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(code_macro_bind_var_(ptr, var));
	Return(code_macro_bind_opt_(ptr, opt));
	if (rest == Nil && key == Nil)
		CodeQueue_cons(ptr, REST_NULL, allow);
	code_macro_bind_rest(ptr, rest);
	Return(code_macro_bind_key_(ptr, key));
	code_ordinary_bind_allow(ptr, rest, key, allow);
	Return(code_macro_bind_aux_(ptr, aux));

	return 0;
}

static void code_macro_bind_whole(CodeMake ptr, addr pos)
{
	if (pos != Nil) {
		code_macro_bind_value(ptr, pos);
	}
	CodeQueue_single(ptr, WHOLE);
}

static int code_macro_bind_(CodeMake ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_whole(ptr, whole);
	Return(code_macro_bind_list_(ptr, args));

	return 0;
}

static void code_macro_env(CodeMake ptr, addr env)
{
	if (env != Nil) {
		CodeQueue_single(ptr, MACRO_ENV);
		code_macro_bind_value(ptr, env);
	}
}

static void code_macro_whole(CodeMake ptr, addr whole)
{
	CodeQueue_single(ptr, POP);
	if (whole != Nil) {
		code_macro_bind_value(ptr, whole);
	}
	CodeQueue_single(ptr, MACRO_WHOLE);
}

static int code_macro_args_(CodeMake ptr, addr scope)
{
	addr list, var, opt, rest, key, allow, aux, whole, env;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	code_macro_special(ptr, list);

	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_env(ptr, env);
	code_macro_whole(ptr, whole);
	Return(code_macro_bind_list_(ptr, list));

	return 0;
}

static int code_macro_function_(CodeMake ptr, addr scope)
{
	addr pos;

	code_queue_push_simple(ptr);

	code_lambda_lexical(ptr, scope);
	Return(code_macro_args_(ptr, scope));
	Return(code_lambda_body_(ptr, scope));
	code_queue_pop(ptr, &pos);
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
	addr name, lambda;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);

	Return(code_make_execute_set_(ptr, lambda));
	CodeQueue_cons(ptr, DEFMACRO, name);
	code_queue_ifpush(ptr);

	return 0;
}


/* deftype */
int code_make_deftype_(CodeMake ptr, addr scope)
{
	addr call, doc;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFTYPE, call, doc);
	code_queue_ifpush(ptr);

	return 0;
}


/* define-compiler-macro */
int code_make_define_compiler_macro_(CodeMake ptr, addr scope)
{
	addr call, doc;

	Return(code_macro_function_(ptr, scope));
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	CodeQueue_double(ptr, DEFINE_COMPILER_MACRO, call, doc);
	code_queue_ifpush(ptr);

	return 0;
}


/* destructuring-bind */
static int code_bind_args_(CodeMake ptr, addr list)
{
	code_macro_special(ptr, list);
	return code_macro_bind_(ptr, list);
}

static int code_make_bind_execute_(CodeMake ptr, addr scope)
{
	addr expr, args, body, free, pos;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);
	GetEvalScopeIndex(scope, 3, &body);
	GetEvalScopeIndex(scope, 4, &free);

	GetConst(COMMON_DESTRUCTURING_BIND, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	Return(code_make_execute_set_(ptr, expr));
	Return(code_make_free_(ptr, free));
	Return(code_bind_args_(ptr, args));
	Return(code_allcons_(ptr, body));

	return 0;
}

int code_make_destructuring_bind_(CodeMake ptr, addr scope)
{
	addr pos;
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	code_queue_push_new(ptr);
	Return(code_make_bind_execute_(ptr, scope));
	code_queue_pop(ptr, &pos);
	code_queue_rollback(ptr, &mode);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* flet */
static void code_make_type_function(CodeMake ptr, addr pos)
{
	addr type;

	if (getcheck_tablefunction(pos)) {
		gettype_tablefunction(pos, &type);
		if ((! type_astert_p(type)) && (! type_function_aster_p(type))) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
		}
	}
}

static int code_make_flet_args_(CodeMake ptr, addr args)
{
	addr pos, value;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &value);

		Return(code_lambda_set_(ptr, value));
		code_make_type_function(ptr, pos);
		index_heap(&value, getlexical_tablefunction(pos));
		CodeQueue_cons(ptr, SETQ_LEXICAL, value);
	}

	return 0;
}

int code_make_flet_(CodeMake ptr, addr scope)
{
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	Return(code_make_free_(ptr, free));
	Return(code_make_flet_args_(ptr, args));
	Return(code_allcons_(ptr, body));

	return 0;
}


/* labels */
static int code_lambda_labels_(CodeMake ptr, addr index, addr scope)
{
	addr pos;
	modeswitch mode;

	code_queue_setmode(ptr, &mode);

	/* code_lambda_function_ */
	code_queue_push_simple(ptr);

	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	CodeQueue_cons(ptr, CALL_NAME, pos);

	code_lambda_lexical(ptr, scope);
	Return(code_lambda_args_(ptr, scope));
	Return(code_lambda_body_(ptr, scope));
	code_queue_pop(ptr, &pos);
	CodeQueue_double(ptr, LABELS_LAMBDA, index, pos);
	Return(code_lambda_info_(ptr, scope));
	code_queue_ifpush(ptr);

	code_queue_rollback(ptr, &mode);

	return 0;
}

static int code_make_labels_args_(CodeMake ptr, addr args)
{
	addr pos, scope, index;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &scope);

		/* labels arguments */
		index_heap(&index, getlexical_tablefunction(pos));
		Return(code_lambda_labels_(ptr, index, scope));
		/* type check */
		code_make_type_function(ptr, pos);
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
	addr args, body, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);

	Return(code_make_free_(ptr, free));
	code_make_labels_lexical(ptr, args);
	Return(code_make_labels_args_(ptr, args));
	Return(code_allcons_(ptr, body));

	return 0;
}


/* values */
static int code_values_set_(CodeMake ptr, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		CodeQueue_single(ptr, VALUES_NIL);
		return 0;
	}

	/* list */
	code_queue_push_new(ptr);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Return(code_make_execute_push_(ptr, pos));
	}
	CodeQueue_single(ptr, VALUES_SET);
	code_queue_pop(ptr, &pos);
	CodeQueue_cons(ptr, EXECUTE_CONTROL_SET, pos);

	return 0;
}

static int code_values_push_(CodeMake ptr, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* list */
	GetCons(cons, &pos, &cons);
	Return(code_make_execute_push_(ptr, pos));
	Return(code_allcons_rem_(ptr, cons));

	return 0;
}

int code_make_values_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			return code_values_set_(ptr, scope);

		case CodeQueue_ModePush:
			return code_values_push_(ptr, scope);

		case CodeQueue_ModeRemove:
		default:
			return code_allcons_(ptr, scope);
	}
}


/* the */
int code_make_the_(CodeMake ptr, addr scope)
{
	addr form, type, check;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &check);
	if (check == Nil)
		return code_make_execute_(ptr, form);

	GetEvalScopeThe(scope, &type);
	get_type_optimized(&type, type);
	get_type_subtypep(&type, type);

	if (code_queue_pushp(ptr)) {
		Return(code_make_execute_set_(ptr, form));
		CodeQueue_cons(ptr, THE_PUSH, type);
	}
	else {
		Return(code_make_execute_set_(ptr, form));
		CodeQueue_cons(ptr, THE_SET, type);
	}

	return 0;
}


/* eval-when */
int code_make_eval_when_(CodeMake ptr, addr scope)
{
	addr cons, compile, load, exec, toplevel, mode;

	GetEvalScopeIndex(scope, 0, &cons);
	GetEvalScopeIndex(scope, 1, &compile);
	GetEvalScopeIndex(scope, 2, &load);
	GetEvalScopeIndex(scope, 3, &exec);
	GetEvalScopeIndex(scope, 4, &toplevel);
	GetEvalScopeIndex(scope, 5, &mode);

	return code_allcons_(ptr, cons);
}


/* locally */
int code_make_locally_(CodeMake ptr, addr scope)
{
	addr cons, free;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);
	Return(code_make_free_(ptr, free));
	Return(code_allcons_(ptr, cons));

	return 0;
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

static int code_if_true_(CodeMake ptr, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if expr then else) */
	code_queue_make_label(ptr, &label_else);
	code_queue_make_label(ptr, &label_end);
	code_queue_if_nil(ptr, label_else);
	Return(code_make_execute_(ptr, then));
	code_queue_goto(ptr, label_end);
	code_queue_push_label(ptr, label_else);
	if (check) {
		Return(code_make_nil_(ptr, NULL));
	}
	else {
		Return(code_make_execute_(ptr, last));
	}
	code_queue_push_label(ptr, label_end);

	return 0;
}

static int code_if_false_(CodeMake ptr, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if (not expr) then else) */
	code_queue_make_label(ptr, &label_else);
	code_queue_make_label(ptr, &label_end);
	code_queue_if_t(ptr, label_else);
	Return(code_make_execute_(ptr, then));
	code_queue_goto(ptr, label_end);
	code_queue_push_label(ptr, label_else);
	if (check) {
		Return(code_make_nil_(ptr, NULL));
	}
	else {
		Return(code_make_execute_(ptr, last));
	}
	code_queue_push_label(ptr, label_end);

	return 0;
}

int code_make_if_(CodeMake ptr, addr scope)
{
	addr expr, then, last;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	Return(code_make_execute_set_(ptr, expr));
	if (code_if_not_p(expr))
		return code_if_false_(ptr, then, last);
	else
		return code_if_true_(ptr, then, last);
}


/* unwind-protect */
int code_make_unwind_protect_(CodeMake ptr, addr scope)
{
	addr form, cons;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);
	/* cleanup */
	code_queue_push_new(ptr);
	Return(code_allcons_rem_(ptr, cons));
	code_queue_pop(ptr, &cons);
	/* protect */
	code_queue_push_new(ptr);
	CodeQueue_cons(ptr, UNWIND_PROTECT, cons);
	Return(code_make_execute_set_(ptr, form));
	code_queue_pop(ptr, &form);
	/* set code */
	code_make_execute_control(ptr, form);

	return 0;
}


/* tagbody */
static int code_tagbody_rem_(CodeMake ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (RefEvalScopeType(pos) != EVAL_PARSE_TAG) {
			Return(code_make_execute_rem_(ptr, pos));
		}
	}

	return 0;
}

static int code_tagbody_cons_(CodeMake ptr, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (RefEvalScopeType(pos) == EVAL_PARSE_TAG) {
			GetEvalScopeValue(pos, &pos);
			CodeQueue_cons(ptr, TAG, pos);
		}
		else {
			Return(code_make_execute_rem_(ptr, pos));
		}
	}

	return 0;
}

static int code_tagbody_body_(CodeMake ptr, addr tag, addr cons)
{
	modeswitch mode;

	/* body */
	CodeQueue_cons(ptr, TAGINFO, tag);
	Return(code_tagbody_cons_(ptr, cons));
	/* return */
	code_queue_setmode(ptr, &mode);
	Return(code_make_nil_(ptr, NULL));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_tagbody_(CodeMake ptr, addr scope)
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
		Return(code_tagbody_rem_(ptr, cons));
		return code_make_nil_(ptr, NULL);
	}
	/* body */
	code_queue_push_new(ptr);
	Return(code_tagbody_body_(ptr, tag, cons));
	code_queue_pop(ptr, &cons);
	/* execute */
	code_make_execute_control(ptr, cons);

	return 0;
}


/* go */
int code_make_go_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	index_heap(&scope, getlexical_tabletagbody(scope));
	CodeQueue_cons(ptr, GO, scope);

	return 0;
}


/* block */
int code_make_block_(CodeMake ptr, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	if (name == Nil)
		return code_allcons_(ptr, cons);
	code_queue_push_new(ptr);
	CodeQueue_cons(ptr, BLOCKINFO, name);
	Return(code_allcons_set_(ptr, cons));
	code_queue_pop(ptr, &cons);
	code_make_execute_control(ptr, cons);

	return 0;
}


/* return-from */
int code_make_return_from_(CodeMake ptr, addr scope)
{
	addr pos, form;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &form);
	/* form */
	Return(code_make_execute_set_(ptr, form));
	/* name */
	index_heap(&pos, getlexical_tableblock(pos));
	CodeQueue_cons(ptr, RETURN_FROM, pos);

	return 0;
}


/* catch */
int code_make_catch_(CodeMake ptr, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	code_queue_push_new(ptr);
	Return(code_make_execute_set_(ptr, name));
	CodeQueue_single(ptr, CATCH);
	Return(code_allcons_set_(ptr, cons));
	code_queue_pop(ptr, &cons);
	code_make_execute_control(ptr, cons);

	return 0;
}


/* throw */
int code_make_throw_(CodeMake ptr, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	code_queue_push_new(ptr);
	Return(code_make_execute_push_(ptr, name));
	Return(code_make_execute_set_(ptr, form));
	CodeQueue_single(ptr, THROW);
	code_queue_pop(ptr, &form);
	CodeQueue_cons(ptr, EXECUTE_CONTROL_SET, form);

	return 0;
}


/* multiple-value-bind */
static void code_make_multiple_value_bind_index(CodeMake ptr, size_t i, addr pos)
{
	addr index, value;

	/* type */
	index_heap(&index, i);
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_double(ptr, BIND1_TYPE, index, value);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_double(ptr, BIND1_SPECIAL, index, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_double(ptr, BIND1_LEXICAL, index, value);
	}
}

static void code_make_multiple_value_bind_list(CodeMake ptr, addr pos)
{
	addr value;

	/* type */
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, BIND2_TYPE, value);
		}
	}

	/* bind */
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &value);
		CodeQueue_cons(ptr, BIND2_SPECIAL, value);
	}
	else {
		index_heap(&value, getlexical_tablevalue(pos));
		CodeQueue_cons(ptr, BIND2_LEXICAL, value);
	}
}

static void code_make_multiple_value_bind_args(CodeMake ptr, addr list)
{
	addr pos;
	size_t i;

	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		if (i < EXECUTE_VALUES)
			code_make_multiple_value_bind_index(ptr, i, pos);
		else
			code_make_multiple_value_bind_list(ptr, pos);
	}
}

static int code_make_multiple_value_bind_execute_(CodeMake ptr, addr scope)
{
	addr args, expr, cons, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);

	Return(code_make_execute_set_(ptr, expr));
	Return(code_make_free_(ptr, free));
	code_make_multiple_value_bind_args(ptr, args);
	Return(code_allcons_(ptr, cons));

	return 0;
}

int code_make_multiple_value_bind_(CodeMake ptr, addr scope)
{
	addr pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 6, &pos); /* allocate */
	if (pos == Nil)
		return code_make_multiple_value_bind_execute_(ptr, scope);

	code_queue_setmode(ptr, &mode);
	code_queue_push_new(ptr);
	Return(code_make_multiple_value_bind_execute_(ptr, scope));
	code_queue_pop(ptr, &pos);
	code_queue_rollback(ptr, &mode);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* multiple-value-call */
int code_make_multiple_value_call_(CodeMake ptr, addr scope)
{
	addr call, args, pos;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(ptr);
	/* call */
	Return(code_make_execute_push_(ptr, call));
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		Return(code_make_execute_set_(ptr, pos));
		CodeQueue_single(ptr, PUSH_VALUES);
	}
	/* call */
	CodeQueue_single(ptr, FUNCALL);
	code_queue_pop(ptr, &pos);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* multiple-value-prog1 */
int code_make_multiple_value_prog1_(CodeMake ptr, addr scope)
{
	addr expr, cons;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &cons);

	/* expr */
	Return(code_make_execute_(ptr, expr));
	/* cons */
	code_queue_push_new(ptr);
	Return(code_allcons_rem_(ptr, cons));
	code_queue_pop(ptr, &cons);
	CodeQueue_cons(ptr, EXECUTE_CONTROL_SAVE, cons);

	return 0;
}


/* nth-value */
int code_make_nth_value_(CodeMake ptr, addr scope)
{
	addr nth, expr, pos;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);
	code_queue_push_new(ptr);
	Return(code_make_execute_push_(ptr, nth));
	Return(code_make_execute_set_(ptr, expr));
	CodeQueue_single(ptr, NTH_VALUE);
	code_queue_pop(ptr, &pos);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* progv */
int code_make_progv_(CodeMake ptr, addr scope)
{
	addr symbols, values, body, pos;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);
	code_queue_push_new(ptr);
	Return(code_make_execute_push_(ptr, symbols));
	Return(code_make_execute_push_(ptr, values));
	CodeQueue_single(ptr, PROGV);
	Return(code_allcons_set_(ptr, body));
	code_queue_pop(ptr, &pos);
	code_make_execute_control(ptr, pos);

	return 0;
}


/* load-time-value */
static void code_make_reference(CodeMake ptr, addr value)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, REFERENCE_SET, value);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, REFERENCE_PUSH, value);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

int code_make_load_time_value_(CodeMake ptr, addr scope)
{
	addr pos, value, index;

	GetEvalScopeIndex(scope, 0, &value);
	GetEvalScopeIndex(scope, 1, &index);
	load_time_value_heap(&pos, value, index);
	code_make_reference(ptr, pos);

	return 0;
}


/*
 *  step
 */
int code_make_step_(CodeMake ptr, addr scope)
{
	addr expr, value;

	/* scope */
	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &value);

	/* code */
	code_queue_push_new(ptr);
	Return(code_make_execute_(ptr, expr));
	code_queue_pop(ptr, &expr);
	CodeQueue_double(ptr, STEP, expr, value);
	code_queue_ifpush(ptr);

	return 0;
}

