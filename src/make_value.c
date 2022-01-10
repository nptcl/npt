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
#include "make_function.h"
#include "make_queue.h"
#include "make_value.h"
#include "scope_object.h"
#include "strvect.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "symbol.h"
#include "type.h"

int code_make_debug_(CodeMake ptr, addr scope, int (*call)(CodeMake, addr))
{
	addr value;

	if (scope == NULL)
		goto normal;
	if (! scope_step_p(scope))
		goto normal;
	GetEvalScopeValue(scope, &value);
	if (value == Nil)
		goto normal;

	/* step */
	CodeQueue_cons(ptr, STEP, value);
	Return((*call)(ptr, scope));
	CodeQueue_single(ptr, STEP_OFF);
	return 0;

	/* normal */
normal:
	return (*call)(ptr, scope);
}


/* nil */
int code_make_nil_(CodeMake ptr, addr scope)
{
	if (scope == NULL)
		goto normal;
	if (! scope_step_p(scope))
		goto normal;
	CodeQueue_cons(ptr, STEP, Nil);
	code_make_single(ptr, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
	CodeQueue_single(ptr, STEP_OFF);
	return 0;

normal:
	code_make_single(ptr, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
	return 0;
}


/* t */
static int code_make_t_call_(CodeMake ptr, addr scope)
{
	code_make_single(ptr, CONSTANT_CODE_T_SET, CONSTANT_CODE_T_PUSH);
	return 0;
}

int code_make_t_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_t_call_);
}


/* value */
static int code_make_value_call_(CodeMake ptr, addr scope)
{
	CheckTypeCodeQueue(ptr->code);
	GetEvalScopeValue(scope, &scope);
	code_make_object(ptr, scope);

	return 0;
}
int code_make_value_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_value_call_);
}

static int code_make_value2_call_(CodeMake ptr, addr scope)
{
	CheckTypeCodeQueue(ptr->code);
	GetEvalScopeIndex(scope, 0, &scope);
	code_make_object(ptr, scope);

	return 0;
}
int code_make_value2_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_value2_call_);
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
		code_escape_wake(ptr);
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
		code_escape_wake(ptr);
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
		code_escape_wake(ptr);
	}
	else {
		index_heap(&pos, getlexical_tablevalue(table));
		CodeQueue_cons(ptr, LEXICAL_REM, pos);
	}
}

static int code_make_symbol_call_(CodeMake ptr, addr scope)
{
	addr symbol, table;

	GetEvalScopeValue(scope, &symbol);
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

int code_make_symbol_(CodeMake ptr, addr scope)
{
	addr symbol;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol))
		return code_make_value_(ptr, scope);

	/* symbol */
	return code_make_debug_(ptr, scope, code_make_symbol_call_);
}


/* declaim */
static void code_declaim_special(CodeMake ptr, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		CodeQueue_cons(ptr, DECLAIM_SPECIAL, pos);
		code_escape_wake(ptr);
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
		code_escape_wake(ptr);
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
		code_escape_wake(ptr);
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
		code_escape_wake(ptr);
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
	fixnum id;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &list);
	if (list == Nil)
		return code_make_execute_(ptr, pos);

	/* lexical */
	code_make_begin(ptr, &id);
	CodeQueue_cons(ptr, LEXICAL, list);
	Return(code_make_execute_(ptr, pos));
	code_make_end(ptr, id);

	return 0;
}


/*
 *  progn
 */
int code_make_progn_(CodeMake ptr, addr scope)
{
	addr form, list;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &list);
	if (form != Nil && scope_step_p(scope)) {
		CodeQueue_cons(ptr, STEP, form);
		Return(code_allcons_(ptr, list, NULL));
		CodeQueue_single(ptr, STEP_OFF);
	}
	else {
		Return(code_allcons_(ptr, list, NULL));
	}

	return 0;
}


/* let */
static void code_make_free_value(CodeMake ptr, addr pos, addr type, addr escape)
{
	if (type_astert_p(type))
		return;
	if (getspecialp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_SPECIAL, pos, type);
		code_jump_escape_wake(ptr, escape);
	}
	else if (getglobalp_tablevalue(pos)) {
		getname_tablevalue(pos, &pos);
		CodeQueue_double(ptr, TYPE_GLOBAL, pos, type);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		/* lexical */
		index_heap(&pos, getlexical_tablevalue(pos));
		/* CodeQueue_double(ptr, TYPE_LEXICAL, pos, type); */
		/* code_jump_escape_wake(ptr, escape); */
	}
}

static void code_make_free_function(CodeMake ptr, addr pos, addr type, addr escape)
{
	addr symbol;

	if (type_astert_p(type) || type_function_aster_p(type))
		return;
	if (! getglobalp_tablefunction(pos)) {
		/* lexical */
		index_heap(&pos, getlexical_tablefunction(pos));
		/* CodeQueue_double(ptr, TYPE_LEXICAL, pos, type); */
		/* code_jump_escape_wake(ptr, escape); */
		return;
	}

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_double(ptr, TYPE_FUNCTION, symbol, type);
		code_jump_escape_wake(ptr, escape);
	}
	else {
		CodeQueue_double(ptr, TYPE_SETF, symbol, type);
		code_jump_escape_wake(ptr, escape);
	}
}

int code_make_free_(CodeMake ptr, addr list, addr escape)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			code_make_free_value(ptr, pos, type, escape);
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			code_make_free_function(ptr, pos, type, escape);
			continue;
		}

		return fmte_("Invalid type object in code-make.", NULL);
	}

	return 0;
}

void code_make_type_value(CodeMake ptr, addr pos, addr escape)
{
	addr type;

	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &type);
		if (! type_astert_p(type)) {
			CodeQueue_cons(ptr, TYPE_RESULT, type);
			code_jump_escape_wake(ptr, escape);
		}
	}
}

static int code_make_let_args_(CodeMake ptr, addr args, addr escape)
{
	addr list, pos, value, index;

	/* value */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
		code_make_type_value(ptr, pos, escape);

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

static int code_make_let_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, alloc, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &alloc);

	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_let_args_(ptr, args, escape));
		Return(code_allcons_(ptr, body, escape));
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* body */
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_let_args_(ptr, args, escape));
		Return(code_allcons_set_(ptr, body, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}

int code_make_let_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_let_call_);
}


/* let* */
static int code_make_leta_args_(CodeMake ptr, addr list, addr escape)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
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

	return 0;
}

static int code_make_leta_call_(CodeMake ptr, addr scope)
{
	addr args, body, free, alloc, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &body);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &alloc);
	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_leta_args_(ptr, args, escape));
		Return(code_allcons_(ptr, body, escape));
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* body */
		Return(code_make_free_(ptr, free, escape));
		Return(code_make_leta_args_(ptr, args, escape));
		Return(code_allcons_set_(ptr, body, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}

int code_make_leta_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_leta_call_);
}

/* setq */
static int code_setq_loop_(CodeMake ptr, addr list, addr escape)
{
	addr pos, value, symbol;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);

		Return(code_make_execute_set_(ptr, value));
		code_jump_escape_wake(ptr, escape);
		code_make_type_value(ptr, pos, escape);

		getname_tablevalue(pos, &symbol);
		if (getspecialp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_SPECIAL, symbol);
			code_jump_escape_wake(ptr, escape);
		}
		else if (getglobalp_tablevalue(pos)) {
			CodeQueue_cons(ptr, SETQ_GLOBAL, symbol);
			code_jump_escape_wake(ptr, escape);
		}
		else {
			index_heap(&value, getlexical_tablevalue(pos));
			CodeQueue_cons(ptr, SETQ_LEXICAL, value);
		}
	}

	return 0;
}

static int code_make_setq_call_(CodeMake ptr, addr scope)
{
	addr list, escape;

	/* nil */
	GetEvalScopeIndex(scope, 0, &list);
	if (list == Nil)
		return code_make_nil_(ptr, NULL);

	/* setq */
	code_queue_make_label(ptr, &escape);
	Return(code_setq_loop_(ptr, list, escape));
	code_queue_ifpush(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_setq_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_setq_call_);
}


/* values */
static int code_values_set_(CodeMake ptr, addr cons)
{
	addr pos, escape;
	fixnum id;

	/* nil */
	if (cons == Nil) {
		CodeQueue_single(ptr, VALUES_NIL);
		return 0;
	}

	/* list */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Return(code_make_execute_push_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}
	CodeQueue_single(ptr, VALUES_SET);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

static int code_values_push_(CodeMake ptr, addr cons)
{
	addr pos, escape;

	/* nil */
	if (cons == Nil)
		return code_make_nil_(ptr, NULL);

	/* list */
	code_queue_make_label(ptr, &escape);
	GetCons(cons, &pos, &cons);
	Return(code_make_execute_push_(ptr, pos));
	code_jump_escape_wake(ptr, escape);
	Return(code_allcons_rem_(ptr, cons, escape));
	code_queue_push_label(ptr, escape);

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
			return code_allcons_(ptr, scope, NULL);
	}
}


/* the */
int code_make_the_(CodeMake ptr, addr scope)
{
	addr form, type, check, escape;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &check);
	if (check == Nil)
		return code_make_execute_(ptr, form);

	GetEvalScopeThe(scope, &type);
	get_type_optimized(&type, type);
	get_type_subtypep(&type, type);

	code_queue_make_label(ptr, &escape);
	if (code_queue_pushp(ptr)) {
		Return(code_make_execute_set_(ptr, form));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_cons(ptr, THE_PUSH, type);
	}
	else {
		Return(code_make_execute_set_(ptr, form));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_cons(ptr, THE_SET, type);
	}
	code_queue_push_label(ptr, escape);

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

	return code_allcons_(ptr, cons, NULL);
}


/* locally */
int code_make_locally_(CodeMake ptr, addr scope)
{
	addr cons, free, escape;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);

	code_queue_make_label(ptr, &escape);
	Return(code_make_free_(ptr, free, escape));
	Return(code_allcons_(ptr, cons, escape));
	code_queue_push_label(ptr, escape);

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
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	if (! symbolp_callname(call))
		return 0;
	GetCallName(call, &call);
	GetConst(COMMON_NOT, &check1);
	GetConst(COMMON_NULL, &check2);

	return call == check1 || call == check2;
}

static int code_if_true_(CodeMake ptr, addr then, addr last, addr escape)
{
	int check;
	addr label;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if expr then else) */
	code_queue_make_label(ptr, &label);
	code_queue_if_nil(ptr, label);
	Return(code_make_execute_(ptr, then));
	code_jump_escape_wake(ptr, escape);
	code_queue_goto(ptr, escape);
	code_queue_push_label(ptr, label);
	if (check)
		return code_make_nil_(ptr, NULL);
	else
		return code_make_execute_(ptr, last);
}

static int code_if_false_(CodeMake ptr, addr then, addr last, addr escape)
{
	int check;
	addr label;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if (not expr) then else) */
	code_queue_make_label(ptr, &label);
	code_queue_if_t(ptr, label);
	Return(code_make_execute_(ptr, then));
	code_jump_escape_wake(ptr, escape);
	code_queue_goto(ptr, escape);
	code_queue_push_label(ptr, label);
	if (check)
		return code_make_nil_(ptr, NULL);
	else
		return code_make_execute_(ptr, last);
}

static int code_make_if_call_(CodeMake ptr, addr scope)
{
	addr expr, then, last, escape;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	/* expr */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);

	/* then, else */
	if (code_if_not_p(expr)) {
		Return(code_if_false_(ptr, then, last, escape));
	}
	else {
		Return(code_if_true_(ptr, then, last, escape));
	}
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_if_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_if_call_);
}


/* unwind-protect */
static int code_make_unwind_protect_call_(CodeMake ptr, addr scope)
{
	addr form, cons, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);

	code_queue_make_label(ptr, &escape);
	/* form */
	Return(code_make_execute_(ptr, form));
	/* protect */
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, SAVE);
	CodeQueue_single(ptr, NORMAL);
	Return(code_allcons_rem_(ptr, cons, escape));
	CodeQueue_single(ptr, RESTORE);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

int code_make_unwind_protect_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_unwind_protect_call_);
}


/* tagbody */
static int code_tagbody_rem_(CodeMake ptr, addr list)
{
	addr pos, escape;

	code_queue_make_label(ptr, &escape);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (RefEvalScopeType(pos) != EVAL_PARSE_TAG) {
			Return(code_make_execute_rem_(ptr, pos));
			code_jump_escape_wake(ptr, escape);
		}
	}
	code_make_nil_(ptr, NULL);
	code_queue_push_label(ptr, escape);

	return 0;
}

static int code_tagbody_cons_(CodeMake ptr, addr cons, addr escape)
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
			code_jump_escape_wake(ptr, escape);
		}
	}

	return 0;
}

static int code_tagbody_body_(CodeMake ptr, addr tag, addr cons)
{
	addr escape, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	CodeQueue_cons(ptr, TAGINFO, tag);
	Return(code_tagbody_cons_(ptr, cons, escape));
	code_make_end(ptr, id);
	Return(code_make_nil_(ptr, NULL));
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_single(ptr, REVERT);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}

static int code_make_tagbody_call_(CodeMake ptr, addr scope)
{
	addr tag, cons;

	/*  code: tagbody
	 *    (code::taginfo tag1 tag2 ...)
	 *    ,@progn...
	 *    nil
	 */
	GetEvalScopeIndex(scope, 0, &tag);
	GetEvalScopeIndex(scope, 1, &cons);
	if (tag == Nil)
		return code_tagbody_rem_(ptr, cons);
	else
		return code_tagbody_body_(ptr, tag, cons);
}

int code_make_tagbody_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_tagbody_call_);
}


/* go */
int code_make_go_(CodeMake ptr, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	index_heap(&scope, getlexical_tabletagbody(scope));
	CodeQueue_cons(ptr, GO, scope);
	code_escape_wake(ptr);

	return 0;
}


/* block */
static int code_make_block_call_(CodeMake ptr, addr scope)
{
	addr name, cons, escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	if (name == Nil)
		return code_allcons_(ptr, cons, NULL);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	CodeQueue_cons(ptr, BLOCKINFO, name);
	Return(code_allcons_(ptr, cons, escape));
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_block_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_block_call_);
}


/* return-from */
static int code_make_return_from_call_(CodeMake ptr, addr scope)
{
	addr pos, form, escape;

	GetEvalScopeIndex(scope, 0, &pos);
	GetEvalScopeIndex(scope, 1, &form);

	/* form */
	code_queue_make_label(ptr, &escape);
	Return(code_make_execute_set_(ptr, form));
	code_jump_escape_wake(ptr, escape);

	/* name */
	index_heap(&pos, getlexical_tableblock(pos));
	CodeQueue_cons(ptr, RETURN_FROM, pos);
	code_escape_wake(ptr);
	code_queue_push_label(ptr, escape);

	return 0;
}

int code_make_return_from_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_return_from_call_);
}


/* catch */
static int code_make_catch_call_(CodeMake ptr, addr scope)
{
	addr name, cons, escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* name */
	Return(code_make_execute_set_(ptr, name));
	code_jump_escape_wake(ptr, escape);
	CodeQueue_single(ptr, CATCH);
	code_escape_wake(ptr);

	/* body */
	Return(code_allcons_set_(ptr, cons, escape));
	code_queue_goto(ptr, normal);

	/* escape */
	code_queue_push_label(ptr, escape);
	CodeQueue_cons(ptr, REVERT_GOTO, normal);
	code_make_end(ptr, id);
	code_queue_goto(ptr, finish);

	/* normal */
	code_queue_push_label(ptr, normal);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	/* finish */
	code_queue_push_label(ptr, finish);

	return 0;
}

int code_make_catch_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_catch_call_);
}


/* throw */
static int code_make_throw_call_(CodeMake ptr, addr scope)
{
	addr name, form, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);

	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	/* name */
	Return(code_make_execute_push_(ptr, name));
	code_jump_escape_wake(ptr, escape);
	/* value */
	Return(code_make_execute_set_(ptr, form));
	code_jump_escape_wake(ptr, escape);
	/* throw */
	CodeQueue_single(ptr, THROW);
	code_escape_wake(ptr);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}

int code_make_throw_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_throw_call_);
}


/* multiple-value-bind */
static void code_make_multiple_value_bind_index(CodeMake ptr,
		size_t i, addr pos, addr escape)
{
	addr index, value;

	/* type */
	index_heap(&index, i);
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_double(ptr, BIND1_TYPE, index, value);
			code_jump_escape_wake(ptr, escape);
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

static void code_make_multiple_value_bind_list(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	/* type */
	if (getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, BIND2_TYPE, value);
			code_jump_escape_wake(ptr, escape);
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

static void code_make_multiple_value_bind_args(CodeMake ptr, addr list, addr escape)
{
	addr pos;
	size_t i;

	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		if (i < EXECUTE_VALUES)
			code_make_multiple_value_bind_index(ptr, i, pos, escape);
		else
			code_make_multiple_value_bind_list(ptr, pos, escape);
	}
}

int code_make_multiple_value_bind_(CodeMake ptr, addr scope)
{
	fixnum id;
	addr args, expr, cons, free, alloc, escape, finish;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);
	GetEvalScopeIndex(scope, 6, &alloc); /* allocate */

	if (alloc == Nil) {
		code_queue_make_label(ptr, &escape);
		/* expr */
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);
		/* bind */
		Return(code_make_free_(ptr, free, escape));
		code_make_multiple_value_bind_args(ptr, args, escape);
		Return(code_allcons_(ptr, cons, escape));
		/* result */
		code_queue_push_label(ptr, escape);
	}
	else {
		/* begin */
		code_queue_make_label(ptr, &escape);
		code_queue_make_label(ptr, &finish);
		code_make_begin(ptr, &id);

		/* expr */
		Return(code_make_execute_set_(ptr, expr));
		code_jump_escape_wake(ptr, escape);

		/* bind */
		Return(code_make_free_(ptr, free, escape));
		code_make_multiple_value_bind_args(ptr, args, escape);
		Return(code_allcons_set_(ptr, cons, escape));
		code_make_end(ptr, id);
		code_queue_ifpush(ptr);
		code_queue_goto(ptr, finish);

		/* escape */
		code_queue_push_label(ptr, escape);
		code_make_end(ptr, id);
		code_queue_push_label(ptr, finish);
	}

	return 0;
}


/* multiple-value-call */
static int code_make_multiple_value_call_body_(CodeMake ptr,
		addr call, addr args, addr escape)
{
	addr pos;

	/* call */
	Return(code_make_execute_push_(ptr, call));
	code_jump_escape_wake(ptr, escape);

	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		Return(code_make_execute_set_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
		CodeQueue_single(ptr, PUSH_VALUES);
	}

	/* call */
	CodeQueue_single(ptr, FUNCALL);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

int code_make_multiple_value_call_(CodeMake ptr, addr scope)
{
	addr call, args, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_multiple_value_call_body_(ptr, call, args, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}


/* multiple-value-prog1 */
int code_make_multiple_value_prog1_(CodeMake ptr, addr scope)
{
	addr expr, cons, escape;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &cons);

	/* expr */
	Return(code_make_execute_(ptr, expr));
	/* cons */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, SAVE);
	Return(code_allcons_rem_(ptr, cons, escape));
	CodeQueue_single(ptr, RESTORE);
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);

	return 0;
}


/* nth-value */
int code_make_nth_value_(CodeMake ptr, addr scope)
{
	addr nth, expr, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);

	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* nth */
	Return(code_make_execute_push_(ptr, nth));
	code_jump_escape_wake(ptr, escape);

	/* expr */
	Return(code_make_execute_set_(ptr, expr));
	code_jump_escape_wake(ptr, escape);

	/* result */
	CodeQueue_single(ptr, NTH_VALUE);
	code_escape_wake(ptr);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}


/* progv */
int code_make_progv_(CodeMake ptr, addr scope)
{
	addr symbols, values, body, escape, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* symbols */
	Return(code_make_execute_push_(ptr, symbols));
	code_jump_escape_wake(ptr, escape);

	/* values */
	Return(code_make_execute_push_(ptr, values));
	code_jump_escape_wake(ptr, escape);

	/* progv */
	CodeQueue_single(ptr, PROGV);
	code_jump_escape_wake(ptr, escape);

	/* body */
	Return(code_allcons_set_(ptr, body, escape));
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);
	code_queue_goto(ptr, finish);

	/* escape */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_push_label(ptr, finish);

	return 0;
}


/* load-time-value */
static void code_make_reference(CodeMake ptr, addr value)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, REFERENCE_SET, value);
			code_escape_wake(ptr);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, REFERENCE_PUSH, value);
			code_escape_wake(ptr);
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
	addr expr;
	fixnum id;

	GetEvalScopeIndex(scope, 0, &expr);
	code_make_begin(ptr, &id);
	CodeQueue_single(ptr, STEP_BEGIN);
	Return(code_make_execute_set_(ptr, expr));
	CodeQueue_single(ptr, STEP_END);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	return 0;
}

