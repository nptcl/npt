#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "eval_table.h"
#include "make.h"
#include "make_call.h"
#include "make_queue.h"
#include "make_typedef.h"
#include "make_value.h"
#include "optimize_common.h"
#include "parse_object.h"
#include "scope_object.h"
#include "symbol.h"
#include "type.h"
#include "typedef.h"

/*
 *  specialized call
 */
static int code_make_specialize_common_p(addr call)
{
	addr common;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeIndex(call, 0, &call);
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
	GetEvalScopeIndex(call, 0, &call);
	getname_tablefunction(call, &call);
	GetCallName(call, &right);
	return RefCallNameType(call) == CALLNAME_SYMBOL && left == right;
}
#define CodeMakeSpeciailizedSymbolP(x,y) \
	code_make_specialize_symbol_p((x),CONSTANT_SYSTEM_##y)

static int code_make_specialize_allcons_(CodeMake ptr, addr args, addr escape)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		Return(code_make_execute_set_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}

	return 0;
}

static int code_make_specialize_body_(CodeMake ptr, addr scope, int *ret)
{
	addr args, escape, normal, finish;
	fixnum id;

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	GetEvalScopeIndex(scope, 1, &args);
	Return(code_make_specialize_allcons_(ptr, args, escape));
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

	return Result(ret, 1);
}

static int code_make_specialize_restart_progn_(CodeMake ptr, addr scope, int *ret)
{
	addr args, list, call;
	addr escape, normal, finish;
	fixnum id;

	GetEvalScopeIndex(scope, 1, &args);
	Return_getcons(args, &list, &args);
	Return_getcons(args, &call, &args);
	if (args != Nil)
		return fmte_("Invalid restart-progn call, ~S.", args, NULL);
	getvalue_tablecall(list, &list);
	getvalue_tablecall(call, &call);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_queue_make_label(ptr, &normal);
	code_queue_make_label(ptr, &finish);
	code_make_begin(ptr, &id);

	/* body */
	Return(code_make_execute_set_(ptr, list));
	code_jump_escape_wake(ptr, escape);
	CodeQueue_single(ptr, RESTART_PROGN);
	Return(code_make_execute_set_(ptr, call));
	code_jump_escape_wake(ptr, escape);
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

	return Result(ret, 1);
}

static int code_make_specialize_type_body_(CodeMake ptr,
		addr scope, constindex type, addr escape)
{
	addr args, pos;

	GetEvalScopeIndex(scope, 1, &args);
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		Return(code_make_execute_push_(ptr, pos));
		code_jump_escape_wake(ptr, escape);
	}
	code_queue_single(ptr, type);
	code_jump_escape_wake(ptr, escape);

	return 0;
}

static int code_make_specialize_type_(CodeMake ptr,
		addr scope, constindex type, int *ret)
{
	addr escape;

	code_queue_make_label(ptr, &escape);
	Return(code_make_specialize_type_body_(ptr, scope, type, escape));
	code_queue_push_label(ptr, escape);

	return Result(ret, 1);
}

static int code_make_specialize_handler_bind_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_HANDLER_BIND, ret);
}

static int code_make_specialize_handler_case_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_HANDLER_CASE, ret);
}

static int code_make_specialize_restart_bind_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_RESTART_BIND, ret);
}

static int code_make_specialize_restart_case_(CodeMake ptr, addr scope, int *ret)
{
	return code_make_specialize_type_(ptr, scope, CONSTANT_CODE_RESTART_CASE, ret);
}

static int code_make_specialize_(CodeMake ptr, addr scope, int *ret)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call);

	/* common-lisp */
	if (code_make_specialize_common_p(call))
		return optimize_common_(ptr, scope, ret);

	/* lisp-system::handler */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER))
		return code_make_specialize_body_(ptr, scope, ret);

	/* lisp-system::restart */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART))
		return code_make_specialize_body_(ptr, scope, ret);

	/* lisp-system::restart-progn */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_PROGN))
		return code_make_specialize_restart_progn_(ptr, scope, ret);

	/* lisp-system::handler-bind */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER_BIND))
		return code_make_specialize_handler_bind_(ptr, scope, ret);

	/* lisp-system::handler-case */
	if (CodeMakeSpeciailizedSymbolP(call, HANDLER_CASE))
		return code_make_specialize_handler_case_(ptr, scope, ret);

	/* lisp-system::restart-bind */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_BIND))
		return code_make_specialize_restart_bind_(ptr, scope, ret);

	/* lisp-system::restart-case */
	if (CodeMakeSpeciailizedSymbolP(call, RESTART_CASE))
		return code_make_specialize_restart_case_(ptr, scope, ret);

	/* lisp-system::optimize-check */
	if (CodeMakeSpeciailizedSymbolP(call, OPTIMIZE_CHECK))
		return optimize_check_code_(ptr, scope, ret);

	return Result(ret, 0);
}


/*
 *  call
 */
static int code_make_call_push_escape_(CodeMake ptr, addr value, addr escape)
{
	unsigned escape_p;

	escape_p = ptr->escape;
	ptr->escape = 0;
	Return(code_make_execute_push_(ptr, value));
	if (ptr->escape)
		code_jump_escape_wake(ptr, escape);
	ptr->escape |= escape_p;

	return 0;
}

static int code_make_call_args_push_(CodeMake ptr, addr pos, addr escape)
{
	addr value;

	/* value */
	getvalue_tablecall(pos, &value);
	Return(code_make_call_push_escape_(ptr, value, escape));

	/* type */
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, CALL_TYPE, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	return 0;
}

static int code_make_call_args_type_(CodeMake ptr, addr args, addr escape)
{
	addr pos;

	while (consp_getcons(args, &pos, &args)) {
		Return(code_make_call_args_push_(ptr, pos, escape));
	}

	return 0;
}

static int code_make_call_args_count_(CodeMake ptr,
		addr list, addr args, addr escape, addr *ret)
{
	addr pos;

	while (list != Nil) {
		GetCdr(list, &list);
		if (! consp_getcons(args, &pos, &args))
			break;
		Return(code_make_call_args_push_(ptr, pos, escape));
	}

	return Result(ret, args);
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

static int code_make_call_args_value_(CodeMake ptr, addr pos, addr key, addr escape)
{
	addr value;

	getvalue_tablecall(pos, &value);

	/* setmode */
	Return(code_make_execute_set_(ptr, value));
	code_jump_escape_wake(ptr, escape);

	/* type check */
	if (getcheck_tablecall(pos)) {
		gettype_tablecall(pos, &value);
		if (! type_astert_p(value)) {
			CodeQueue_cons(ptr, TYPE_RESULT, value);
			code_jump_escape_wake(ptr, escape);
		}
	}

	/* key */
	CodeQueue_cons(ptr, CALL_KEY, key);
	code_jump_escape_wake(ptr, escape);

	/* push */
	CodeQueue_single(ptr, PUSH_RESULT);

	return 0;
}

static int code_make_call_args_key_(CodeMake ptr, addr key, addr args, addr escape)
{
	int kv;
	addr pos;

	for (kv = 0; args != Nil; kv = ! kv) {
		GetCons(args, &pos, &args);
		if (kv == 0) {
			Return(code_make_call_args_push_(ptr, pos, escape));
		}
		else {
			Return(code_make_call_args_value_(ptr, pos, key, escape));
		}
	}

	return 0;
}

static int code_make_call_args_(CodeMake ptr, addr first, addr args, addr escape)
{
	addr var, opt, key;

	/* type */
	if (code_make_call_type(first, &first))
		return code_make_call_args_type_(ptr, args, escape);

	GetArrayA2(first, 0, &var);
	GetArrayA2(first, 1, &opt);
	GetArrayA2(first, 3, &key);

	/* var, &optional */
	Return(code_make_call_args_count_(ptr, var, args, escape, &args));
	Return(code_make_call_args_count_(ptr, opt, args, escape, &args));
	if (! consp(args))
		return 0;

	/* not key */
	if (! consp(key))
		return code_make_call_args_type_(ptr, args, escape);

	/* key */
	Return(code_make_call_args_key_(ptr, key, args, escape));

	return 0;
}

static void code_make_call_global(CodeMake ptr, addr pos)
{
	addr symbol;

	getname_tablefunction(pos, &pos);
	GetCallName(pos, &symbol);
	if (symbolp_callname(pos)) {
		CodeQueue_cons(ptr, CALL_FUNCTION, symbol);
	}
	else {
		CodeQueue_cons(ptr, CALL_SETF, symbol);
	}
}

static void code_make_call_lexical(CodeMake ptr, addr pos)
{
	index_heap(&pos, getlexical_tablefunction(pos));
	CodeQueue_cons(ptr, CALL_LEXICAL, pos);
}

static void code_make_call_function(CodeMake ptr, addr table)
{
	if (getglobalp_tablefunction(table))
		code_make_call_global(ptr, table);
	else
		code_make_call_lexical(ptr, table);
}

static int code_make_call_first_(CodeMake ptr, addr pos, addr escape)
{
	addr table;

	switch (RefEvalScopeType(pos)) {
		case EVAL_PARSE_FUNCTION:
			GetEvalScopeIndex(pos, 0, &table);
			code_make_call_function(ptr, table);
			code_jump_escape_wake(ptr, escape);
			return 0;

		case EVAL_PARSE_LAMBDA:
			/* execute */
			Return(code_make_execute_set_(ptr, pos));
			code_jump_escape_wake(ptr, escape);
			/* call */
			CodeQueue_single(ptr, CALL_RESULT);
			code_jump_escape_wake(ptr, escape);
			return 0;

		default:
			return fmte_("Invaild call type.", NULL);
	}
}

static int code_make_call_name_(CodeMake ptr, addr pos)
{
	switch (RefEvalScopeType(pos)) {
		case EVAL_PARSE_FUNCTION:
			GetEvalScopeIndex(pos, 0, &pos);
			getname_tablefunction(pos, &pos);
			CodeQueue_cons(ptr, CALL_NAME, pos);
			return 0;

		case EVAL_PARSE_LAMBDA:
			GetConst(COMMON_LAMBDA, &pos);
			CodeQueue_cons(ptr, CALL_NAME, pos);
			return 0;

		default:
			return fmte_("Invaild call type.", NULL);
	}
}

static int code_make_call_call_(CodeMake ptr, addr scope)
{
	int check;
	addr first, args, escape;
	fixnum id;

	Return(code_make_specialize_(ptr, scope, &check));
	if (check)
		return 0;

	/* call */
	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &args);

	/* begin */
	code_queue_make_label(ptr, &escape);
	code_make_begin(ptr, &id);

	/* execute */
	Return(code_make_call_name_(ptr, first));
	Return(code_make_call_args_(ptr, first, args, escape));
	Return(code_make_call_first_(ptr, first, escape));

	/* end */
	code_queue_push_label(ptr, escape);
	code_make_end(ptr, id);
	code_queue_ifpush(ptr);

	return 0;
}

int code_make_call_(CodeMake ptr, addr scope)
{
	return code_make_debug_(ptr, scope, code_make_call_call_);
}

