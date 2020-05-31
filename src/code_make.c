#include "callname.h"
#include "code_make.h"
#include "code_queue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "declare.h"
#include "eval.h"
#include "eval_table.h"
#include "function.h"
#include "optimize_common.h"
#include "scope_object.h"
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
		code_make_execute_normal(local, code, pos);
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
static void code_make_let_args(LocalRoot local, addr code, addr args, addr *ret)
{
	addr list, x, y;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCons(x, &x, &y);
		code_queue_push_simple(local, code);
		code_make_execute_set(local, code, y);
		code_queue_pop(local, code, &y);
		cons_heap(&list, x, list);
		cons_heap(&list, y, list);
	}
	nreverse(ret, list);
}

static void code_make_let_body(LocalRoot local, addr code, addr list, addr *ret)
{
	code_queue_push_simple(local, code);
	code_allcons_set(local, code, list);
	code_queue_pop(local, code, ret);
}

static void code_make_let(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free, allocate;

	/* make code */
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &allocate);
	code_make_let_args(local, code, args, &args);
	code_make_let_body(local, code, cons, &cons);
	/* execute */
	list_heap(&cons, args, cons, free, allocate, NULL);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, LET_PUSH, cons);
	else
		CodeQueue_cons(local, code, LET_SET, cons);
}


/* let* */
static void code_make_leta(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free, allocate;

	/* make code */
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	GetEvalScopeIndex(scope, 4, &allocate);
	code_make_let_args(local, code, args, &args);
	code_make_let_body(local, code, cons, &cons);
	/* execute */
	list_heap(&cons, args, cons, free, allocate, NULL);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, LETA_PUSH, cons);
	else
		CodeQueue_cons(local, code, LETA_SET, cons);
}


/* setq */
static void code_setq_execute(LocalRoot local, addr code, addr list, addr *ret)
{
	addr root, pos, form;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &form);

		code_queue_push_simple(local, code);
		code_make_execute_set(local, code, form);
		code_queue_pop(local, code, &form);

		cons_heap(&root, pos, root);
		cons_heap(&root, form, root);
	}
	nreverse(ret, root);
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
	code_setq_execute(local, code, list, &list);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, SETQ_PUSH, list);
	else
		CodeQueue_cons(local, code, SETQ_SET, list);
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
static void ordinary_bind_opt(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		code_queue_push_simple(local, code);
		code_make_execute_set(local, code, init);
		code_queue_pop(local, code, &init);
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void ordinary_bind_key(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		code_queue_push_simple(local, code);
		code_make_execute_set(local, code, init);
		code_queue_pop(local, code, &init);
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void ordinary_bind_aux(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		code_queue_push_simple(local, code);
		code_make_execute_set(local, code, init);
		code_queue_pop(local, code, &init);
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}


/*
 *  lambda
 */
static void list_nil_heap(addr *ret, ...)
{
	addr list, x, cdr;
	va_list va;

	va_start(va, ret);
	list = Nil;
	for (;;) {
		x = va_arg(va, addr);
		if (x == NULL)
			break;
		cons_heap(&list, x, list);
	}
	va_end(va);

	while (list != Nil) {
		GetCons(list, &x, &cdr);
		if (x != Nil)
			break;
		list = cdr;
	}
	nreverse(ret, list);
}

static void code_lambda_args(LocalRoot local, addr code, addr scope)
{
	addr list, var, opt, rest, key, allow, aux;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	ordinary_bind_opt(local, code, opt, &opt);
	ordinary_bind_key(local, code, key, &key);
	ordinary_bind_aux(local, code, aux, &aux);
	list_nil_heap(&list, var, opt, rest, key, allow, aux, NULL);
	SetEvalScopeIndex(scope, EvalLambda_Args, list);
}

static void code_lambda_body(LocalRoot local, addr code, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	code_queue_push_simple(local, code);
	code_allcons_set(local, code, list);
	code_queue_pop(local, code, &list);
	SetEvalScopeIndex(scope, EvalLambda_Cons, list);
}

static void code_lambda_value(addr scope)
{
	addr call, pos, value;

	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Table, &pos);
	value = Nil;
	if ((call != Nil) && getreference_tablefunction(pos))
		index_heap(&value, getlexical_tablefunction(pos));
	SetEvalScopeIndex(scope, EvalLambda_Self, value);
}

static void code_lambda_function(LocalRoot local, addr code, addr scope)
{
	addr pos;

	code_lambda_args(local, code, scope);
	code_lambda_body(local, code, scope);
	code_lambda_value(scope);

	/* execute */
	code_queue_push_simple(local, code);
	CodeQueue_cons(local, code, LAMBDA_EXECUTE, scope);
	code_queue_pop(local, code, &pos);
	SetEvalScopeIndex(scope, EvalLambda_Code, pos);

	/* lambda */
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, LAMBDA_PUSH, scope);
	else
		CodeQueue_cons(local, code, LAMBDA_SET, scope);
}

static void code_make_lambda(LocalRoot local, addr code, addr scope)
{
	if (! code_queue_remp(code))
		code_lambda_function(local, code, scope);
}


/* defun */
static void code_lambda_set(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_setmode(code, &mode);
	code_lambda_function(local, code, scope);
	code_queue_rollback(code, &mode);
}

static void code_lambda_push(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_pushmode(code, &mode);
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
static void code_macro_bind_args(LocalRoot, addr, addr, addr *);
static void code_macro_bind_var(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var))
			code_macro_bind_args(local, code, var, &var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void code_macro_bind_args(LocalRoot local, addr code, addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_var(local, code, var, &var);
	ordinary_bind_opt(local, code, opt, &opt);
	ordinary_bind_key(local, code, key, &key);
	ordinary_bind_aux(local, code, aux, &aux);
	list_nil_heap(ret, whole, env, var, opt, rest, key, allow, aux, NULL);
}

static void code_macro_args(LocalRoot local, addr code, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	code_macro_bind_args(local, code, list, &list);
	SetEvalScopeIndex(scope, EvalLambda_Args, list);
}

static void code_macro_function(LocalRoot local, addr code, addr scope)
{
	addr pos;

	code_macro_args(local, code, scope);
	code_lambda_body(local, code, scope);

	/* execute */
	code_queue_push_simple(local, code);
	CodeQueue_cons(local, code, MACRO_EXECUTE, scope);
	code_queue_pop(local, code, &pos);
	SetEvalScopeIndex(scope, EvalLambda_Code, pos);

	/* lambda */
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, MACRO_PUSH, scope);
	else
		CodeQueue_cons(local, code, MACRO_SET, scope);
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
	CodeQueue_double(local, code, DEFMACRO, name, lambda);
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
static void code_bind_body(LocalRoot local, addr code, addr scope)
{
	addr list;

	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	code_queue_push_simple(local, code);
	code_allcons_set(local, code, list);
	code_queue_pop(local, code, &list);
	SetEvalScopeIndex(scope, EvalLambda_Cons, list);
}

static void code_make_destructuring_bind(LocalRoot local, addr code, addr scope)
{
	addr expr, macro;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &macro); /* macro-lambda */
	code_macro_args(local, code, macro);
	code_bind_body(local, code, macro);

	code_make_execute_set(local, code, expr);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, BIND_PUSH, macro);
	else
		CodeQueue_cons(local, code, BIND_SET, macro);
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
static void code_make_flet_args(LocalRoot local, addr code, addr args, addr *ret)
{
	addr list, x, y;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCons(x, &x, &y);
		code_queue_push_simple(local, code);
		code_lambda_push(local, code, y);
		code_queue_pop(local, code, &y);
		cons_heap(&list, x, list);
		cons_heap(&list, y, list);
	}
	nreverse(ret, list);
}

static void code_make_flet(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free;

	/* make code */
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_make_flet_args(local, code, args, &args);
	code_make_let_body(local, code, cons, &cons);
	/* execute */
	list_heap(&cons, args, cons, free, NULL);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, FLET_PUSH, cons);
	else
		CodeQueue_cons(local, code, FLET_SET, cons);
}


/* labels */
static void code_make_labels_args(LocalRoot local, addr code, addr args, addr *ret)
{
	addr list, x, y;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &x, &args);
		GetCons(x, &x, &y);
		code_queue_push_simple(local, code);
		code_lambda_set(local, code, y);
		code_queue_pop(local, code, &y);
		cons_heap(&list, x, list);
		cons_heap(&list, y, list);
	}
	nreverse(ret, list);
}

static void code_make_labels(LocalRoot local, addr code, addr scope)

{
	addr args, cons, free;

	/* make code */
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_make_labels_args(local, code, args, &args);
	code_make_let_body(local, code, cons, &cons);
	/* execute */
	list_heap(&cons, args, cons, free, NULL);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, LABELS_PUSH, cons);
	else
		CodeQueue_cons(local, code, LABELS_SET, cons);
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
	CodeQueue_cons(local, code, EXECUTE_NORMAL_SET, pos);
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


/* locally */
static void code_make_locally(LocalRoot local, addr code, addr scope)
{
	addr cons, free;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);
	if (free != Nil)
		CodeQueue_cons(local, code, LOCALLY_DECLARE, free);
	code_allcons(local, code, cons);
}


/* if */
static void code_make_if(LocalRoot local, addr code, addr scope)
{
	addr expr, then, last;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	code_make_execute_set(local, code, expr);
	/* then */
	code_queue_push_simple(local, code);
	code_make_execute(local, code, then);
	code_queue_pop(local, code, &then);
	/* else */
	code_queue_push_simple(local, code);
	code_make_execute(local, code, last);
	code_queue_pop(local, code, &last);
	/* code */
	CodeQueue_double(local, code, IF, then, last);
}


/* unwind-protect */
static void code_make_unwind_protect(LocalRoot local, addr code, addr scope)
{
	addr form, cons;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);
	/* cleanup */
	code_queue_push_simple(local, code);
	code_allcons_rem(local, code, cons);
	code_queue_pop(local, code, &cons);
	/* protect */
	code_queue_push_new(local, code);
	CodeQueue_cons(local, code, UNWIND_PROTECT, cons);
	code_make_execute_set(local, code, form);
	code_queue_pop(local, code, &form);
	/* set code */
	code_make_execute_normal(local, code, form);
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
	CodeQueue_cons(local, code, EXECUTE_NORMAL_SET, form);
}


/* multiple-value-bind */
static void code_make_multiple_value_bind(LocalRoot local, addr code, addr scope)
{
	addr args, expr, cons, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);

	/* execute */
	code_make_execute_set(local, code, expr);
	code_make_let_body(local, code, cons, &cons);
	list_heap(&cons, args, cons, free, NULL);
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, BIND_VALUES_PUSH, cons);
	else
		CodeQueue_cons(local, code, BIND_VALUES_SET, cons);
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
	code_make_execute_normal(local, code, pos);
}


/* multiple-value-prog1 */
static void code_make_multiple_value_prog1(LocalRoot local, addr code, addr scope)
{
	enum CodeQueue_Mode mode;
	addr first, cons;

	mode = code_queue_mode(code);
	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &cons);

	/* rem */
	if (mode == CodeQueue_ModeRemove) {
		cons_heap(&cons, first, cons);
		code_allcons(local, code, cons);
		return;
	}

	/* first */
	code_queue_push_simple(local, code);
	code_make_execute_set(local, code, first);
	code_queue_pop(local, code, &first);
	/* cons */
	code_queue_push_simple(local, code);
	code_allcons_rem(local, code, cons);
	code_queue_pop(local, code, &cons);
	/* execute */
	if (code_queue_pushp(code))
		CodeQueue_double(local, code, PROG1_PUSH, first, cons);
	else
		CodeQueue_double(local, code, PROG1_SET, first, cons);
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
	code_make_execute_normal(local, code, pos);
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
	code_make_execute_normal(local, code, pos);
}


/* load-time-value */
static void code_make_load_time_value_body(LocalRoot local, addr code, addr scope)
{
	addr check, root, index, expr, value, list, readonly;
	size_t size, i;

	GetEvalScopeIndex(scope, 0, &check);
	GetEvalScopeIndex(scope, 1, &root);
	GetEvalScopeIndex(scope, 2, &index);
	GetEvalScopeIndex(scope, 3, &value);
	Check(check == Nil, "check error");

	code_queue_push_new(local, code);
	/* alloc */
	CodeQueue_cons(local, code, LOAD_TIME_VALUE_ALLOC, index);
	GetIndex(index, &size);
	/* set */
	for (i = 0; i < size; i++) {
		GetCons(value, &list, &value);
		List_bind(list, &index, &expr, &readonly, NULL);
		code_make_execute_set(local, code, expr);
		CodeQueue_double(local, code, LOAD_TIME_VALUE_VALUE, index, readonly);
	}
	code_make_execute(local, code, root);
	code_queue_pop(local, code, &expr);
	code_make_execute_normal(local, code, expr);
}

static void code_make_load_time_value_expr(LocalRoot local, addr code, addr scope)
{
	addr check, index, expr, readonly;

	GetEvalScopeIndex(scope, 0, &check);
	GetEvalScopeIndex(scope, 1, &index);
	GetEvalScopeIndex(scope, 2, &expr);
	GetEvalScopeIndex(scope, 3, &readonly);
	Check(check != Nil, "check error");

	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, LOAD_TIME_VALUE_SET, index);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, LOAD_TIME_VALUE_PUSH, index);
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
	code_make_execute_switch(local, code, args);

	return 1;
}

static int code_make_specialize_restart(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	code_make_specialize_allcons(local, code, args);
	code_queue_pop(local, code, &args);
	code_make_execute_switch(local, code, args);

	return 1;
}

static int code_make_specialize_push_return(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	code_queue_push_new(local, code);
	code_make_specialize_allcons(local, code, args);
	code_queue_pop(local, code, &args);
	code_make_execute_normal(local, code, args);

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
static void code_make_call_args(LocalRoot local, addr code, addr args)
{
	addr pos, value;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &value);
		code_make_execute_push(local, code, value);
		if (getcheck_tablecall(pos)) {
			gettype_tablecall(pos, &value);
			if (! type_astert_p(value)) {
				CodeQueue_cons(local, code, CALL_TYPE, value);
			}
		}
	}
}

static int code_make_common_p(addr x)
{
	addr y;

	getname_tablefunction(x, &x);
	GetCallName(x, &x);
	GetPackageSymbol(x, &x);
	GetConst(PACKAGE_COMMON_LISP, &y);

	return x == y;
}

static int code_make_call_common(LocalRoot local, addr code, addr table)
{
	int symbolp, globalp;
	addr name, value;

	getname_tablefunction(table, &name);
	symbolp = symbolp_callname(name);
	globalp = getglobalp_tablefunction(table);
	GetCallName(name, &name);

	if (! globalp)
		return 0;
	if (symbolp)
		GetFunctionSymbol(name, &value);
	else
		getsetf_symbol(name, &value);
	if (value == Unbound)
		return 0;

	/* common-lisp function */
	CodeQueue_cons(local, code, CALL, value);
	return 1;
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
	if (code_make_common_p(table)) {
		if (code_make_call_common(local, code, table))
			return;
	}
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
	/* args -> first*/
	code_make_call_args(local, code, args);
	code_make_call_first(local, code, first);
	/* execute */
	code_queue_pop(local, code, &pos);
	code_make_execute_normal(local, code, pos);
}


/*
 *  code
 */
typedef void (*code_make_calltype)(LocalRoot, addr, addr);
static code_make_calltype CodeMakeTable[EVAL_PARSE_SIZE];

_g void code_make_execute(LocalRoot local, addr code, addr scope)
{
	EvalParse type;
	code_make_calltype call;

	Check(! eval_scope_p(scope), "type error");
	GetEvalScopeType(scope, &type);
	call = CodeMakeTable[type];
	if (call == NULL) {
		fmte("Invalid scope type.", NULL);
		return;
	}
	(*call)(local, code, scope);
}

_g void code_make(LocalRoot local, addr *ret, addr scope)
{
	addr code;
	LocalStack stack;

	push_local(local, &stack);
	code_queue_local(local, &code);
	code_make_execute_set(local, code, scope);
	code_queue_pop(local, code, ret);
	rollback_local(local, stack);
}

_g void init_code_make(void)
{
	CodeMakeTable[EVAL_PARSE_NIL] = code_make_nil;
	CodeMakeTable[EVAL_PARSE_T] = code_make_t;
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
	CodeMakeTable[EVAL_PARSE_VALUES] = code_make_values;
	CodeMakeTable[EVAL_PARSE_LOCALLY] = code_make_locally;
	CodeMakeTable[EVAL_PARSE_CALL] = code_make_call;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = code_make_multiple_value_bind;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = code_make_multiple_value_call;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = code_make_multiple_value_prog1;
	CodeMakeTable[EVAL_PARSE_NTH_VALUE] = code_make_nth_value;
	CodeMakeTable[EVAL_PARSE_PROGV] = code_make_progv;
	CodeMakeTable[EVAL_PARSE_LOAD_TIME_VALUE] = code_make_load_time_value;
}

