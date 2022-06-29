#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "execute.h"
#include "parse_object.h"
#include "scope_declare.h"
#include "scope_defun.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_let.h"
#include "scope_object.h"
#include "scope_typedef.h"
#include "type.h"
#include "type_object.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  defun
 */
static int scope_defun_update_(Execute ptr, struct lambda_struct *str)
{
	addr stack, table;

	Return(getglobal_eval_(ptr, &stack));
	Return(push_tablefunction_global_(ptr, stack, str->call, &table));
	settype_tablefunction(table, str->the);

	return 0;
}

static int scope_defun_the_(addr eval, struct lambda_struct *str)
{
	addr cdr, type, null, setf, call;

	switch (RefCallNameType(str->call)) {
		case CALLNAME_SYMBOL:
			GetTypeTable(&type, Symbol);
			break;

		case CALLNAME_SETF:
			/* (setf hello) -> (cons (eql setf) (cons (eql hello) null)) */
			GetConst(COMMON_SETF, &setf);
			type_eql_heap(setf, &setf);
			GetCallName(str->call, &call);
			type_eql_heap(call, &call);
			GetTypeTable(&null, Null);
			type2_heap(LISPDECL_CONS, call, null, &cdr);
			type2_heap(LISPDECL_CONS, setf, cdr, &type);
			break;

		default:
			return fmte_("callname error.", NULL);
	}
	SetEvalScopeThe(eval, type);

	return 0;
}

int scope_defun_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_lambda_object_(ptr, str, &eval));
	Return(scope_defun_update_(ptr, str));
	Return(scope_defun_the_(eval, str));
	SetEvalScopeValue(eval, str->defun);

	return Result(ret, eval);
}


/*
 *  deftype
 */
int scope_deftype_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  define-compiler-macro
 */
int scope_define_compiler_macro_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  destructuring-bind
 */
static int scope_bind_lambda_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_macro_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_bind_call_(Execute ptr, addr *ret, addr form, addr expr, addr args)
{
	addr lambda, eval;
	struct lambda_struct str;

	/* macro-lambda */
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(args, 0, &str.args);
	GetEvalParse(args, 1, &str.decl);
	GetEvalParse(args, 2, &str.doc);
	GetEvalParse(args, 3, &str.cons);
	Return(scope_bind_lambda_(ptr, &str, &lambda));

	/* result */
	Return(eval_scope_size_(ptr, &eval, 5,
				EVAL_PARSE_DESTRUCTURING_BIND, str.body_the, form));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, str.args);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.cons);
	SetEvalScopeIndex(eval, 4, str.free);
	return Result(ret, eval);
}


/*
 *  flet
 */
static int scope_flet_lambda_(Execute ptr, addr list, addr *call, addr *ret)
{
	addr pos;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	*call = str.call;
	str.call = Nil;
	Return(scope_lambda_object_(ptr, &str, &pos));
	SetEvalScopeIndex(pos, EvalLambda_Call, *call);

	return Result(ret, pos);
}

static int scope_flet_init_(Execute ptr, struct let_struct *str)
{
	addr args, root, call, eval;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_flet_lambda_(ptr, eval, &call, &eval));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void scope_flet_maketable(Execute ptr, struct let_struct *str)
{
	addr stack, args, list, call;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &list, &args);
		GetCar(list, &call);
		make_tablefunction_stack(ptr, &call, stack, call);
		SetCar(list, call);
	}
}

static int scope_checktype_function_(Execute ptr, addr table, addr eval)
{
	int false_p, exclude_p;
	addr name, type;

	getname_tablefunction(table, &name);
	gettype_tablefunction(table, &type);
	if (type == Nil)
		return 0;
	GetEvalScopeThe(eval, &eval);
	Return(checktype_p_(ptr, type, eval, &false_p, &exclude_p));
	if (exclude_p) {
		GetCallName(name, &name);
		Return(type_object_(ptr, &eval, eval));
		Return(type_object_(ptr, &type, type));
		Return(fmte_("The function ~S must be ~S type, but ~S.",
					name, eval, type, NULL));
	}

	return 0;
}

static int scope_flet_applytable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(update_tablefunction_(ptr, stack, call));
		Return(scope_checktype_function_(ptr, call, eval));
	}

	return 0;
}

static int scope_flet_ignore_(addr stack)
{
	enum IgnoreType ignore;
	int reference;
	addr list, pos, call, value;

	GetEvalStackScope(stack, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) != EvalTable_Function)
			continue;
		get_evaltable(pos, &value);
		getname_tablefunction(value, &call);
		/* check ignore */
		ignore = getignore_tablefunction(value);
		reference = getreference_tablefunction(value);

		if (ignore == IgnoreType_None && (! reference)) {
			name_callname_heap(call, &call);
			Return(call_simple_style_warning_va_(NULL,
						"Unused function ~S.", call, NULL));
		}
		if (ignore == IgnoreType_Ignore && reference) {
			name_callname_heap(call, &call);
			Return(call_simple_style_warning_va_(NULL,
						"Ignore function ~S used.", call, NULL));
		}
	}

	return 0;
}

static int scope_flet_execute_(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(scope_flet_init_(ptr, str));
	scope_flet_maketable(ptr, str);
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_flet_applytable_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(scope_flet_ignore_(stack));

	return 0;
}

int scope_flet_call_(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(scope_flet_execute_(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/*
 *  labels
 */
static void scope_labels_table_push(Execute ptr, addr stack, addr list)
{
	addr call, args, decl, pos;

	Lista_bind(list, &call, &args, &decl, &pos, NULL);
	make_tablefunction_stack(ptr, &pos, stack, call);
	apply_declare_function_stack(ptr->local, stack, call, decl);
}

static void scope_labels_table(Execute ptr, struct let_struct *str)
{
	addr args, stack, eval;

	args = str->args;
	stack = str->stack;
	while (args != Nil) {
		GetCons(args, &eval, &args);
		scope_labels_table_push(ptr, stack, eval);
	}
}

static int scope_labels_lambda_(Execute ptr, addr list, addr *ret)
{
	addr eval;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	Return(scope_lambda_object_(ptr, &str, &eval));
	return Result(ret, eval);
}

static int scope_labels_init_(Execute ptr, struct let_struct *str)
{
	addr stack, args, root, call, eval;

	stack = str->stack;
	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_labels_lambda_(ptr, eval, &eval));
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		Return(push_tablefunction_global_(ptr, stack, call, &call));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static int scope_labels_checktype_(Execute ptr, struct let_struct *str)
{
	addr args, call, eval;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(scope_checktype_function_(ptr, call, eval));
	}

	return 0;
}

static int scope_labels_execute_(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	scope_labels_table(ptr, str);
	Return(scope_labels_init_(ptr, str));
	Return(scope_labels_checktype_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(scope_flet_ignore_(stack));

	return 0;
}

int scope_labels_call_(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(scope_labels_execute_(ptr, str));

	return freestack_eval_(ptr, str->stack);
}

