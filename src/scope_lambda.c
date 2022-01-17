#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "hold.h"
#include "parse_object.h"
#include "scope_declare.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_let.h"
#include "scope_object.h"
#include "scope_typedef.h"
#include "type_table.h"
#include "typedef.h"

void scope_init_lambda(struct lambda_struct *str, EvalParse eval, int globalp)
{
	clearpoint(str);
	str->stack = str->call = str->table = str->lexical =
		str->args = str->decl = str->doc = str->cons =
		str->clos = str->free = str->the =
		str->form = str->defun = str->body_the = Nil;
	str->globalp = globalp;
	str->eval = eval;
}

void localhold_lambda_struct(LocalRoot local, struct lambda_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->call, str->table, str->lexical,
			str->args, str->decl, str->doc, str->cons,
			str->clos, str->free, str->the,
			str->form, str->defun, str->body_the, NULL);
}

void lambda_lexical_heap(addr stack, addr *ret)
{
	addr index, list, root, pos;

	getlexical_index_heap(stack, &index);
	if (index == Nil) {
		*ret = Nil;
		return;
	}

	/* closure index */
	GetEvalStackLexical(stack, &list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) == EvalTable_Value) {
			get_evaltable(pos, &pos);
			if (getbasep_tablevalue(pos)) {
				index_heap(&pos, getlexical_tablevalue(pos));
				cons_heap(&root, pos, root);
			}
		}
	}

	/* result */
	cons_heap(ret, index, root);
}


/*
 *  lambda
 */
static int scope_lambda_init_var_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr var, list;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &var, &args);
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(&list, var, list);
	}
	nreverse(ret, list);

	return 0;
}

static int scope_lambda_init_opt_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		Return(ifdeclvalue_(ptr, stack, svar, decl, &svar));
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_key_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		Return(ifdeclvalue_(ptr, stack, svar, decl, &svar));
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_aux_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_lambda_init_(Execute ptr, struct lambda_struct *str)
{
	addr stack, decl, args, var, opt, rest, key, allow, aux;

	stack = str->stack;
	decl = str->decl;
	args = str->args;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* scope */
	Return(scope_lambda_init_var_(ptr, stack, var, decl, &var));
	Return(scope_lambda_init_opt_(ptr, stack, opt, decl, &opt));
	Return(ifdeclvalue_(ptr, stack, rest, decl, &rest));
	Return(scope_lambda_init_key_(ptr, stack, key, decl, &key));
	Return(scope_lambda_init_aux_(ptr, stack, aux, decl, &aux));
	list_heap(&str->args, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static void scope_lambda_tablevalue_force(addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		setcheck_tablevalue(pos, 1);
	}
}

static void scope_lambda_tablevalue_single(addr pos)
{
	if (pos != Nil) {
		setcheck_tablevalue(pos, 1);
	}
}

static int scope_lambda_tablevalue_opt_(Execute ptr, addr args)
{
	addr list, var, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(checkvalue_bind_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_key_(Execute ptr, addr args)
{
	addr list, var, name, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(checkvalue_bind_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_aux_(Execute ptr, addr args)
{
	addr list, var, init;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(checkvalue_bind_(ptr, var, init));
	}

	return 0;
}

static int scope_lambda_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	scope_lambda_tablevalue_force(var);
	Return(scope_lambda_tablevalue_opt_(ptr, opt));
	scope_lambda_tablevalue_single(rest);
	Return(scope_lambda_tablevalue_key_(ptr, key));
	Return(scope_lambda_tablevalue_aux_(ptr, aux));

	return 0;
}

static void scope_lambda_type_var(addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_opt(addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCar(var, &var);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_rest(addr rest, addr *ret)
{
	if (rest != Nil)
		GetTypeTable(ret, T);
}

static void scope_lambda_type_key(addr args, addr allow, addr *ret)
{
	addr root, var, name;

	if (allow != Nil) {
		/* &allow-other-keys */
		*ret = T;
		return;
	}
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &name);
		GetCar(name, &name);
		gettype_tablevalue(var, &var);
		copyheap(&var, var);
		cons_heap(&var, name, var);
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);
}

static void scope_lambda_type_make(addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, array;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	/* arg-typespec */
	scope_lambda_type_var(var, &var);
	scope_lambda_type_opt(opt, &opt);
	scope_lambda_type_rest(rest, &rest);
	scope_lambda_type_key(key, allow, &key);

	/* type-function vector */
	vector2_heap(&array, 4);
	SetArrayA2(array, 0, var);
	SetArrayA2(array, 1, opt);
	SetArrayA2(array, 2, rest);
	SetArrayA2(array, 3, key); /* &key or &allow-other-keys */
	*ret = array;
}

static void scope_lambda_type_incomplete(addr args, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	scope_lambda_type_make(args, &args);
	type3_heap(LISPDECL_FUNCTION, args, aster, Nil, ret);
}

static void scope_lambda_type_table(Execute ptr, struct lambda_struct *str, addr type)
{
	addr table;

	CheckType(str->call, LISPTYPE_CALLNAME);
	make_tablefunction_stack(ptr, &table, str->stack, str->call);
	setglobalp_tablefunction(table, 0);
	settype_tablefunction(table, type);
	str->table = table;
}

static void scope_lambda_declare(Execute ptr, struct lambda_struct *str)
{
	addr type;

	/* incomplete type */
	scope_lambda_type_incomplete(str->args, &type);
	str->the = type;

	/* tablefunction */
	if (str->call != Nil)
		scope_lambda_type_table(ptr, str, type);
}

static int scope_lambda_progn_(Execute ptr, struct lambda_struct *str)
{
	addr the, type;

	Return(scope_allcons(ptr, &str->cons, &type, str->cons));
	gchold_pushva_local(ptr->local, str->cons, type, NULL);
	/* (function [args] *) -> (function [args] [values]) */
	the = str->the;
	SetArrayType(the, 1, type);
	copylocal_object(NULL, &the, the);
	if (str->table != Nil)
		settype_tablefunction(str->table, the);
	str->the = the;

	return 0;
}

static void scope_lambda_closure_list(addr stack, addr *ret)
{
	addr list, root, pos;

	GetEvalStackLexical(stack, &list);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (getclosurep_evaltable(pos))
			cons_heap(&root, pos, root);
	}
	*ret = root;
}

static int scope_lambda_closure_(Execute ptr, struct lambda_struct *str)
{
	addr stack, list;

	Return(getstack_eval_(ptr, &stack));
	scope_lambda_closure_list(stack, &list);
	str->clos = list;

	return 0;
}

static void scope_lambda_lexical(struct lambda_struct *str)
{
	lambda_lexical_heap(str->stack, &str->lexical);
}

static int scope_lambda_heap_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(eval_scope_size_(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	SetEvalScopeIndex(eval, EvalLambda_Table, str->table);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	SetEvalScopeIndex(eval, EvalLambda_Form, str->form);
	SetEvalScopeIndex(eval, EvalLambda_Defun, str->defun);
	SetEvalScopeIndex(eval, EvalLambda_Lexical, str->lexical);

	return Result(ret, eval);
}

static int scope_lambda_execute_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack;

	stack = str->stack;
	Return(scope_lambda_init_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_lambda_tablevalue_(ptr, str->args));
	scope_lambda_declare(ptr, str);
	Return(scope_lambda_progn_(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(scope_lambda_closure_(ptr, str));
	scope_lambda_lexical(str);
	Return(scope_lambda_heap_(ptr, str, ret));

	return 0;
}

int scope_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_lambda_call_(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_LAMBDA, 0);
	GetEvalParse(eval, 0, &str.form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	Return(scope_lambda_object_(ptr, &str, &eval));
	SetEvalScopeValue(eval, str.form);

	return Result(ret, eval);
}


/*
 *  macro-lambda
 */
static int macro_lambda_init_args_(Execute ptr, addr, addr, addr, addr *);
static int macro_lambda_init_var_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_lambda_init_args_(ptr, stack, var, decl, &var));
		}
		else {
			Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		}
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int macro_lambda_init_rest_(Execute ptr,
		addr stack, addr rest, addr decl, addr *ret)
{
	addr var, type;

	if (rest == Nil) {
		*ret = Nil;
	}
	else {
		GetCons(rest, &var, &type);
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(ret, var, type);
	}

	return 0;
}

static int macro_lambda_init_args_(Execute ptr,
		addr stack, addr args, addr decl, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_lambda_init_var_(ptr, stack, var, decl, &var));
	Return(scope_lambda_init_opt_(ptr, stack, opt, decl, &opt));
	Return(macro_lambda_init_rest_(ptr, stack, rest, decl, &rest));
	Return(scope_lambda_init_key_(ptr, stack, key, decl, &key));
	Return(scope_lambda_init_aux_(ptr, stack, aux, decl, &aux));
	Return(ifdeclvalue_(ptr, stack, whole, decl, &whole));
	Return(ifdeclvalue_(ptr, stack, env, decl, &env));
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int macro_lambda_init_(Execute ptr, struct lambda_struct *str)
{
	return macro_lambda_init_args_(ptr, str->stack, str->args, str->decl, &str->args);
}

static int macro_lambda_tablevalue_(Execute ptr, addr args);
static int macro_lambda_tablevalue_var_(Execute ptr, addr args)
{
	addr var;

	while (args != Nil) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_lambda_tablevalue_(ptr, var));
		}
		else {
			scope_lambda_tablevalue_single(var);
		}
	}

	return 0;
}

static void macro_lambda_tablevalue_rest(addr rest)
{
	if (rest != Nil) {
		GetCar(rest, &rest);
		setcheck_tablevalue(rest, 1);
	}
}

static int macro_lambda_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_lambda_tablevalue_var_(ptr, var));
	Return(scope_lambda_tablevalue_opt_(ptr, opt));
	macro_lambda_tablevalue_rest(rest);
	Return(scope_lambda_tablevalue_key_(ptr, key));
	Return(scope_lambda_tablevalue_aux_(ptr, aux));
	scope_lambda_tablevalue_single(whole);
	scope_lambda_tablevalue_single(env);

	return 0;
}

static int macro_lambda_progn_(Execute ptr, struct lambda_struct *str)
{
	return scope_allcons(ptr, &str->cons, &str->body_the, str->cons);
}

static int scope_macro_lambda_heap_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(eval_scope_size_(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	SetEvalScopeIndex(eval, EvalLambda_Args, str->args);
	SetEvalScopeIndex(eval, EvalLambda_Decl, str->decl);
	SetEvalScopeIndex(eval, EvalLambda_Doc, str->doc);
	SetEvalScopeIndex(eval, EvalLambda_Cons, str->cons);
	SetEvalScopeIndex(eval, EvalLambda_Clos, str->clos);
	SetEvalScopeIndex(eval, EvalLambda_The, str->the);
	SetEvalScopeIndex(eval, EvalLambda_Free, str->free);
	SetEvalScopeIndex(eval, EvalLambda_Lexical, str->lexical);

	return Result(ret, eval);
}

int scope_macro_lambda_execute_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack;

	stack = str->stack;
	Return(macro_lambda_init_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(macro_lambda_tablevalue_(ptr, str->args));
	Return(macro_lambda_progn_(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(scope_lambda_closure_(ptr, str));
	scope_lambda_lexical(str);

	return scope_macro_lambda_heap_(ptr, str, ret);
}

int scope_macro_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(scope_macro_lambda_execute_(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

static void scope_macro_lambda_the(addr eval)
{
	addr type;
	GetTypeCompiled(&type, MacroFunction);
	SetEvalScopeThe(eval, type);
}

int scope_macro_lambda_call_(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(scope_macro_lambda_object_(ptr, str, &eval));
	scope_macro_lambda_the(eval);
	return Result(ret, eval);
}

