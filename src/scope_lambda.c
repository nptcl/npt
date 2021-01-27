#include "callname.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "copy.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "parse.h"
#include "parse_object.h"
#include "scope_object.h"
#include "scope_declare.h"
#include "scope_lambda.h"
#include "symbol.h"
#include "type_table.h"
#include "type_object.h"

/*
 *  function
 */
static int dynamic_stack_tablefunction(addr stack, addr call, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_FUNCTION, &key);
	if (getplist(table, key, &value) == 0
			&& find_list_callname_unsafe(call, value)) {
		*ret = 1;
		return 1;
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getdynamic_tablefunction(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablefunction(addr stack, addr call)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablefunction(addr stack, addr call, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_FUNCTION, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_IGNORE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignore;
			return 1;
		}
		GetConst(COMMON_IGNORABLE, &check);
		if (check == value) {
			*ret = IgnoreType_Ignorable;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getignore_tablefunction(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablefunction(addr stack, addr call)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int inline_stack_tablefunction(addr stack, addr call, enum InlineType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* inline, notinline declaration */
	GetConst(SYSTEM_INLINE, &key);
	if (getpplist_callname(table, key, call, &value) == 0) {
		GetConst(COMMON_INLINE, &check);
		if (check == value) {
			*ret = InlineType_Inline;
			return 1;
		}
		GetConst(COMMON_NOTINLINE, &check);
		if (check == value) {
			*ret = InlineType_NotInline;
			return 1;
		}
		/* through */
	}
	/* table value */
	if (getfunction_scope_evalstack(stack, call, &value)) {
		*ret = getinline_tablefunction(value);
		return 1;
	}

	return 0;
}
static int inline_tablefunction_(Execute ptr,
		addr stack, addr call, enum InlineType *ret)
{
	enum InlineType result;

	/* local stack */
	while (stack != Nil) {
		if (inline_stack_tablefunction(stack, call, &result)) {
			return Result(ret, result);
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	if (inline_stack_tablefunction(stack, call, &result)) {
		return Result(ret, result);
	}

	/* not inline or notinline */
	return Result(ret, InlineType_None);
}

static void gettype_global_callname(LocalRoot local, addr call, addr *ret)
{
	CallNameType check;

	GetCallNameType(call, &check);
	GetCallName(call, &call);
	if (check == CALLNAME_SYMBOL)
		gettype_function_symbol(call, ret);
	else
		gettype_setf_symbol(call, ret);
}

static int type_free_tablefunction(addr stack, addr call, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_FUNCTION, &key);
	return getpplist_callname(stack, key, call, ret) == 0;
}

static int type_boundary_tablefunction(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, &call))
		return 0;
	gettype_tablefunction(call, ret);
	return 1;
}

static int type_tablefunction_(Execute ptr, LocalRoot local,
		addr stack, addr call, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablefunction(stack, call, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablefunction(stack, call, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	/* free declaration */
	if (type_free_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablefunction(stack, call, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_global_callname(local, call, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse(ret, root);
	return 0;
}

static void push_tablefunction_lexical(Execute ptr, addr stack, addr pos)
{
	addr name;

	if (stack == Nil) {
		setglobalp_tablefunction(pos, 1);
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tablefunction(pos, &name);
		setlexical_tablefunction(pos, increment_stack_eval(stack));
		setfunction_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablefunction_lexical(ptr, stack, pos);
	}
}

static void tablefunction_update(Execute ptr, addr stack, addr call)
{
	push_tablefunction_lexical(ptr, stack, call);
}

static int make_tablefunction_stack(Execute ptr, addr *ret, addr stack, addr call)
{
	addr pos, aster;

	CheckType(call, LISPTYPE_CALLNAME);
	if (getfunction_scope_evalstack(stack, call, ret))
		return 0;
	copyheap(&call, call);
	make_tablefunction(&pos, call);
	GetTypeTable(&aster, Function);
	settype_tablefunction(pos, aster);
	setfunction_scope_evalstack(stack, pos);
	tablefunction_update(ptr, stack, pos);
	*ret = pos;

	return 1;
}

static int update_tablefunction_(Execute ptr, addr stack, addr pos)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr name, type;

	/* scope */
	getname_tablefunction(pos, &name);
	Return(globalp_tablefunction_(ptr, stack, name, &globalp));
	dynamic = dynamic_tablefunction(stack, name);
	ignore = ignore_tablefunction(stack, name);
	Return(inline_tablefunction_(ptr, stack, name, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, name, &type));
	if (type_and_array(NULL, type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return 0;
}
static int push_tablefunction_global_(Execute ptr, addr stack, addr call, addr *ret)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr pos, type;

	/* scope */
	Return(globalp_tablefunction_(ptr, stack, call, &globalp));
	dynamic = dynamic_tablefunction(stack, call);
	ignore = ignore_tablefunction(stack, call);
	Return(inline_tablefunction_(ptr, stack, call, &Inline));
	Return(type_tablefunction_(ptr, NULL, stack, call, &type));
	if (type_and_array(NULL, type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	make_tablefunction_stack(ptr, &pos, stack, call);
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);

	return Result(ret, pos);
}

static int callname_global_tablefunction_(Execute ptr, addr *ret, addr call)
{
	addr stack;

	Return(getglobal_eval_(ptr, &stack));
	if (! find_tablefunction(stack, call, ret)) {
		Return(push_tablefunction_global_(ptr, stack, call, ret));
	}

	return 0;
}

static void push_closure_function(addr stack, addr call, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablefunction(value);
	/* destination table */
	make_redirect_tablefunction(&pos, value);
	setclosurep_tablefunction(pos, 1);
	setclosure_tablefunction(pos, lexical);
	setlexical_tablefunction(pos, increment_stack_eval(stack));
	setfunction_lexical_evalstack(stack, pos);
	setfunction_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int callname_tablefunction_(Execute ptr,
		addr stack, addr call, addr *value, int *ret)
{
	int check;
	addr next;

	/* global */
	if (stack == Nil) {
		Return(callname_global_tablefunction_(ptr, value, call));
		return Result(ret, 1); /* global-scope */
	}

	/* local */
	if (getfunction_scope_evalstack(stack, call, value)) {
		setreference_tablefunction(*value, 1);
		return Result(ret, getglobalp_tablefunction(*value));
	}

	/* next */
	GetEvalStackNext(stack, &next);
	Return(callname_tablefunction_(ptr, next, call, value, &check));
	if (check) {
		return Result(ret, 1); /* global-scope */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_function(stack, call, *value, value);
	}

	return Result(ret, 0); /* local-scope */
}

static int scope_function_object(Execute ptr, addr *ret, addr eval)
{
	addr type;

	gettype_function(eval, &type);
	if (type == Nil)
		GetTypeTable(&type, Function);

	return make_eval_scope_(ptr, ret, EVAL_PARSE_FUNCTION, type, eval);
}

static int scope_function_callname(Execute ptr, addr *ret, addr call)
{
	int ignore;
	addr value, type, stack;

	Return(getstack_eval_(ptr, &stack));
	Return(callname_tablefunction_(ptr, stack, call, &value, &ignore));
	gettype_tablefunction(value, &type);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_FUNCTION, type, value);
}

int scope_function_call(Execute ptr, addr *ret, addr eval)
{
	GetEvalParse(eval, 0, &eval);
	if (functionp(eval))
		return scope_function_object(ptr, ret, eval);
	if (callnamep(eval))
		return scope_function_callname(ptr, ret, eval);

	return fmte_("Invalid object type ~S", eval, NULL);
}


/*
 *  lambda
 */
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

static int lambda_init_var_(Execute ptr, addr stack, addr args, addr decl, addr *ret)
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

static int lambda_init_opt(Execute ptr, addr stack, addr args, addr decl, addr *ret)
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

static int lambda_init_key(Execute ptr, addr stack, addr args, addr decl, addr *ret)
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

static int lambda_init_aux(Execute ptr, addr stack, addr args, addr decl, addr *ret)
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

static int lambda_init(Execute ptr, struct lambda_struct *str)
{
	addr stack, decl, args, var, opt, rest, key, allow, aux;

	stack = str->stack;
	decl = str->decl;
	args = str->args;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* scope */
	Return(lambda_init_var_(ptr, stack, var, decl, &var));
	Return(lambda_init_opt(ptr, stack, opt, decl, &opt));
	Return(ifdeclvalue_(ptr, stack, rest, decl, &rest));
	Return(lambda_init_key(ptr, stack, key, decl, &key));
	Return(lambda_init_aux(ptr, stack, aux, decl, &aux));
	list_heap(&str->args, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static void lambda_tablevalue_force(addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		setcheck_tablevalue(pos, 1);
	}
}

static void lambda_tablevalue_single(addr pos)
{
	if (pos != Nil) {
		setcheck_tablevalue(pos, 1);
	}
}

static int lambda_tablevalue_opt_(Execute ptr, addr args)
{
	addr list, var, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int lambda_tablevalue_key_(Execute ptr, addr args)
{
	addr list, var, name, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int lambda_tablevalue_aux_(Execute ptr, addr args)
{
	addr list, var, init;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int lambda_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	lambda_tablevalue_force(var);
	Return(lambda_tablevalue_opt_(ptr, opt));
	lambda_tablevalue_single(rest);
	Return(lambda_tablevalue_key_(ptr, key));
	Return(lambda_tablevalue_aux_(ptr, aux));

	return 0;
}

static void type_ordinary_var(addr args, addr *ret)
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

static void type_ordinary_opt(addr args, addr *ret)
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

static void type_ordinary_rest(addr rest, addr *ret)
{
	if (rest != Nil)
		GetTypeTable(ret, T);
}

static void type_ordinary_key(addr args, addr allow, addr *ret)
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

static void make_type_ordinary(addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, array;

	/* destructuring-bind */
	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);

	/* arg-typespec */
	type_ordinary_var(var, &var);
	type_ordinary_opt(opt, &opt);
	type_ordinary_rest(rest, &rest);
	type_ordinary_key(key, allow, &key);

	/* type-function vector */
	vector2_heap(&array, 4);
	SetArrayA2(array, 0, var);
	SetArrayA2(array, 1, opt);
	SetArrayA2(array, 2, rest);
	SetArrayA2(array, 3, key); /* &key or &allow-other-keys */
	*ret = array;
}

static void lambda_type_incomplete(addr args, addr *ret)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	make_type_ordinary(args, &args);
	type3_heap(LISPDECL_FUNCTION, args, aster, Nil, ret);
}

static void lambda_make_table(Execute ptr, struct lambda_struct *str, addr type)
{
	addr table;

	CheckType(str->call, LISPTYPE_CALLNAME);
	make_tablefunction_stack(ptr, &table, str->stack, str->call);
	setglobalp_tablefunction(table, 0);
	settype_tablefunction(table, type);
	str->table = table;
}

static void lambda_declare(Execute ptr, struct lambda_struct *str)
{
	addr type;

	/* incomplete type */
	lambda_type_incomplete(str->args, &type);
	str->the = type;

	/* tablefunction */
	if (str->call != Nil)
		lambda_make_table(ptr, str, type);
}

static int lambda_progn(Execute ptr, struct lambda_struct *str)
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

static void lambda_closure_list(addr stack, addr *ret)
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

static int lambda_closure_(Execute ptr, struct lambda_struct *str)
{
	addr stack, list;

	Return(getstack_eval_(ptr, &stack));
	lambda_closure_list(stack, &list);
	str->clos = list;

	return 0;
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

static void lambda_lexical(struct lambda_struct *str)
{
	lambda_lexical_heap(str->stack, &str->lexical);
}

static int lambda_execute(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack, eval;

	/* lambda */
	stack = str->stack;
	Return(lambda_init(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(lambda_tablevalue_(ptr, str->args));
	lambda_declare(ptr, str);
	Return(lambda_progn(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(lambda_closure_(ptr, str));
	lambda_lexical(str);

	/* eval */
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

void localhold_lambda_struct(LocalRoot local, struct lambda_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->call, str->table, str->lexical,
			str->args, str->decl, str->doc, str->cons,
			str->clos, str->free, str->the,
			str->form, str->defun, str->body_the, NULL);
}

static int lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(lambda_execute(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_lambda_call(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_LAMBDA, 0);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	GetEvalParse(eval, 4, &str.form);
	return lambda_object(ptr, &str, ret);
}


/*
 *  defun
 */
static int defun_update_(Execute ptr, struct lambda_struct *str)
{
	addr stack, table;

	Return(getglobal_eval_(ptr, &stack));
	Return(push_tablefunction_global_(ptr, stack, str->call, &table));
	settype_tablefunction(table, str->the);

	return 0;
}

static int defun_the_(addr eval, struct lambda_struct *str)
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

int scope_defun_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(lambda_object(ptr, str, &eval));
	Return(defun_update_(ptr, str));
	Return(defun_the_(eval, str));

	return Result(ret, eval);
}


/*
 *  macro-lambda
 */
static int macro_init_args(Execute ptr, addr, addr, addr, addr *);
static int macro_init_var(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_init_args(ptr, stack, var, decl, &var));
		}
		else {
			Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		}
		cons_heap(&root, var, root);
	}
	nreverse(ret, root);

	return 0;
}

static int macro_init_rest_(Execute ptr, addr stack, addr rest, addr decl, addr *ret)
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

static int macro_init_args(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_init_var(ptr, stack, var, decl, &var));
	Return(lambda_init_opt(ptr, stack, opt, decl, &opt));
	Return(macro_init_rest_(ptr, stack, rest, decl, &rest));
	Return(lambda_init_key(ptr, stack, key, decl, &key));
	Return(lambda_init_aux(ptr, stack, aux, decl, &aux));
	Return(ifdeclvalue_(ptr, stack, whole, decl, &whole));
	Return(ifdeclvalue_(ptr, stack, env, decl, &env));
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int macro_init(Execute ptr, struct lambda_struct *str)
{
	return macro_init_args(ptr, str->stack, str->args, str->decl, &str->args);
}

static int macro_tablevalue_(Execute ptr, addr args);
static int macro_tablevalue_var_(Execute ptr, addr args)
{
	addr var;

	while (args != Nil) {
		GetCons(args, &var, &args);
		if (consp(var)) {
			Return(macro_tablevalue_(ptr, var));
		}
		else {
			lambda_tablevalue_single(var);
		}
	}

	return 0;
}

static void macro_tablevalue_rest(addr rest)
{
	if (rest != Nil) {
		GetCar(rest, &rest);
		setcheck_tablevalue(rest, 1);
	}
}

static int macro_tablevalue_(Execute ptr, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_tablevalue_var_(ptr, var));
	Return(lambda_tablevalue_opt_(ptr, opt));
	macro_tablevalue_rest(rest);
	Return(lambda_tablevalue_key_(ptr, key));
	Return(lambda_tablevalue_aux_(ptr, aux));
	lambda_tablevalue_single(whole);
	lambda_tablevalue_single(env);

	return 0;
}

static int macro_progn(Execute ptr, struct lambda_struct *str)
{
	return scope_allcons(ptr, &str->cons, &str->body_the, str->cons);
}

static int macro_execute(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack, eval;

	/* lambda */
	stack = str->stack;
	Return(macro_init(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(macro_tablevalue_(ptr, str->args));
	Return(macro_progn(ptr, str));
	Return(ignore_checkvalue_(stack));
	Return(lambda_closure_(ptr, str));
	lambda_lexical(str);

	/* eval */
	Return(eval_scope_size_(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil));
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

static int macro_lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_lambda_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(macro_execute(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

static void macro_lambda_the(addr eval)
{
	addr type;
	GetTypeCompiled(&type, MacroFunction);
	SetEvalScopeThe(eval, type);
}

int scope_macro_lambda_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(macro_lambda_object(ptr, str, &eval));
	macro_lambda_the(eval);
	return Result(ret, eval);
}


/*
 *  deftype
 */
int scope_deftype_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(macro_lambda_object(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  define-compiler-macro
 */
int scope_define_compiler_macro_call(Execute ptr,
		struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(macro_lambda_object(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  destructuring-bind
 */
static int scope_bind_lambda(Execute ptr, struct lambda_struct *str, addr *ret)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_lambda_struct(ptr->local, str);
	Return(macro_execute(ptr, str, ret));
	Return(freestack_eval_(ptr, str->stack));
	str->stack = NULL;

	return 0;
}

int scope_bind_call(Execute ptr, addr *ret, addr expr, addr args)
{
	addr lambda, eval;
	struct lambda_struct str;

	/* macro-lambda */
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(args, 0, &str.args);
	GetEvalParse(args, 1, &str.decl);
	GetEvalParse(args, 2, &str.doc);
	GetEvalParse(args, 3, &str.cons);
	Return(scope_bind_lambda(ptr, &str, &lambda));

	/* result */
	Return(eval_scope_size_(ptr, &eval, 5,
				EVAL_PARSE_DESTRUCTURING_BIND, str.body_the, Nil));
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
static int flet_call(Execute ptr, addr list, addr *call, addr *ret)
{
	addr pos;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	*call = str.call;
	str.call = Nil;
	Return(lambda_object(ptr, &str, &pos));
	SetEvalScopeIndex(pos, EvalLambda_Call, *call);

	return Result(ret, pos);
}

static int flet_init(Execute ptr, struct let_struct *str)
{
	addr args, root, call, eval;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(flet_call(ptr, eval, &call, &eval));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void flet_maketable(Execute ptr, struct let_struct *str)
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

static int checktype_function_(addr table, addr eval)
{
	int check, errp;

	gettype_tablefunction(table, &table);
	GetEvalScopeThe(eval, &eval);
	Return(checktype_p_(eval, table, &check, &errp));
	if (check)
		return fmte_("Invalid function type.", NULL);

	return 0;
}

static int flet_applytable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(update_tablefunction_(ptr, stack, call));
		Return(checktype_function_(call, eval));
	}

	return 0;
}

static int ignore_checkfunction_(addr stack)
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

static int flet_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(flet_init(ptr, str));
	flet_maketable(ptr, str);
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(flet_applytable_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkfunction_(stack));

	return 0;
}

int scope_flet_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(flet_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/*
 *  labels
 */
static void labels_table_push(Execute ptr, addr stack, addr list)
{
	addr call, args, decl, pos;

	Lista_bind(list, &call, &args, &decl, &pos, NULL);
	make_tablefunction_stack(ptr, &pos, stack, call);
	apply_declare_function_stack(ptr->local, stack, call, decl);
}

static void labels_table(Execute ptr, struct let_struct *str)
{
	addr args, stack, eval;

	args = str->args;
	stack = str->stack;
	while (args != Nil) {
		GetCons(args, &eval, &args);
		labels_table_push(ptr, stack, eval);
	}
}

static int labels_call(Execute ptr, addr list, addr *ret)
{
	addr eval;
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	Return(lambda_object(ptr, &str, &eval));
	return Result(ret, eval);
}

static int labels_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, root, call, eval;

	stack = str->stack;
	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(labels_call(ptr, eval, &eval));
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		Return(push_tablefunction_global_(ptr, stack, call, &call));
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static int labels_checktype_(Execute ptr, struct let_struct *str)
{
	addr args, call, eval;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		Return(checktype_function_(call, eval));
	}

	return 0;
}

static int labels_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	labels_table(ptr, str);
	Return(labels_init(ptr, str));
	Return(labels_checktype_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkfunction_(stack));

	return 0;
}

int scope_labels_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(labels_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}

