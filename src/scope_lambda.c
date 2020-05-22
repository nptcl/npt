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
	if (getplistplist_callname(table, key, call, &value) == 0) {
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
	if (getplistplist_callname(table, key, call, &value) == 0) {
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
static enum InlineType inline_tablefunction(Execute ptr, addr stack, addr call)
{
	enum InlineType result;

	/* local stack */
	while (stack != Nil) {
		if (inline_stack_tablefunction(stack, call, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	getglobal_eval(ptr, &stack);
	if (inline_stack_tablefunction(stack, call, &result)) {
		return result;
	}

	/* not inline or notinline */
	return InlineType_None;
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
	return getplistplist_callname(stack, key, call, ret) == 0;
}

static int type_boundary_tablefunction(addr stack, addr call, addr *ret)
{
	if (! find_tablefunction(stack, call, &call))
		return 0;
	gettype_tablefunction(call, ret);
	return 1;
}

static void type_tablefunction(Execute ptr, LocalRoot local,
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
	getglobal_eval(ptr, &stack);
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
	nreverse_list_unsafe(ret, root);
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

static void update_tablefunction(Execute ptr, addr stack, addr pos)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr name, type;

	/* scope */
	getname_tablefunction(pos, &name);
	globalp = globalp_tablefunction(ptr, stack, name);
	dynamic = dynamic_tablefunction(stack, name);
	ignore = ignore_tablefunction(stack, name);
	Inline = inline_tablefunction(ptr, stack, name);
	type_tablefunction(ptr, NULL, stack, name, &type);
	if (type_and_array(NULL, type, &type))
		GetTypeTable(&type, Function);
	Check(type == Nil, "type error");

	/* make table */
	setglobalp_tablefunction(pos, globalp);
	setdynamic_tablefunction(pos, dynamic);
	setignore_tablefunction(pos, ignore);
	setinline_tablefunction(pos, Inline);
	settype_tablefunction(pos, type);
}
static void push_tablefunction_global(Execute ptr, addr stack, addr call, addr *ret)
{
	enum InlineType Inline;
	enum IgnoreType ignore;
	int globalp, dynamic;
	addr pos, type;

	/* scope */
	globalp = globalp_tablefunction(ptr, stack, call);
	dynamic = dynamic_tablefunction(stack, call);
	ignore = ignore_tablefunction(stack, call);
	Inline = inline_tablefunction(ptr, stack, call);
	type_tablefunction(ptr, NULL, stack, call, &type);
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
	*ret = pos;
}

static void callname_global_tablefunction(Execute ptr, addr *ret, addr call)
{
	addr stack;

	getglobal_eval(ptr, &stack);
	if (! find_tablefunction(stack, call, ret))
		push_tablefunction_global(ptr, stack, call, ret);
}

static void push_closure_function(addr stack, addr call, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablefunction(value);
	/* destination table */
	copy_tablefunction(&pos, value);
	setclosurep_tablefunction(pos, 1);
	setclosure_tablefunction(pos, lexical);
	setlexical_tablefunction(pos, increment_stack_eval(stack));
	setfunction_lexical_evalstack(stack, pos);
	setfunction_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int callname_tablefunction(Execute ptr, addr stack, addr call, addr *ret)
{
	addr next;

	/* global */
	if (stack == Nil) {
		callname_global_tablefunction(ptr, ret, call);
		return 1; /* global-scope */
	}

	/* local */
	if (getfunction_scope_evalstack(stack, call, ret)) {
		setreference_tablefunction(*ret, 1);
		return getglobalp_tablefunction(*ret);
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (callname_tablefunction(ptr, next, call, ret)) {
		return 1; /* global-scope */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_function(stack, call, *ret, ret);
	}

	return 0; /* local-scope */
}

static int scope_function_object(Execute ptr, addr *ret, addr eval)
{
	addr type;

	gettype_function(eval, &type);
	if (type == Nil)
		GetTypeTable(&type, Function);
	make_eval_scope(ptr, ret, EVAL_PARSE_FUNCTION, type, eval);

	return 0;
}

static int scope_function_callname(Execute ptr, addr *ret, addr call)
{
	addr value, type, stack;

	getstack_eval(ptr, &stack);
	callname_tablefunction(ptr, stack, call, &value);
	copy_tablefunction(&value, value);
	gettype_tablefunction(value, &type);
	make_eval_scope(ptr, ret, EVAL_PARSE_FUNCTION, type, value);

	return 0;
}

_g int scope_function_call(Execute ptr, addr *ret, addr eval)
{
	GetEvalParse(eval, 0, &eval);
	if (functionp(eval))
		return scope_function_object(ptr, ret, eval);
	if (callnamep(eval))
		return scope_function_callname(ptr, ret, eval);
	fmte("Invalid object type ~S", eval, NULL);

	return 0;
}


/*
 *  lambda
 */
_g void scope_init_lambda(struct lambda_struct *str, EvalParse eval, int globalp)
{
	clearpoint(str);
	str->stack = str->call = str->table = str->lexical =
		str->args = str->decl = str->doc = str->cons =
		str->clos = str->free = str->the =
		str->form = str->defun = Nil;
	str->globalp = globalp;
	str->eval = eval;
}

static void lambda_init_var(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr var, list;

	list = Nil;
	while (args != Nil) {
		GetCons(args, &var, &args);
		ifdeclvalue(ptr, stack, var, decl, &var);
		cons_heap(&list, var, list);
	}
	nreverse_list_unsafe(ret, list);
}

static int lambda_init_opt(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		ifdeclvalue(ptr, stack, var, decl, &var);
		ifdeclvalue(ptr, stack, svar, decl, &svar);
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int lambda_init_key(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		Return(scope_eval(ptr, &init, init));
		ifdeclvalue(ptr, stack, var, decl, &var);
		ifdeclvalue(ptr, stack, svar, decl, &svar);
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int lambda_init_aux(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		Return(scope_eval(ptr, &init, init));
		ifdeclvalue(ptr, stack, var, decl, &var);
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

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
	lambda_init_var(ptr, stack, var, decl, &var);
	Return(lambda_init_opt(ptr, stack, opt, decl, &opt));
	ifdeclvalue(ptr, stack, rest, decl, &rest);
	Return(lambda_init_key(ptr, stack, key, decl, &key));
	Return(lambda_init_aux(ptr, stack, aux, decl, &aux));
	list_heap(&str->args, var, opt, rest, key, allow, aux, NULL);

	return 0;
}

static void lambda_tablevalue_opt(addr args)
{
	addr list, var, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		checktype_value(var, init);
	}
}

static void lambda_tablevalue_key(addr args)
{
	addr list, var, name, init, svar;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		checktype_value(var, init);
	}
}

static void lambda_tablevalue_aux(addr args)
{
	addr list, var, init;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		checktype_value(var, init);
	}
}

static void lambda_tablevalue(addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	lambda_tablevalue_opt(opt);
	lambda_tablevalue_key(key);
	lambda_tablevalue_aux(aux);
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
	nreverse_list_unsafe(ret, root);
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
	nreverse_list_unsafe(ret, root);
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
	nreverse_list_unsafe(ret, root);
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

static void lambda_closure(Execute ptr, struct lambda_struct *str)
{
	addr stack, list;

	getstack_eval(ptr, &stack);
	lambda_closure_list(stack, &list);
	str->clos = list;
}

_g void lambda_lexical_heap(addr stack, addr *ret)
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
	apply_declare(ptr, stack, str->decl, &str->free);
	lambda_tablevalue(str->args);
	lambda_declare(ptr, str);
	Return(lambda_progn(ptr, str));
	ignore_checkvalue(stack);
	lambda_closure(ptr, str);
	lambda_lexical(str);

	/* eval */
	eval_scope_size(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil);
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

static void localhold_lambda_struct(LocalRoot local, struct lambda_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->call, str->table, str->lexical,
			str->args, str->decl, str->doc, str->cons,
			str->clos, str->free, str->the,
			str->form, str->defun, NULL);
}

static int lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	str->stack = newstack_lambda(ptr);
	localhold_lambda_struct(ptr->local, str);
	Return(lambda_execute(ptr, str, ret));
	freestack_eval(ptr, str->stack);
	str->stack = NULL;

	return 0;
}

_g int scope_lambda_call(Execute ptr, addr *ret, addr eval)
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
static void defun_update(Execute ptr, struct lambda_struct *str)
{
	addr stack, table;

	getglobal_eval(ptr, &stack);
	push_tablefunction_global(ptr, stack, str->call, &table);
	settype_tablefunction(table, str->the);
}

static void defun_the(addr eval, struct lambda_struct *str)
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
			fmte("callname error.", NULL);
			return;
	}
	SetEvalScopeThe(eval, type);
}

_g int scope_defun_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(lambda_object(ptr, str, &eval));
	defun_update(ptr, str);
	defun_the(eval, str);

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
			ifdeclvalue(ptr, stack, var, decl, &var);
		}
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

static void macro_init_rest(Execute ptr, addr stack, addr rest, addr decl, addr *ret)
{
	addr var, type;

	if (rest == Nil) {
		*ret = Nil;
	}
	else {
		GetCons(rest, &var, &type);
		ifdeclvalue(ptr, stack, var, decl, &var);
		cons_heap(ret, var, type);
	}
}

static int macro_init_args(Execute ptr, addr stack, addr args, addr decl, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_init_var(ptr, stack, var, decl, &var));
	Return(lambda_init_opt(ptr, stack, opt, decl, &opt));
	macro_init_rest(ptr, stack, rest, decl, &rest);
	Return(lambda_init_key(ptr, stack, key, decl, &key));
	Return(lambda_init_aux(ptr, stack, aux, decl, &aux));
	ifdeclvalue(ptr, stack, whole, decl, &whole);
	ifdeclvalue(ptr, stack, env, decl, &env);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);

	return 0;
}

static int macro_init(Execute ptr, struct lambda_struct *str)
{
	return macro_init_args(ptr, str->stack, str->args, str->decl, &str->args);
}

static void macro_tablevalue(addr args);
static void macro_tablevalue_var(addr args)
{
	addr var;

	while (args != Nil) {
		GetCons(args, &var, &args);
		if (consp(var))
			macro_tablevalue(var);
	}
}

static void macro_tablevalue(addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	macro_tablevalue_var(var);
	lambda_tablevalue_opt(opt);
	lambda_tablevalue_key(key);
	lambda_tablevalue_aux(aux);
}

static int macro_progn(Execute ptr, struct lambda_struct *str)
{
	return scope_allcons(ptr, &str->cons, NULL, str->cons);
}

static int macro_execute(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr stack, eval;

	/* lambda */
	stack = str->stack;
	Return(macro_init(ptr, str));
	apply_declare(ptr, stack, str->decl, &str->free);
	macro_tablevalue(str->args);
	Return(macro_progn(ptr, str));
	ignore_checkvalue(stack);
	lambda_closure(ptr, str);
	lambda_lexical(str);

	/* eval */
	eval_scope_size(ptr, &eval, EvalLambda_Size, str->eval, str->the, Nil);
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
	str->stack = newstack_lambda(ptr);
	localhold_lambda_struct(ptr->local, str);
	Return(macro_execute(ptr, str, ret));
	freestack_eval(ptr, str->stack);
	str->stack = NULL;

	return 0;
}

static void macro_lambda_the(addr eval)
{
	addr type;
	GetTypeCompiled(&type, MacroFunction);
	SetEvalScopeThe(eval, type);
}

_g int scope_macro_lambda_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(macro_lambda_object(ptr, str, &eval));
	macro_lambda_the(eval);
	return Result(ret, eval);
}


/*
 *  deftype
 */
_g int scope_deftype_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(macro_lambda_object(ptr, str, &eval));
	SetEvalScopeIndex(eval, EvalLambda_Call, str->call);
	return Result(ret, eval);
}


/*
 *  define-compiler-macro
 */
_g int scope_define_compiler_macro_call(Execute ptr,
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
static int dbind_lambda_object(Execute ptr, struct lambda_struct *str, addr *ret)
{
	str->stack = newstack_nil(ptr);
	localhold_lambda_struct(ptr->local, str);
	Return(macro_execute(ptr, str, ret));
	freestack_eval(ptr, str->stack);
	str->stack = NULL;

	return 0;
}

_g int scope_destructuring_bind_call(Execute ptr, struct lambda_struct *str, addr *ret)
{
	addr eval;

	Return(dbind_lambda_object(ptr, str, &eval));
	macro_lambda_the(eval);
	return Result(ret, eval);
}


/*
 *  flet
 */
static int flet_call(Execute ptr, addr list, addr *call, addr *ret)
{
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	*call = str.call;
	str.call = Nil;
	return lambda_object(ptr, &str, ret);
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
	nreverse_list_unsafe(&str->args, root);

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

static void checktype_function(addr table, addr eval)
{
	int check;

	gettype_tablefunction(table, &table);
	GetEvalScopeThe(eval, &eval);
	(void)checktype_p(eval, table, &check);
	if (check)
		fmte("Invalid function type.", NULL);
}

static void flet_applytable(Execute ptr, struct let_struct *str)
{
	addr stack, args, call, eval;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		update_tablefunction(ptr, stack, call);
		checktype_function(call, eval);
	}
}

static void ignore_checkfunction(addr stack)
{
	enum IgnoreType ignore;
	int reference;
	addr list, pos, call, symbol, value;

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
		GetCallName(call, &symbol);

		if (ignore == IgnoreType_None && (! reference)) {
			fmtw("Unused variable ~S.", symbol, NULL);
		}
		if (ignore == IgnoreType_Ignore && reference) {
			fmtw("Ignore variable ~S used.", symbol, NULL);
		}
	}
}

static int flet_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(flet_init(ptr, str));
	flet_maketable(ptr, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	flet_applytable(ptr, str);
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	ignore_checkfunction(stack);

	return 0;
}

_g int scope_flet_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	localhold_let_struct(ptr->local, str);
	Return(flet_execute(ptr, str));
	freestack_eval(ptr, str->stack);

	return 0;
}


/*
 *  labels
 */
static void ifdeclcall(Execute ptr, addr stack, addr call, addr decl, addr *ret)
{
	addr pos;

	make_tablefunction_stack(ptr, &pos, stack, call);
	apply_declare_function_stack(ptr->local, stack, call, decl);
	push_tablefunction_global(ptr, stack, call, ret);
}

static int labels_call(Execute ptr, addr list, addr *ret)
{
	struct lambda_struct str;

	scope_init_lambda(&str, EVAL_PARSE_EMPTY, 0);
	List_bind(list, &str.call, &str.args, &str.decl, &str.doc, &str.cons, NULL);
	return lambda_object(ptr, &str, ret);
}

static int labels_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, root, call, eval;

	stack = str->stack;
	args = str->args;
	decl = str->decl;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(labels_call(ptr, eval, &eval));
		GetEvalScopeIndex(eval, EvalLambda_Call, &call);
		ifdeclcall(ptr, stack, call, decl, &call);
		cons_heap(&call, call, eval);
		cons_heap(&root, call, root);
	}
	nreverse_list_unsafe(&str->args, root);

	return 0;
}

static void labels_checktype(Execute ptr, struct let_struct *str)
{
	addr args, call, eval;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &call, &args);
		GetCons(call, &call, &eval);
		checktype_function(call, eval);
	}
}

static int labels_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(labels_init(ptr, str));
	apply_declare(ptr, stack, str->decl, &str->free);
	labels_checktype(ptr, str);
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	ignore_checkfunction(stack);

	return 0;
}

_g int scope_labels_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	localhold_let_struct(ptr->local, str);
	Return(labels_execute(ptr, str));
	freestack_eval(ptr, str->stack);

	return 0;
}


/*
 *  call
 */
static int call_first(Execute ptr, addr *ret, addr first)
{
	switch (RefEvalParseType(first)) {
		case EVAL_PARSE_FUNCTION:
			return scope_function_call(ptr, ret, first);

		case EVAL_PARSE_LAMBDA:
			return scope_lambda_call(ptr, ret, first);

		default:
			fmte("Invalid parse object.", NULL);
			break;
	}

	return 0;
}

static void check_tablecall_warning(addr name, addr type, addr expected)
{
	type_object(&type, type);
	type_object(&expected, expected);
	type_error_stdarg(name, expected,
			"The object ~S expected a ~S type but the initialize form is ~S type.",
			name, expected, type, NULL);
}

static int check_tablecall(Execute ptr, addr eval, addr right, addr *ret)
{
	int check;
	addr table, type, name;

	/* tablecall */
	Return(scope_eval(ptr, &eval, eval));
	make_tablecall(&table);
	copylocal_object(NULL, &right, right);
	settype_tablecall(table, right);
	setvalue_tablecall(table, eval);
	/* checktype */
	GetEvalScopeThe(eval, &type);
	if (checktype_p(type, right, &check)) {
		GetEvalScopeValue(eval, &name);
		check_tablecall_warning(name, type, right);
	}
	setcheck_tablecall(table, check);
	/* result */
	return Result(ret, table);
}

static int callargs_var(Execute ptr, addr array, addr *args, addr *root)
{
	addr eval, right;
	LocalHold hold;

	GetArrayA2(array, 0, &array);
	hold = LocalHold_array(ptr, 1);
	while (array != Nil) {
		if (*args == Nil)
			break;
		GetCons(*args, &eval, args);
		GetCons(array, &right, &array);
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	return 0;
}

static int callargs_opt(Execute ptr, addr array, addr *args, addr *root)
{
	addr eval, right;
	LocalHold hold;

	GetArrayA2(array, 1, &array);
	hold = LocalHold_array(ptr, 1);
	while (*args != Nil && array != Nil) {
		GetCons(*args, &eval, args);
		GetCons(array, &right, &array);
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	return 0;
}

static void callargs_keyvalue(int keyvalue, addr cons, addr *ret)
{
	if (keyvalue == 0) {
		/* name */
		GetCar(cons, &cons);
		type_eql_heap(cons, ret);
	}
	else {
		/* type */
		GetCdr(cons, &cons);
		copylocal_object(NULL, ret, cons);
	}
}

static void callargs_key(int keyvalue, addr cons, addr *ret)
{
	addr pos, array;
	size_t size, i;

	/* &allow-other-keys */
	if (cons == T) {
		if (keyvalue)
			GetTypeTable(ret, T);
		else
			GetTypeTable(ret, Symbol);
		return;
	}

	/* &key */
	size = length_list_unsafe(cons);
	if (size == 1) {
		GetCar(cons, &pos);
		callargs_keyvalue(keyvalue, pos, ret);
		return;
	}

	/* or */
	vector4_heap(&array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		callargs_keyvalue(keyvalue, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_heap(LISPDECL_OR, array, ret);
}

static void type_and_nil(LocalRoot local, addr type1, addr type2, addr *ret)
{
	int check1, check2;

	check1 = (type1 == Nil);
	check2 = (type2 == Nil);
	if (check1 && check2) {
		GetTypeTable(ret, Asterisk);
		return;
	}
	if (check1) {
		*ret = type2;
		return;
	}
	if (check2) {
		*ret = type1;
		return;
	}
	else {
		type2and_alloc(local, type1, type2, ret);
		return;
	}
}

static int callargs_restkey(Execute ptr,
		addr array, addr *args, addr *root, int *result)
{
	int keyvalue;
	addr rest, key, eval, type1, type2, right;
	LocalHold hold;

	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);
	if (rest == Nil && key == Nil) {
		*result = *args != Nil;
		return 0;
	}

	hold = LocalHold_array(ptr, 1);
	for (keyvalue = 0; *args != Nil; keyvalue = (! keyvalue)) {
		GetCons(*args, &eval, args);
		type1 = type2 = Nil;
		/* &rest */
		if (rest != Nil)
			copylocal_object(NULL, &type1, rest);
		/* &key */
		if (key != Nil)
			callargs_key(keyvalue, key, &type2);
		/* result */
		type_and_nil(NULL, type1, type2, &right);
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	/* error check */
	if (key != Nil && keyvalue)
		fmte("Invalid keyword argument ~S.", key, NULL);
	*result = 0;

	return 0;
}

static int callargs_check(Execute ptr, addr array, addr args, addr *ret)
{
	int check;
	addr root;
	LocalHold hold;

	root = Nil;
	hold = LocalHold_array(ptr, 1);
	/* var */
	if (callargs_var(ptr, array, &args, &root))
		goto toofew;
	localhold_set(hold, 0, root);
	/* opt */
	if (args == Nil)
		goto final;
	Return(callargs_opt(ptr, array, &args, &root));
	localhold_set(hold, 0, root);
	/* rest, key */
	if (args == Nil)
		goto final;
	Return(callargs_restkey(ptr, array, &args, &root, &check));
	localhold_set(hold, 0, root);
	if (check)
		goto toomany;
	goto final;

toofew:
	fmtw("Too few arguments.", NULL);
	goto final;
toomany:
	fmtw("Too many arguments.", NULL);
	goto final;
final:
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int asterisk_function_argument(addr type, addr *ret)
{
	if (type_asterisk_p(type)) {
		return 1;
	}
	if (type_function_p(type)) {
		GetArrayType(type, 0, &type); /* arguement */
		if (type_asterisk_p(type))
			return 1;
		*ret = type;
		return 0;
	}
	infobit(type);
	Abort("type error");
	return 0;
}

static int callargs_nocheck(Execute ptr, addr args, addr *ret)
{
	addr root, eval, type;
	LocalHold hold;

	GetTypeTable(&type, Asterisk);
	hold = LocalHold_array(ptr, 1);
	for (root = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(check_tablecall(ptr, eval, type, &eval));
		cons_heap(&root, eval, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	return 0;
}

static int call_args(Execute ptr, addr *ret, addr first, addr args)
{
	GetEvalScopeThe(first, &first);
	if (asterisk_function_argument(first, &first)) {
		Return(callargs_nocheck(ptr, args, ret));
	}
	else {
		Return(callargs_check(ptr, first, args, ret));
	}

	return 0;
}

static void call_result(addr *ret, addr first)
{
	addr type;

	GetEvalScopeThe(first, &type);
	CheckType(type, LISPTYPE_TYPE);
	if (type_asterisk_p(type)) {
		*ret = type;
		return;
	}
	Check(! type_function_p(type), "type decl error");
	GetArrayType(type, 1, ret); /* values */
	CheckType(*ret, LISPTYPE_TYPE);
}

_g int scope_call_call(Execute ptr, addr first, addr args, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(call_first(ptr, &first, first));
	localhold_push(hold, first);
	Return(call_args(ptr, &args, first, args));
	localhold_push(hold, args);
	call_result(&type, first);
	localhold_push(hold, type);
	localhold_end(hold);

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_CALL, type, Nil);
	SetEvalScopeIndex(eval, 0, first);
	SetEvalScopeIndex(eval, 1, args);
	return Result(ret, eval);
}

