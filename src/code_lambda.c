#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "eval.h"
#include "eval_table.h"
#include "execute.h"
#include "execute_object.h"
#include "function.h"
#include "lambda.h"
#include "scope_object.h"
#include "symbol.h"

/*
 *  free-declare
 */
static int let_free_value(Execute ptr, addr pos, addr type)
{
	int specialp;

	specialp = getspecialp_tablevalue(pos);
	getname_tablevalue(pos, &pos);
	if (specialp)
		getspecial_local(ptr, pos, &pos);
	else
		getvalue_tablevalue(ptr, pos, &pos);

	return typep_unbound_error(ptr, pos, type);
}

static int let_free_function(Execute ptr, addr pos, addr type)
{
	addr name, value;

	if (getglobalp_tablefunction(pos)) {
		getname_tablefunction(pos, &name);
		if (symbolp_callname(name)) {
			GetFunctionSymbol(pos, &value);
		}
		else {
			getsetf_symbol(pos, &value);
		}
	}
	else {
		getvalue_tablefunction(ptr, pos, &value);
	}

	return typep_unbound_error(ptr, value, type);
}

_g int let_free_code(Execute ptr, addr list)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			Return(let_free_value(ptr, pos, type));
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			Return(let_free_function(ptr, pos, type));
			continue;
		}
		Abort("type error");
	}

	return 0;
}


/*
 *  lambda object
 */
static void getclosure_type_code(Execute ptr, addr table, addr *ret)
{
	addr pos;
	size_t dst, src;

	CheckTableTable(table);
	get_evaltable(table, &pos);
	switch (gettype_evaltable(table)) {
		case EvalTable_Value:
			src = getclosure_tablevalue(pos); /* from */
			dst = getlexical_tablevalue(pos); /* to */
			getlow_lexical_control(ptr, src, &pos);
			closure_heap(ret, pos, dst);
			break;

		case EvalTable_Function:
			src = getclosure_tablefunction(pos); /* from */
			dst = getlexical_tablefunction(pos); /* to */
			getlow_lexical_control(ptr, src, &pos);
			closure_heap(ret, pos, dst);
			break;

		case EvalTable_TagBody:
			src = getclosure_tabletagbody(pos); /* from */
			dst = getlexical_tabletagbody(pos); /* to */
			getlow_lexical_control(ptr, src, &pos);
			closure_heap(ret, pos, dst);
			break;

		case EvalTable_Block:
			src = getclosure_tableblock(pos); /* from */
			dst = getlexical_tableblock(pos); /* to */
			getlow_lexical_control(ptr, src, &pos);
			closure_heap(ret, pos, dst);
			break;

		default:
			Abort("Invalid eval-table type.");
			break;
	}
}

static void lambda_closure_code(Execute ptr, addr pos, addr list)
{
	addr root, x;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &x, &list);
		getclosure_type_code(ptr, x, &x);
		cons_heap(&root, x, root);
	}
	SetDataFunction(pos, root);
}

static void lambda_recursive_code(Execute ptr, addr pos, addr value)
{
	addr x, list;
	size_t index;

	/* closure */
	GetIndex(value, &index);
	closure_heap(&x, pos, index);

	/* append */
	GetDataFunction(pos, &list);
	cons_heap(&list, x, list);
	SetDataFunction(pos, list);
}

static void lambda_info_code(Execute ptr, addr scope, addr pos)
{
	addr value;

	/* type */
	GetEvalScopeIndex(scope, EvalLambda_The, &value);
	settype_function(pos, value);
	/* documentation */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &value);
	setdocumentation_function(pos, value);
	/* form */
	GetEvalScopeIndex(scope, EvalLambda_Form, &value);
	setlambda_expression_function(pos, value);
	/* defun */
	GetEvalScopeIndex(scope, EvalLambda_Defun, &value);
	if (value != Nil)
		setdefunform_function(pos, value);
	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &value);
	if (value != Nil)
		lambda_closure_code(ptr, pos, value);
	/* self */
	GetEvalScopeIndex(scope, EvalLambda_Self, &value);
	if (value != Nil)
		lambda_recursive_code(ptr, pos, value);
}

static void lambda_object_code(Execute ptr, addr scope, addr *ret)
{
	addr pos, value;

	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	GetEvalScopeIndex(scope, EvalLambda_Code, &value);
	function_heap(&pos, pos, value);
	lambda_info_code(ptr, scope, pos);
	*ret = pos;
}

_g int lambda_set_code(Execute ptr, CodeValue x)
{
	lambda_object_code(ptr, x.pos, &x.pos);
	setresult_control(ptr, x.pos);
	return 0;
}

_g int lambda_push_code(Execute ptr, CodeValue x)
{
	lambda_object_code(ptr, x.pos, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  execute
 */
static int lambda_args_value_(Execute ptr, addr pos, addr value, int ignore)
{
	addr type, name;

	/* type check */
	if (! ignore && getcheck_tablevalue(pos)) {
		gettype_tablevalue(pos, &type);
		Return(typep_error(ptr, value, type));
	}

	/* push */
	getname_tablevalue(pos, &name);
	if (getspecialp_tablevalue(pos))
		pushspecial_control(ptr, name, value);
	else
		setvalue_tablevalue(ptr, pos, value);

	return 0;
}

static int lambda_args_init_(Execute ptr, addr var, addr init)
{
	addr control;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	/* code */
	Return(runcode_control(ptr, init));
	getresult_control(ptr, &init);
	localhold_set(hold, 0, init);
	Return(free_control_(ptr, control));
	localhold_end(hold);
	return lambda_args_value_(ptr, var, init, 0);
}

static int lambda_args_var_(Execute ptr, addr cons, addr *args)
{
	addr left, right;

	while (cons != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(cons, &left, &cons);
		Return_getcons(*args, &right, args);
		Return(lambda_args_value_(ptr, left, right, 0));
	}

	return 0;
}

static int lambda_args_opt_(Execute ptr, addr cons, addr *args)
{
	addr left, right, var, init, svar;

	while (cons != Nil) {
		Return_getcons(cons, &left, &cons);
		List_bind(left, &var, &init, &svar, NULL);
		if (*args != Nil) {
			Return_getcons(*args, &right, args);
			Return(lambda_args_value_(ptr, var, right, 0));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, T, 1));
			}
		}
		else {
			Return(lambda_args_init_(ptr, var, init));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_args_rest_(Execute ptr, addr rest, addr args)
{
	/*copylist_force_heap(args, &args);*/
	/*copy_list_heap_safe(&args, args);*/
	copyheap(&args, args);
	return lambda_args_value_(ptr, rest, args, 1);
}

static int lambda_args_key_find_(addr key, addr list, int *ret)
{
	addr name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		/* (var name init svar) */
		Return_getcdr(name, &name);
		Return_getcar(name, &name);
		if (key == name)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int lambda_args_key_(Execute ptr, addr cons, int allow, addr args)
{
	int check;
	addr left, right, var, name, init, svar, value;

	/* format check */
	for (right = args; right != Nil; ) {
		Return_getcons(right, &left, &right);
		if (right == Nil)
			return fmte_("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			return fmte_("&key name ~S must be a symbol.", left, NULL);
		if (! allow) {
			Return(lambda_args_key_find_(left, cons, &check));
			if (! check) {
				if (find_keyword_allow_other_keys(args))
					allow = 1;
				else
					return fmte_("&key name ~S don't accept.", left, NULL);
			}
		}
		Return_getcons(right, &left, &right);
	}

	/* bind */
	while (cons != Nil) {
		Return_getcons(cons, &right, &cons);
		List_bind(right, &var, &name, &init, &svar, NULL);
		if (getplist(args, name, &value) == 0) {
			Return(lambda_args_value_(ptr, var, value, 0));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, T, 1));
			}
		}
		else {
			Return(lambda_args_init_(ptr, var, init));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_args_allow_(Execute ptr, addr args)
{
	addr left;

	while (args != Nil) {
		Return_getcons(args, &left, &args);
		if (args == Nil)
			return fmte_("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			return fmte_("&key name ~S must be a symbol.", left, NULL);
		Return_getcons(args, &left, &args); /* ignore */
	}

	return 0;
}

static int lambda_args_aux_(Execute ptr, addr cons)
{
	addr list, var, init;

	while (cons != Nil) {
		Return_getcons(cons, &list, &cons);
		List_bind(list, &var, &init, NULL);
		Return(lambda_args_init_(ptr, var, init));
	}

	return 0;
}

static int lambda_args_empty(addr args)
{
	return (args == Nil)? 0: fmte_("Too many argument.", NULL);
}

static int lambda_args_code(Execute ptr, addr list)
{
	addr args, pos, rest, key, allow, aux;

	getargs_list_control_unsafe(ptr, 0, &args);
	/* empty */
	if (list == Nil)
		return lambda_args_empty(args);
	/* var */
	GetCons(list, &pos, &list);
	Return(lambda_args_var_(ptr, pos, &args));
	if (list == Nil)
		return lambda_args_empty(args);
	/* opt */
	GetCons(list, &pos, &list);
	Return(lambda_args_opt_(ptr, pos, &args));
	if (list == Nil)
		return lambda_args_empty(args);
	/* argument */
	GetCons(list, &rest, &list);
	key = allow = aux = Nil;
	if (list != Nil) {
		GetCons(list, &key, &list);
		if (list != Nil) {
			GetCons(list, &allow, &list);
			if (list != Nil) {
				GetCar(list, &aux);
			}
		}
	}
	/* rest */
	if (rest != Nil) {
		Return(lambda_args_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	/* key */
	if (key != Nil) {
		Return(lambda_args_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_args_allow_(ptr, args));
	}
	/* aux */
	if (aux != Nil) {
		Return(lambda_args_aux_(ptr, aux));
	}

	return 0;
}

static void lambda_lexical_restore(Execute ptr, addr list)
{
	addr pos;
	size_t index;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPSYSTEM_CLOSURE);
		index = lexical_closure(pos);
		get_closure(pos, &pos);
		setlow_lexical_control(ptr, index, pos);
	}
}

static void lambda_lexical_closure(Execute ptr, addr list)
{
	addr pos;
	size_t index;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_INDEX);
		GetIndex(pos, &index);
		reference_lexical_control(ptr, index);
	}
}

static void lambda_lexical_code(Execute ptr, addr data, addr list)
{
	addr pos;
	size_t size;

	if (list == Nil)
		return;
	/* allocate */
	GetCons(list, &pos, &list);
	GetIndex(pos, &size);
	lexical_control(ptr, size);
	/* restore */
	if (data != Nil)
		lambda_lexical_restore(ptr, data);
	/* closure */
	if (list != Nil)
		lambda_lexical_closure(ptr, list);
}

_g int lambda_execute_code(Execute ptr, CodeValue x)
{
	addr control, data, list;

	getdata_control(ptr, &data);
	push_args_control(ptr, &control);
	/* lexical */
	GetEvalScopeIndex(x.pos, EvalLambda_Lexical, &list);
	lambda_lexical_code(ptr, data, list);
	/* declare */
	GetEvalScopeIndex(x.pos, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(x.pos, EvalLambda_Args, &list);
	Return(lambda_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(x.pos, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}


/*
 *  macro object
 */
static void macro_object_code(Execute ptr, addr scope, addr *ret)
{
	addr pos, value;

	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	GetEvalScopeIndex(scope, EvalLambda_Code, &value);
	macro_heap(&pos, pos, value);
	lambda_info_code(ptr, scope, pos);
	*ret = pos;
}

_g int macro_set_code(Execute ptr, CodeValue x)
{
	macro_object_code(ptr, x.pos, &x.pos);
	setresult_control(ptr, x.pos);
	return 0;
}

_g int macro_push_code(Execute ptr, CodeValue x)
{
	macro_object_code(ptr, x.pos, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  macro-execute
 */
static int macro_args_whole_(Execute ptr, addr whole, addr form)
{
	if (whole != Nil) {
		copy_list_heap_safe(&form, form);
		Return(lambda_args_value_(ptr, whole, form, 1));
	}

	return 0;
}

static int macro_args_env_(Execute ptr, addr env, addr argenv)
{
	if (env != Nil) {
		Return(lambda_args_value_(ptr, env, argenv, 1));
	}
	return 0;
}

static int macro_args_list_(Execute ptr, addr right, addr args);
static int macro_args_var_(Execute ptr, addr var, addr *args)
{
	addr left, right;

	while (var != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(var, &left, &var);
		Return_getcons(*args, &right, args);
		if (consp(left)) {
			Return(macro_args_list_(ptr, left, right));
		}
		else {
			Return(lambda_args_value_(ptr, left, right, 0));
		}
	}

	return 0;
}

static int macro_args_rest_(Execute ptr, addr rest, addr args)
{
	addr var;

	Return_getcons(rest, &var, &rest);  /* (value . &rest) */
	copy_list_heap_safe(&args, args);
	return lambda_args_value_(ptr, var, args, 1);
}

static int macro_args_args(Execute ptr, addr args, addr list)
{
	addr pos, rest, key, allow, aux;

	/* empty */
	if (list == Nil)
		return lambda_args_empty(args);
	/* var */
	GetCons(list, &pos, &list);
	Return(macro_args_var_(ptr, pos, &args));
	if (list == Nil)
		return lambda_args_empty(args);
	/* opt */
	GetCons(list, &pos, &list);
	Return(lambda_args_opt_(ptr, pos, &args));
	if (list == Nil)
		return lambda_args_empty(args);
	/* argument */
	GetCons(list, &rest, &list);
	key = allow = aux = Nil;
	if (list != Nil) {
		GetCons(list, &key, &list);
		if (list != Nil) {
			GetCons(list, &allow, &list);
			if (list != Nil) {
				GetCar(list, &aux);
			}
		}
	}
	/* rest */
	if (rest != Nil) {
		Return(macro_args_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	/* key */
	if (key != Nil) {
		Return(lambda_args_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_args_allow_(ptr, args));
	}
	/* aux */
	if (aux != Nil) {
		Return(lambda_args_aux_(ptr, aux));
	}

	return 0;
}

static int macro_args_list_(Execute ptr, addr list, addr args)
{
	addr whole;

	GetCons(list, &whole, &list);
	GetCdr(list, &list);
	Return(macro_args_whole_(ptr, whole, args));

	return macro_args_args(ptr, args, list);
}

static int macro_args_call_(Execute ptr, addr list, addr args, addr aenv)
{
	addr whole, env;

	GetCons(list, &whole, &list);
	GetCons(list, &env, &list);
	Return(macro_args_whole_(ptr, whole, args));
	Return_getcdr(args, &args);
	Return(macro_args_env_(ptr, env, aenv));

	return macro_args_args(ptr, args, list);
}

static int macro_args_code(Execute ptr, addr list)
{
	addr args, form, env;

	getargs_list_control_unsafe(ptr, 0, &args);
	/* (lambda (form env) ...) */
	if (! consp(args))
		return fmte_("Too few argument in macro function.", NULL);
	Return_getcons(args, &form, &args);
	if (! consp(args))
		return fmte_("Too few argument in macro function.", NULL);
	Return_getcons(args, &env, &args);
	if (args != Nil)
		return fmte_("Too many argument in macro function.", NULL);
	return macro_args_call_(ptr, list, form, env);
}

_g int macro_execute_code(Execute ptr, CodeValue x)
{
	addr control, data ,list;

	getdata_control(ptr, &data);
	push_args_control(ptr, &control);
	/* lexical */
	GetEvalScopeIndex(x.pos, EvalLambda_Lexical, &list);
	lambda_lexical_code(ptr, data, list);
	/* declare */
	GetEvalScopeIndex(x.pos, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(x.pos, EvalLambda_Args, &list);
	Return(macro_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(x.pos, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}


/*
 *  destructuring-bind
 */
static int bind_args_code(Execute ptr, addr args)
{
	addr list;
	getresult_control(ptr, &list);
	return macro_args_list_(ptr, args, list);
}

_g int bind_set_code(Execute ptr, CodeValue x)
{
	addr control, list;

	push_new_control(ptr, &control);
	/* declare */
	GetEvalScopeIndex(x.pos, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(x.pos, EvalLambda_Args, &list);
	Return(bind_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(x.pos, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}

_g int bind_push_code(Execute ptr, CodeValue x)
{
	Return(bind_set_code(ptr, x));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);

	return 0;
}


/*
 *  flet
 */
static int flet_args_code(Execute ptr, addr list)
{
	addr args, pos, value;

	/* value */
	args = list;
	while (args != Nil) {
		GetCdr(args, &args);
		GetCons(args, &value, &args);
		Return(runcode_simple(ptr, value));
	}

	/* bind */
	getargs_list_control_unsafe(ptr, 0, &args);
	while (list != Nil) {
		GetCons(args, &value, &args);
		GetCons(list, &pos, &list);
		GetCdr(list, &list);
		setvalue_tablefunction(ptr, pos, value);
	}
	setargs_nil_control(ptr);

	return 0;
}

_g int flet_set_code(Execute ptr, CodeValue x)
{
	addr control, args, body, free;

	List_bind(x.pos, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(flet_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int flet_push_code(Execute ptr, CodeValue x)
{
	Return(flet_set_code(ptr, x));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);

	return 0;
}


/*
 *  labels
 */
static int labels_args_code(Execute ptr, addr list)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(list, &value, &list);
		Return(runcode_simple(ptr, value));
		getresult_control(ptr, &value);
		setvalue_tablefunction(ptr, pos, value);
	}

	return 0;
}

_g int labels_set_code(Execute ptr, CodeValue x)
{
	addr control, args, body, free;

	List_bind(x.pos, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(labels_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int labels_push_code(Execute ptr, CodeValue x)
{
	Return(labels_set_code(ptr, x));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);

	return 0;
}


/*
 *  locally
 */
_g int locally_declare_code(Execute ptr, CodeValue x)
{
	return let_free_code(ptr, x.pos);
}


/*
 *  multiple-value-bind
 */
static int bind_values_args_code(Execute ptr, addr list)
{
	addr values, pos, value;

	getvalues_list_control_local(ptr, &values);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(values, &value, &values);
		Return(lambda_args_value_(ptr, pos, value, 0));
	}

	return 0;
}

_g int bind_values_set_code(Execute ptr, CodeValue x)
{
	addr control, args, body, free;

	List_bind(x.pos, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(bind_values_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int bind_values_push_code(Execute ptr, CodeValue x)
{
	Return(bind_values_set_code(ptr, x));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);

	return 0;
}

