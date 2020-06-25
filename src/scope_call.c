#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "copy.h"
#include "control_execute.h"
#include "equal.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "parse.h"
#include "parse_object.h"
#include "scope_call.h"
#include "scope_declare.h"
#include "scope_object.h"
#include "symbol.h"
#include "type_object.h"
#include "type_subtypep.h"
#include "type_table.h"

/*
 *  symbol
 */
static void warning_global_lexical(addr symbol)
{
	/* fmtw("Undefined variable ~S.", symbol, NULL); */
}

static int symbol_global_tablevalue(Execute ptr, addr symbol, addr *ret)
{
	int specialp;
	addr stack;

	getglobal_eval(ptr, &stack);
	if (! find_tablevalue(stack, symbol, ret)) {
		/* heap object */
		push_tablevalue_global(ptr, stack, symbol, ret);
		specialp = getspecialp_tablevalue(*ret);
		if (! specialp)
			warning_global_lexical(symbol);
		return specialp;
	}
	return getspecialp_tablevalue(*ret);
}

static void push_closure_value(addr stack, addr symbol, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tablevalue(value);
	/* destination table */
	copy_tablevalue(&pos, value);
	setclosurep_tablevalue(pos, 1);
	setclosure_tablevalue(pos, lexical);
	setlexical_tablevalue(pos, increment_stack_eval(stack));
	setvalue_lexical_evalstack(stack, pos);
	setvalue_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int symbol_tablevalue(Execute ptr,
		addr stack, addr symbol, int basep, addr *ret)
{
	addr next;

	/* global */
	if (stack == Nil) {
		return symbol_global_tablevalue(ptr, symbol, ret);
	}

	/* local */
	if (getvalue_scope_evalstack(stack, symbol, ret)) {
		if (basep)
			setbasep_tablevalue(*ret, 1);
		setreference_tablevalue(*ret, 1);
		return getspecialp_tablevalue(*ret);
	}

	/* basep */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		basep = 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (symbol_tablevalue(ptr, next, symbol, basep, ret)) {
		return 1; /* special */
	}

	/* global */
	if (getglobalp_tablevalue(*ret)) {
		return 0; /* lexical */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_value(stack, symbol, *ret, ret);
	}

	return 0; /* lexical */
}

static int find_symbol_scope(Execute ptr, addr symbol, addr *ret)
{
	int specialp;
	addr stack, value;

	getstack_eval(ptr, &stack);
	specialp = symbol_tablevalue(ptr, stack, symbol, 0, &value);
	copy_tablevalue(ret, value);

	return specialp;
}

static void scope_symbol_heap(Execute ptr, addr *ret, addr type, addr symbol)
{
	eval_scope_size(ptr, ret, 1, EVAL_PARSE_SYMBOL, type, symbol);
}

static void make_scope_symbol(Execute ptr, addr symbol, addr *ret)
{
	addr value, type, pos;

	Check(! symbolp(symbol), "type error");
	find_symbol_scope(ptr, symbol, &value);
	gettype_tablevalue(value, &type);
	scope_symbol_heap(ptr, &pos, type, symbol);
	SetEvalScopeIndex(pos, 0, value);
	*ret = pos;
}

static int symbol_macrolet_global_p(Execute ptr, addr symbol, addr *ret)
{
	addr stack, table, key;

	/* global stack */
	getglobal_eval(ptr, &stack);
	GetEvalStackTable(stack, &table);

	/* global special */
	if (getvalue_scope_evalstack(stack, symbol, &key)) {
		if (getspecialp_tablevalue(key)) {
			return 0; /* special variable */
		}
	}

	/* symbol special */
	if (specialp_symbol(symbol)) {
		return 0;
	}

	/* define-symbol-macro */
	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	if (getpplist(table, key, symbol, ret) == 0) {
		return 1; /* define-symbol-macro */
	}

	/* symbol info */
	evalsymbol_macro_symbol(symbol, &table);
	if (table != Unbound) {
		*ret = table;
		return 1;
	}

	return 0;
}

static int symbol_macrolet_p(Execute ptr, addr symbol, addr *ret)
{
	addr stack, table, key;

	Check(! symbolp(symbol), "type error");

	/* local */
	getstack_eval(ptr, &stack);
	while (stack != Nil) {
		/* local variable */
		if (getvalue_scope_evalstack(stack, symbol, &key)) {
			return 0; /* shadow */
		}

		/* symbol-macrolet */
		GetConst(SYSTEM_SYMBOL_MACROLET, &key);
		GetEvalStackTable(stack, &table);
		if (getpplist(table, key, symbol, ret) == 0) {
			return 1; /* symbol-macrolet */
		}

		/* next */
		GetEvalStackNext(stack, &stack);
	}

	/* global */
	return symbol_macrolet_global_p(ptr, symbol, ret);
}

static int scope_symbol_replace(Execute ptr, addr *ret, addr form)
{
	addr hook, call, env;

	/* hook */
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	getspecialcheck_local(ptr, hook, &hook);
	/* symbol-macro-expander */
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &call);
	GetFunctionSymbol(call, &call);
	/* call */
	if (consp(form)) {
		GetCons(form, &form, &env);
	}
	else {
		env = Nil;
	}
	Return(callclang_funcall(ptr, &form, hook, call, form, env, NULL));

	return scope_eval(ptr, ret, form);
}

static void make_scope_keyword(Execute ptr, addr symbol, addr *ret)
{
	addr type;

	GetTypeTable(&type, Keyword);
	scope_symbol_heap(ptr, ret, type, symbol);
}

_g int scope_symbol_call(Execute ptr, addr *ret, addr eval)
{
	addr form;

	if (keywordp(eval))
		make_scope_keyword(ptr, eval, ret);
	else if (symbol_macrolet_p(ptr, eval, &form))
		return scope_symbol_replace(ptr, ret, form);
	else
		make_scope_symbol(ptr, eval, ret);

	return 0;
}


/*
 *  setq
 */
_g int scope_setq_call(Execute ptr, addr cons, addr *ret, addr *type)
{
	addr root, var, form;
	LocalHold hold;

	if (cons == Nil) {
		GetTypeTable(type, Null);
		*ret = Nil;
		return 0;
	}

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; cons != Nil; ) {
		GetCons(cons, &var, &cons);
		GetCons(var, &var, &form);
		find_symbol_scope(ptr, var, &var);
		Return(scope_eval(ptr, &form, form));
		GetEvalScopeThe(form, type);
		checktype_value(var, form);

		cons_heap(&var, var, form);
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  define-symbol-macro
 */
static void push_symbol_macrolet(addr stack, addr symbol, addr form, addr env)
{
	addr key, table, temp;

	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	GetEvalStackTable(stack, &table);
	if (getpplist(table, key, symbol, &temp)) {
		/* not found */
		cons_heap(&form, form, env);
		if (setpplist_heap(table, key, symbol, form, &table))
			SetEvalStackTable(stack, table);
	}
}

_g void scope_define_symbol_macro_call(Execute ptr,
		addr symbol, addr form, addr body, addr *ret)
{
	addr stack, eval;

	getglobal_eval(ptr, &stack);
	push_symbol_macrolet(stack, symbol, form, Nil); /* null env */
	GetTypeTable(&eval, Symbol);
	eval_scope_size(ptr, &eval, 3, EVAL_PARSE_DEFINE_SYMBOL_MACRO, eval, Nil);
	SetEvalScopeIndex(eval, 0, symbol);
	SetEvalScopeIndex(eval, 1, form);
	SetEvalScopeIndex(eval, 2, body);
	*ret = eval;
}


/*
 *  symbol-macrolet
 */
static void apply_symbol_macrolet(addr stack, addr args)
{
	addr list, symbol, form, env;

	while (args != Nil) {
		GetCons(args, &list, &args);
		List_bind(list, &symbol, &form, &env, NULL);
		push_symbol_macrolet(stack, symbol, form, env);
	}
}

static int symbol_macrolet_execute(Execute ptr,
		addr args, addr decl, addr cons, addr *ret, addr *type, addr *free)
{
	addr stack;

	stack = newstack_nil(ptr);
	apply_declare(ptr, stack, decl, free);
	apply_symbol_macrolet(stack, args);
	Return(scope_allcons(ptr, ret, type, cons));
	freestack_eval(ptr, stack);

	return 0;
}

_g int scope_symbol_macrolet_call(Execute ptr,
		addr args, addr decl, addr cons, addr *ret)
{
	addr eval, free;

	/* symbol-macrolet -> locally */
	Return(symbol_macrolet_execute(ptr, args, decl, cons, &cons, &eval, &free));
	eval_scope_size(ptr, &eval, 3, EVAL_PARSE_LOCALLY, eval, Nil);
	SetEvalScopeIndex(eval, 0, decl);
	SetEvalScopeIndex(eval, 1, cons);
	SetEvalScopeIndex(eval, 2, free);
	return Result(ret, eval);
}


/*
 *  values
 */
_g int scope_values_call(Execute ptr, addr args, addr *rargs, addr *rtype)
{
	addr root, var, rest, eval, type;
	LocalHold hold;

	/* progn and typelist */
	hold = LocalHold_array(ptr, 2);
	for (root = var = Nil; args != Nil; ) {
		GetCons(args, &eval, &args);
		Return(scope_eval(ptr, &eval, eval));
		GetEvalScopeThe(eval, &type);
		/* push */
		cons_heap(&root, eval, root);
		cons_heap(&var, type, var);
		localhold_set(hold, 0, root);
		localhold_set(hold, 1, var);
	}
	localhold_end(hold);
	nreverse(rargs, root);
	nreverse(&var, var);

	/* (values ... &rest nil) */
	GetTypeTable(&rest, Nil);
	type_values_heap(var, Nil, rest, Nil, rtype);

	return 0;
}


/*
 *  the
 */
static void scope_the_check_warning(addr type, addr expected)
{
	type_object(&type, type);
	type_object(&expected, expected);
	type_error_stdarg(Nil, expected,
			"The special operator THE accept a ~S type, "
			"but actually the form is ~S type.",
			expected, type, NULL);
}

static void scope_the_check(addr eval, addr right, addr *ret)
{
	int check;
	addr left;

	GetEvalScopeThe(eval, &left);
	if (checktype_p(left, right, &check))
		scope_the_check_warning(left, right);
	*ret = check? T: Nil;
}

_g int scope_the_call(Execute ptr, addr type, addr form, addr *ret)
{
	addr eval, check;

	Return(scope_eval(ptr, &form, form));
	scope_the_check(form, type, &check);
	/* result */
	eval_scope_size(ptr, &eval, 1, EVAL_PARSE_THE, type, form);
	SetEvalScopeIndex(eval, 0, check);
	return Result(ret, eval);
}


/*
 *  locally
 */
static int locally_execute(Execute ptr,
		addr decl, addr cons, addr *ret, addr *type, addr *free)
{
	addr stack;

	stack = newstack_nil(ptr);
	apply_declare(ptr, stack, decl, free);
	Return(scope_allcons(ptr, ret, type, cons));
	freestack_eval(ptr, stack);

	return 0;
}

_g int scope_locally_call(Execute ptr, addr decl, addr cons, addr *ret)
{
	addr eval, type, free;

	Return(locally_execute(ptr, decl, cons, &cons, &type, &free));
	eval_scope_size(ptr, &eval, 3, EVAL_PARSE_LOCALLY, type, Nil);
	SetEvalScopeIndex(eval, 0, decl);
	SetEvalScopeIndex(eval, 1, cons);
	SetEvalScopeIndex(eval, 2, free);
	return Result(ret, eval);
}


/*
 *  tagbody
 */
static void push_tabletagbody_lexical(addr stack, addr pos)
{
	addr name;

	Check(stack == Nil, "stack error");
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tabletagbody(pos, &name);
		setlexical_tabletagbody(pos, increment_stack_eval(stack));
		settagbody_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tabletagbody_lexical(stack, pos);
	}
}

static void push_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr pos;

	if (gettagbody_scope_evalstack(stack, tag, ret))
		return;
	make_tabletagbody(&pos, tag);
	settagbody_scope_evalstack(stack, pos);
	push_tabletagbody_lexical(stack, pos);
	*ret = pos;
}

static void tagbody_call_push(addr stack, addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Check(RefEvalParseType(pos) != EVAL_PARSE_TAG, "type error");
		GetEvalParse(pos, 0, &pos);
		push_tabletagbody(stack, pos, &pos);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static void tagbody_call_find(addr list, addr pos, addr *ret)
{
	addr value, check;

	GetEvalParse(pos, 0, &pos);
	while (list != Nil) {
		GetCons(list, &value, &list);
		getname_tabletagbody(value, &check);
		if (eql_function(pos, check)) {
			*ret = value;
			return;
		}
	}

	/* error */
	*ret = 0;
	fmte("Invalid tag name.", NULL);
}

static int tagbody_allcons(Execute ptr, addr tag, addr body, addr *ret)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; body != Nil; ) {
		GetCons(body, &pos, &body);
		if (RefEvalParseType(pos) == EVAL_PARSE_TAG) {
			tagbody_call_find(tag, pos, &pos);
			make_eval_scope(ptr, &pos, EVAL_PARSE_TAG, Nil, pos);
		}
		else {
			Return(scope_eval(ptr, &pos, pos));
		}
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static void tagbody_call_remove(addr stack, addr list, addr *ret)
{
	addr root, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (getreference_tabletagbody(pos))
			cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

_g int scope_tagbody_call(Execute ptr, addr tag, addr body, addr *rtag, addr *rbody)
{
	addr stack;

	stack = newstack_nil(ptr);
	tagbody_call_push(stack, tag, &tag);
	Return(tagbody_allcons(ptr, tag, body, rbody));
	tagbody_call_remove(stack, tag, rtag);
	freestack_eval(ptr, stack);

	return 0;
}


/*
 *  go
 */
static void push_closure_tagbody(addr stack, addr tag, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tabletagbody(value);
	/* destination table */
	copy_tabletagbody(&pos, value);
	setclosurep_tabletagbody(pos, 1);
	setclosure_tabletagbody(pos, lexical);
	setlexical_tabletagbody(pos, increment_stack_eval(stack));
	settagbody_lexical_evalstack(stack, pos);
	settagbody_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int go_tabletagbody(addr stack, addr tag, addr *ret)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (gettagbody_scope_evalstack(stack, tag, ret)) {
		setreference_tabletagbody(*ret, 1);
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! go_tabletagbody(next, tag, ret)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_tagbody(stack, tag, *ret, ret);
	}

	return 1;
}

_g void scope_go_call(Execute ptr, addr *ret, addr tag)
{
	addr stack, table;

	getstack_eval(ptr, &stack);
	if (! go_tabletagbody(stack, tag, &table))
		fmte("Tag ~S is not found.", tag, NULL);
	*ret = table;
}


/*
 *  block
 */
static void push_tableblock_lexical(addr stack, addr pos)
{
	addr name;

	Check(stack == Nil, "stack error");
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		getname_tableblock(pos, &name);
		setlexical_tableblock(pos, increment_stack_eval(stack));
		setblock_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tableblock_lexical(stack, pos);
	}
}

static void push_tableblock(addr stack, addr name, addr *ret)
{
	addr pos;

	if (getblock_scope_evalstack(stack, name, ret))
		return;
	make_tableblock(&pos, name);
	setblock_scope_evalstack(stack, pos);
	push_tableblock_lexical(stack, pos);
	*ret = pos;
}

static void block_call_remove(addr stack, addr pos, addr *ret)
{
	if (getreference_tableblock(pos))
		*ret = pos;
	else
		*ret = Nil;
}

_g int scope_block_call(Execute ptr, addr name, addr cons,
		addr *rname, addr *rcons, addr *rtype)
{
	addr stack;

	stack = newstack_nil(ptr);
	push_tableblock(stack, name, &name);
	Return(scope_allcons(ptr, rcons, rtype, cons));
	block_call_remove(stack, name, rname);
	freestack_eval(ptr, stack);

	return 0;
}


/*
 *  return-from
 */
static void push_closure_block(addr stack, addr tag, addr value, addr *ret)
{
	addr pos;
	size_t lexical;

	/* source table */
	lexical = getlexical_tableblock(value);
	/* destination table */
	copy_tableblock(&pos, value);
	setclosurep_tableblock(pos, 1);
	setclosure_tableblock(pos, lexical);
	setlexical_tableblock(pos, increment_stack_eval(stack));
	setblock_lexical_evalstack(stack, pos);
	setblock_scope_evalstack(stack, pos);
	/* result */
	*ret = pos;
}

static int name_tableblock(addr stack, addr tag, addr *ret)
{
	addr next;

	/* not found */
	if (stack == Nil) {
		return 0;
	}

	/* local */
	if (getblock_scope_evalstack(stack, tag, ret)) {
		setreference_tableblock(*ret, 1);
		return 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	if (! name_tableblock(next, tag, ret)) {
		return 0;
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_block(stack, tag, *ret, ret);
	}

	return 1;
}

_g int scope_return_from_call(Execute ptr,
		addr name, addr form, addr *rname, addr *rexpr)
{
	addr stack;

	getstack_eval(ptr, &stack);
	if (! name_tableblock(stack, name, rname))
		fmte("Cannot find block name ~S.", name, NULL);
	return scope_eval(ptr, rexpr, form);
}


/* multiple-value-bind */
_g void scope_init_mvbind(struct mvbind_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

static void mvbind_maketable(Execute ptr, struct mvbind_struct *str)
{
	int allocate;
	addr stack, decl, args, root, var;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	allocate = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		check_scope_variable(var);
		ifdeclvalue(ptr, stack, var, decl, &var);
		allocate |= getspecialp_tablevalue(var);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);
	str->allocate = allocate? T: Nil;
}

static int mvbind_execute(Execute ptr, struct mvbind_struct *str)
{
	addr stack;

	stack = str->stack;
	mvbind_maketable(ptr, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	ignore_checkvalue(stack);

	return 0;
}

_g int scope_multiple_value_bind_call(Execute ptr, struct mvbind_struct *str)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &str->expr, str->expr));
	str->stack = newstack_nil(ptr);
	Return(mvbind_execute(ptr, str));
	freestack_eval(ptr, str->stack);

	return 0;
}


/* multiple-value-call */
static int scope_multiple_value_call_type(addr expr, addr *ret)
{
	GetEvalScopeThe(expr, &expr);
	if (! type_function_p(expr))
		return 1;
	GetArrayType(expr, 1, ret); /* result */

	return 0;
}

_g int scope_multiple_value_call_call(Execute ptr, addr expr, addr cons, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	if (scope_multiple_value_call_type(expr, &type))
		GetTypeTable(&type, Asterisk);

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_CALL, type, Nil);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}

