#include "code_values.h"
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
#include "scope_let.h"
#include "scope_object.h"
#include "subtypep.h"
#include "symbol.h"
#include "type_object.h"
#include "type_table.h"

/*
 *  symbol
 */
static int warning_global_lexical_(addr symbol)
{
	/*  return call_simple_style_warning_va_(NULL,
	 *      "Undefined variable ~S.", symbol, NULL);
	 */
	return 0;
}

static int symbol_global_tablevalue_(Execute ptr, addr symbol, addr *value, int *ret)
{
	int specialp;
	addr stack;

	Return(getglobal_eval_(ptr, &stack));
	if (! find_tablevalue(stack, symbol, value)) {
		/* heap object */
		Return(push_tablevalue_global_(ptr, stack, symbol, value));
		specialp = getspecialp_tablevalue(*value);
		if (! specialp) {
			Return(warning_global_lexical_(symbol));
		}
		return Result(ret, specialp);
	}
	return Result(ret, getspecialp_tablevalue(*value));
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

static int symbol_global_special_tablevalue_(Execute ptr, addr symbol, addr *value)
{
	addr stack, pos;

	Return(getglobal_eval_(ptr, &stack));
	if (find_tablevalue(stack, symbol, &pos)) {
		if (getspecialp_tablevalue(pos))
			return Result(value, pos);
	}

	/* heap object */
	Return(push_tablevalue_special_global_(ptr, stack, symbol, value));
	Check(! getspecialp_tablevalue(*value), "special error");

	return 0;
}

static int symbol_special_tablevalue_(Execute ptr,
		addr stack, addr symbol, addr *value)
{
	addr next;

	/* global */
	if (stack == Nil) {
		return symbol_global_special_tablevalue_(ptr, symbol, value);
	}

	/* local */
	if (getvalue_scope_evalstack(stack, symbol, value)) {
		if (getspecialp_tablevalue(*value))
			return 0;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	return symbol_special_tablevalue_(ptr, next, symbol, value);
}

static int symbol_tablevalue_(Execute ptr,
		addr stack, addr symbol, int basep, addr *value, int *ret)
{
	int check;
	addr next;

	/* global */
	if (stack == Nil) {
		return symbol_global_tablevalue_(ptr, symbol, value, ret);
	}

	/* local */
	if (getvalue_scope_evalstack(stack, symbol, value)) {
		if (basep)
			setbasep_tablevalue(*value, 1);
		setreference_tablevalue(*value, 1);
		return Result(ret, getspecialp_tablevalue(*value));
	}

	/* declare special */
	if (find_special_evalstack(stack, symbol)) {
		symbol_special_tablevalue_(ptr, stack, symbol, value);
		return Result(ret, 1);
	}

	/* basep */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		basep = 1;
	}

	/* next */
	GetEvalStackNext(stack, &next);
	Return(symbol_tablevalue_(ptr, next, symbol, basep, value, &check));
	if (check) {
		return Result(ret, 1); /* special */
	}

	/* global */
	if (getglobalp_tablevalue(*value)) {
		return Result(ret, 0); /* lexical */
	}

	/* closure */
	if (RefEvalStackType(stack) == EVAL_STACK_MODE_LAMBDA) {
		push_closure_value(stack, symbol, *value, value);
	}

	return Result(ret, 0); /* lexical */
}

static int find_symbol_scope_(Execute ptr, addr symbol, addr *value, int *ret)
{
	int specialp;
	addr stack, pos;

	Return(getstack_eval_(ptr, &stack));
	Return(symbol_tablevalue_(ptr, stack, symbol, 0, &pos, &specialp));
	copy_tablevalue(value, pos);

	return Result(ret, specialp);
}

static int scope_symbol_heap_(Execute ptr, addr *ret, addr type, addr symbol)
{
	return eval_scope_size_(ptr, ret, 1, EVAL_PARSE_SYMBOL, type, symbol);
}

static int make_scope_symbol_(Execute ptr, addr symbol, addr *ret)
{
	int ignore;
	addr value, type, pos;

	Check(! symbolp(symbol), "type error");
	Return(find_symbol_scope_(ptr, symbol, &value, &ignore));
	gettype_tablevalue(value, &type);
	Return(scope_symbol_heap_(ptr, &pos, type, symbol));
	SetEvalScopeIndex(pos, 0, value);

	return Result(ret, pos);
}

static int make_scope_keyword_(Execute ptr, addr symbol, addr *ret)
{
	addr type;
	GetTypeTable(&type, Keyword);
	return scope_symbol_heap_(ptr, ret, type, symbol);
}

int scope_symbol_call(Execute ptr, addr *ret, addr eval)
{
	if (keywordp(eval))
		return make_scope_keyword_(ptr, eval, ret);
	else
		return make_scope_symbol_(ptr, eval, ret);
}


/*
 *  setq
 */
int scope_setq_call(Execute ptr, addr cons, addr *ret, addr *type)
{
	int ignore;
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
		Return(find_symbol_scope_(ptr, var, &var, &ignore));
		Return(scope_eval(ptr, &form, form));
		GetEvalScopeThe(form, type);
		Return(checktype_value_(ptr, var, form));

		cons_heap(&var, var, form);
		cons_heap(&root, var, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}


/*
 *  values
 */
int scope_values_call(Execute ptr, addr args, addr *rargs, addr *rtype)
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

	/* (values ... &rest null) */
	GetTypeTable(&rest, Null);
	type_values_heap(var, Nil, rest, Nil, rtype);

	return 0;
}


/*
 *  the
 */
static int scope_the_check_warning_(Execute ptr, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, Nil, expected,
			"The special operator THE accept a ~S type, "
			"but actually the form is ~S type.",
			expected, type, NULL);
}

static int scope_the_check_(Execute ptr, addr eval, addr right, addr *ret)
{
	int check, errp;
	addr left;

	GetEvalScopeThe(eval, &left);
	Return(checktype_p_(ptr, left, right, &check, &errp));
	if (errp) {
		Return(scope_the_check_warning_(ptr, left, right));
	}

	return Result(ret, check? T: Nil);
}

int scope_the_call(Execute ptr, addr type, addr form, addr *ret)
{
	addr eval, check;

	Return(scope_eval(ptr, &form, form));
	Return(scope_the_check_(ptr, form, type, &check));
	/* result */
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_THE, type, form));
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

	Return(newstack_nil_(ptr, &stack));
	Return(apply_declare_(ptr, stack, decl, free));
	Return(scope_allcons(ptr, ret, type, cons));

	return freestack_eval_(ptr, stack);
}

int scope_locally_call(Execute ptr, addr decl, addr cons, addr *ret)
{
	addr eval, type, free;

	Return(locally_execute(ptr, decl, cons, &cons, &type, &free));
	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_LOCALLY, type, Nil));
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

static int tagbody_call_find_(addr list, addr pos, addr *ret)
{
	addr value, check;

	GetEvalParse(pos, 0, &pos);
	while (list != Nil) {
		GetCons(list, &value, &list);
		getname_tabletagbody(value, &check);
		if (eql_function(pos, check))
			return Result(ret, value);
	}

	/* error */
	*ret = 0;
	return fmte_("Invalid tag name.", NULL);
}

static int tagbody_allcons(Execute ptr, addr tag, addr body, addr *ret)
{
	addr root, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; body != Nil; ) {
		GetCons(body, &pos, &body);
		if (RefEvalParseType(pos) == EVAL_PARSE_TAG) {
			Return(tagbody_call_find_(tag, pos, &pos));
			Return(make_eval_scope_(ptr, &pos, EVAL_PARSE_TAG, Nil, pos));
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

int scope_tagbody_call(Execute ptr, addr tag, addr body, addr *rtag, addr *rbody)
{
	addr stack;

	Return(newstack_nil_(ptr, &stack));
	tagbody_call_push(stack, tag, &tag);
	Return(tagbody_allcons(ptr, tag, body, rbody));
	tagbody_call_remove(stack, tag, rtag);

	return freestack_eval_(ptr, stack);
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

int scope_go_call_(Execute ptr, addr *ret, addr tag)
{
	addr stack, table;

	Return(getstack_eval_(ptr, &stack));
	if (! go_tabletagbody(stack, tag, &table))
		return fmte_("Tag ~S is not found.", tag, NULL);

	return Result(ret, table);
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

int scope_block_call(Execute ptr, addr name, addr cons,
		addr *rname, addr *rcons, addr *rtype)
{
	addr stack;

	Return(newstack_nil_(ptr, &stack));
	push_tableblock(stack, name, &name);
	Return(scope_allcons(ptr, rcons, rtype, cons));
	block_call_remove(stack, name, rname);

	return freestack_eval_(ptr, stack);
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

int scope_return_from_call(Execute ptr,
		addr name, addr form, addr *rname, addr *rexpr)
{
	addr stack;

	Return(getstack_eval_(ptr, &stack));
	if (! name_tableblock(stack, name, rname))
		return fmte_("Cannot find block name ~S.", name, NULL);

	return scope_eval(ptr, rexpr, form);
}


/* multiple-value-bind */
void scope_init_mvbind(struct mvbind_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

static int mvbind_maketable_(Execute ptr, struct mvbind_struct *str)
{
	int allocate;
	addr stack, decl, args, root, var;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	allocate = 0;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		Return(check_scope_variable_(var));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		allocate |= getspecialp_tablevalue(var);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);
	str->allocate = allocate? T: Nil;

	return 0;
}

static int mvbind_execute(Execute ptr, struct mvbind_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(mvbind_maketable_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));

	return 0;
}

int scope_multiple_value_bind_call(Execute ptr, struct mvbind_struct *str)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &str->expr, str->expr));
	Return(newstack_nil_(ptr, &(str->stack)));
	Return(mvbind_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
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

int scope_multiple_value_call_call(Execute ptr, addr expr, addr cons, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	if (scope_multiple_value_call_type(expr, &type))
		GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_CALL, type, Nil));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}

