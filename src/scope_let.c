#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "copy.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "scope_declare.h"
#include "scope_let.h"
#include "scope_object.h"
#include "subtypep.h"
#include "symbol.h"
#include "type.h"
#include "type_error.h"
#include "type_table.h"
#include "type_object.h"

/*
 *  let
 */
void scope_init_let(struct let_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

int check_scope_variable_(addr symbol)
{
	Check(! symbolp(symbol), "type error");
	if (keywordp(symbol))
		return fmte_("Keyword ~S can't be use a variable.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		return fmte_("The constant of symbol ~S can't use a variable.", symbol, NULL);

	return 0;
}

static int let_init(Execute ptr, struct let_struct *str)
{
	addr args, root, var, init;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(check_scope_variable_(var));
		Return(scope_eval(ptr, &init, init));
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void push_tablevalue_lexical(Execute ptr, addr stack, addr pos, int doublep)
{
	if (stack == Nil) {
		setglobalp_tablevalue(pos, 1);
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		setlexical_tablevalue(pos, increment_stack_eval(stack));
		setvalue_lexical_evalstack(stack, pos);
		if (doublep) {
			setlet_tablevalue(pos, increment_stack_eval(stack));
			setvalue_lexical_evalstack(stack, pos);
		}
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablevalue_lexical(ptr, stack, pos, doublep);
	}
}

static void push_tablevalue_special(Execute ptr, addr stack, addr pos)
{
	if (stack == Nil) {
		return;
	}
	if (eval_stack_lambda_lexical_p(stack)) {
		/* lexical or lambda */
		setlet_tablevalue(pos, increment_stack_eval(stack));
		setvalue_lexical_evalstack(stack, pos);
	}
	else {
		GetEvalStackNext(stack, &stack);
		push_tablevalue_special(ptr, stack, pos);
	}
}

static void tablevalue_update(Execute ptr, addr stack, addr var, int doublep)
{
	if (var == Nil)
		return;
	if (! getspecialp_tablevalue(var)) {
		push_tablevalue_lexical(ptr, stack, var, doublep);
		return;
	}
	if (doublep) {
		push_tablevalue_special(ptr, stack, var);
		return;
	}
}

static int make_tablevalue_stack(Execute ptr,
		addr *ret, addr stack, addr symbol, int doublep)
{
	addr pos, aster;

	CheckType(symbol, LISPTYPE_SYMBOL);
	if (getvalue_scope_evalstack(stack, symbol, ret))
		return 0;
	make_tablevalue(&pos, symbol);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	setvalue_scope_evalstack(stack, pos);
	tablevalue_update(ptr, stack, pos, doublep);
	*ret = pos;

	return 1;
}

static int make_tablevalue_special_stack(Execute ptr,
		addr *ret, addr stack, addr symbol)
{
	addr pos, aster;

	CheckType(symbol, LISPTYPE_SYMBOL);
	if (find_global_special_evalstack(stack, symbol, ret))
		return 0;
	make_tablevalue(&pos, symbol);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	push_global_special_evalstack(stack, pos);
	*ret = pos;

	return 1;
}

static int let_maketable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, list, var, eval;

	stack = str->stack;
	decl = str->decl;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &list, &args);
		GetCar(list, &var);
		make_tablevalue_stack(ptr, &eval, stack, var, 1);
		Return(apply_declare_let_stack_(ptr, stack, var, decl));
		SetCar(list, eval);
	}

	return 0;
}

static int dynamic_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, value;

	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_VALUE, &key);
	if (find_plistlist_evalstack(stack, key, symbol)) {
		*ret = 1;
		return 1;
	}

	/* table value */
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getdynamic_tablevalue(value);
		return 1;
	}

	return 0;
}
static int dynamic_tablevalue(addr stack, addr symbol)
{
	int result;

	/* local stack */
	while (stack != Nil) {
		if (dynamic_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* dynamic-extent declaration don't allow in procalamation. */
	/* not dynamic-extent */
	return 0;
}

static int ignore_stack_tablevalue(addr stack, addr symbol, enum IgnoreType *ret)
{
	addr key, table, value, check;

	GetEvalStackTable(stack, &table);
	/* ignore, ignorable declaration */
	GetConst(SYSTEM_IGNORE_VALUE, &key);
	if (getpplist(table, key, symbol, &value) == 0) {
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
	if (getvalue_scope_evalstack(stack, symbol, &value)) {
		*ret = getignore_tablevalue(value);
		return 1;
	}

	return 0;
}
static enum IgnoreType ignore_tablevalue(addr stack, addr symbol)
{
	enum IgnoreType result;

	/* local stack */
	while (stack != Nil) {
		if (ignore_stack_tablevalue(stack, symbol, &result)) {
			return result;
		}
		GetEvalStackNext(stack, &stack);
	}

	/* ignore and ignorable declaration don't allow in procalamation. */
	/* not ignore or ignorable */
	return IgnoreType_None;
}

static int type_free_tablevalue(addr stack, addr symbol, addr *ret)
{
	addr key;
	GetEvalStackTable(stack, &stack);
	GetConst(SYSTEM_TYPE_VALUE, &key);
	return getpplist(stack, key, symbol, ret) == 0;
}

static int type_boundary_tablevalue(addr stack, addr symbol, addr *ret)
{
	if (! find_tablevalue(stack, symbol, &symbol))
		return 0;
	gettype_tablevalue(symbol, ret);
	return 1;
}

static int type_tablevalue_(Execute ptr, LocalRoot local,
		addr stack, addr symbol, int specialp, addr *ret)
{
	int check;
	addr root, type;

	root = Nil;
	/* local stack */
	while (stack != Nil) {
		/* free declaration */
		if (type_free_tablevalue(stack, symbol, &type))
			cons_alloc(local, &root, type, root);
		/* boundary declaration */
		check = type_boundary_tablevalue(stack, symbol, &type);
		if (check) {
			cons_alloc(local, &root, type, root);
			if (! specialp)
				goto final;
		}
		/* next scope */
		GetEvalStackNext(stack, &stack);
	}

	/* global stack */
	Return(getglobal_eval_(ptr, &stack));
	/* free declaration */
	if (type_free_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* boundary declaration */
	if (type_boundary_tablevalue(stack, symbol, &type)) {
		cons_alloc(local, &root, type, root);
		goto final;
	}
	/* symbol declaration */
	gettype_value_symbol(symbol, &type);
	if (type != Nil)
		cons_alloc(local, &root, type, root);
	/* final */
final:
	nreverse(ret, root);
	return 0;
}

static int type_and_array(addr cons, addr *ret)
{
	addr array, type, pos;
	size_t size;

	/* array size */
	size = 0;
	type = Nil;
	for (array = cons; array != Nil; ) {
		GetCons(array, &pos, &array);
		if (! type_astert_p(pos)) {
			type = pos;
			size++;
		}
	}
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		CheckType(type, LISPTYPE_TYPE);
		copylocal_object(NULL, ret, type);
		return 0;
	}

	/* type-and */
	vector4_heap(&array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_astert_p(pos)) {
			copylocal_object(NULL, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_heap(LISPDECL_AND, array, ret);
	return 0;
}

static int update_tablevalue_(Execute ptr, addr stack, addr pos)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr name, type;

	/* scope */
	getname_tablevalue(pos, &name);
	Return(specialp_tablevalue_(ptr, stack, name, &specialp));
	dynamic = dynamic_tablevalue(stack, name);
	ignore = ignore_tablevalue(stack, name);
	Return(type_tablevalue_(ptr, NULL, stack, name, specialp, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);

	return 0;
}

int push_tablevalue_global_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr pos, type;

	/* scope */
	Return(specialp_tablevalue_(ptr, stack, symbol, &specialp));
	dynamic = dynamic_tablevalue(stack, symbol);
	ignore = ignore_tablevalue(stack, symbol);
	Return(type_tablevalue_(ptr, NULL, stack, symbol, specialp, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_stack(ptr, &pos, stack, symbol, 0);
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);

	return Result(ret, pos);
}

int push_tablevalue_special_global_(Execute ptr, addr stack, addr symbol, addr *ret)
{
	addr pos, type;

	/* scope */
	Return(type_tablevalue_(ptr, NULL, stack, symbol, 1, &type));
	if (type_and_array(type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_special_stack(ptr, &pos, stack, symbol);
	setspecialp_tablevalue(pos, 1);
	setdynamic_tablevalue(pos, 0);
	setignore_tablevalue(pos, IgnoreType_None);
	settype_tablevalue(pos, type);

	return Result(ret, pos);
}

static int checktype_subtypep_(Execute ptr, addr x, addr y, int *check, int *errp)
{
	SubtypepResult value;

	CheckType(x, LISPTYPE_TYPE);
	CheckType(y, LISPTYPE_TYPE);
	Return(subtypep_scope_(ptr, x, y, Nil, &value));
	switch (value) {
		case SUBTYPEP_INCLUDE:
			/* type check can skip. */
			*check = 0;
			return Result(errp, 0);

		case SUBTYPEP_EXCLUDE:
			/* error, output to warning mesasge. */
			*check = 1;
			return Result(errp, 1);

		case SUBTYPEP_FALSE:
		case SUBTYPEP_INVALID:
		default:
			/* type check must execute. */
			*check = 1;
			return Result(errp, 0);
	}
}

int checktype_p_(Execute ptr, addr x, addr y, int *check, int *errp)
{
	int value;

	/* type-error */
	Return(check_error_type_(ptr, x, &value));
	if (! value)
		goto type_error;
	Return(check_error_type_(ptr, y, &value));
	if (! value)
		goto type_error;

	/* subtypep */
	return checktype_subtypep_(ptr, x, y, check, errp);

type_error:
	*check = 1;
	*errp = 0;
	return 0;
}

static int checktype_warning_(Execute ptr, addr name, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, name, expected,
			"The object ~S must be a ~S type but ~S type.",
			name, expected, type, NULL);
}

int checktype_value_(Execute ptr, addr value, addr init)
{
	int check, errp;
	addr type, name;

	gettype_tablevalue(value, &type);
	GetEvalScopeThe(init, &init);
	Return(checktype_p_(ptr, init, type, &check, &errp));
	if (errp) {
		getname_tablevalue(value, &name);
		Return(checktype_warning_(ptr, name, type, init));
	}
	setcheck_tablevalue(value, check);

	return 0;
}

static int let_applytable_(Execute ptr, struct let_struct *str)
{
	addr stack, args, var, init;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(update_tablevalue_(ptr, stack, var));
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

int ignore_checkvalue_(addr stack)
{
	enum IgnoreType ignore;
	int reference, special;
	addr list, pos, name, value;

	GetEvalStackScope(stack, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (gettype_evaltable(pos) != EvalTable_Value)
			continue;
		get_evaltable(pos, &value);
		getname_tablevalue(value, &name);
		/* check ignore */
		ignore = getignore_tablevalue(value);
		reference = getreference_tablevalue(value);
		special = getspecialp_tablevalue(value);

		if (ignore == IgnoreType_None && (! reference) && (! special)) {
			Return(call_simple_style_warning_va_(NULL,
						"Unused variable ~S.", name, NULL));
		}
		if (ignore == IgnoreType_Ignore && reference) {
			Return(call_simple_style_warning_va_(NULL,
						"Ignore variable ~S used.", name, NULL));
		}
	}

	return 0;
}

static int let_allocate_args(struct let_struct *str)
{
	addr list, var;

	list = str->args;
	while (list != Nil) {
		GetCons(list, &var, &list);
		GetCar(var, &var);
		if (getspecialp_tablevalue(var))
			return 1;
	}

	return 0;
}

static void let_allocate(struct let_struct *str)
{
	str->allocate = let_allocate_args(str)? T: Nil;
}

static int let_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(let_init(ptr, str));
	Return(let_maketable_(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(let_applytable_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));
	let_allocate(str);

	return 0;
}

void localhold_let_struct(LocalRoot local, struct let_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->args, str->decl,
			str->doc, str->cons, str->free, str->the, str->allocate, NULL);
}

int scope_let_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(let_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}


/*
 *  let*
 */
int ifdeclvalue_(Execute ptr, addr stack, addr var, addr decl, addr *ret)
{
	addr pos, aster;

	if (var == Nil)
		return Result(ret, Nil);
	make_tablevalue_stack(ptr, &pos, stack, var, 0);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	Return(apply_declare_value_stack_(ptr, stack, var, decl));
	return push_tablevalue_global_(ptr, stack, var, ret);
}

static int leta_init(Execute ptr, struct let_struct *str)
{
	addr stack, args, decl, root, var, init;

	stack = str->stack;
	args = str->args;
	decl = str->decl;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(check_scope_variable_(var));
		Return(scope_eval(ptr, &init, init));
		Return(ifdeclvalue_(ptr, stack, var, decl, &var));
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static int leta_checktype_(Execute ptr, struct let_struct *str)
{
	addr args, var, init;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		Return(checktype_value_(ptr, var, init));
	}

	return 0;
}

static int leta_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(leta_init(ptr, str));
	Return(apply_declare_(ptr, stack, str->decl, &str->free));
	Return(leta_checktype_(ptr, str));
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	Return(ignore_checkvalue_(stack));
	let_allocate(str);

	return 0;
}

int scope_leta_call(Execute ptr, struct let_struct *str)
{
	Return(newstack_nil_(ptr, &(str->stack)));
	localhold_let_struct(ptr->local, str);
	Return(leta_execute(ptr, str));

	return freestack_eval_(ptr, str->stack);
}

