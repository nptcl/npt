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
#include "symbol.h"
#include "type.h"
#include "type_table.h"
#include "type_object.h"
#include "type_subtypep.h"

/*
 *  let
 */
_g void scope_init_let(struct let_struct *str)
{
	clearpoint(str);
	str->stack = str->args = str->decl = str->doc
		= str->cons = str->free = str->the = str->allocate = Nil;
}

_g void check_scope_variable(addr symbol)
{
	Check(! symbolp(symbol), "type error");
	if (keywordp(symbol))
		fmte("Keyword ~S can't be use a variable.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		fmte("The constant of symbol ~S can't use a variable.", symbol, NULL);
}

static int let_init(Execute ptr, struct let_struct *str)
{
	addr args, root, var, init;

	args = str->args;
	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		check_scope_variable(var);
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

static void let_maketable(Execute ptr, struct let_struct *str)
{
	addr stack, args, list, var;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &list, &args);
		GetCar(list, &var);
		make_tablevalue_stack(ptr, &var, stack, var, 1);
		SetCar(list, var);
	}
}

static int dynamic_stack_tablevalue(addr stack, addr symbol, int *ret)
{
	addr key, table, value;

	GetEvalStackTable(stack, &table);
	/* dynamic-extent declaration */
	GetConst(SYSTEM_DYNAMIC_VALUE, &key);
	if (getplist(table, key, &value) == 0 && find_list_eq_unsafe(symbol, value)) {
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

static void type_tablevalue(Execute ptr, LocalRoot local,
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
	getglobal_eval(ptr, &stack);
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
}

_g int type_and_array(LocalRoot local, addr cons, addr *ret)
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
		copylocal_object(local, ret, type);
		return 0;
	}

	/* type-and */
	vector4_alloc(local, &array, size);
	for (size = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		CheckType(pos, LISPTYPE_TYPE);
		if (! type_astert_p(pos)) {
			copylocal_object(local, &pos, pos);
			SetArrayA4(array, size++, pos);
		}
	}
	type1_alloc(local, LISPDECL_AND, array, ret);
	return 0;
}

static void update_tablevalue(Execute ptr, addr stack, addr pos)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr name, type;

	/* scope */
	getname_tablevalue(pos, &name);
	specialp = specialp_tablevalue(ptr, stack, name);
	dynamic = dynamic_tablevalue(stack, name);
	ignore = ignore_tablevalue(stack, name);
	type_tablevalue(ptr, NULL, stack, name, specialp, &type);
	if (type_and_array(NULL, type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);
}
_g void push_tablevalue_global(Execute ptr, addr stack, addr symbol, addr *ret)
{
	enum IgnoreType ignore;
	int specialp, dynamic;
	addr pos, type;

	/* scope */
	specialp = specialp_tablevalue(ptr, stack, symbol);
	dynamic = dynamic_tablevalue(stack, symbol);
	ignore = ignore_tablevalue(stack, symbol);
	type_tablevalue(ptr, NULL, stack, symbol, specialp, &type);
	if (type_and_array(NULL, type, &type))
		GetTypeTable(&type, Asterisk);
	Check(type == Nil, "type error");

	/* make table */
	make_tablevalue_stack(ptr, &pos, stack, symbol, 0);
	setspecialp_tablevalue(pos, specialp);
	setdynamic_tablevalue(pos, dynamic);
	setignore_tablevalue(pos, ignore);
	settype_tablevalue(pos, type);
	*ret = pos;
}

_g int checktype_p(addr left, addr right, int *check)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	switch (subtypep_result(left, right, 1)) {
		case SUBTYPEP_INCLUDE:
			/* type check can skip. */
			*check = 0;
			return 0;

		case SUBTYPEP_EXCLUDE:
			/* error, output to warning mesasge. */
			*check = 1;
			return 1;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_INVALID:
		default:
			/* type check must execute. */
			*check = 1;
			return 0;
	}
}

static void checktype_warning(addr name, addr type, addr expected)
{
	type_object(&type, type);
	type_object(&expected, expected);
	type_error_stdarg(name, expected,
			"The object ~S must be a ~S type but ~S type.",
			name, expected, type, NULL);
}

_g void checktype_value(addr value, addr init)
{
	int check;
	addr type, name;

	gettype_tablevalue(value, &type);
	GetEvalScopeThe(init, &init);
	if (checktype_p(init, type, &check)) {
		getname_tablevalue(value, &name);
		checktype_warning(name, type, init);
	}
	setcheck_tablevalue(value, check);
}

static void let_applytable(Execute ptr, struct let_struct *str)
{
	addr stack, args, var, init;

	stack = str->stack;
	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		update_tablevalue(ptr, stack, var);
		checktype_value(var, init);
	}
}

_g void ignore_checkvalue(addr stack)
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
			fmtw("Unused variable ~S.", name, NULL);
		}
		if (ignore == IgnoreType_Ignore && reference) {
			fmtw("Ignore variable ~S used.", name, NULL);
		}
	}
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

static int let_allocate_decl(struct let_struct *str)
{
	return str->decl != Nil;
}

static void let_allocate(struct let_struct *str)
{
	int check = let_allocate_args(str) || let_allocate_decl(str);
	str->allocate = check? T: Nil;
}

static int let_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(let_init(ptr, str));
	let_maketable(ptr, str);
	apply_declare(ptr, stack, str->decl, &str->free);
	let_applytable(ptr, str);
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	ignore_checkvalue(stack);
	let_allocate(str);

	return 0;
}

_g void localhold_let_struct(LocalRoot local, struct let_struct *str)
{
	gchold_pushva_force_local(local,
			str->stack, str->args, str->decl,
			str->doc, str->cons, str->free, str->the, str->allocate, NULL);
}

_g int scope_let_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	localhold_let_struct(ptr->local, str);
	Return(let_execute(ptr, str));
	freestack_eval(ptr, str->stack);

	return 0;
}


/*
 *  let*
 */
_g void ifdeclvalue(Execute ptr, addr stack, addr var, addr decl, addr *ret)
{
	addr pos, aster;

	if (var == Nil) {
		*ret = Nil;
		return;
	}
	make_tablevalue_stack(ptr, &pos, stack, var, 0);
	GetTypeTable(&aster, Asterisk);
	settype_tablevalue(pos, aster);
	apply_declare_value_stack(ptr->local, stack, var, decl);
	push_tablevalue_global(ptr, stack, var, ret);
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
		check_scope_variable(var);
		Return(scope_eval(ptr, &init, init));
		ifdeclvalue(ptr, stack, var, decl, &var);
		cons_heap(&var, var, init);
		cons_heap(&root, var, root);
	}
	nreverse(&str->args, root);

	return 0;
}

static void leta_checktype(Execute ptr, struct let_struct *str)
{
	addr args, var, init;

	args = str->args;
	while (args != Nil) {
		GetCons(args, &var, &args);
		GetCons(var, &var, &init);
		checktype_value(var, init);
	}
}

static int leta_execute(Execute ptr, struct let_struct *str)
{
	addr stack;

	stack = str->stack;
	Return(leta_init(ptr, str));
	apply_declare(ptr, stack, str->decl, &str->free);
	leta_checktype(ptr, str);
	Return(scope_allcons(ptr, &str->cons, &str->the, str->cons));
	ignore_checkvalue(stack);
	let_allocate(str);

	return 0;
}

_g int scope_leta_call(Execute ptr, struct let_struct *str)
{
	str->stack = newstack_nil(ptr);
	localhold_let_struct(ptr->local, str);
	Return(leta_execute(ptr, str));
	freestack_eval(ptr, str->stack);

	return 0;
}

