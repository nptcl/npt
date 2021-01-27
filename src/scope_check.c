#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "eval_table.h"
#include "hold.h"
#include "parse_object.h"
#include "scope_check.h"
#include "scope_lambda.h"
#include "scope_object.h"
#include "type.h"
#include "type_object.h"
#include "type_table.h"

/*
 *  first
 */
static int scope_call_first(Execute ptr, addr *ret, addr first)
{
	switch (RefEvalParseType(first)) {
		case EVAL_PARSE_FUNCTION:
			return scope_function_call(ptr, ret, first);

		case EVAL_PARSE_LAMBDA:
			return scope_lambda_call(ptr, ret, first);

		default:
			return fmte_("Invalid parse object.", NULL);
	}

	return 0;
}


/*
 *  arguments
 */
static int check_tablecall_warning_(Execute ptr, addr name, addr type, addr expected)
{
	Return(type_object_(&type, type));
	Return(type_object_(&expected, expected));
	return call_type_error_va_(ptr, name, expected,
			"The object ~S expected a ~S type but the initialize form is ~S type.",
			name, expected, type, NULL);
}

static int check_tablecall(Execute ptr, addr eval, addr right, addr *ret)
{
	int check, errp;
	addr table, type, name;

	/* tablecall */
	Return(scope_eval(ptr, &eval, eval));
	make_tablecall(&table);
	copylocal_object(NULL, &right, right);
	settype_tablecall(table, right);
	setvalue_tablecall(table, eval);
	/* checktype */
	GetEvalScopeThe(eval, &type);
	Return(checktype_p_(type, right, &check, &errp));
	if (errp) {
		GetEvalScopeValue(eval, &name);
		Return(check_tablecall_warning_(ptr, name, type, right));
	}
	setcheck_tablecall(table, check);
	/* result */
	return Result(ret, table);
}

static int callargs_var(Execute ptr, addr array, addr *args, addr *root, int *ret)
{
	addr eval, right;
	LocalHold hold;

	GetArrayA2(array, 0, &array);
	hold = LocalHold_array(ptr, 1);
	while (array != Nil) {
		if (*args == Nil)
			return Result(ret, 1);
		GetCons(*args, &eval, args);
		GetCons(array, &right, &array);
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	return Result(ret, 0);
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

static int callargs_restonly_(Execute ptr, addr rest, addr *args, addr *root)
{
	LocalHold hold;
	addr eval;

	hold = LocalHold_array(ptr, 1);
	copylocal_object(NULL, &rest, rest);
	localhold_push(hold, rest);

	while (*args != Nil) {
		GetCons(*args, &eval, args);
		Return(check_tablecall(ptr, eval, rest, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	return 0;
}

static void type_and_nil(addr type1, addr type2, addr *ret)
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
		type2and_heap(type1, type2, ret);
		return;
	}
}

static int callargs_rest_or_key_(Execute ptr, addr rest, addr *args, addr *root)
{
	int kv;
	LocalHold hold;
	addr eval, type1, type2;

	hold = LocalHold_array(ptr, 1);
	/* type */
	if (rest == Nil) {
		GetTypeTable(&type1, Symbol);
		GetTypeTable(&type2, T);
	}
	else {
		/* type1 == (or symbol rest), type2 == rest */
		GetTypeTable(&type1, Symbol);
		copylocal_object(NULL, &rest, rest);
		type_and_nil(type1, rest, &type1);
		type2 = rest;
	}
	localhold_push(hold, type1);
	localhold_push(hold, type2);

	/* check */
	for (kv = 0; *args != Nil; kv = ! kv) {
		GetCons(*args, &eval, args);
		rest = (kv == 0)? type1: type2;
		Return(check_tablecall(ptr, eval, rest, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

	/* key-value error */
	if (kv)
		return fmte_("There is no value in &key argument.", NULL);

	return 0;
}

static int callargs_restkey(Execute ptr,
		addr array, addr *args, addr *root, int *ret)
{
	addr rest, key;

	/* type */
	GetArrayA2(array, 2, &rest);
	GetArrayA2(array, 3, &key);

	/* no-rest and no-key */
	if (rest == Nil && key == Nil)
		return Result(ret, (*args != Nil));

	/* &rest only */
	if (key == Nil) {
		Return(callargs_restonly_(ptr, rest, args, root));
		return Result(ret, 0);
	}

	/* [&rest] &key &allow-other-keys */
	Return(callargs_rest_or_key_(ptr, rest, args, root));
	return Result(ret, 0);
}

static int callargs_toomany(Execute ptr, addr *args, addr *root)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	GetTypeTable(&type, Asterisk);
	while (consp(*args)) {
		GetCons(*args, &eval, args);
		Return(check_tablecall(ptr, eval, type, &eval));
		cons_heap(root, eval, *root);
		localhold_set(hold, 0, *root);
	}
	localhold_end(hold);

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
	Return(callargs_var(ptr, array, &args, &root, &check));
	if (check)
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
	check = 0;
	Return(callargs_restkey(ptr, array, &args, &root, &check));
	localhold_set(hold, 0, root);
	if (check)
		goto toomany;
	goto final;

toofew:
	Return(call_simple_style_warning_va_(NULL, "Too few arguments.", NULL));
	goto final;

toomany:
	Return(call_simple_style_warning_va_(NULL, "Too many arguments.", NULL));
	Return(callargs_toomany(ptr, &args, &root));
	goto final;

final:
	localhold_end(hold);
	nreverse(ret, root);

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
	nreverse(ret, root);

	return 0;
}

static int scope_call_args(Execute ptr, addr *ret, addr first, addr args)
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


/*
 *  values
 */
static void scope_call_values(addr *ret, addr first)
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


/*
 *  scope
 */
int scope_call_call_(Execute ptr, addr first, addr args, addr *ret)
{
	addr eval, type;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	Return(scope_call_first(ptr, &first, first));
	localhold_push(hold, first);
	Return(scope_call_args(ptr, &args, first, args));
	localhold_push(hold, args);
	scope_call_values(&type, first);
	localhold_push(hold, type);
	localhold_end(hold);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CALL, type, Nil));
	SetEvalScopeIndex(eval, 0, first);
	SetEvalScopeIndex(eval, 1, args);
	return Result(ret, eval);
}

