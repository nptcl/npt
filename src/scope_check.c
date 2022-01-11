#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "eval_table.h"
#include "hold.h"
#include "parse_object.h"
#include "scope_check.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_let.h"
#include "scope_object.h"
#include "type.h"
#include "type_function.h"
#include "type_object.h"
#include "type_table.h"

/*
 *  first
 */
static int scope_call_first(Execute ptr, addr *ret, addr first)
{
	switch (RefEvalParseType(first)) {
		case EVAL_PARSE_FUNCTION:
			return scope_function_call_(ptr, ret, first);

		case EVAL_PARSE_LAMBDA:
			return scope_lambda_call_(ptr, ret, first);

		default:
			*ret = Nil;
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
	return checktype_error_(ptr, name, expected,
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
	Return(checktype_p_(ptr, type, right, &check, &errp));
	if (errp) {
		GetEvalScopeValue(eval, &name);
		Return(check_tablecall_warning_(ptr, name, type, right));
	}
	setcheck_tablecall(table, check);
	/* result */
	return Result(ret, table);
}

static int scope_call_args_loop(Execute ptr, addr *ret, addr type, addr args)
{
	addr root, eval, right;
	LocalHold hold;
	size_t i;

	hold = LocalHold_array(ptr, 1);
	root = Nil;
	for (i = 0; args != Nil; i++) {
		GetCons(args, &eval, &args);
		Return(gettype_ordcall_(type, i, &right));
		Return(check_tablecall(ptr, eval, right, &eval));
		cons_heap(&root, eval, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

static int scope_call_size_check_(Execute ptr, addr type, addr args)
{
	int check;
	size_t size;

	size = length_list_unsafe(args);
	Return(size_check_ordcall_(type, size, &check));
	if (! check) {
		Return(call_simple_style_warning_va_(NULL, "Invalid function call.", NULL));
	}

	return 0;
}

static int scope_call_args(Execute ptr, addr *ret, addr pos, addr args)
{
	addr type;

	GetEvalScopeThe(pos, &type);
	Return(scope_call_size_check_(ptr, type, args));
	return scope_call_args_loop(ptr, ret, type, args);
}


/*
 *  values
 */
static void scope_call_values(addr *ret, addr pos)
{
	GetEvalScopeThe(pos, &pos);
	CheckType(pos, LISPTYPE_TYPE);
	make_ordvalues_heap(pos, ret);
}


/*
 *  scope
 */
int scope_call_call_(Execute ptr, addr form, addr first, addr args, addr *ret)
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

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CALL, type, form));
	SetEvalScopeIndex(eval, 0, first);
	SetEvalScopeIndex(eval, 1, args);
	return Result(ret, eval);
}

