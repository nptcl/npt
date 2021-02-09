#include "callname.h"
#include "clos_generic.h"
#include "code_lambda.h"
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
 *  lambda object
 */
int pop_code(Execute ptr, CodeValue x)
{
	Return(popargs_control_(ptr, &x.pos));
	if (x.pos == Unbound)
		return fmte_("Too few argument.", NULL);
	setresult_control(ptr, x.pos);

	return 0;
}

int pop_unbound_code(Execute ptr, CodeValue x)
{
	Return(popargs_control_(ptr, &x.pos));
	setresult_control(ptr, x.pos);

	return 0;
}

int getf_code(Execute ptr, CodeValue x)
{
	addr list, key, value;

	GetArgsControl(ptr, &list);
	while (GetType(list) == LISPTYPE_CONS) {
		GetCons(list, &key, &list);
		if (GetType(list) != LISPTYPE_CONS)
			return fmte_("Invalid property list ~S.", list, NULL);
		GetCons(list, &value, &list);
		if (key == x.pos) {
			setresult_control(ptr, value);
			return 0;
		}
	}

	/* not found */
	setresult_control(ptr, Unbound);
	return 0;
}

int rest_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	copy_list_alloc_safe(NULL, &pos, pos);
	setresult_control(ptr, pos);

	return 0;
}

int allow_other_keys_code(Execute ptr, CodeValue x)
{
	addr list, key1, key2, keys;

	GetArgsControl(ptr, &list);

	/* :allow-other-keys t */
	GetConst(KEYWORD_ALLOW_OTHER_KEYS, &key1);
	if (! getplist_safe(list, key1, &key1)) {
		if (key1 != Nil)
			return 0;
	}

	/* check */
	while (GetType(list) == LISPTYPE_CONS) {
		GetCons(list, &key1, &list);
		keys = x.pos;
		for (;;) {
			if (keys == Nil)
				return fmte_("The key ~S cannot accept the function.", key1, NULL);
			GetCons(keys, &key2, &keys);
			if (key1 == key2)
				break;
		}
		if (GetType(list) != LISPTYPE_CONS)
			break;
		GetCdr(list, &list);
	}

	return 0;
}

static int rest_keys_code(Execute ptr)
{
	addr list, x;

	GetArgsControl(ptr, &list);
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		if (! symbolp(x))
			return fmte_("The key name ~S must be a symbol type.", x, NULL);
		if (! consp_getcdr(list, &list))
			return fmte_("There is no value in the ~S &key argument.", x, NULL);
		Return_getcons(list, &x, &list);
	}

	return 0;
}

int rest_null_code(Execute ptr, CodeValue x)
{
	addr list;

	if (x.pos != Nil)
		return rest_keys_code(ptr);

	/* rest-null */
	GetArgsControl(ptr, &list);
	if (list != Nil)
		return fmte_("Too many arguments.", NULL);

	return 0;
}

int whole_code(Execute ptr, CodeValue x)
{
	addr list;

	/* (args) */
	getresult_control(ptr, &list);
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	return 0;
}

int lambda_code(Execute ptr, CodeValue x)
{
	addr pos;
	function_heap(&pos, Nil, x.pos);
	setresult_control(ptr, pos);
	return 0;
}

int lambda_name_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	SetNameFunction(pos, x.pos);
	return 0;
}

int lambda_type_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	settype_function(pos, x.pos);
	return 0;
}

int lambda_doc_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	return set_documentation_function_object_(pos, x.pos);
}

int lambda_form_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	setlambda_expression_function(pos, x.pos);
	return 0;
}

int lambda_defun_code(Execute ptr, CodeValue x)
{
	addr pos;
	getresult_control(ptr, &pos);
	setdefunform_function(pos, x.pos);
	return 0;
}

static void getclosure_list_code(Execute ptr, addr pos, addr list, addr *ret)
{
	addr x, y, z;
	fixnum type;
	size_t dst, src;

	List_bind(list, &x, &y, &z, NULL);
	GetFixnum(x, &type);
	GetIndex(y, &src);
	GetIndex(z, &dst);

	switch ((enum EvalTable)type) {
		case EvalTable_Value:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Function:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_TagBody:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Block:
			getlow_lexical_control(ptr, src, &x);
			closure_heap(ret, x, dst);
			break;

		case EvalTable_Self:
			closure_heap(ret, pos, dst);
			break;

		default:
			Abort("Invalid eval-table type.");
			break;
	}
}

int lambda_closure_code(Execute ptr, CodeValue x)
{
	addr list, pos, root, value;

	getresult_control(ptr, &pos);
	list = x.pos;
	root = Nil;
	while (list != Nil) {
		GetCons(list, &value, &list);
		getclosure_list_code(ptr, pos, value, &value);
		cons_heap(&root, value, root);
	}
	nreverse(&root, root);
	SetDataFunction(pos, root);

	return 0;
}

int lambda_lexical_code(Execute ptr, CodeValue x)
{
	addr list, pos, data;
	size_t value;

	/* allocate */
	GetCons(x.pos, &pos, &list);
	GetIndex(pos, &value);
	lexical_control(ptr, value);

	/* restore closure */
	getdata_control(ptr, &data);
	while (data != Nil) {
		GetCons(data, &pos, &data);
		CheckType(pos, LISPSYSTEM_CLOSURE);
		value = lexical_closure(pos);
		get_closure(pos, &pos);
		setlow_lexical_control(ptr, value, pos);
	}

	/* set closure */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_INDEX);
		GetIndex(pos, &value);
		reference_lexical_control(ptr, value);
	}

	return 0;
}

int lambda_cache_code(Execute ptr, CodeValue x)
{
	addr jump, pos;
	size_t index;

	List_bind(x.pos, &jump, &pos, NULL);
	GetValueSymbol(pos, &pos);
	if (pos == Unbound)
		return 0;

	/* cache hit */
	setresult_control(ptr, pos);
	GetIndex(jump, &index);
	return goto_control_(ptr, index);
}

int lambda_cache_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	CheckType(pos, LISPTYPE_FUNCTION);
	CheckType(x.pos, LISPTYPE_SYMBOL);
	SetValueSymbol(x.pos, pos);

	return 0;
}


/*
 *  macro object
 */
int macro_code(Execute ptr, CodeValue x)
{
	addr pos;
	macro_heap(&pos, Nil, x.pos);
	setresult_control(ptr, pos);
	return 0;
}

int macro_special_code(Execute ptr, CodeValue x)
{
	pushspecial_control(ptr, x.pos, Unbound);
	return 0;
}

int macro_env_code(Execute ptr, CodeValue x)
{
	addr list;

	GetArgsControl(ptr, &list);
	/* ((call . args) env) */
	Return_getcdr(list, &list); /* (env) */
	Return_getcar(list, &list); /* env */
	setresult_control(ptr, list);

	return 0;
}

int macro_whole_code(Execute ptr, CodeValue x)
{
	addr list;

	/* (call . args) */
	getresult_control(ptr, &list);
	Return_getcdr(list, &list); /* (args) */
	SetControl(ptr->control, Control_Cons, list);
	SetControl(ptr->control, Control_ConsTail, Nil);

	return 0;
}

int labels_make_code(Execute ptr, CodeValue x)
{
	addr list, pos;
	size_t index;

	list = x.pos;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetIndex(pos, &index);
		function_empty_heap(&pos, Nil);
		set_lexical_control(ptr, index, pos);
	}

	return 0;
}

int labels_lambda_code(Execute ptr, CodeValue x)
{
	addr pos, code;
	size_t index;

	List_bind(x.pos, &pos, &code, NULL);
	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	SetCodeFunction(pos, code);
	setresult_control(ptr, pos);

	return 0;
}


/*
 *  multiple-value-bind
 */
int bind1_type_code(Execute ptr, CodeValue x)
{
	addr value, index, type;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &type);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;

	return call_typep_error_(ptr, value, type);
}

int bind1_special_code(Execute ptr, CodeValue x)
{
	addr value, index, symbol;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &symbol);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;
	pushspecial_control(ptr, symbol, value);

	return 0;
}

int bind1_lexical_code(Execute ptr, CodeValue x)
{
	addr value, index, lexical;
	size_t i;

	GetCons(x.pos, &index, &value);
	GetCar(value, &lexical);
	GetIndex(index, &i);
	getvalues_control(ptr, i, &value);
	if (value == Unbound)
		value = Nil;
	GetIndex(lexical, &i);
	set_lexical_control(ptr, i, value);

	return 0;
}

int bind2_type_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_root_control(ptr, &value);
	if (value == Unbound)
		value = Nil;

	return call_typep_error_(ptr, value, x.pos);
}

int bind2_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_pop_control(ptr, &value);
	if (value == Unbound)
		value = Nil;
	pushspecial_control(ptr, x.pos, value);

	return 0;
}

int bind2_lexical_code(Execute ptr, CodeValue x)
{
	addr value;

	getvalues_pop_control(ptr, &value);
	if (value == Unbound)
		value = Nil;
	set_lexical_control(ptr, x.index, value);

	return 0;
}

