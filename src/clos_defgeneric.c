#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_defgeneric.h"
#include "clos_generic.h"
#include "clos_instance.h"
#include "clos_method.h"
#include "clos_object.h"
#include "clos_redefine.h"
#include "clos_type.h"
#include "closget_class.h"
#include "closget_generic.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute_values.h"
#include "hashtable.h"
#include "hold.h"
#include "lambda.h"
#include "symbol.h"
#include "typedef.h"

struct generic_argument {
	unsigned lambda_p : 1;
	unsigned generic_p : 1;
	unsigned method_p : 1;
	unsigned combination_p : 1;
	unsigned order_p : 1;
	unsigned declare_p : 1;
	unsigned doc_p : 1;
	unsigned redefined : 1;
	Execute ptr;
	addr env, instance, callname, name, lambda, args;
	addr generic, method, combination, order, declare, doc;
};


/*
 *  defgeneric
 */
void generic_cache_heap(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_CACHE,
			8,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

static int generic_make_methods_(Execute ptr, addr *ret, addr comb)
{
	size_t size;

	Return(method_combination_qualifiers_count_(ptr, comb, &size));
	vector4_heap(ret, size);

	return 0;
}

static int generic_make_instance_(Execute ptr,
		addr *ret, addr call, addr args, int finalp)
{
	addr name, lambda, pos, vector, method, cache;

	CheckType(call, LISPTYPE_CALLNAME);
	CheckType(args, LISPSYSTEM_ARGUMENT);
	Check(ArgumentStruct(args)->type != ArgumentType_generic, "argument error");

	/* object */
	name_callname_heap(call, &name);
	Return(argument_generic_lambda_heap_(&lambda, args));

	/* methods */
	Return(generic_make_methods_(ptr, &vector, Nil));
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_cache_heap(&cache);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	Return(clos_instance_heap_(ptr, pos, &pos));

	/* setf */
	Return(stdset_generic_name_(pos, name));
	Return(stdset_generic_methods_(pos, Nil));
	Return(stdset_generic_lambda_list_(pos, lambda));
	Return(stdset_generic_argument_precedence_order_(pos, Nil));
	Return(stdset_generic_method_class_(pos, method));
	Return(stdset_generic_method_combination_(pos, Nil));
	Return(stdset_generic_vector_(pos, vector));
	Return(stdset_generic_remove_(pos, Nil));
	Return(stdset_generic_argument_(pos, args));
	Return(stdset_generic_cache_(pos, cache));
	Return(stdset_generic_precedence_index_(pos, Nil));

	/* result */
	if (finalp) {
		Return(generic_finalize_(pos));
	}
	Return(setglobal_parse_callname_(name, pos));
	return Result(ret, pos);
}

int generic_make_(Execute ptr, addr *ret, addr call, addr args)
{
#ifdef LISP_DEBUG
	addr pos;

	clos_find_generic_nil(call, &pos);
	Check(pos != Nil, "generic function error");
#endif
	CheckType(call, LISPTYPE_CALLNAME);
	return generic_make_instance_(ptr, ret, call, args, 0);
}

int generic_make_empty_(Execute ptr, addr call, addr args, addr *ret)
{
	return generic_make_instance_(ptr, ret, call, args, 1);
}


/*
 *  generic-new
 */
static int generic_new_order_(addr args, addr order, addr *ret)
{
	addr var, x, list;
	size_t size1, size2, index;

	if (order == Nil)
		return Result(ret, Nil);
	GetArgument(args, ArgumentIndex_var, &var);
	Return(length_list_safe_(var, &size1));
	Return(length_list_safe_(order, &size2));
	if (size1 != size2)
		goto error1;
	for (list = Nil; var != Nil; ) {
		GetCons(var, &x, &var);
		if (! position_list_eq_unsafe(x, order, &index))
			goto error2;
		index_heap(&x, index);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
	return 0;

error1:
	*ret = Nil;
	return call_simple_program_error_va_(NULL, "Length of "
			":argument-precedence-order is not equal to lambda-list.", NULL);

error2:
	*ret = Nil;
	return call_simple_program_error_va_(NULL, "The variable ~S is "
			"not exist in :argument-precedence-order", x, NULL);
}

static int generic_find_method_combination_(struct generic_argument *str, addr *ret)
{
	int check;
	addr pos, standard;
	Execute ptr;

	/* standard */
	ptr = str->ptr;
	pos = str->combination;
	if (pos == Nil)
		return Result(ret, Nil);

	/* standard object */
	GetConst(CLOS_COMBINATION_STANDARD, &standard);
	if (pos == standard)
		return Result(ret, Nil);

	/* ensure-generic-function */
	Return(clos_combination_p_(ptr, pos, &check));
	if (check)
		return Result(ret, pos);

	/* defgeneric */
	return clos_find_method_combination_(ptr, pos, ret);
}

static int generic_new_(struct generic_argument *str, addr *ret)
{
	addr pos, comb, vector, cache, order;
	Execute ptr;

	ptr = str->ptr;
	/* check */
	Return(generic_new_order_(str->args, str->order, &order));

	/* (make-instance generic-function-class) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	Return(funcall1_control_(ptr, &pos, pos, str->generic, NULL));

	/* value */
	Return(generic_find_method_combination_(str, &comb));
	Return(generic_make_methods_(ptr, &vector, comb));
	generic_cache_heap(&cache);

	/* setf */
	Return(stdset_generic_name_(pos, str->name));
	Return(stdset_generic_methods_(pos, Nil));
	Return(stdset_generic_lambda_list_(pos, str->lambda));
	Return(stdset_generic_argument_precedence_order_(pos, str->order));
	Return(stdset_generic_declarations_(pos, str->declare));
	Return(stdset_generic_method_class_(pos, str->method));
	Return(stdset_generic_method_combination_(pos, comb));
	Return(stdset_generic_vector_(pos, vector));
	Return(stdset_generic_argument_(pos, str->args));
	Return(stdset_generic_documentation_(pos, str->doc));
	Return(stdset_generic_cache_(pos, cache));
	Return(stdset_generic_precedence_index_(pos, order));

	/* result */
	Return(generic_finalize_(pos));
	Return(setglobal_parse_callname_(str->name, pos));
	return Result(ret, pos);
}


/*
 *  generic-change
 */
static int generic_change_remove_(struct generic_argument *str)
{
	int ignore;
	addr gen, method, list;
	Execute ptr;

	ptr = str->ptr;
	gen = str->instance;
	if (str->redefined) {
		Return(stdget_generic_remove_(gen, &list));
		while (list != Nil) {
			GetCons(list, &method, &list);
			Return(method_remove_method_unsafe_(ptr, gen, method, &ignore));
		}
	}
	Return(stdset_generic_remove_(gen, Nil));

	return 0;
}

static int generic_change_equal_(struct generic_argument *str, int *ret)
{
	addr gen1, gen2;

	gen1 = str->generic;
	Return(clos_class_of_(str->instance, &gen2));

	return Result(ret, (gen1 == gen2));
}

static int generic_change_lambda_list_(struct generic_argument *str)
{
	addr pos, lambda, args;

	if (str->lambda_p) {
		pos = str->instance;
		lambda = str->lambda;
		Return(argument_generic_heap_(str->ptr->local, &args, lambda));
		Return(stdset_generic_lambda_list_(pos, lambda));
		Return(stdset_generic_argument_(pos, args));
		str->args = args;
	}

	return 0;
}

static int generic_change_order_(struct generic_argument *str)
{
	addr pos, order, index;

	/* &key */
	pos = str->instance;
	if (str->order_p) {
		order = str->order;
	}
	else {
		Return(stdget_generic_argument_precedence_order_(pos, &order));
	}

	/* set */
	Return(generic_new_order_(str->args, order, &index));
	Return(stdset_generic_argument_precedence_order_(pos, order));
	Return(stdset_generic_precedence_index_(pos, index));

	return 0;
}

static int generic_change_change_class_(struct generic_argument *str)
{
	int check;

	Return(generic_change_equal_(str, &check));
	if (! check) {
		Return(clos_change_class_(str->ptr, str->instance, str->generic, Nil));
	}

	return 0;
}

static int generic_change_combination_(struct generic_argument *str)
{
	addr pos, value;

	if (str->combination_p) {
		pos = str->instance;
		Return(generic_find_method_combination_(str, &value));
		Return(stdset_generic_method_combination_(pos, value));
	}

	return 0;
}

static int generic_change_method_(struct generic_argument *str)
{
	addr gen, vector, list, method;
	Execute ptr;

	/* vector */
	ptr = str->ptr;
	gen = str->instance;
	Return(stdget_generic_method_combination_(gen, &vector));
	Return(generic_make_methods_(ptr, &vector, vector));
	Return(stdset_generic_vector_(gen, vector));

	/* methods */
	Return(stdget_generic_methods_(gen, &list));
	Return(stdset_generic_methods_(gen, Nil));

	/* add method */
	while (list != Nil) {
		GetCons(list, &method, &list);
		Return(method_add_method_(ptr, gen, method));
	}

	return 0;
}

static int generic_change_execute_(struct generic_argument *str)
{
	addr pos, cache;

	Return(generic_change_lambda_list_(str));
	Return(generic_change_order_(str));
	Return(generic_change_change_class_(str));
	Return(generic_change_combination_(str));
	Return(generic_change_method_(str));
	generic_cache_heap(&cache);

	/* setf */
	pos = str->instance;
	Return(stdset_generic_declarations_(pos, str->declare));
	Return(stdset_generic_method_class_(pos, str->method));
	Return(stdset_generic_documentation_(pos, str->doc));
	Return(stdset_generic_cache_(pos, cache));

	/* result */
	return generic_finalize_(pos);
}

static void generic_change_copy_vector(addr methods, addr *ret)
{
	addr pos, list;
	size_t size, i;

	LenArrayA4(methods, &size);
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(methods, i, &list);
		copy_list_heap_unsafe(&list, list);
		SetArrayA4(pos, i, list);
	}
	*ret = pos;
}

static int generic_change_copy_(addr gen, addr *ret)
{
	addr pos, value;

	clos_allcopy_alloc(NULL, gen, &pos);

	/* methods */
	Return(stdget_generic_methods_(pos, &value));
	copy_list_heap_unsafe(&value, value);
	Return(stdset_generic_methods_(pos, value));

	/* vector */
	Return(stdget_generic_vector_(pos, &value));
	generic_change_copy_vector(value, &value);
	Return(stdset_generic_vector_(pos, value));

	return Result(ret, pos);
}

static int generic_change_call_(struct generic_argument *str)
{
	int check;
	addr pos, save, list;
	LocalHold hold;

	/* save */
	pos = str->instance;
	hold = LocalHold_array(str->ptr, 4);
	localhold_set(hold, 0, str->callname);
	localhold_set(hold, 1, str->args);

	/* instance */
	Return(generic_change_copy_(pos, &save));
	localhold_set(hold, 2, save);

	/* method */
	Return(generic_change_remove_(str));
	Return(stdget_generic_methods_(pos, &list));
	localhold_set(hold, 3, list);

	/* execute */
	check = generic_change_execute_(str);
	if (check)
		clos_swap(pos, save);
	localhold_end(hold);

	return check;
}

static int generic_change_(struct generic_argument *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)generic_change_call_(str);
	return pop_control_(ptr, control);
}


/*
 *  ensure-generic-function
 */
int ensure_generic_function_name_(addr name, addr *ret)
{
	addr call, value, expected;

	*ret = Nil;
	Return(parse_callname_error_(&call, name));
	if (! symbolp_callname(call))
		return Result(ret, call);

	/* macro */
	getmacro_symbol(name, &value);
	if (value != Unbound) {
		GetConst(COMMON_GENERIC_FUNCTION, &expected);
		return call_type_error_va_(NULL, name, expected,
				"ENSURE-GENERIC-FUNCTION don't accept "
				"a macro symbol ~S.", name, NULL);
	}

	/* special-operator */
	if (get_special_operator(name)) {
		GetConst(COMMON_GENERIC_FUNCTION, &expected);
		return call_type_error_va_(NULL, name, expected,
				"ENSURE-GENERIC-FUNCTION don't accept "
				"a special symbol ~S.", name, NULL);
	}

	return Result(ret, call);
}

static int ensure_generic_function_call_(Execute ptr,
		addr clos, addr name, addr rest, addr *ret)
{
	addr call;

	/* (apply #'ensure-generic-function-using-class clos name rest) */
	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &call);
	Return(getfunction_global_(call, &call));
	Return(applya_control_(ptr, call, clos, name, rest, NULL));
	getresult_control(ptr, ret);

	return 0;
}

int ensure_generic_function_common_(Execute ptr, addr name, addr rest, addr *ret)
{
	int check;
	addr call, value, expected;

	/* symbol or (setf name) */
	Return(ensure_generic_function_name_(name, &call));

	/* class-of */
	getglobal_parse_callname(call, &value);
	if (value == Unbound)
		return ensure_generic_function_call_(ptr, Nil, name, rest, ret);
	Return(clos_generic_p_(ptr, value, &check));
	if (check)
		return ensure_generic_function_call_(ptr, value, name, rest, ret);

	/* error */
	*ret = Nil;
	GetConst(COMMON_GENERIC_FUNCTION, &expected);
	return call_type_error_va_(ptr, value, expected,
			"Invalid generic-function argument ~S.", name, NULL);
}


/*
 *  ensure-generic-function-using-class
 */
static int mop_generic_struct_(
		struct generic_argument *str,
		Execute ptr, addr instance, addr name, addr rest)
{
	int lambda_p, generic_p, method_p, combination_p, order_p, declare_p, doc_p;
	addr call, order, decl, doc, env, gen, args, lambda, method, comb, redefined;

	/* arguments */
	lambda_p = generic_p = method_p = combination_p = 1;
	order_p = declare_p = doc_p = 1;
	if (GetKeyArgs(rest, KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &order)) {
		order_p = 0;
		order = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_DECLARE, &decl)) {
		declare_p = 0;
		decl = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc)) {
		doc_p = 0;
		doc = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_LAMBDA_LIST, &lambda)) {
		lambda_p = 0;
		lambda = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_METHOD_COMBINATION, &comb)) {
		combination_p = 0;
		comb = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_GENERIC_FUNCTION_CLASS, &gen)) {
		generic_p = 0;
		GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gen);
	}
	if (GetKeyArgs(rest, KEYWORD_METHOD_CLASS, &method)) {
		method_p = 0;
		GetConst(CLOS_STANDARD_METHOD, &method);
	}
	if (GetKeyArgs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	if (GetKeyArgs(rest, CLOSNAME_REDEFINED, &redefined))
		redefined = Nil;

	/* name */
	Check(callnamep(name), "type error");
	Return(parse_callname_error_(&call, name));

	/* lambda-list */
	Check(argumentp(lambda), "type error");
	Return(argument_generic_heap_(ptr->local, &args, lambda));

	/* value */
	str->ptr = ptr;
	str->env = env;
	str->instance = instance;
	str->callname = call;
	str->name = name;
	str->args = args;
	str->lambda = lambda;
	str->generic = gen;
	str->method = method;
	str->combination = comb;
	str->order = order;
	str->declare = decl;
	str->doc = doc;
	str->redefined = (redefined != Nil)? 1: 0;

	str->lambda_p = (lambda_p? 1: 0);
	str->generic_p = (generic_p? 1: 0);
	str->method_p = (method_p? 1: 0);
	str->combination_p = (combination_p? 1: 0);
	str->order_p = (order_p? 1: 0);
	str->declare_p = (declare_p? 1: 0);
	str->doc_p = (doc_p? 1: 0);

	return 0;
}

int mop_generic_new_(Execute ptr, addr name, addr rest, addr *ret)
{
	struct generic_argument str;
	Return(mop_generic_struct_(&str, ptr, Nil, name, rest));
	return generic_new_(&str, ret);
}

int mop_generic_change_(Execute ptr, addr clos, addr name, addr rest)
{
	struct generic_argument str;
	Return(mop_generic_struct_(&str, ptr, clos, name, rest));
	return generic_change_(&str);
}


/*
 *  syscall
 */
int system_generic_define_(Execute ptr, addr name, addr args, addr *ret)
{
	addr key;
	LocalRoot local;
	LocalStack stack;

	/* `(redefined t ,@args) */
	local = ptr->local;
	push_local(local, &stack);
	GetConst(CLOSNAME_REDEFINED, &key);
	cons_local(local, &args, T, args);
	cons_local(local, &args, key, args);

	/* call */
	Return(ensure_generic_function_common_(ptr, name, args, ret));

	/* rollback */
	rollback_local(local, stack);
	return 0;
}

int system_generic_method_(addr gen, addr args)
{
	addr list, pos;

	Return(stdget_generic_remove_(gen, &list));
	while (args != Nil) {
		GetCons(args, &pos, &args);
		cons_heap(&list, pos, list);
	}
	Return(stdset_generic_remove_(gen, list));

	return 0;
}

