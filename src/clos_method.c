#include "clos.h"
#include "clos_cache.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "condition.h"
#include "cons.h"
#include "function.h"
#include "hashtable.h"
#include "lambda.h"
#include "sequence.h"


/*
 *  defmethod
 */
void make_instance_standard_method(LocalRoot local,
		addr *ret, addr clos,
		addr lambda_list, addr lambda_parse,
		addr qualifiers, addr specializers,
		addr function)
{
	addr instance;

	/* make-instance */
	if (clos == Nil)
		GetConst(CLOS_STANDARD_METHOD, &clos);
	make_instance_restrict_alloc(local, clos, &instance);
	std_update_class_of(instance, clos);
	/* lambda-list */
	setf_clos_elt(instance, Clos_method_lambda_list, lambda_list);
	/* lambda-parse */
	setf_clos_elt(instance, Clos_method_lambda_parse, lambda_parse);
	/* qualifiers */
	setf_clos_elt(instance, Clos_method_qualifiers, qualifiers);
	/* specializers */
	setf_clos_elt(instance, Clos_method_specializers, specializers);
	/* function */
	setf_clos_elt(instance, Clos_method_function, function);
	*ret = instance;
}

void make_instance_standard_method_function(LocalRoot local,
		addr *ret, addr clos, addr call)
{
	make_instance_standard_method(local, ret, clos, Nil, Nil, Nil, Nil, call);
}

void specializer_list(addr *ret, addr cons)
{
	addr var, spec, result;

	/* (var opt rest key allow aux) */
	GetCar(cons, &cons);
	/* ((symbol spec) ...) */
	for (result = Nil; cons != Nil; ) {
		/* (symbol spec) */
		GetCons(cons, &var, &cons);
		GetCons(var, &var, &spec);
		GetCar(spec, &spec);
		/* spec */
		if (GetType(spec) == LISPTYPE_CONS) {
			/* (eql spec) */
			GetCdr(spec, &spec);
			GetCar(spec, &spec);
			intern_eql_specializer(spec, &spec);
		}
		else {
			/* type */
			spec = find_class(spec);
		}
		cons_heap(&result, spec, result);
	}
	nreverse_list_unsafe(ret, result);
}

void make_instance_standard_method_lambda(LocalRoot local,
		addr *ret, addr clos, addr lambda)
{
	addr parse, spec;

	lambda_specialized(local, &parse, lambda);
	specializer_list(&spec, parse);
	make_instance_standard_method(NULL, ret, clos, lambda, parse, Nil, spec, Nil);
}


/*
 *  add-method
 */
static void check_method_class(addr generic, addr method)
{
	clos_elt(generic, Clos_generic_method_class, &generic);
	if (! std_subtype_p(method, generic))
		fmte("The method don't push in the generic-function.", NULL);
}

static void check_method_qualifiers(Execute ptr, addr generic, addr method)
{
	addr qua, combination;

	clos_elt(method, Clos_method_qualifiers, &qua);
	clos_elt(generic, Clos_generic_method_combination, &combination);
	if (! check_qualifiers_equal(ptr, combination, qua))
		fmte("The qualifiers ~S is not found in the method-combination.", qua, NULL);
}

static int list_length_compare(addr left, addr right)
{
	int check1, check2;

	check1 = (left == Nil);
	check2 = (right == Nil);
	if (check1 && check2) return 0;
	if (check1) return -1;
	if (check2) return 1;
	GetCdr(left, &left);
	GetCdr(right, &right);

	return list_length_compare(left, right);
}

static int null_set_difference(addr key1, addr key2)
{
	int check;
	addr left, right, loop;

	/* (null (set-difference key1 key2 :key #'cadr :test #'eq)) */
	while (key1 != Nil) {
		GetCons(key1, &left, &key1);
		/* cadr */
		GetCdr(left, &left);
		GetCar(left, &left);
		/* (find left key2) */
		check = 0;
		for (loop = key2; loop != Nil; ) {
			GetCons(loop, &right, &loop);
			/* cadr */
			GetCdr(right, &right);
			GetCar(right, &right);
			if (left == right) {
				check = 1;
				break;
			}
		}
		if (check == 0) {
			/* not found */
			return 0;
		}
	}

	return 1;
}

static int check_all_keys(addr key1, addr key2)
{
	int check1, check2;

	check1 = (key1 == T);
	check2 = (key2 == T);
	if (check1 && check2) return 1;
	if (check1) return 1;
	if (check2) return 0;
	return null_set_difference(key1, key2);
}

static void arguments_check1(addr var1, addr var2)
{
	if (list_length_compare(var1, var2))
		fmte("The count of variable is not equal to the generic function.", NULL);
}

static void arguments_check2(addr opt1, addr opt2)
{
	if (list_length_compare(opt1, opt2))
		fmte("The count of &optional is not equal to the generic function.", NULL);
}

static void arguments_check3(addr rest1, addr key1, addr rest2, addr key2)
{
	int check1, check2, checka;

	check1 = ((rest1 != Nil) || (key1 != Nil));
	check2 = ((rest2 != Nil) || (key2 != Nil));
	checka = ((! check1) && (! check2));
	if (! (checka || check2))
		fmte("The method must have &rest or &key arguments.", NULL);
}

static void arguments_check4(addr key1, addr rest2, addr key2, addr allow2)
{
	if ((key1 != Nil) &&
			! ((rest2 != Nil) || (allow2 != Nil) || check_all_keys(key1, key2))) {
		fmte("The &key arguments in the method must have "
				"all &key arguments in generic function.", NULL);
	}
}

static void check_method_arguments(addr generic, addr method)
{
	addr cons;
	addr var1, opt1, rest1, key1;
	addr var2, opt2, rest2, key2, allow2;

	/* generic-lambda-list */
	clos_elt(generic, Clos_generic_lambda_list, &cons);
	GetCons(cons, &var1, &cons);
	GetCons(cons, &opt1, &cons);
	GetCons(cons, &rest1, &cons);
	GetCar(cons, &key1);
	if (opt1 == T) opt1 = Nil;

	/* method-lambda-list */
	clos_elt(method, Clos_method_lambda_parse, &cons);
	GetCons(cons, &var2, &cons);
	GetCons(cons, &opt2, &cons);
	GetCons(cons, &rest2, &cons);
	GetCons(cons, &key2, &cons);
	GetCar(cons, &allow2);
	if (opt2 == T) opt2 = Nil;

	/* check */
	arguments_check1(var1, var2);
	arguments_check2(opt1, opt2);
	arguments_check3(rest1, key1, rest2, key2);
	arguments_check4(key1, rest2, key2, allow2);
}

static void method_eqlcheck(addr method, addr *ret)
{
	addr cons, spec;

	clos_elt(method, Clos_method_specializers, &method);
	for (cons = Nil; method != Nil; ) {
		GetCons(method, &spec, &method);
		spec = eql_specializer_p(spec)? T: Nil;
		cons_heap(&cons, spec, cons);
	}
	nreverse_list_unsafe(ret, cons);
}

static void update_method_and_cache_eqlcheck(addr generic, addr method)
{
	int update;
	addr eqlcheck, check, specs, spec, next;

	clos_elt(generic, Clos_generic_eqlcheck, &eqlcheck);
	clos_elt(method, Clos_method_specializers, &specs);

	/* update eqlcheck */
	for (update = 0; eqlcheck != Nil; eqlcheck = next) {
		GetCons(eqlcheck, &check, &next);
		Check(specs == Nil, "method-specializers error");
		GetCons(specs, &spec, &specs);
		spec = (check != Nil || eql_specializer_p(spec))? T: Nil;
		if (spec != check) {
			SetCar(eqlcheck, spec);
			update = 1;
		}
	}

	/* clear cache */
	if (update) {
		clos_elt(generic, Clos_generic_cache, &check);
		clear_hashtable(check);
	}
}

static void update_method_check(addr generic, addr method)
{
	if (clos_elt_boundp(generic, Clos_generic_eqlcheck)) {
		update_method_and_cache_eqlcheck(generic, method);
	}
	else {
		method_eqlcheck(method, &method);
		setf_clos_elt(generic, Clos_generic_eqlcheck, method);
	}
}

static void generic_function_push_method(Execute ptr,
		addr generic, addr method)
{
	addr methods, combination, qualifiers, cons;
	size_t position;

	clos_elt(generic, Clos_generic_methods, &methods);
	clos_elt(generic, Clos_generic_method_combination, &combination);
	clos_elt(method, Clos_method_qualifiers, &qualifiers);
	qualifiers_position(ptr, qualifiers, combination, &position);
	GetArrayA4(methods, position, &cons);
	cons_heap(&cons, method, cons);
	SetArrayA4(methods, position, cons);
}

static int every_remove_cache_check(addr eqlcheck, addr args, addr keys)
{
	int every;
	addr check, arg, key;

	while (eqlcheck != Nil) {
		GetCons(eqlcheck, &check, &eqlcheck);
		Check(args == Nil, "args error");
		Check(keys == Nil, "keys error");
		GetCons(args, &arg, &args);
		GetCons(keys, &key, &keys);
		if (check != Nil)
			every = subclass_eql_specializer(key, arg, 1);
		else
			every = std_subclass_p(key, arg);
		if (! every) return 0;
	}

	return 1;
}

static void generic_function_remove_cache(LocalRoot local,
		addr generic, addr method)
{
	addr eqlcheck, args, cache, key, keys;
	LocalStack stack;

	if (! clos_elt_boundp(generic, Clos_generic_eqlcheck)) return;
	clos_elt(generic, Clos_generic_cache, &cache);
	clos_elt(generic, Clos_generic_eqlcheck, &eqlcheck);
	clos_elt(method, Clos_method_specializers, &args);

	push_local(local, &stack);
	allkeys_hashtable_local(local, cache, &keys);
	while (keys != Nil) {
		GetCons(keys, &key, &keys);
		if (every_remove_cache_check(eqlcheck, args, key))
			delete_hashtable(cache, key);
	}
	rollback_local(local, stack);
}

static void std_find_method_nil(Execute ptr,
		addr generic, addr qualifiers, addr specializer, addr *ret)
{
	addr methods, combination, method, check;
	size_t position;

	clos_elt(generic, Clos_generic_methods, &methods);
	clos_elt(generic, Clos_generic_method_combination, &combination);
	if (! qualifiers_position_nil(ptr, qualifiers, combination, &position)) {
		GetArrayA4(methods, position, &methods);
		while (methods != Nil) {
			GetCons(methods, &method, &methods);
			clos_elt(method, Clos_method_specializers, &check);
			if (cache_equal_function(specializer, check)) {
				*ret = method;
				return;
			}
		}
	}
	*ret = Nil;
}

void std_find_method(Execute ptr,
		addr generic, addr qualifiers, addr specializer, addr *ret)
{
	std_find_method_nil(ptr, generic, qualifiers, specializer, ret);
	if (*ret == Nil)
		fmte("No method found.", NULL);
}

static int std_remove_method_execute(Execute ptr, addr generic, addr method)
{
	addr methods, combination, qualifiers, cons;
	size_t position;

	clos_elt(generic, Clos_generic_methods, &methods);
	clos_elt(generic, Clos_generic_method_combination, &combination);
	clos_elt(method, Clos_method_qualifiers, &qualifiers);
	if (qualifiers_position_nil(ptr, qualifiers, combination, &position)) return 0;
	GetArrayA4(methods, position, &cons);
	if (! delete1_cons_eq_unsafe(method, cons, &cons)) return 0;
	SetArrayA4(methods, position, cons);

	return 1;
}

void std_remove_method(Execute ptr, addr generic, addr method)
{
	if (! std_remove_method_execute(ptr, generic, method)) return;
	generic_function_remove_cache(ptr->local, generic, method);
	std_finalize_generic_function(ptr, generic);
}

static void check_replace_method(Execute ptr,
		addr generic, addr method, addr *ret)
{
	addr qualifiers, specializers;

	clos_elt(method, Clos_method_qualifiers, &qualifiers);
	clos_elt(method, Clos_method_specializers, &specializers);
	std_find_method_nil(ptr, generic, qualifiers, specializers, ret);
}

static void update_replace_method(Execute ptr,
		addr generic, addr method, addr check_method)
{
	std_remove_method_execute(ptr, generic, check_method);
	generic_function_push_method(ptr, generic, method);
}

static void check_add_method(Execute ptr, addr generic, addr method)
{
	check_method_class(generic, method);
	check_method_qualifiers(ptr, generic, method);
	check_method_arguments(generic, method);
}

void std_add_method(Execute ptr, addr generic, addr method)
{
	addr check_method;

	check_add_method(ptr, generic, method);
	check_replace_method(ptr, generic, method, &check_method);
	if (check_method != Nil) {
		update_replace_method(ptr, generic, method, check_method);
	}
	else {
		update_method_check(generic, method);
		generic_function_push_method(ptr, generic, method);
		setf_clos_elt(method, Clos_method_generic_function, generic);
	}
	generic_function_remove_cache(ptr->local, generic, method);
	std_finalize_generic_function(ptr, generic);
}

void document_standard_method(addr clos, addr *ret)
{
	clos_elt(clos, Clos_method_function, &clos);
	getdocumentation_function(clos, ret);
}

void setf_document_standard_method(addr clos, addr value)
{
	clos_elt(clos, Clos_method_function, &clos);
	setdocumentation_function(clos, value);
}

