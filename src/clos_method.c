#include "callname.h"
#include "clos.h"
#include "clos_cache.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "function.h"
#include "hashtable.h"
#include "lambda.h"
#include "sequence.h"
#include "symbol.h"

/*
 *  access
 */
static void stdget_method_constant(addr pos, addr *ret,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_METHOD, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_method_constant(addr pos, addr value,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_METHOD, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetMethod(p,r,a,b) \
	stdget_method_constant((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)
#define StdSetMethod(p,r,a,b) \
	stdset_method_constant((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

_g void stdget_method_function(addr pos, addr *ret)
{
	StdGetMethod(pos, ret, function, FUNCTION);
}
_g void stdset_method_function(addr pos, addr value)
{
	StdSetMethod(pos, value, function, FUNCTION);
}

_g void stdget_method_generic_function(addr pos, addr *ret)
{
	StdGetMethod(pos, ret, generic_function, GENERIC_FUNCTION);
}
_g void stdset_method_generic_function(addr pos, addr value)
{
	StdSetMethod(pos, value, generic_function, GENERIC_FUNCTION);
}

_g void stdget_method_lambda_list(addr pos, addr *ret)
{
	StdGetMethod(pos, ret, lambda_list, LAMBDA_LIST);
}
_g void stdset_method_lambda_list(addr pos, addr value)
{
	StdSetMethod(pos, value, lambda_list, LAMBDA_LIST);
}

_g void stdget_method_qualifiers(addr pos, addr *ret)
{
	StdGetMethod(pos, ret, qualifiers, QUALIFIERS);
}
_g void stdset_method_qualifiers(addr pos, addr value)
{
	StdSetMethod(pos, value, qualifiers, QUALIFIERS);
}

_g void stdget_method_specializers(addr pos, addr *ret)
{
	StdGetMethod(pos, ret, specializers, SPECIALIZERS);
}
_g void stdset_method_specializers(addr pos, addr value)
{
	StdSetMethod(pos, value, specializers, SPECIALIZERS);
}


/*
 *  defmethod
 */
static void method_instance_alloc(LocalRoot local, addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	addr pos;

	/* make-instance */
	if (clos == Nil)
		GetConst(CLOS_STANDARD_METHOD, &clos);
	clos_instance_alloc(local, clos, &pos);
	stdset_method_lambda_list(pos, lambda);
	stdset_method_qualifiers(pos, qua);
	stdset_method_specializers(pos, spec);
	stdset_method_function(pos, call);
	*ret = pos;
}

static void method_instance_heap(addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	method_instance_alloc(NULL, ret, clos, lambda, qua, spec, call);
}

_g void method_instance_call(LocalRoot local, addr *ret, addr clos, addr call)
{
	method_instance_alloc(local, ret, clos, Nil, Nil, Nil, call);
}

static void method_specializer_list(addr *ret, addr list)
{
	addr root, var, spec;

	/* var opt rest key */
	GetArgument(list, ArgumentIndex_var, &list);
	/* ((symbol spec) ...) */
	for (root = Nil; list != Nil; ) {
		/* (symbol spec) */
		GetCons(list, &var, &list);
		GetCons(var, &var, &spec);
		GetCar(spec, &spec);
		/* spec */
		if (consp(spec)) {
			/* (eql spec) */
			GetCdr(spec, &spec); /* eql */
			GetCar(spec, &spec); /* spec */
			clos_intern_specializer(spec, &spec);
		}
		else {
			/* type */
			if (! closp(spec))
				clos_find_class(spec, &spec);
		}
		cons_heap(&root, spec, root);
	}
	nreverse(ret, root);
}

_g void method_instance_lambda(LocalRoot local, addr *ret, addr clos, addr lambda)
{
	addr spec;

	Check(! argumentp(lambda), "type error");
	method_specializer_list(&spec, lambda);
	method_instance_heap(ret, clos, lambda, Nil, spec, Nil);
}


/*
 *  add-method
 */
static void method_check_generic_function(addr gen, addr method)
{
	addr check;

	stdget_method_generic_function(method, &check);
	if (check != Nil && method != gen) {
		fmte("The method ~S is already exists "
				"in the generic-function ~S.", method, gen, NULL);
	}
}

static void method_check_method_class(addr gen, addr method)
{
	stdget_generic_method_class(gen, &gen);
	if (! clos_subtype_p(method, gen))
		fmte("The method don't push in the generic-function.", NULL);
}

static int method_check_method_qualifiers_(Execute ptr, addr gen, addr method)
{
	int check;
	addr qua, comb;

	stdget_method_qualifiers(method, &qua);
	stdget_generic_method_combination(gen, &comb);
	Return(check_qualifiers_equal_(ptr, comb, qua, &check));
	if (! check)
		fmte("The qualifiers ~S is not found in the method-combination.", qua, NULL);

	return 0;
}

static int method_null_set_difference(addr key1, addr key2)
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

static void method_arguments_check1(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->var != str2->var)
		fmte("The count of variable is not equal to the generic function.", NULL);
}

static void method_arguments_check2(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->opt != str2->opt)
		fmte("The count of &optional is not equal to the generic function.", NULL);
}

static void method_arguments_check3(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	int check1, check2, checka;

	check1 = str1->rest || str1->keyp;
	check2 = str2->rest || str2->keyp;
	checka = ((! check1) && (! check2));
	if (! (checka || check2))
		fmte("The method must have &rest or &key arguments.", NULL);
}

static void method_arguments_check4(
		addr pos1, const struct argument_struct *str1,
		addr pos2, const struct argument_struct *str2)
{
	if (! str1->key)
		return; /* not keyp */
	if (str2->rest)
		return; /* &key ..., &rest -> ok */
	if (str2->allow)
		return; /* &key ..., &key &allow-other-keys -> ok */
	if (str1->keyp && str2->keyp)
		return; /* &key, &key -> ok */
	if (str1->keyp)
		return; /* &key, ... -> ok */
	if (str2->keyp)
		goto error; /* ..., &key -> error */
	/* &key ..., &key ... */
	GetArgument(pos1, ArgumentIndex_key, &pos1);
	GetArgument(pos2, ArgumentIndex_key, &pos2);
	if (! method_null_set_difference(pos1, pos2))
		goto error;
	return;

error:
	fmte("The &key arguments in the method must have "
			"all &key arguments in generic function.", NULL);
}

static void method_check_method_arguments(addr gen, addr method)
{
	addr pos1, pos2;
	struct argument_struct *str1, *str2;

	/* generic-lambda-list */
	stdget_generic_lambda_list(gen, &pos1);
	CheckType(pos1, LISPSYSTEM_ARGUMENT);
	str1 = ArgumentStruct(pos1);
	Check(str1->type != ArgumentType_generic, "type error");

	/* method-lambda-list */
	stdget_method_lambda_list(method, &pos2);
	CheckType(pos2, LISPSYSTEM_ARGUMENT);
	str2 = ArgumentStruct(pos2);
	Check(str2->type != ArgumentType_method, "type error");

	/* check */
	method_arguments_check1(str1, str2);
	method_arguments_check2(str1, str2);
	method_arguments_check3(str1, str2);
	method_arguments_check4(pos1, str1, pos2, str2);
}

static void method_eqlcheck(addr method, addr *ret)
{
	addr cons, spec;

	stdget_method_specializers(method, &method);
	for (cons = Nil; method != Nil; ) {
		GetCons(method, &spec, &method);
		spec = clos_specializer_p(spec)? T: Nil;
		cons_heap(&cons, spec, cons);
	}
	nreverse(ret, cons);
}

static void method_update_eqlcheck(addr gen, addr method, int deletep)
{
	int update;
	addr eqlcheck, check, specs, spec, next;

	stdget_generic_eqlcheck(gen, &eqlcheck);
	stdget_method_specializers(method, &specs);

	/* update eqlcheck */
	for (update = 0; eqlcheck != Nil; eqlcheck = next) {
		GetCons(eqlcheck, &check, &next);
		Check(specs == Nil, "method-specializers error");
		GetCons(specs, &spec, &specs);
		spec = (check != Nil || clos_specializer_p(spec))? T: Nil;
		if (spec != check) {
			SetCar(eqlcheck, spec);
			update = 1;
		}
	}

	/* clear cache */
	if (deletep && update) {
		stdget_generic_cache(gen, &check);
		clear_hashtable(check);
	}
}

static void method_update_check(addr gen, addr method, int deletep)
{
	if (stdboundp_generic_eqlcheck(gen)) {
		method_update_eqlcheck(gen, method, deletep);
	}
	else {
		method_eqlcheck(method, &method);
		stdset_generic_eqlcheck(gen, method);
	}
}

static int method_push_generic_(Execute ptr, addr gen, addr method)
{
	int check;
	addr methods, comb, qua, cons;
	size_t index;

	stdget_generic_methods(gen, &methods);
	stdget_generic_method_combination(gen, &comb);
	stdget_method_qualifiers(method, &qua);
	Return(qualifiers_position_(ptr, qua, comb, &index, &check));
	GetArrayA4(methods, index, &cons);
	cons_heap(&cons, method, cons);
	SetArrayA4(methods, index, cons);

	return 0;
}

static int method_cache_check(addr eqlcheck, addr args, addr keys)
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
			every = generic_eql_specializer(key, arg, 1);
		else
			every = clos_subclass_p(key, arg);
		if (! every) return 0;
	}

	return 1;
}

static void method_cache_remove(LocalRoot local, addr gen, addr method)
{
	addr eqlcheck, args, cache, key, keys;
	LocalStack stack;

	if (! stdboundp_generic_eqlcheck(gen)) return;
	stdget_generic_cache(gen, &cache);
	stdget_generic_eqlcheck(gen, &eqlcheck);
	stdget_method_specializers(method, &args);

	push_local(local, &stack);
	allkeys_hashtable_local(local, cache, &keys);
	while (keys != Nil) {
		GetCons(keys, &key, &keys);
		if (method_cache_check(eqlcheck, args, key))
			delete_hashtable(cache, key);
	}
	rollback_local(local, stack);
}

static int method_find_method_nil_(Execute ptr,
		addr gen, addr qua, addr spec, addr *ret)
{
	int check;
	addr methods, comb, method, value;
	size_t index;

	stdget_generic_methods(gen, &methods);
	stdget_generic_method_combination(gen, &comb);
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (! check) {
		GetArrayA4(methods, index, &methods);
		while (methods != Nil) {
			GetCons(methods, &method, &methods);
			stdget_method_specializers(method, &value);
			if (cache_equal_function(spec, value))
				return Result(ret, method);
		}
	}

	return Result(ret, Nil);
}

_g int method_find_method_(Execute ptr, addr gen, addr qua, addr spec, addr *ret)
{
	Return(method_find_method_nil_(ptr, gen, qua, spec, ret));
	if (*ret == Nil)
		fmte("No method found.", NULL);

	return 0;
}

static int method_remove_method_execute_(Execute ptr, addr gen, addr method, int *ret)
{
	int check;
	addr methods, comb, qua, cons;
	size_t index;

	stdget_generic_methods(gen, &methods);
	stdget_generic_method_combination(gen, &comb);
	stdget_method_qualifiers(method, &qua);
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (check)
		return Result(ret, 0);
	GetArrayA4(methods, index, &cons);
	if (! delete1_list_eq_unsafe(method, cons, &cons))
		return Result(ret, 0);
	SetArrayA4(methods, index, cons);
	stdset_method_generic_function(method, Nil);

	return Result(ret, 1);
}

_g int method_remove_method_(Execute ptr, addr gen, addr method)
{
	int check;

	Return(method_remove_method_execute_(ptr, gen, method, &check));
	if (! check)
		return 0;
	method_cache_remove(ptr->local, gen, method);
	generic_finalize(gen);

	return 0;
}

static int method_replace_check_(Execute ptr, addr gen, addr method, addr *ret)
{
	addr qualifiers, specializers;

	stdget_method_qualifiers(method, &qualifiers);
	stdget_method_specializers(method, &specializers);
	return method_find_method_nil_(ptr, gen, qualifiers, specializers, ret);
}

static int method_add_replace_(Execute ptr,
		addr gen, addr method, addr check_method)
{
	int check;
	Return(method_remove_method_execute_(ptr, gen, check_method, &check));
	return method_push_generic_(ptr, gen, method);
}

static int method_add_check_(Execute ptr, addr gen, addr method)
{
	method_check_generic_function(gen, method);
	method_check_method_class(gen, method);
	Return(method_check_method_qualifiers_(ptr, gen, method));
	method_check_method_arguments(gen, method);

	return 0;
}

_g int method_add_method_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		Return(method_add_replace_(ptr, gen, method, check_method));
	}
	else {
		method_update_check(gen, method, 1);
		Return(method_push_generic_(ptr, gen, method));
		stdset_method_generic_function(method, gen);
	}
	method_cache_remove(ptr->local, gen, method);
	generic_finalize(gen);

	return 0;
}


/*
 *  common_objects
 */
static int method_add_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Check(! clos_generic_p(gen), "generic error");
	Check(! clos_method_p(method), "method error");
	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		fmte("The method is already exists.", NULL);
		return 0;
	}
	else {
		method_update_check(gen, method, 1);
		Return(method_push_generic_(ptr, gen, method));
		stdset_method_generic_function(method, gen);
	}
	method_cache_remove(ptr->local, gen, method);
	generic_finalize(gen);

	return 0;
}
#ifdef LISP_DEBUG
static int common_method_add_(Execute ptr, addr gen, addr method)
{
	return method_add_(ptr, gen, method);
}
#else
static int common_method_add_(Execute ptr, addr gen, addr method)
{
	Check(! clos_generic_p(gen), "generic error");
	Check(! clos_method_p(method), "method error");
	Return(method_push_generic_(ptr, gen, method));
	stdset_method_generic_function(method, gen);
	return 0;
}
#endif

_g void common_method_add(Execute ptr, addr gen, addr method)
{
	Error(common_method_add_(ptr, gen, method));
}

static void defmethod_make_generic_function(addr name, addr lambda, addr *ret)
{
	Check(! callnamep(name), "type error");
	Check(! argumentp(lambda), "type error");
	argument_method_to_generic(lambda, &lambda);
	generic_empty(name, lambda, ret);
}

_g int ensure_method_common_(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call)
{
	addr gen, method, clos;

	parse_callname_error(&name, name);
	getglobal_callname(name, &gen);
	if (! argumentp(lambda))
		argument_method_heap(ptr->local, &lambda, lambda);
	if (gen == Unbound)
		defmethod_make_generic_function(name, lambda, &gen);
	if (! clos_generic_p(gen))
		fmte("The function ~S is not generic-function.", gen, NULL);
	stdget_generic_method_class(gen, &clos);
	method_instance_heap(&method, clos, lambda, qua, spec, call);
	Return(method_add_(ptr, gen, method));

	return Result(ret, method);
}

static void common_method_set_finalize(addr gen)
{
	addr pos, list, method;
	size_t size, i;

	stdget_generic_methods(gen, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &list);
		while (list != Nil) {
			GetCons(list, &method, &list);
			method_update_check(gen, method, 0);
		}
	}
	generic_finalize(gen);
}

_g void common_method_finalize(addr gen)
{
#ifdef LISP_DEBUG
	addr pos;

	/* eqlcheck clear */
	stdset_generic_eqlcheck(gen, Unbound);
	/* cache clear */
	stdget_generic_cache(gen, &pos);
	clear_hashtable(pos);
#endif
	/* build generic */
	common_method_set_finalize(gen);
}


/*
 *  document
 */
_g void methodget_document(addr clos, addr *ret)
{
	stdget_method_function(clos, &clos);
	getdocumentation_function(clos, ret);
}

_g void methodset_document(addr clos, addr value)
{
	stdget_method_function(clos, &clos);
	setdocumentation_function(clos, value);
}


/*
 *  common
 */
_g void method_make_method_lambda(addr list, addr env, addr *ret)
{
	/* `(lambda (,method ,next &rest ,args)
	 *    (flet ((next-method-p ()
	 *             (clos::flet-method-p ,next))
	 *           (call-next-method (&rest ,rest)
	 *             (clos::flet-next-method ,method ,next ,args ,rest)))
	 *      (declare (ignorable #'next-method-p #'call-next-method))
	 *      (apply (lambda ,lambda-list ,@form) ,args)))
	 */
	addr lambda, apply, next1, next2, call1, call2, a, b, c;
	addr method, next, args, rest, ignorable, declare, arest, flet;

	/* gensym */
	make_symbolchar(&method, "METHOD");
	make_symbolchar(&next, "NEXT");
	make_symbolchar(&args, "ARGS");
	make_symbolchar(&rest, "REST");
	/* constant */
	GetConst(COMMON_NEXT_METHOD_P, &next1);
	GetConst(CLOSNAME_FLET_METHOD_P, &next2);
	GetConst(COMMON_CALL_NEXT_METHOD, &call1);
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &call2);
	/* apply */
	GetConst(COMMON_APPLY, &apply);
	list_heap(&apply, apply, list, args, NULL);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_FUNCTION, &a);
	list_heap(&b, a, next1, NULL);
	list_heap(&c, a, call1, NULL);
	list_heap(&ignorable, ignorable, b, c, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	/* next-method-p */
	list_heap(&next2, next2, next, NULL);
	list_heap(&next1, next1, Nil, next2, NULL);
	/* call-next-method */
	list_heap(&call2, call2, method, next, args, rest, NULL);
	GetConst(AMPERSAND_REST, &arest);
	list_heap(&rest, arest, rest, NULL);
	list_heap(&call1, call1, rest, call2, NULL);
	/* flet */
	list_heap(&next1, next1, call1, NULL);
	GetConst(COMMON_FLET, &flet);
	list_heap(&flet, flet, next1, declare, apply, NULL);
	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
	list_heap(&method, method, next, arest, args, NULL);
	list_heap(ret, lambda, method, flet, NULL);
}

