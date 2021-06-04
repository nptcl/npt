#include "callname.h"
#include "clos.h"
#include "clos_cache.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
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
static int stdget_method_constant_(addr pos, addr *ret,
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
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_method_constant_(addr pos, addr value,
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
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdGetMethod_(p,r,a,b) \
	stdget_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)
#define StdSetMethod_(p,r,a,b) \
	stdset_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

int stdget_method_function_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, function, FUNCTION);
}
int stdset_method_function_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, function, FUNCTION);
}

int stdget_method_generic_function_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, generic_function, GENERIC_FUNCTION);
}
int stdset_method_generic_function_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, generic_function, GENERIC_FUNCTION);
}

int stdget_method_lambda_list_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_method_lambda_list_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_method_qualifiers_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, qualifiers, QUALIFIERS);
}
int stdset_method_qualifiers_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, qualifiers, QUALIFIERS);
}

int stdget_method_specializers_(addr pos, addr *ret)
{
	return StdGetMethod_(pos, ret, specializers, SPECIALIZERS);
}
int stdset_method_specializers_(addr pos, addr value)
{
	return StdSetMethod_(pos, value, specializers, SPECIALIZERS);
}


/*
 *  defmethod
 */
static int method_instance_alloc_(LocalRoot local, addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	addr pos;

	/* make-instance */
	if (clos == Nil)
		GetConst(CLOS_STANDARD_METHOD, &clos);
	Return(clos_instance_alloc_(local, clos, &pos));
	Return(stdset_method_lambda_list_(pos, lambda));
	Return(stdset_method_qualifiers_(pos, qua));
	Return(stdset_method_specializers_(pos, spec));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int method_instance_heap_(addr *ret, addr clos,
		addr lambda, addr qua, addr spec, addr call)
{
	return method_instance_alloc_(NULL, ret, clos, lambda, qua, spec, call);
}

int method_instance_call_(LocalRoot local, addr *ret, addr clos, addr call)
{
	return method_instance_alloc_(local, ret, clos, Nil, Nil, Nil, call);
}

static int method_specializer_list_(addr *ret, addr list)
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
			Return(clos_intern_specializer_(spec, &spec));
		}
		else {
			/* type */
			if (! closp(spec)) {
				Return(clos_find_class_(spec, &spec));
			}
		}
		cons_heap(&root, spec, root);
	}
	nreverse(ret, root);

	return 0;
}

int method_instance_lambda_(LocalRoot local, addr *ret, addr clos, addr lambda)
{
	addr spec;

	Check(! argumentp(lambda), "type error");
	Return(method_specializer_list_(&spec, lambda));
	return method_instance_heap_(ret, clos, lambda, Nil, spec, Nil);
}


/*
 *  add-method
 */
static int method_check_generic_function_(addr gen, addr method)
{
	addr check;

	Return(stdget_method_generic_function_(method, &check));
	if (check != Nil && method != gen) {
		return fmte_("The method ~S is already exists "
				"in the generic-function ~S.", method, gen, NULL);
	}

	return 0;
}

static int method_check_method_class_(addr gen, addr method)
{
	int check;

	Return(stdget_generic_method_class_(gen, &gen));
	Return(clos_subtype_p_(method, gen, &check));
	if (! check)
		return fmte_("The method don't push in the generic-function.", NULL);

	return 0;
}

static int method_check_method_qualifiers_(Execute ptr, addr gen, addr method)
{
	int check;
	addr qua, comb;

	Return(stdget_method_qualifiers_(method, &qua));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(check_qualifiers_equal_(ptr, comb, qua, &check));
	if (! check) {
		return fmte_("The qualifiers ~S "
				"is not found in the method-combination.", qua, NULL);
	}

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

static int method_arguments_check1_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->var != str2->var) {
		return fmte_("The count of variable is "
				"not equal to the generic function.", NULL);
	}

	return 0;
}

static int method_arguments_check2_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	if (str1->opt != str2->opt) {
		return fmte_("The count of &optional is "
				"not equal to the generic function.", NULL);
	}

	return 0;
}

static int method_arguments_check3_(
		const struct argument_struct *str1,
		const struct argument_struct *str2)
{
	int check1, check2, checka;

	check1 = str1->rest || str1->keyp;
	check2 = str2->rest || str2->keyp;
	checka = ((! check1) && (! check2));
	if (! (checka || check2))
		return fmte_("The method must have &rest or &key arguments.", NULL);

	return 0;
}

static int method_arguments_check4_(
		addr pos1, const struct argument_struct *str1,
		addr pos2, const struct argument_struct *str2)
{
	if (! str1->key)
		return 0; /* not keyp */
	if (str2->rest)
		return 0; /* &key ..., &rest -> ok */
	if (str2->allow)
		return 0; /* &key ..., &key &allow-other-keys -> ok */
	if (str1->keyp && str2->keyp)
		return 0; /* &key, &key -> ok */
	if (str1->keyp)
		return 0; /* &key, ... -> ok */
	if (str2->keyp)
		goto error; /* ..., &key -> error */
	/* &key ..., &key ... */
	GetArgument(pos1, ArgumentIndex_key, &pos1);
	GetArgument(pos2, ArgumentIndex_key, &pos2);
	if (! method_null_set_difference(pos1, pos2))
		goto error;
	return 0;

error:
	return fmte_("The &key arguments in the method must have "
			"all &key arguments in generic function.", NULL);
}

static int method_check_method_arguments_(addr gen, addr method)
{
	addr pos1, pos2;
	struct argument_struct *str1, *str2;

	/* generic-lambda-list */
	Return(stdget_generic_lambda_list_(gen, &pos1));
	CheckType(pos1, LISPSYSTEM_ARGUMENT);
	str1 = ArgumentStruct(pos1);
	Check(str1->type != ArgumentType_generic, "type error");

	/* method-lambda-list */
	Return(stdget_method_lambda_list_(method, &pos2));
	CheckType(pos2, LISPSYSTEM_ARGUMENT);
	str2 = ArgumentStruct(pos2);
	Check(str2->type != ArgumentType_method, "type error");

	/* check */
	Return(method_arguments_check1_(str1, str2));
	Return(method_arguments_check2_(str1, str2));
	Return(method_arguments_check3_(str1, str2));
	Return(method_arguments_check4_(pos1, str1, pos2, str2));

	return 0;
}

static int method_eqlcheck_(addr method, addr *ret)
{
	int check;
	addr cons, spec;

	Return(stdget_method_specializers_(method, &method));
	for (cons = Nil; method != Nil; ) {
		GetCons(method, &spec, &method);
		Return(clos_specializer_p_(spec, &check));
		spec = check? T: Nil;
		cons_heap(&cons, spec, cons);
	}
	nreverse(ret, cons);

	return 0;
}

static int method_update_eqlcheck_p_(addr value, addr spec, int *ret)
{
	if (value != Nil)
		return Result(ret, 1);
	else
		return clos_specializer_p_(spec, ret);
}

static int method_update_eqlcheck_(addr gen, addr method, int deletep)
{
	int update, check;
	addr eqlcheck, value, specs, spec, next;

	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_method_specializers_(method, &specs));

	/* update eqlcheck */
	for (update = 0; eqlcheck != Nil; eqlcheck = next) {
		GetCons(eqlcheck, &value, &next);
		Check(specs == Nil, "method-specializers error");
		GetCons(specs, &spec, &specs);
		Return(method_update_eqlcheck_p_(value, spec, &check));
		spec = check? T: Nil;
		if (spec != value) {
			SetCar(eqlcheck, spec);
			update = 1;
		}
	}

	/* clear cache */
	if (deletep && update) {
		Return(stdget_generic_cache_(gen, &value));
		clear_hashtable(value);
	}

	return 0;
}

static int method_update_check_(addr gen, addr method, int deletep)
{
	int check;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (check) {
		Return(method_update_eqlcheck_(gen, method, deletep));
	}
	else {
		Return(method_eqlcheck_(method, &method));
		Return(stdset_generic_eqlcheck_(gen, method));
	}

	return 0;
}

static int method_push_generic_(Execute ptr, addr gen, addr method)
{
	int check;
	addr methods, comb, qua, cons;
	size_t index;

	Return(stdget_generic_methods_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(stdget_method_qualifiers_(method, &qua));
	Return(qualifiers_position_(ptr, qua, comb, &index, &check));
	GetArrayA4(methods, index, &cons);
	cons_heap(&cons, method, cons);
	SetArrayA4(methods, index, cons);

	return 0;
}

static int method_cache_check_(addr eqlcheck, addr args, addr keys, int *ret)
{
	int every;
	addr check, arg, key;

	while (eqlcheck != Nil) {
		GetCons(eqlcheck, &check, &eqlcheck);
		Check(args == Nil, "args error");
		Check(keys == Nil, "keys error");
		GetCons(args, &arg, &args);
		GetCons(keys, &key, &keys);
		if (check != Nil) {
			Return(generic_eql_specializer_(key, arg, 1, &every));
		}
		else {
			Return(clos_subclass_p_(key, arg, &every));
		}
		if (! every)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int method_cache_remove_(LocalRoot local, addr gen, addr method)
{
	int check;
	addr eqlcheck, args, cache, key, keys;
	LocalStack stack;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (! check)
		return 0;
	Return(stdget_generic_cache_(gen, &cache));
	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_method_specializers_(method, &args));

	push_local(local, &stack);
	allkeys_hashtable_local(local, cache, &keys);
	while (keys != Nil) {
		GetCons(keys, &key, &keys);
		Return(method_cache_check_(eqlcheck, args, key, &check));
		if (check) {
			Return(delete_hashtable_(cache, key, &check));
		}
	}
	rollback_local(local, stack);

	return 0;
}

static int method_find_method_nil_(Execute ptr,
		addr gen, addr qua, addr spec, addr *ret)
{
	int check;
	addr methods, comb, method, value;
	size_t index;

	Return(stdget_generic_methods_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (! check) {
		GetArrayA4(methods, index, &methods);
		while (methods != Nil) {
			GetCons(methods, &method, &methods);
			Return(stdget_method_specializers_(method, &value));
			Return(cache_equal_function_(spec, value, &check));
			if (check)
				return Result(ret, method);
		}
	}

	return Result(ret, Nil);
}

int method_find_method_(Execute ptr, addr gen, addr qua, addr spec, addr *ret)
{
	Return(method_find_method_nil_(ptr, gen, qua, spec, ret));
	if (*ret == Nil)
		return fmte_("No method found.", NULL);

	return 0;
}

static int method_remove_method_execute_(Execute ptr, addr gen, addr method, int *ret)
{
	int check;
	addr methods, comb, qua, cons;
	size_t index;

	Return(stdget_generic_methods_(gen, &methods));
	Return(stdget_generic_method_combination_(gen, &comb));
	Return(stdget_method_qualifiers_(method, &qua));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (check)
		return Result(ret, 0);
	GetArrayA4(methods, index, &cons);
	if (! delete1_list_eq_unsafe(method, cons, &cons))
		return Result(ret, 0);
	SetArrayA4(methods, index, cons);
	Return(stdset_method_generic_function_(method, Nil));

	return Result(ret, 1);
}

int method_remove_method_(Execute ptr, addr gen, addr method)
{
	int check;

	Return(method_remove_method_execute_(ptr, gen, method, &check));
	if (! check)
		return 0;
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}

static int method_replace_check_(Execute ptr, addr gen, addr method, addr *ret)
{
	addr qualifiers, specializers;

	Return(stdget_method_qualifiers_(method, &qualifiers));
	Return(stdget_method_specializers_(method, &specializers));
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
	Return(method_check_generic_function_(gen, method));
	Return(method_check_method_class_(gen, method));
	Return(method_check_method_qualifiers_(ptr, gen, method));
	Return(method_check_method_arguments_(gen, method));
	return 0;
}

int method_add_method_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		Return(method_add_replace_(ptr, gen, method, check_method));
	}
	else {
		Return(method_update_check_(gen, method, 1));
		Return(method_push_generic_(ptr, gen, method));
		Return(stdset_method_generic_function_(method, gen));
	}
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}


/*
 *  common_objects
 */
#ifdef LISP_DEBUG
int common_method_add_(Execute ptr, addr gen, addr method)
{
	addr check_method;

	Check(! clos_generic_p_debug(gen), "generic error");
	Check(! clos_method_p_debug(method), "method error");
	Return(method_add_check_(ptr, gen, method));
	Return(method_replace_check_(ptr, gen, method, &check_method));
	if (check_method != Nil) {
		return fmte_("The method is already exists.", NULL);
	}
	else {
		Return(method_update_check_(gen, method, 1));
		Return(method_push_generic_(ptr, gen, method));
		Return(stdset_method_generic_function_(method, gen));
	}
	Return(method_cache_remove_(ptr->local, gen, method));
	return generic_finalize_(gen);
}
#else
int common_method_add_(Execute ptr, addr gen, addr method)
{
	Check(! clos_generic_p_debug(gen), "generic error");
	Check(! clos_method_p_debug(method), "method error");
	Return(method_push_generic_(ptr, gen, method));
	return stdset_method_generic_function_(method, gen);
}
#endif

static int defmethod_make_generic_function_(addr name, addr lambda, addr *ret)
{
	Check(! callnamep(name), "type error");
	Check(! argumentp(lambda), "type error");
	argument_method_to_generic(lambda, &lambda);
	return generic_empty_(name, lambda, ret);
}

int ensure_method_common_(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call)
{
	int check;
	addr gen, method, clos;

	Return(parse_callname_error_(&name, name));
	getglobal_callname(name, &gen);
	if (! argumentp(lambda)) {
		Return(argument_method_heap_(ptr->local, &lambda, lambda));
	}
	if (gen == Unbound) {
		Return(defmethod_make_generic_function_(name, lambda, &gen));
	}
	Return(clos_generic_p_(gen, &check));
	if (! check)
		return fmte_("The function ~S is not generic-function.", gen, NULL);
	Return(stdget_generic_method_class_(gen, &clos));
	Return(method_instance_heap_(&method, clos, lambda, qua, spec, call));
	Return(method_add_method_(ptr, gen, method));

	return Result(ret, method);
}

static int common_method_set_finalize_(addr gen)
{
	addr pos, list, method;
	size_t size, i;

	Return(stdget_generic_methods_(gen, &pos));
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &list);
		while (list != Nil) {
			GetCons(list, &method, &list);
			Return(method_update_check_(gen, method, 0));
		}
	}
	return generic_finalize_(gen);
}

int common_method_finalize_(addr gen)
{
#ifdef LISP_DEBUG
	addr pos;

	/* eqlcheck clear */
	Return(stdset_generic_eqlcheck_(gen, Unbound));
	/* cache clear */
	Return(stdget_generic_cache_(gen, &pos));
	clear_hashtable(pos);
#endif
	/* build generic */
	return common_method_set_finalize_(gen);
}


/*
 *  document
 */
int methodget_document_(addr clos, addr *ret)
{
	Return(stdget_method_function_(clos, &clos));
	return get_documentation_function_object_(clos, ret);
}

int methodset_document_(addr clos, addr value)
{
	Return(stdget_method_function_(clos, &clos));
	return set_documentation_function_object_(clos, value);
}


/*
 *  common
 */
void method_make_method_lambda(addr list, addr env, addr *ret)
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

