#include <stdarg.h>
#include "build.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "condition.h"
#include "lambda.h"
#include "package.h"
#include "package_intern.h"
#include "sequence.h"
#include "symbol.h"

/*
 *  error
 */
static int lambda_list_fmte_(const char *fmt, ...)
{
	va_list va;

	va_start(va, fmt);
	Return(call_simple_program_error_stdarg_(NULL, fmt, va));
	va_end(va);

	return 0;
}


/*
 *  parse
 */
enum AMPERSAND_ARGUMENT {
	AMPERSAND_GENERIC,
	AMPERSAND_ORDINARY,
	AMPERSAND_METHOD_COMBINATION,
	AMPERSAND_MACRO,
	AMPERSAND_SIZE
};

static int constant_eq(constindex index, addr pos)
{
	addr check;
	GetConstant(index, &check);
	return check == pos;
}

static int member_ampersand(addr pos, enum AMPERSAND_ARGUMENT ampersand)
{
	/* generic */
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, pos))
		return 1;
	if (constant_eq(CONSTANT_AMPERSAND_REST, pos))
		return 1;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, pos))
		return 1;
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, pos))
		return 1;
	if (ampersand <= AMPERSAND_GENERIC)
		return 0;
	/* ordinary */
	if (constant_eq(CONSTANT_AMPERSAND_AUX, pos))
		return 1;
	if (ampersand <= AMPERSAND_ORDINARY)
		return 0;
	/* method-combination */
	if (constant_eq(CONSTANT_AMPERSAND_WHOLE, pos))
		return 1;
	if (ampersand <= AMPERSAND_METHOD_COMBINATION)
		return 0;
	/* macro */
	if (constant_eq(CONSTANT_AMPERSAND_BODY, pos))
		return 1;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, pos))
		return 1;

	return 0;
}

static int variable_check_(addr var, enum AMPERSAND_ARGUMENT ampersand)
{
	if (var == Nil || var == T) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (! symbolp(var)) {
		return lambda_list_fmte_("The variable ~S "
				"is not a symbol.", var, NULL);
	}
	if (GetStatusReadOnly(var)) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (member_ampersand(var, ampersand)) {
		return lambda_list_fmte_("The symbol ~S "
				"don't use in the lambda-list.", var, NULL);
	}

	return 0;
}

/* push-varcons */
static void varcons_local(LocalRoot local, addr *ret)
{
	queue_local(local, ret);
}

static void varcons_data(addr instance, addr *ret)
{
	rootqueue(instance, ret);
}

static int push_varcons_(LocalRoot local,
		addr instance, addr var, enum AMPERSAND_ARGUMENT ampersand)
{
	addr data;

	Return(variable_check_(var, ampersand));
	varcons_data(instance, &data);
	if (find_list_eq_unsafe(var, data))
		return lambda_list_fmte_("The variable ~S is already used.", var, NULL);
	pushqueue_local(local, instance, var);

	return 0;
}

static int push_namecons_(LocalRoot local, addr instance, addr name)
{
	addr data;

	varcons_data(instance, &data);
	if (find_list_eq_unsafe(name, data))
		return lambda_list_fmte_("The key name ~S is already used.", name, NULL);
	pushqueue_local(local, instance, name);

	return 0;
}

static int make_keyword_from_symbol_(addr symbol, addr *ret)
{
	addr keyword, package;

	if (! symbolp(symbol))
		return lambda_list_fmte_("The variable ~S must be a symbol.", symbol, NULL);
	GetConst(PACKAGE_KEYWORD, &keyword);
	GetPackageSymbol(symbol, &package);
	if (package == keyword)
		return Result(ret, symbol);

	GetNameSymbol(symbol, &symbol);
	return intern_package_(keyword, symbol, ret, NULL);
}

static int list2_check(addr cons, addr *ret1, addr *ret2)
{
	addr pos1, pos2;

	if (GetType(cons) != LISPTYPE_CONS)
		return 1;
	GetCons(cons, &pos1, &cons);
	if (GetType(cons) != LISPTYPE_CONS)
		return 1;
	GetCons(cons, &pos2, &cons);
	if (cons != Nil)
		return 1;
	*ret1 = pos1;
	*ret2 = pos2;

	return 0;
}

static int key_name_values_(addr pos, addr *symbol, addr *name)
{
	if (GetType(pos) == LISPTYPE_CONS) {
		/* swap (name, symbol) -> (symbol, name). */
		if (list2_check(pos, name, symbol)) {
			*symbol = *name = NULL;
			return lambda_list_fmte_("The variable ~S "
					"must be a symbol or (name symbol) list.", pos, NULL);
		}
		return 0;
	}
	else {
		*symbol = pos;
		return make_keyword_from_symbol_(pos, name);
	}
}


/*
 *  lambda-macro
 */
static int push_varcons_macro_(LocalRoot local, addr instance, addr key)
{
	return push_varcons_(local, instance, key, AMPERSAND_MACRO);
}

#define nextcons_finish_rest(one, cons) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (! consp(cons)) goto finish_rest; \
	GetCar(cons, &one); \
}

#define nextcons_finish_error(one, cons, name) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (! consp(cons)) { \
		addr __symbol; \
		GetConst(AMPERSAND_##name, &__symbol); \
		return lambda_list_fmte_("After ~A " \
				"parameter don't allow a dot list.", __symbol, NULL); \
	} \
	GetCar(cons, &one); \
}

#define environment_expander(local, instance, envcheck, env, one, cons) { \
	if (! envcheck) { \
		return lambda_list_fmte_("&environment don't accept " \
				"at no top-level argument.", NULL); \
	} \
	if (env != Nil) { \
		return lambda_list_fmte_("&environment must be " \
				"at once in arguments.", NULL); \
	} \
	GetCdr(cons, &cons); \
	if (cons == Nil) { \
		return lambda_list_fmte_("&environment parameter " \
				"must be a one argument.", NULL); \
	} \
	if (! consp(cons)) { \
		return lambda_list_fmte_("After &environment " \
				"don't allow a dot list.", NULL); \
	} \
	GetCar(cons, &one); \
	Return(push_varcons_macro_(local, instance, one)); \
	env = one; \
}

static int ordinary_opt_default_(addr cons, addr init,
		addr *retvar, addr *retinit, addr *retsup)
{
	addr base, var, sup;

	base = cons;
	var = sup = Nil;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	/* (var init) */
	GetCons(cons, &init, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	/* (var init sup) */
	GetCons(cons, &sup, &cons);
	if (sup == Nil) {
		return lambda_list_fmte_("The supplied variable ~S "
				"must not be a constant symbol.", sup, NULL);
	}
	if (cons == Nil)
		goto final;
	/* (var init sup . error) */
	return lambda_list_fmte_("Too many argument ~S.", base, NULL);

final:
	*retvar = var;
	*retinit = init;
	*retsup = sup;
	return 0;
}

static int ordinary_opt_(addr cons, addr *retvar, addr *retinit, addr *retsup)
{
	return ordinary_opt_default_(cons, Nil, retvar, retinit, retsup);
}

static int ordinary_key_default_(LocalRoot local, addr name, addr cons, addr value,
		addr *retvar, addr *retname, addr *retinit, addr *retsup)
{
	Return(ordinary_opt_default_(cons, value, &cons, retinit, retsup));
	Return(key_name_values_(cons, retvar, retname));
	return push_namecons_(local, name, *retname);
}

static int ordinary_key_(LocalRoot local, addr name, addr cons,
		addr *retvar, addr *retname, addr *retinit, addr *retsup)
{
	return ordinary_key_default_(local,
			name, cons, Nil, retvar, retname, retinit, retsup);
}

static int ordinary_aux_(addr cons, addr *retvar, addr *retinit)
{
	addr base, var, init;

	base = cons;
	var = init = Nil;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	/* (var init) */
	GetCons(cons, &init, &cons);
	if (cons == Nil)
		goto final;
	/* (var init . error) */
	return lambda_list_fmte_("Too many argument ~S.", base, NULL);

final:
	*retvar = var;
	*retinit = init;
	return 0;
}

static int lambda_macro_call_(LocalRoot local,
		addr *ret, addr cons, addr instance, addr defvalue)
{
	int envcheck;
	addr name, var, opt, rest, key, key_p, allow, aux, one;
	addr whole, env, temp, symbol, init, sup;

	var = opt = rest = key = key_p = allow = aux = whole = env = Nil;
	envcheck = (instance == Nil);
	if (envcheck)
		varcons_local(local, &instance);
	varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (! consp(cons))
		goto finish_rest;
	GetCar(cons, &one);
	if (constant_eq(CONSTANT_AMPERSAND_WHOLE, one))
		goto whole_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

whole_argument:
	GetCdr(cons, &cons);
	if (cons == Nil) {
		return lambda_list_fmte_("&whole parameter must be a one argument.", NULL);
	}
	if (! consp(cons)) {
		return lambda_list_fmte_("After &whole parameter "
				"don't allow a dot list.", NULL);
	}
	GetCar(cons, &one);
	Return(push_varcons_macro_(local, instance, one));
	whole = one;
	nextcons_finish_rest(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

var_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);

var_argument:
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto optional_environment;
	if (listp(one)) {
		Return(lambda_macro_call_(local, &one, one, instance, defvalue));
	}
	else {
		Return(push_varcons_macro_(local, instance, one));
	}
	cons_heap(&var, one, var);
	nextcons_finish_rest(one, cons);
	goto var_argument;

optional_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

optional_argument:
	nextcons_finish_rest(one, cons);
optional_loop:
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto rest_environment;
	Return(ordinary_opt_default_(one, defvalue, &symbol, &init, &sup));
	Return(push_varcons_macro_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_macro_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish_rest(one, cons);
	goto optional_loop;

rest_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_rest(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_BODY, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (! consp(cons))
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &temp);
	Return(push_varcons_macro_(local, instance, temp));
	cons_heap(&rest, temp, one);

	nextcons_finish_error(one, cons, REST);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto key_environment;
	return lambda_list_fmte_("After &rest/&body ~S "
			"must be a null or &key arguments.", one, NULL);

key_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, REST);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

key_argument:
	key_p = T;
	nextcons_finish_error(one, cons, KEY);
key_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto aux_environment;
	Return(ordinary_key_default_(local,
				name, one, defvalue, &symbol, &one, &init, &sup));
	Return(push_varcons_macro_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_macro_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish_error(one, cons, KEY);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish_error(one, cons, ALLOW);
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto aux_environment;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

aux_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, KEY);
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

aux_argument:
	nextcons_finish_error(one, cons, AUX);
aux_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_varcons_macro_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish_error(one, cons, AUX);
	goto aux_loop;

finish_environment:
	environment_expander(local, instance, envcheck, env, one, cons);
	nextcons_finish_error(one, cons, AUX);
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish_rest:
	Return(push_varcons_macro_(local, instance, cons));
	conscar_heap(&rest, cons);
	goto finish;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt,
			rest,  /* (var . &rest) (var . &body) (var . nil) */
			(key != Nil? key: key_p), allow, aux, whole, env, NULL);
	return 0;
}

int lambda_macro_(LocalRoot local, addr *ret, addr cons, addr instance)
{
	return lambda_macro_call_(local, ret, cons, instance, Nil);
}


/*
 *  lambda-ordinary
 */
int lambda_deftype_(LocalRoot local, addr *ret, addr cons, addr instance)
{
	addr aster;
	GetConst(COMMON_ASTERISK, &aster);
	quotelist_heap(&aster, aster);
	return lambda_macro_call_(local, ret, cons, instance, aster);
}


/*
 *  lambda-ordinary
 */
static int push_varcons_ordinary_(LocalRoot local, addr instance, addr key)
{
	return push_varcons_(local, instance, key, AMPERSAND_ORDINARY);
}


/*
 *  lambda-generic-function
 */
static int generic_function_key_cons_(addr cons, addr *symbol, addr *name)
{
	addr var;

	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil) {
		goto final;
	}
	/* (var . error) */
	return lambda_list_fmte_("&key parameter ~S "
			"must be at many one arguments.", cons, NULL);

final:
	return key_name_values_(var, symbol, name);
}

static int generic_function_key_(LocalRoot local,
		addr instance, addr cons, addr *var, addr *name)
{
	Return(generic_function_key_cons_(cons, var, name));
	return push_namecons_(local, instance, *name);
}

static int push_varcons_generic_function_(LocalRoot local,
		addr instance, addr key)
{
	return push_varcons_(local, instance, key, AMPERSAND_GENERIC);
}

#define nextcons_finish(one, cons) { \
	GetCdr(cons, &cons); \
	if (cons == Nil) goto finish; \
	if (GetType(cons) != LISPTYPE_CONS) { \
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL); \
	} \
	GetCar(cons, &one); \
}

int lambda_generic_function_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, one, symbol;

	var = opt = rest = key = key_p = allow = one = symbol = Nil;
	varcons_local(local, &instance);
	varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &one);

var_argument:
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	Return(push_varcons_generic_function_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons);
optional_loop:
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (singlep(one))
		GetCar(one, &one);
	Return(push_varcons_generic_function_(local, instance, one));
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &rest);
	Return(push_varcons_generic_function_(local, instance, rest));
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons);
key_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	Return(generic_function_key_(local, name, one, &symbol, &one));
	Return(push_varcons_generic_function_(local, instance, symbol));
	list_heap(&one, symbol, one, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons);
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, NULL);
	return 0;
}

void atleast_argument_count(addr cons, size_t *ret)
{
	GetCar(cons, &cons);
	*ret = length_list_unsafe(cons);
}


/*
 *  lambda-specialized
 */
static int specialized_var_cons_(addr cons, addr *retvar, addr *retspec)
{
	addr var, spec;

	spec = T;
	/* var */
	if (GetType(cons) != LISPTYPE_CONS) {
		var = cons;
		goto final;
	}
	/* (var) */
	GetCons(cons, &var, &cons);
	if (cons == Nil)
		goto final;
	if (GetType(cons) != LISPTYPE_CONS) {
		return lambda_list_fmte_("The dot list don't allow "
				"in the argument ~S.", cons, NULL);
	}
	/* (var specializer) */
	GetCons(cons, &spec, &cons);
	if (cons == Nil)
		goto final;
	/* (var specializer . error) */
	return lambda_list_fmte_("The variable parameter "
			"must be at many two arguments.", NULL);

final:
	*retvar = var;
	*retspec = spec;
	return 0;
}

static int check_specializer_form(addr spec)
{
	enum LISPTYPE type;
	addr left, check;

	/* symbol */
	if (spec == Nil || spec == T)
		return 1;
	type = GetType(spec);
	if (type == LISPTYPE_SYMBOL)
		return 1;
	/* (eql spec) */
	if (type != LISPTYPE_CONS)
		return 0;
	GetCons(spec, &left, &spec);
	GetConst(COMMON_EQL, &check);
	if (left != check)
		return 0;
	if (GetType(spec) != LISPTYPE_CONS)
		return 0;
	GetCdr(spec, &spec);

	return spec == Nil;
}

static int specialized_var_(addr cons, addr *var, addr *spec)
{
	Return(specialized_var_cons_(cons, var, spec));
	if (! check_specializer_form(*spec)) {
		return lambda_list_fmte_("The parameter ~S "
				"don't allow a specializer form.", *spec, NULL);
	}

	return 0;
}

int lambda_specialized_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, aux, one;
	addr symbol, init, sup;

	var = opt = rest = key = key_p = allow = aux = Nil;
	varcons_local(local, &instance);
	varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &one);

var_argument:
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(specialized_var_(one, &symbol, &one));
	Return(push_varcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, one, NULL);
	cons_heap(&var, one, var);
	nextcons_finish(one, cons);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons);
optional_loop:
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_opt_(one, &symbol, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &rest);
	Return(push_varcons_ordinary_(local, instance, rest));
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons);
key_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_key_(local, name, one, &symbol, &one, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

aux_argument:
	nextcons_finish(one, cons);
aux_loop:
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_varcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish(one, cons);
	goto aux_loop;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, aux, NULL);
	return 0;
}


/*
 *  lambda_ordinary
 */
static int lambda_ordinary_g_(LocalRoot local, addr *ret, addr cons, addr g)
{
	addr instance, name, var, opt, rest, key, key_p, allow, aux, one;
	addr symbol, init, sup;

	var = opt = rest = key = key_p = allow = aux = Nil;
	varcons_local(local, &instance);
	varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &one);

var_argument:
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(push_varcons_ordinary_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons);
	goto var_argument;

optional_argument:
	nextcons_finish(one, cons);
optional_loop:
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_opt_default_(one, g, &symbol, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons);
	goto optional_loop;

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (GetType(cons) != LISPTYPE_CONS)
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &rest);
	Return(push_varcons_ordinary_(local, instance, rest));
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &rest ~S "
			"must be a null or &key arguments.", cons, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons);
key_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	Return(ordinary_key_default_(local, name, one, g, &symbol, &one, &init, &sup));
	Return(push_varcons_ordinary_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_ordinary_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_AUX, one))
		goto aux_argument;
	return lambda_list_fmte_("After &allow-other-keys ~S must be a null.", cons, NULL);

aux_argument:
	nextcons_finish(one, cons);
aux_loop:
	Return(ordinary_aux_(one, &symbol, &init));
	Return(push_varcons_ordinary_(local, instance, symbol));
	list_heap(&one, symbol, init, NULL);
	cons_heap(&aux, one, aux);
	nextcons_finish(one, cons);
	goto aux_loop;

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	nreverse(&aux, aux);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, aux, NULL);
	return 0;
}

int lambda_ordinary_(LocalRoot local, addr *ret, addr cons)
{
	return lambda_ordinary_g_(local, ret, cons, Nil);
}


/*
 *  lambda_defsetf
 */
static int variable_check_defsetf_(addr var)
{
	if (var == Nil || var == T) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (! symbolp(var)) {
		return lambda_list_fmte_("The variable ~S is not a symbol.", var, NULL);
	}
	if (GetStatusReadOnly(var)) {
		return lambda_list_fmte_("The symbol ~S "
				"must not be a constant symbol.", var, NULL);
	}
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, var))
		return 0;
	if (constant_eq(CONSTANT_AMPERSAND_REST, var))
		return 0;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, var))
		return 0;
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, var))
		return 0;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, var))
		return 0;
	if (constant_eq(CONSTANT_AMPERSAND_AUX, var))
		goto error;
	if (constant_eq(CONSTANT_AMPERSAND_WHOLE, var))
		goto error;
	if (constant_eq(CONSTANT_AMPERSAND_BODY, var))
		goto error;
	return 0;

error:
	return lambda_list_fmte_("The symbol ~S don't use in defsetf.", var, NULL);
}

static int push_varcons_defsetf_(LocalRoot local, addr instance, addr var)
{
	addr data;

	Return(variable_check_defsetf_(var));
	varcons_data(instance, &data);
	if (find_list_eq_unsafe(var, data))
		return lambda_list_fmte_("The variable ~S is already used.", var, NULL);
	pushqueue_local(local, instance, var);

	return 0;
}

#define environment_expander_defsetf(local, instance, env, one, cons) { \
	if (env != Nil) { \
		return lambda_list_fmte_("&environment must be at once in arguments.", NULL); \
	} \
	GetCdr(cons, &cons); \
	if (cons == Nil) { \
		return lambda_list_fmte_("&environment parameter must be a one argument.", NULL); \
	} \
	if (! consp(cons)) { \
		return lambda_list_fmte_("After &environment don't allow a dot list.", NULL); \
	} \
	GetCar(cons, &one); \
	Return(push_varcons_defsetf_(local, instance, one)); \
	env = one; \
}

int lambda_defsetf_(LocalRoot local, addr *ret, addr cons)
{
	addr instance, name, var, opt, rest, key, key_p, allow, one;
	addr env, symbol, init, sup;

	var = opt = rest = key = key_p = allow = env = Nil;
	varcons_local(local, &instance);
	varcons_local(local, &name);
	if (cons == Nil)
		goto finish;
	if (! consp(cons))
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &one);
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto var_environment;
	goto var_argument;

var_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons);

var_argument:
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto optional_environment;
	Return(push_varcons_defsetf_(local, instance, one));
	cons_heap(&var, one, var);
	nextcons_finish(one, cons);
	goto var_argument;

optional_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_OPTIONAL, one))
		goto optional_argument;
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

optional_argument:
	nextcons_finish(one, cons);
optional_loop:
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto rest_environment;
	Return(ordinary_opt_(one, &symbol, &init, &sup));
	Return(push_varcons_defsetf_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_defsetf_(local, instance, sup));
	}
	list_heap(&one, symbol, init, sup, NULL);
	cons_heap(&opt, one, opt);
	nextcons_finish(one, cons);
	goto optional_loop;

rest_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_REST, one))
		goto rest_argument;
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

rest_argument:
	GetCdr(cons, &cons);
	if (cons == Nil)
		return lambda_list_fmte_("&rest parameter must be a one argument.", NULL);
	if (! consp(cons))
		return lambda_list_fmte_("Dot list don't allow in this lambda-list.", NULL);
	GetCar(cons, &rest);
	Return(push_varcons_defsetf_(local, instance, rest));
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto key_environment;
	return lambda_list_fmte_("After &rest/&body ~S "
			"must be a null or &key arguments.", one, NULL);

key_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_KEY, one))
		goto key_argument;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

key_argument:
	key_p = T;
	nextcons_finish(one, cons);
key_loop:
	if (constant_eq(CONSTANT_AMPERSAND_ALLOW, one))
		goto allow_argument;
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	Return(ordinary_key_(local, name, one, &symbol, &one, &init, &sup));
	Return(push_varcons_defsetf_(local, instance, symbol));
	if (sup != Nil) {
		Return(push_varcons_defsetf_(local, instance, sup));
	}
	list_heap(&one, symbol, one, init, sup, NULL);
	cons_heap(&key, one, key);
	nextcons_finish(one, cons);
	goto key_loop;

allow_argument:
	allow = T;
	nextcons_finish(one, cons);
	if (constant_eq(CONSTANT_AMPERSAND_ENVIRONMENT, one))
		goto finish_environment;
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish_environment:
	environment_expander_defsetf(local, instance, env, one, cons);
	nextcons_finish(one, cons);
	return lambda_list_fmte_("Invalid ~A argument.", one, NULL);

finish:
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	list_heap(ret, var, opt, rest, (key != Nil? key: key_p), allow, env, NULL);
	return 0;
}

void getenvironment_macro_lambda(addr pos, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole;
	List_bind(pos, &var, &opt, &rest, &key, &allow, &aux, &whole, ret, NULL);
}

int define_modify_macro_heap_(LocalRoot local, addr *ret, addr *rest, addr list)
{
	addr pos, root, x;
	struct argument_struct *str;

	Return(argument_ordinary_heap_(local, &pos, list));
	str = ArgumentStruct(pos);
	if (str->keyp) {
		return lambda_list_fmte_("Don't use &key arguments "
				"in define-modify-macro lambda list ~S.", list, NULL);
	}
	if (str->aux) {
		return lambda_list_fmte_("Don't use &aux arguments "
				"in define-modify-macro lambda list ~S.", list, NULL);
	}

	/* var */
	root = Nil;
	GetArgument(pos, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		cons_heap(&root, x, root);
	}

	/* opt */
	GetArgument(pos, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCar(x, &x);
		cons_heap(&root, x, root);
	}
	nreverse(ret, root);

	/* rest */
	GetArgument(pos, ArgumentIndex_rest, rest);

	return 0;
}


/*
 *  argument
 */
int argumentp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_ARGUMENT;
}

void getargument(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	GetArgument_Low(pos, index, ret);
}

void setargument(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	SetArgument_Low(pos, index, value);
}

struct argument_struct *argumentstruct(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARGUMENT);
	return ArgumentStruct_Low(pos);
}

void argument_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct argument_struct *str;

	alloc_smallsize(local, &pos, LISPSYSTEM_ARGUMENT,
			ArgumentIndex_size, sizeoft(struct argument_struct));
	str = ArgumentStruct(pos);
	clearpoint(str);
	*ret = pos;
}

void argument_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	argument_alloc(local, ret);
}

void argument_heap(addr *ret)
{
	argument_alloc(NULL, ret);
}

static int argument_ordinary_heap_g_(LocalRoot local, addr *ret, addr list, addr g)
{
	addr pos, var, opt, rest, key, allow, aux;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_ordinary_g_(local, &list, list, g));
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_ordinary;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* aux */
	str->aux = length_list_unsafe(aux);
	SetArgument(pos, ArgumentIndex_aux, aux);
	/* result */
	return Result(ret, pos);
}

int argument_ordinary_heap_(LocalRoot local, addr *ret, addr list)
{
	return argument_ordinary_heap_g_(local, ret, list, Nil);
}

int argument_generic_heap_(LocalRoot local, addr *ret, addr list)
{
	addr pos, var, opt, rest, key, allow;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_generic_function_(local, &list, list));
	List_bind(list, &var, &opt, &rest, &key, &allow, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key == T) {
		str->keyp = 1;
	}
	else if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* result */
	return Result(ret, pos);
}

int argument_method_heap_(LocalRoot local, addr *ret, addr list)
{
	addr pos, var, opt, rest, key, allow, aux;
	struct argument_struct *str;

	CheckLocal(local);
	/* parse */
	Return(lambda_specialized_(local, &list, list));
	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = length_list_unsafe(var);
	SetArgument(pos, ArgumentIndex_var, var);
	/* opt */
	str->opt = length_list_unsafe(opt);
	SetArgument(pos, ArgumentIndex_opt, opt);
	/* rest */
	if (rest != Nil) {
		str->rest = 1;
		str->restbody = 1;
		SetArgument(pos, ArgumentIndex_rest, rest);
		SetArgument(pos, ArgumentIndex_restbody, rest);
	}
	/* key */
	if (key == T) {
		str->keyp = 1;
	}
	else if (key != Nil) {
		str->keyp = 1;
		str->key = length_list_unsafe(key);
		SetArgument(pos, ArgumentIndex_key, key);
	}
	/* allow-other-keys */
	if (allow != Nil) {
		str->allow = 1;
	}
	/* aux */
	str->aux = length_list_unsafe(aux);
	SetArgument(pos, ArgumentIndex_aux, aux);
	/* result */
	return Result(ret, pos);
}

int argument_combination_heap_(LocalRoot local, addr *ret, addr list)
{
	addr whole, a, b, check;
	struct argument_struct *str;

	whole = Nil;
	if (! consp(list))
		goto ordinary;
	GetCons(list, &a, &b);
	GetConst(AMPERSAND_WHOLE, &check);
	if (a != check)
		goto ordinary;
	if (! consp(b))
		return lambda_list_fmte_("There is no variable after &whole.", NULL);
	GetCons(b, &whole, &list);
	Return(variable_check_(whole, AMPERSAND_ORDINARY));

ordinary:
	Return(argument_ordinary_heap_(local, &list, list));
	str = ArgumentStruct(list);
	str->type = ArgumentType_combination;
	if (whole != Nil) {
		str->whole = 1;
		SetArgument(list, ArgumentIndex_whole, whole);
	}
	return Result(ret, list);
}

int argument_boa_heap_(LocalRoot local, addr *ret, addr list, addr g)
{
	struct argument_struct *str;

	Return(argument_ordinary_heap_g_(local, &list, list, g));
	str = ArgumentStruct(list);
	str->type = ArgumentType_boa;
	return Result(ret, list);
}


/*
 *  expand
 */
int argument_generic_lambda_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	Check(str->type != ArgumentType_generic, "type error");
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_rest, &list);
		cons_heap(&root, list, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &list);
		cons_heap(&root, list, root);
	}
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, NULL));
			list_heap(&var, b, a, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* allow-other-keys */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &list);
		cons_heap(&root, list, root);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int argument_expand_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b, c, d;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			if (str->type == ArgumentType_method) {
				GetCar(var, &var);
			}
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, NULL));
			if (c == Nil)
				list_heap(&var, a, b, NULL);
			else
				list_heap(&var, a, b, c, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_rest, &list);
		cons_heap(&root, list, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &list);
		cons_heap(&root, list, root);
	}
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, &d, NULL));
			list_heap(&a, b, a, NULL);
			if (d == Nil)
				list_heap(&var, a, c, NULL);
			else
				list_heap(&var, a, c, d, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* allow-other-keys */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &list);
		cons_heap(&root, list, root);
	}

	/* aux */
	if (str->aux) {
		GetConst(AMPERSAND_AUX, &list);
		cons_heap(&root, list, root);
		GetArgument(pos, ArgumentIndex_aux, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			cons_heap(&root, var, root);
		}
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

int argument_ordinary_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_ordinary, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_method_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_method, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_method_keywords_heap_(addr pos, addr *ret, int *allow)
{
	addr root, list, key;
	struct argument_struct *str;

	str = ArgumentStruct(pos);
	Check(str->type != ArgumentType_method, "type error");
	/* key */
	GetArgument(pos, ArgumentIndex_key, &list);
	for (root = Nil; list != Nil; ) {
		/* (var name init sup) */
		Return_getcons(list, &key, &list);
		Return_getcdr(key, &key);
		Return_getcar(key, &key);
		cons_heap(&root, key, root);
	}
	/* result */
	nreverse(ret, root);
	*allow = (int)str->allow;
	return 0;
}

void argument_method_to_generic(addr method, addr *ret)
{
	addr pos, list, root, var;
	struct argument_struct *str1, *str2;

	/* method */
	str1 = ArgumentStruct(method);
	Check(str1->type != ArgumentType_method, "type error");

	/* generic */
	argument_heap(&pos);
	str2 = ArgumentStruct(pos);
	str2->type = ArgumentType_generic;
	/* var */
	str2->var = str1->var;
	GetArgument(method, ArgumentIndex_var, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_var, root);
	/* opt */
	str2->opt = str1->opt;
	GetArgument(method, ArgumentIndex_opt, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_opt, root);
	/* rest */
	str2->rest = str1->rest;
	str2->restbody = str1->restbody;
	GetArgument(method, ArgumentIndex_rest, &list);
	SetArgument(pos, ArgumentIndex_rest, list);
	/* key */
	str2->key = str1->key;
	str2->keyp = str1->keyp;
	GetArgument(method, ArgumentIndex_key, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_key, root);
	/* aux */
	str2->aux = str1->aux;
	GetArgument(method, ArgumentIndex_aux, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}
	SetArgument(pos, ArgumentIndex_aux, root);
	/* result */
	*ret = pos;
}

int argument_boa_lambda_heap_(addr *ret, addr pos)
{
	Check(ArgumentStruct(pos)->type != ArgumentType_boa, "type error");
	return argument_expand_heap_(ret, pos);
}

int argument_boa_variables_heap_(addr *ret, addr pos)
{
	addr root, list, var, a, b, c, d;
	struct argument_struct *str;

	root = Nil;
	str = ArgumentStruct(pos);
	/* var */
	if (str->var) {
		GetArgument(pos, ArgumentIndex_var, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			if (str->type == ArgumentType_method) {
				GetCar(var, &var);
			}
			cons_heap(&root, var, root);
		}
	}

	/* opt */
	if (str->opt) {
		GetArgument(pos, ArgumentIndex_opt, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, NULL));
			cons_heap(&root, a, root);
		}
	}

	/* rest */
	if (str->rest) {
		GetArgument(pos, ArgumentIndex_rest, &a);
		cons_heap(&root, a, root);
	}

	/* key */
	if (str->key) {
		GetArgument(pos, ArgumentIndex_key, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			Return(list_bind_(var, &a, &b, &c, &d, NULL));
			cons_heap(&root, a, root);
		}
	}

	/* aux */
	if (str->aux) {
		GetArgument(pos, ArgumentIndex_aux, &list);
		while (list != Nil) {
			GetCons(list, &var, &list);
			GetCar(var, &a);
			cons_heap(&root, a, root);
		}
	}

	/* result */
	nreverse(ret, root);
	return 0;
}


/*
 *  :allow-other-keys
 */
int find_keyword_allow_other_keys(addr list)
{
	addr pos;
	GetConst(KEYWORD_ALLOW_OTHER_KEYS, &pos);
	return getplist_safe(list, pos, &pos) == 0 && pos != Nil;
}

