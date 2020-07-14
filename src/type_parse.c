#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "copy.h"
#include "function.h"
#include "heap.h"
#include "hold.h"
#include "integer.h"
#include "memory.h"
#include "rational.h"
#include "real.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_deftype.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"
#include "type_upgraded.h"

static int localhold_parse_type(LocalHold hold,
		Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type(ptr, ret, pos, env));
	localhold_push(hold, *ret);
	return 0;
}


/*
 *  and/or
 */
static int typelist_array4(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, aster, list, array;
	size_t size;
	LocalHold hold;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0, list = right; list != Nil; size++) {
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &pos, &list);
		if (pos == aster)
			return fmte_("~A arguments don't use *.", left, NULL);
	}
	if (0xFFFFFFFFUL < size)
		return fmte_("~A arguments S~ too long.", left, right, NULL);

	vector4_heap(&array, size);
	hold = LocalHold_local_push(ptr, array);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &pos, &right);
		Return(parse_type(ptr, &pos, pos, env));
		SetArrayA4(array, size, pos);
	}
	localhold_end(hold);
	type1_heap(type, array, ret);

	return 0;
}


/*
 *  eql
 */
static int typelist_eql(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A type must be a one argument ~S.", left, right, NULL);
	copyheap(&pos, pos);
	type_eql_heap(pos, ret);

	return 0;
}


/*
 *  member
 */
static int typelist_member(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	size_t size;
	addr pos, array;

	for (size = 0, pos = right; pos != Nil; size++) {
		if (! consp(pos))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCdr(pos, &pos);
	}
	if (0xFFFFFFFFUL < size)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	vector4_heap(&array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &pos, &right);
		copyheap(&pos, pos);
		SetArrayA4(array, size, pos);
	}
	type1_heap(type, array, ret);

	return 0;
}


/*
 *  mod
 */
static int typelist_mod(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one integer.", right, NULL);
	if (! integerp(pos))
		return fmte_("~A argument ~S must be an integer type.", left, pos, NULL);
	if (! plusp_integer(pos))
		return fmte_("~A argument ~S must be a plus integer.", left, pos, NULL);
	copyheap(&pos, pos);
	type1_heap(type, pos, ret);

	return 0;
}


/*
 *  not
 */
static int typelist_not(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one argument.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &list);
	if (pos == list)
		return fmte_("~A argument don't be *.", left, NULL);
	Return(parse_type(ptr, &pos, pos, env));
	type1_heap(type, pos, ret);

	return 0;
}


/*
 *  satisfies
 */
static int typelist_satisfies(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr pos, list;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &pos, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S must be one symbol.", left, right, NULL);
	if (! symbolp(pos))
		return fmte_("~A argument ~S must be a symbol.", left, pos, NULL);
	copyheap(&pos, pos);
	type_satisfies_heap(pos, ret);

	return 0;
}


/*
 *  cons
 */
static int typelist_cons(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, car, cdr, aster;
	LocalHold hold;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &car, &list);
	GetConst(COMMON_ASTERISK, &aster);
	hold = LocalHold_local(ptr);
	if (list == Nil) {
		/* one argument */
		if (car == aster)
			goto asterisk;
		Return(localhold_parse_type(hold, ptr, &car, car, env));
		GetTypeTable(&cdr, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &cdr, &list);
		if (list != Nil) {
			return fmte_("~A arguments ~S must have 1 or 2 arguments.",
					left, right, NULL);
		}
		if (car == aster && cdr == aster)
			goto asterisk;
		Return(localhold_parse_type(hold, ptr, &car, car, env));
		Return(localhold_parse_type(hold, ptr, &cdr, cdr, env));
	}
	localhold_end(hold);
	type2_heap(type, car, cdr, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  function
 */
static int type_function_lambda(Execute ptr, addr *ret, addr list, addr env)
{
	addr const_opt, const_rest, const_key;
	addr var, opt, rest, key, one, name, type;
	LocalHold hold;

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
	GetConst(AMPERSAND_KEY, &const_key);
	var = opt = rest = key = one = Nil;
	hold = LocalHold_array(ptr, 5);

var_label:
	if (list == Nil)
		goto final;
	getcons(list, &one, &list);
	if (one == const_opt)
		goto opt_label;
	if (one == const_rest)
		goto rest_label;
	if (one == const_key)
		goto key_label;
	Return(parse_type(ptr, &one, one, env));
	cons_heap(&var, one, var);
	localhold_set(hold, 0, var);
	goto var_label;

opt_label:
	if (list == Nil)
		goto final;
	getcons(list, &one, &list);
	if (one == const_opt)
		return fmte_("&optional parameter don't allow this place.", NULL);
	if (one == const_rest)
		goto rest_label;
	if (one == const_key)
		goto key_label;
	Return(parse_type(ptr, &one, one, env));
	cons_heap(&opt, one, opt);
	localhold_set(hold, 1, opt);
	goto opt_label;

rest_label:
	if (list == Nil)
		return fmte_("After &rest parameter must be have a typespec.", NULL);
	getcons(list, &one, &list);
	if (one == const_opt || one == const_rest || one == const_key)
		return fmte_("After &rest parameter don't allow to be a &-symbol.", NULL);
	Return(parse_type(ptr, &rest, one, env));
	localhold_set(hold, 2, rest);
	if (list == Nil)
		goto final;
	getcons(list, &one, &list);
	if (one != const_key)
		return fmte_("After &rest argument don't allow to be a type.", NULL);
	goto key_label;

key_label:
	if (list == Nil)
		goto final;
	getcons(list, &one, &list);
	if (one == const_opt || one == const_rest || one == const_key)
		return fmte_("After &key parameter don't allow to be a &-symbol.", NULL);
	if (! consp(one))
		return fmte_("After &key parameter must be a list.", NULL);
	getcons(one, &name, &one);
	copyheap(&name, name);
	localhold_set(hold, 3, name);
	getcons(one, &type, &one);
	if (one != Nil)
		return fmte_("&key parameter must be a (key type) list.", NULL);
	Return(parse_type(ptr, &type, type, env));
	cons_heap(&one, name, type);
	cons_heap(&key, one, key);
	localhold_set(hold, 4, key);
	goto key_label;

final:
	localhold_end(hold);
	vector2_heap(&one, 4);
	nreverse(&var, var);
	nreverse(&opt, opt);
	nreverse(&key, key);
	SetArrayA2(one, 0, var);
	SetArrayA2(one, 1, opt);
	SetArrayA2(one, 2, rest);
	SetArrayA2(one, 3, key);
	return Result(ret, one);
}

static int type_function_list(Execute ptr, addr *ret, addr right, addr env)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	CheckType(aster, LISPTYPE_SYMBOL);
	if (right == aster) {
		GetTypeTable(ret, Asterisk);
		return 0;
	}

	return type_function_lambda(ptr, ret, right, env);
}

/*  typespec* [&optional typespec*] [&rest typespec]
 *  typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys]
 */
#undef LISP_VALUES_ALLOW_ENABLE
static int type_values_typespec(Execute ptr, addr list, addr env,
		addr *retvar, addr *retopt, addr *retrest, addr *retallow)
{
	addr var, vars, opt, rest, allow;
	addr const_opt, const_rest;
#ifdef LISP_VALUES_ALLOW_ENABLE
	addr const_allow;
#endif
	LocalHold hold;

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
#ifdef LISP_VALUES_ALLOW_ENABLE
	GetConst(AMPERSAND_ALLOW, &const_allow);
#endif
	vars = opt = rest = allow = Nil;
	hold = LocalHold_array(ptr, 3);

var_label:
	if (list == Nil)
		goto final;
	getcons(list, &var, &list);
	if (var == const_opt)
		goto optional_label;
	if (var == const_rest)
		goto rest_label;
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	Return(parse_type(ptr, &var, var, env));
	cons_heap(&vars, var, vars);
	localhold_set(hold, 0, vars);
	goto var_label;

optional_label:
	if (list == Nil)
		goto final;
	getcons(list, &var, &list);
	if (var == const_rest)
		goto rest_label;
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	Return(parse_type(ptr, &var, var, env));
	cons_heap(&opt, var, opt);
	localhold_set(hold, 1, opt);
	goto optional_label;

rest_label:
	if (list == Nil)
		return fmte_("After &rest argument must be a type.", NULL);
	getcons(list, &var, &list);
	if (var == const_opt || var == const_rest)
		return fmte_("After &rest argument must be a type.", NULL);
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		return fmte_("After &rest argument must be a type.", NULL);
#endif
	Return(parse_type(ptr, &rest, var, env));
	localhold_set(hold, 2, rest);
	if (list == Nil)
		goto final;
	getcons(list, &var, &list);
#ifdef LISP_VALUES_ALLOW_ENABLE
	if (var == const_allow)
		goto allow_label;
#endif
	return fmte_("Invalid values form.", NULL);

#ifdef LISP_VALUES_ALLOW_ENABLE
allow_label:
	allow = T;
	if (list != Nil)
		return fmte_("After &allow-other-keys must be nil.", NULL);
	goto final;
#endif

final:
	localhold_end(hold);
	nreverse(retvar, vars);
	nreverse(retopt, opt);
	*retrest = rest;
	*retallow = allow;

	return 0;
}

static int type_values(Execute ptr, addr *ret, addr right, addr env)
{
	addr var, opt, rest, allow;

	var = opt = rest = allow = Nil;
	Return(type_values_typespec(ptr, right, env, &var, &opt, &rest, &allow));
	if (rest == Nil)
		GetTypeTable(&rest, T);
	type_values_heap(var, opt, rest, allow, ret);

	return 0;
}

static int type_function_values(Execute ptr, addr *ret, addr type, addr env)
{
	addr pos, check, list;

	if (! consp(type))
		return parse_type(ptr, ret, type, env);
	getcons(type, &pos, &list);
	GetConst(COMMON_VALUES, &check);
	if (check != pos)
		return parse_type(ptr, ret, type, env);
	else
		return type_values(ptr, ret, list, env);
}

static int typelist_function(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;
	LocalHold hold;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(type_function_list(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil) {
			return fmte_("~A arguments ~S must have 1 or 2 arguments.",
					left, right, NULL);
		}
		if (first == aster && second == aster)
			goto asterisk;
		Return(type_function_list(ptr, &first, first, env));
		hold = LocalHold_local_push(ptr, first);
		Return(type_function_values(ptr, &second, second, env));
		localhold_end(hold);
	}
	type3_heap(type, first, second, Nil, ret);
	return 0;

asterisk:
	GetTypeTable(&aster, Asterisk);
	type3_heap(type, aster, aster, Nil, ret);
	return 0;
}


/*
 *  array
 */
static int parse_array_length(addr right, size_t *rsize, int *ret)
{
	addr aster, left;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0; right != Nil; size++) {
		if (! consp(right)) {
			*ret = 0;
			*rsize = 0;
			return fmte_("The dimension parameter ~S must be a list.", right, NULL);
		}
		GetCons(right, &left, &right);
		if (left != aster)
			return Result(ret, 0);
	}
	*rsize = size;
	return Result(ret, 1);
}

static int parse_array_fixnum_check(addr *ret, addr pos)
{
	if ((! fixnump(pos)) || RefFixnum(pos) < 0) {
		return fmte_("The dimension value ~S "
				"must be a non-negative fixnum.", pos, NULL);
	}
	copyheap(ret, pos);
	return 0;
}

static int parse_array_dimension(addr *ret, addr right)
{
	addr aster, left, array;
	size_t size;

	/* length */
	for (size = 0, left = right; left != Nil; size++) {
		if (! consp(left))
			return fmte_("The dimension parameter ~S must be a list.", left, NULL);
		GetCdr(left, &left);
	}

	/* make vector */
	GetConst(COMMON_ASTERISK, &aster);
	vector4_heap(&array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &left, &right);
		if (left == aster) {
			GetTypeTable(&left, Asterisk);
		}
		else {
			Return(parse_array_fixnum_check(&left, left));
		}
		SetArrayA4(array, size, left);
	}

	return Result(ret, array);
}

static int parse_array_second(addr *ret, addr right)
{
	int check;
	addr aster;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		/* dimension arguments, 0 */
		fixnum_heap(ret, 0);
	}
	else if (consp(right)) {
		/* dimension arguments */
		Return(parse_array_length(right, &size, &check));
		if (check) {
			if (FIXNUM_MAX < size)
				return fmte_("size overflow.", NULL);
			fixnum_heap(ret, (fixnum)size);
		}
		else {
			Return(parse_array_dimension(ret, right));
		}
	}
	else {
		/* finxum arguments */
		Return(parse_array_fixnum_check(ret, right));
	}

	return 0;
}

static int typelist_array(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(parse_type(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil)
			return fmte_("~A type arguments too long.", left, NULL);
		if (first == aster && second == aster)
			goto asterisk;
		if (first == aster) {
			GetTypeTable(&first, Asterisk);
		}
		else {
			Return(parse_type(ptr, &first, first, env));
		}
		if (second == aster) {
			GetTypeTable(&second, Asterisk);
		}
		else {
			Return(parse_array_second(&second, second));
		}
	}
	if (! type_asterisk_p(first))
		upgraded_array_type(first, &first);
	type2_heap(type, first, second, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  vector
 */
static int typelist_vector(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first, second;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first == aster)
			goto asterisk;
		Return(parse_type(ptr, &first, first, env));
		GetTypeTable(&second, Asterisk);
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second, &list);
		if (list != Nil)
			return fmte_("~A arguments ~S too long.", left, right, NULL);
		if (first == aster && second == aster)
			goto asterisk;
		if (first == aster) {
			GetTypeTable(&first, Asterisk);
		}
		else {
			Return(parse_type(ptr, &first, first, env));
		}
		if (second == aster) {
			GetTypeTable(&second, Asterisk);
		}
		else {
			Return(parse_array_fixnum_check(&second, second));
		}
	}
	if (! type_asterisk_p(first))
		upgraded_array_type(first, &first);
	type2_heap(type, first, second, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  size
 */
static int typelist_size(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(parse_array_fixnum_check(&first, first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  range
 */
static int type_range_element(addr left, addr right, int (*call)(addr),
		addr *ret1, addr *ret2)
{
	addr pos, list;

	if (consp(right)) {
		GetCons(right, &pos, &list);
		if (list != Nil) {
			return fmte_("~A argument ~S "
					"must be a real or (real) form.", left, right, NULL);
		}
		Return((*call)(pos));
		*ret1 = T;
		copyheap(ret2, pos);
	}
	else {
		Return((*call)(right));
		*ret1 = Nil;
		copyheap(ret2, right);
	}

	return 0;
}

/* (integer 10 (20))  -> (integer nil 10 t 20) */
static int typelist_range(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env,
		int (*call)(addr))
{
	addr list, aster, first1, first2, second1, second2;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first1, &list);
	GetConst(COMMON_ASTERISK, &aster);
	if (list == Nil) {
		/* one argument */
		if (first1 == aster)
			goto asterisk;
		Return(type_range_element(left, first1, call, &first1, &first2));
		GetTypeTable(&second1, Asterisk);
		second2 = second1;
	}
	else {
		/* two arguments */
		if (! consp(list))
			return fmte_("Invalid ~A form ~S.", left, right, NULL);
		GetCons(list, &second1, &list);
		if (list != Nil)
			return fmte_("~A arguments ~S too long.", left, right, NULL);
		if (first1 == aster && second1 == aster)
			goto asterisk;
		if (first1 == aster) {
			GetTypeTable(&first1, Asterisk);
			first2 = first1;
		}
		else {
			Return(type_range_element(left, first1, call, &first1, &first2));
		}
		if (second1 == aster) {
			GetTypeTable(&second1, Asterisk);
			second2 = second1;
		}
		else {
			Return(type_range_element(left, second1, call, &second1, &second2));
		}
	}
	type4_heap(type, first1, first2, second1, second2, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  real
 */
static int typelist_real_p(addr pos)
{
	if (! realp(pos))
		return fmte_("REAL argument ~S must be a real.", pos, NULL);
	return 0;
}
static int typelist_real(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_real_p);
}


/*
 *  rational
 */
static int typelist_rational_p(addr pos)
{
	if (! rationalp(pos))
		return fmte_("RATIONAL argument ~S must be a rational.", pos, NULL);
	return 0;
}
static int typelist_rational(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_rational_p);
}


/*
 *  integer
 */
static int typelist_integer_p(addr pos)
{
	if (! integerp(pos))
		return fmte_("INTEGER argument ~S must be an integer.", pos, NULL);
	return 0;
}
static int typelist_integer(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_integer_p);
}


/*
 *  float
 */
static int typelist_float_p(addr pos)
{
	if (! floatp(pos))
		return fmte_("FLOAT argument ~S must be a float.", pos, NULL);
	return 0;
}
static int typelist_float(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_float_p);
}


/*
 *  short-float
 */
static int typelist_short_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_SHORT_FLOAT)
		return fmte_("SHORT-FLOAT argument ~S must be a short-float.", pos, NULL);
	return 0;
}
static int typelist_short(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_short_float_p);
}


/*
 *  single-float
 */
static int typelist_single_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_SINGLE_FLOAT)
		return fmte_("SINGLE-FLOAT argument ~S must be a single-float.", pos, NULL);
	return 0;
}
static int typelist_single(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_single_float_p);
}


/*
 *  double-float
 */
static int typelist_double_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_DOUBLE_FLOAT)
		return fmte_("DOUBLE-FLOAT argument ~S must be a double-float.", pos, NULL);
	return 0;
}
static int typelist_double(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_double_float_p);
}


/*
 *  long-float
 */
static int typelist_long_float_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_LONG_FLOAT)
		return fmte_("LONG-FLOAT argument ~S must be a long-float.", pos, NULL);
	return 0;
}
static int typelist_long(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	return typelist_range(ptr, ret, type, left, right, env, typelist_long_float_p);
}


/*
 *  byte
 */
static int type_byte_integer_check(addr *ret, addr pos)
{
	if (GetType(pos) != LISPTYPE_FIXNUM || (! plusp_integer(pos)))
		return fmte_("The imension value ~S must be a positive integer.", pos, NULL);
	copyheap(ret, pos);
	return 0;
}

static int typelist_byte(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(type_byte_integer_check(&first, first));
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  complex
 */
static int typelist_complex(Execute ptr, addr *ret,
		enum LISPDECL type, addr left, addr right, addr env)
{
	addr list, aster, first;

	/* no arguments */
	if (right == Nil)
		goto asterisk;

	/* one argument */
	if (! consp(right))
		return fmte_("Invalid ~A form ~S.", left, right, NULL);
	GetCons(right, &first, &list);
	if (list != Nil)
		return fmte_("~A arguments ~S too long.", left, right, NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster)
		goto asterisk;
	Return(parse_type(ptr, &first, first, env));
	upgraded_complex_type(first, &first);
	type1_heap(type, first, ret);
	return 0;

asterisk:
	return parse_type(ptr, ret, left, env);
}


/*
 *  parse-type
 */
typedef int (*call_typelist)(Execute, addr *, enum LISPDECL, addr, addr, addr);
static call_typelist TypeParseList[LISPDECL_SIZE];
#define DefListInit(a, b) (TypeParseList[LISPDECL_##a] = typelist_##b)

_g void init_type_parse(void)
{
	/* Compound-type */
	DefListInit(AND,                 array4      );
	DefListInit(OR,                  array4      );
	DefListInit(EQL,                 eql         );
	DefListInit(MEMBER,              member      );
	DefListInit(MOD,                 mod         );
	DefListInit(NOT,                 not         );
	DefListInit(SATISFIES,           satisfies   );
	/* LispInit(VALUES,              values      ); */

	/* Atomic-type */
	DefListInit(CONS,                cons        );
	DefListInit(FUNCTION,            function    );
	DefListInit(COMPILED_FUNCTION,   function    );
	DefListInit(ARRAY,               array       );
	DefListInit(SIMPLE_ARRAY,        array       );
	DefListInit(VECTOR,              vector      );
	DefListInit(SIMPLE_VECTOR,       size        );
	DefListInit(BIT_VECTOR,          size        );
	DefListInit(SIMPLE_BIT_VECTOR,   size        );
	DefListInit(STRING,              size        );
	DefListInit(BASE_STRING,         size        );
	DefListInit(SIMPLE_STRING,       size        );
	DefListInit(SIMPLE_BASE_STRING,  size        );
	DefListInit(REAL,                real        );
	DefListInit(RATIONAL,            rational    );
	DefListInit(INTEGER,             integer     );
	DefListInit(SIGNED_BYTE,         byte        );
	DefListInit(UNSIGNED_BYTE,       byte        );
	DefListInit(COMPLEX,             complex     );
	DefListInit(FLOAT,               float       );
	DefListInit(SHORT_FLOAT,         short       );
	DefListInit(SINGLE_FLOAT,        single      );
	DefListInit(DOUBLE_FLOAT,        double      );
	DefListInit(LONG_FLOAT,          long        );
}

#define SetTypeParseObject(x,v) (*(enum LISPDECL *)PtrBodyB2(x) = (v))
#define GetTypeParseObject(x,r) (*(r) = *(enum LISPDECL *)PtrBodyB2(x))
_g void make_type_parse_object(addr *ret, enum LISPDECL type)
{
	addr pos;

	heap_body2(&pos, LISPSYSTEM_TYPE_PARSE, sizeoft(enum LISPDECL));
	SetTypeParseObject(pos, type);
	*ret = pos;
}

static void define_type_parse_object(enum LISPDECL type, constindex name)
{
	addr symbol, pos;

	GetConstant(name, &symbol);
	CheckType(symbol, LISPTYPE_SYMBOL);
	make_type_parse_object(&pos, type);
	setlisttype_symbol(symbol, pos);
}
#define DefListType(a) define_type_parse_object(LISPDECL_##a, CONSTANT_COMMON_##a)

_g void build_type_parse(void)
{
	/* Compound-type */
	DefListType(AND                 );
	DefListType(OR                  );
	DefListType(EQL                 );
	DefListType(MEMBER              );
	DefListType(MOD                 );
	DefListType(NOT                 );
	DefListType(SATISFIES           );
	/* ListType(VALUES              );*/

	/* Atomic-type */
	DefListType(CONS                );
	DefListType(FUNCTION            );
	DefListType(COMPILED_FUNCTION   );
	DefListType(ARRAY               );
	DefListType(SIMPLE_ARRAY        );
	DefListType(VECTOR              );
	DefListType(SIMPLE_VECTOR       );
	DefListType(BIT_VECTOR          );
	DefListType(SIMPLE_BIT_VECTOR   );
	DefListType(STRING              );
	DefListType(BASE_STRING         );
	DefListType(SIMPLE_STRING       );
	DefListType(SIMPLE_BASE_STRING  );
	DefListType(REAL                );
	DefListType(RATIONAL            );
	DefListType(INTEGER             );
	DefListType(SIGNED_BYTE         );
	DefListType(UNSIGNED_BYTE       );
	DefListType(COMPLEX             );
	DefListType(FLOAT               );
	DefListType(SHORT_FLOAT         );
	DefListType(SINGLE_FLOAT        );
	DefListType(DOUBLE_FLOAT        );
	DefListType(LONG_FLOAT          );
}

static int parse_type_default(Execute ptr,
		addr *ret, addr symbol, addr args, addr env)
{
	enum LISPDECL type;
	call_typelist call;
	addr pos;

	CheckType(symbol, LISPTYPE_SYMBOL);
	getlisttype_symbol(symbol, &pos);
	if (pos == Nil)
		return Result(ret, NULL);
	CheckType(pos, LISPSYSTEM_TYPE_PARSE);
	GetTypeParseObject(pos, &type);
	call = TypeParseList[type];

	return (*call)(ptr, ret, type, symbol, args, env);
}

static int parse_type_list(Execute ptr, addr *ret, addr pos, addr env)
{
	addr symbol, args, check;

	GetCons(pos, &symbol, &args);
	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);

	/* lisp type */
	Return(parse_type_default(ptr, &check, symbol, args, env));
	if (check)
		return Result(ret, check);

	/* deftype */
	Return(execute_list_deftype(ptr, &check, pos, env));
	if (check)
		return parse_type(ptr, ret, check, env);

	/* error */
	return Result(ret, NULL);
}

static int parse_type_symbol(Execute ptr, addr *ret, addr pos, addr env)
{
	addr check;

	Return(find_symbol_type(ptr, &check, pos, env));
	if (check == NULL)
		return Result(ret, NULL);
	else
		return parse_type(ptr, ret, check, env);
}

static int parse_type_null(Execute ptr, addr *ret, addr pos, addr env)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return parse_type_symbol(ptr, ret, pos, env);

		case LISPTYPE_CONS:
			return parse_type_list(ptr, ret, pos, env);

		case LISPTYPE_TYPE:
			type_throw_heap(pos, ret);
			return 0;

		default:
			return Result(ret, NULL);
	}
}

_g int parse_type(Execute ptr, addr *ret, addr pos, addr env)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, pos, env, NULL);
	Return(parse_type_null(ptr, ret, pos, env));
	localhold_end(hold);

	if (*ret == NULL)
		return fmte_("Invalid type-spec ~S.", pos, NULL);

	return 0;
}

_g int parse_type_not(Execute ptr, addr *ret, addr pos, addr env)
{
	Return(parse_type(ptr, &pos, pos, env));
	type_copy_unsafe_heap(&pos, pos);
	type_revnotdecl(pos);
	return Result(ret, pos);
}

_g int parse_type_noaster(Execute ptr, addr *ret, addr pos, addr env)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	if (pos == aster)
		return fmte_("Don't allow to use asterisk type.", NULL);

	return parse_type(ptr, ret, pos, env);
}

_g void parse_type_unsafe(addr *ret, addr pos)
{
	if (parse_type(Execute_Thread, ret, pos, Nil)) {
		Abort("parse-type error.");
	}
}

/* debug */
_g int parse_type_values(Execute ptr, addr *ret, addr type, addr env)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, type, env, NULL);
	Return(type_function_values(ptr, ret, type, env));
	localhold_end(hold);

	return 0;
}

