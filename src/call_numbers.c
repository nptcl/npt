#include "build.h"
#include "call_numbers.h"
#include "cmpl.h"
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "integer.h"
#include "number.h"
#include "number_equal.h"
#include "number_multi.h"
#include "number_random.h"
#include "number_plus.h"
#include "object.h"
#include "real_equal.h"
#include "real_plus.h"
#include "setf.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  =
 */
_g int number_equal_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  /=
 */
_g int number_not_equal_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right, list;

	while (rest != Nil) {
		for (list = rest; list != Nil; ) {
			Return_getcons(list, &right, &list);
			Return(equal_number_(local, left, right, &check));
			if (check)
				return Result(ret, 0);
		}
		Return_getcons(rest, &left, &rest);
	}

	return Result(ret, 1);
}


/*
 *  <
 */
_g int number_less_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(less_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  >
 */
_g int number_greater_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(greater_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  <=
 */
_g int number_less_equal_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(less_equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  >=
 */
_g int number_greater_equal_common(LocalRoot local, addr left, addr rest, int *ret)
{
	int check;
	addr right;

	for (; rest != Nil; left = right) {
		Return_getcons(rest, &right, &rest);
		Return(greater_equal_number_(local, left, right, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}


/*
 *  max
 */
_g int max_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	int check;
	addr right;

	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(less_number_(local, left, right, &check));
		if (check)
			left = right;
	}

	return Result(ret, left);
}


/*
 *  min
 */
_g int min_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	int check;
	addr right;

	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(greater_number_(local, left, right, &check));
		if (check)
			left = right;
	}

	return Result(ret, left);
}


/*
 *  +
 */
_g int plus_common(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 0);
		return 0;
	}

	/* list */
	Return_getcons(rest, &left, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(plus_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  -
 */
_g int minus_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		return sign_reverse_number_common_(left, ret);
	}

	/* list */
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(minus_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  *
 */
_g int asterisk_common(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 1);
		return 0;
	}

	/* list */
	Return_getcons(rest, &left, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(multi_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  /
 */
_g int slash_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		return inverse_number_heap_(local, left, ret);
	}

	/* list */
	while (rest != Nil) {
		Return_getcons(rest, &right, &rest);
		Return(div_number_heap_(local, left, right, &left));
	}

	return Result(ret, left);
}


/*
 *  incf
 */
static int incf_expand_common(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, plus;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("INCF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (+ r value))  ;; (setq g (1+ r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	Return_getcar(g, &g);
	cons_heap(&args, g, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* setq */
	GetConst(COMMON_SETQ, &setq);
	if (value == Unbound) {
		GetConst(COMMON_ONE_PLUS, &plus);
		list_heap(&plus, plus, r, NULL);
	}
	else {
		GetConst(COMMON_PLUS, &plus);
		list_heap(&plus, plus, r, value, NULL);
	}
	list_heap(&setq, setq, g, plus, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, setq, w, g, NULL);

	return 0;
}

_g int incf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
	}
	else {
		Return_getcons(args, &value, &args);
		if (args != Nil)
			goto error;
	}
	return incf_expand_common(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("INCF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  decf
 */
static int decf_expand_common(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, minus;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("DECF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (- r value))  ;; (setq g (1- r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	Return_getcar(g, &g);
	cons_heap(&args, g, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* setq */
	GetConst(COMMON_SETQ, &setq);
	if (value == Unbound) {
		GetConst(COMMON_ONE_MINUS, &minus);
		list_heap(&minus, minus, r, NULL);
	}
	else {
		GetConst(COMMON_MINUS, &minus);
		list_heap(&minus, minus, r, value, NULL);
	}
	list_heap(&setq, setq, g, minus, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, setq, w, g, NULL);

	return 0;
}

_g int decf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (args == Nil) {
		value = Unbound;
	}
	else {
		Return_getcons(args, &value, &args);
		if (args != Nil)
			goto error;
	}
	return decf_expand_common(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("DECF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  random
 */
_g int random_common(Execute ptr, addr limit, addr state, addr *ret)
{
	if (state == Unbound) {
		/* symbol-value *random-state* */
		GetConst(SPECIAL_RANDOM_STATE, &state);
		Return(getspecialcheck_local_(ptr, state, &state));
	}
	return random_number_common(ptr->local, limit, state, ret);
}


/*
 *  conjugate
 */
_g int conjugate_common(addr var, addr *ret)
{
	addr real, imag;

	if (complexp(var)) {
		GetRealComplex(var, &real);
		GetImagComplex(var, &imag);
		Return(sign_reverse_real_common_(imag, &imag));
		Return(complex_heap_(ret, real, imag));
		return 0;
	}
	if (realp(var))
		return Result(ret, var);

	/* error */
	*ret = Nil;
	return TypeError_(var, NUMBER);
}


/*
 *  realpart
 */
_g int realpart_common_(addr var, addr *ret)
{
	if (complexp(var)) {
		GetRealComplex(var, &var);
	}

	return real_throw_heap_(var, ret);
}


/*
 *  imagpart
 */
_g int imagpart_common_(addr var, addr *ret)
{
	if (complexp(var)) {
		GetImagComplex(var, &var);
		return real_throw_heap_(var, ret);
	}
	else {
		fixnum_heap(ret, 0);
		return 0;
	}
}


/*
 *  parse-integer
 */
_g int parse_integer_common(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2)
{
	addr radix, junk;
	size_t size, start, end;

	string_length(var, &size);
	Return(keyword_start_end_(size, rest, &start, &end));
	if (GetKeyArgs(rest, KEYWORD_RADIX, &radix))
		fixnum_heap(&radix, 10);
	if (GetKeyArgs(rest, KEYWORD_JUNK_ALLOWED, &junk))
		junk = Nil;
	return parse_integer_clang(local, var, start, end,
			(unsigned)RefFixnum(radix), junk != Nil, ret1, ret2);
}

