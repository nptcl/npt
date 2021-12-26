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
int number_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int number_not_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int number_less_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int number_greater_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int number_less_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int number_greater_equal_common_(LocalRoot local, addr left, addr rest, int *ret)
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
int max_common_(LocalRoot local, addr left, addr rest, addr *ret)
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
int min_common_(LocalRoot local, addr left, addr rest, addr *ret)
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
int plus_common_(LocalRoot local, addr rest, addr *ret)
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
int minus_common_(LocalRoot local, addr left, addr rest, addr *ret)
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
int asterisk_common_(LocalRoot local, addr rest, addr *ret)
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
int slash_common_(LocalRoot local, addr left, addr rest, addr *ret)
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
static int incf_expand_common_(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, plus;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("INCF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (+ r value)))  ;; (g (1+ r))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	/* g */
	if (value == Unbound) {
		GetConst(COMMON_ONE_PLUS, &plus);
		list_heap(&plus, plus, r, NULL);
	}
	else {
		GetConst(COMMON_PLUS, &plus);
		list_heap(&plus, plus, r, value, NULL);
	}
	Return_getcar(g, &g);
	list_heap(&plus, g, plus, NULL);
	cons_heap(&args, plus, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

int incf_common_(Execute ptr, addr form, addr env, addr *ret)
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
	return incf_expand_common_(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("INCF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  decf
 */
static int decf_expand_common_(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, minus;

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	if (! singlep(g))
		return fmte_("DECF place ~S don't allow a multiple store value.", place, NULL);

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (- r value)))  ;; (g (1- r))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		Return_getcons(a, &c, &a);
		Return_getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	/* g */
	if (value == Unbound) {
		GetConst(COMMON_ONE_MINUS, &minus);
		list_heap(&minus, minus, r, NULL);
	}
	else {
		GetConst(COMMON_MINUS, &minus);
		list_heap(&minus, minus, r, value, NULL);
	}
	Return_getcar(g, &g);
	list_heap(&minus, g, minus, NULL);
	cons_heap(&args, minus, args);
	nreverse(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, w, g, NULL);

	return 0;
}

int decf_common_(Execute ptr, addr form, addr env, addr *ret)
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
	return decf_expand_common_(ptr, ret, place, value, env);

error:
	*ret = Nil;
	return fmte_("DECF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  random
 */
int random_common_(Execute ptr, addr limit, addr state, addr *ret)
{
	if (state == Unbound) {
		/* symbol-value *random-state* */
		GetConst(SPECIAL_RANDOM_STATE, &state);
		Return(getspecialcheck_local_(ptr, state, &state));
	}
	return random_number_common_(ptr->local, limit, state, ret);
}


/*
 *  conjugate
 */
int conjugate_common_(addr var, addr *ret)
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
int realpart_common_(addr var, addr *ret)
{
	if (complexp(var)) {
		GetRealComplex(var, &var);
	}

	return real_throw_heap_(var, ret);
}


/*
 *  imagpart
 */
static int imagpart_complex_common_(addr var, addr *ret)
{
	GetImagComplex(var, &var);
	return real_throw_heap_(var, ret);
}

static int imagpart_rational_common_(addr var, addr *ret)
{
	fixnum_heap(ret, 0);
	return 0;
}

static int imagpart_single_common_(addr var, addr *ret)
{
	single_float value;

	GetSingleFloat(var, &value);
	if (value == 0.0f)  /* plus or minus */
		return Result(ret, var);

	single_float_heap(ret, 0.0f);
	return 0;
}

static int imagpart_double_common_(addr var, addr *ret)
{
	double_float value;

	GetDoubleFloat(var, &value);
	if (value == 0.0)  /* plus or minus */
		return Result(ret, var);

	double_float_heap(ret, 0.0);
	return 0;
}

static int imagpart_long_common_(addr var, addr *ret)
{
	long_float value;

	GetLongFloat(var, &value);
	if (value == 0.0L)  /* plus or minus */
		return Result(ret, var);

	long_float_heap(ret, 0.0L);
	return 0;
}

int imagpart_common_(addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_COMPLEX:
			return imagpart_complex_common_(var, ret);

		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return imagpart_rational_common_(var, ret);

		case LISPTYPE_SINGLE_FLOAT:
			return imagpart_single_common_(var, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return imagpart_double_common_(var, ret);

		case LISPTYPE_LONG_FLOAT:
			return imagpart_long_common_(var, ret);

		default:
			*ret = Nil;
			return TypeError_(var, NUMBER);
	}
}


/*
 *  parse-integer
 */
int parse_integer_common_(LocalRoot local,
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

