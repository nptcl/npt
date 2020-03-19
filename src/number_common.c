#include "build.h"
#include "cmpl.h"
#include "common_header.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "integer.h"
#include "number.h"
#include "number_common.h"
#include "number_random.h"
#include "object.h"
#include "setf.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  =
 */
_g int number_equal_common(LocalRoot local, addr left, addr rest)
{
	addr right;

	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! equal_number(local, left, right))
			return 0;
	}

	return 1;
}


/*
 *  /=
 */
_g int number_not_equal_common(LocalRoot local, addr left, addr rest)
{
	addr right, list;

	while (rest != Nil) {
		for (list = rest; list != Nil; ) {
			getcons(list, &right, &list);
			if (equal_number(local, left, right))
				return 0;
		}
		getcons(rest, &left, &rest);
	}

	return 1;
}


/*
 *  <
 */
_g int number_less_common(LocalRoot local, addr left, addr rest)
{
	addr right;

	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! less_number(local, left, right))
			return 0;
	}

	return 1;
}


/*
 *  >
 */
_g int number_greater_common(LocalRoot local, addr left, addr rest)
{
	addr right;

	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! greater_number(local, left, right))
			return 0;
	}

	return 1;
}

/*
 *  <=
 */
_g int number_less_equal_common(LocalRoot local, addr left, addr rest)
{
	addr right;

	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! less_equal_number(local, left, right))
			return 0;
	}

	return 1;
}


/*
 *  >=
 */
_g int number_greater_equal_common(LocalRoot local, addr left, addr rest)
{
	addr right;

	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! greater_equal_number(local, left, right))
			return 0;
	}

	return 1;
}


/*
 *  max
 */
_g void max_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	while (rest != Nil) {
		getcons(rest, &right, &rest);
		if (less_number(local, left, right))
			left = right;
	}
	*ret = left;
}


/*
 *  min
 */
_g void min_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	while (rest != Nil) {
		getcons(rest, &right, &rest);
		if (greater_number(local, left, right))
			left = right;
	}
	*ret = left;
}


/*
 *  +
 */
_g void plus_common(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 0);
		return;
	}

	/* list */
	getcons(rest, &left, &rest);
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		plus_number_heap(local, left, right, &left);
	}
	*ret = left;
}


/*
 *  -
 */
_g void minus_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		sign_reverse_number_common(left, ret);
		return;
	}

	/* list */
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		minus_number_heap(local, left, right, &left);
	}
	*ret = left;
}


/*
 *  *
 */
_g void asterisk_common(LocalRoot local, addr rest, addr *ret)
{
	addr left, right;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(ret, 1);
		return;
	}

	/* list */
	getcons(rest, &left, &rest);
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		multi_number_heap(local, left, right, &left);
	}
	*ret = left;
}


/*
 *  /
 */
_g void slash_common(LocalRoot local, addr left, addr rest, addr *ret)
{
	addr right;

	/* nil */
	if (rest == Nil) {
		inverse_number_heap(local, left, ret);
		return;
	}

	/* list */
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		div_number_heap(local, left, right, &left);
	}
	*ret = left;
}


/*
 *  incf
 */
static void incf_expand_common(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, plus;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	if (! singlep(g)) {
		_fmte("INCF place ~S don't allow a multiple store value.", place, NULL);
		return;
	}

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (+ r value))  ;; (setq g (1+ r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		getcons(a, &c, &a);
		getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	getcar(g, &g);
	cons_heap(&args, g, args);
	nreverse_list_unsafe(&args, args);
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
}

_g void incf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	getcdr(form, &form);
	if (form == Nil) goto error;
	getcons(form, &place, &args);
	if (args == Nil) {
		value = Unbound;
	}
	else {
		getcons(args, &value, &args);
		if (args != Nil) goto error;
	}
	incf_expand_common(ptr, ret, place, value, env);
	return;

error:
	_fmte("INCF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  decf
 */
static void decf_expand_common(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, minus;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	if (! singlep(g)) {
		_fmte("DECF place ~S don't allow a multiple store value.", place, NULL);
		return;
	}

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (- r value))  ;; (setq g (1- r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		getcons(a, &c, &a);
		getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	getcar(g, &g);
	cons_heap(&args, g, args);
	nreverse_list_unsafe(&args, args);
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
}

_g void decf_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, value;

	getcdr(form, &form);
	if (form == Nil) goto error;
	getcons(form, &place, &args);
	if (args == Nil) {
		value = Unbound;
	}
	else {
		getcons(args, &value, &args);
		if (args != Nil) goto error;
	}
	decf_expand_common(ptr, ret, place, value, env);
	return;

error:
	_fmte("DECF ~S must be (place &optional value) form.", form, NULL);
}


/*
 *  random
 */
_g void random_common(Execute ptr, addr limit, addr state, addr *ret)
{
	if (state == Unbound) {
		/* symbol-value *random-state* */
		GetConst(SPECIAL_RANDOM_STATE, &state);
		getspecialcheck_local(ptr, state, &state);
	}
	random_number_common(ptr->local, limit, state, ret);
}


/*
 *  conjugate
 */
_g void conjugate_common(addr var, addr *ret)
{
	addr real, imag;

	if (complexp(var)) {
		GetRealComplex(var, &real);
		GetImagComplex(var, &imag);
		sign_reverse_real_common(imag, &imag);
		complex_heap(ret, real, imag);
		return;
	}
	if (realp(var)) {
		*ret = var;
		return;
	}
	TypeError(var, NUMBER);
	*ret = Nil;
}


/*
 *  realpart
 */
_g void realpart_common(addr var, addr *ret)
{
	if (complexp(var)) {
		GetRealComplex(var, &var);
	}
	real_throw_heap(var, ret);
}


/*
 *  imagpart
 */
_g void imagpart_common(addr var, addr *ret)
{
	if (complexp(var)) {
		GetImagComplex(var, &var);
		real_throw_heap(var, ret);
	}
	else {
		fixnum_heap(ret, 0);
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
	if (getkeyargs(rest, KEYWORD_RADIX, &radix)) fixnum_heap(&radix, 10);
	if (getkeyargs(rest, KEYWORD_JUNK_ALLOWED, &junk)) junk = Nil;
	return parse_integer_clang(local, var, start, end,
			(unsigned)RefFixnum(radix), junk != Nil, ret1, ret2);
}

