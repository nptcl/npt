#include "character.h"
#include "character_name.h"
#include "condition.h"
#include "cons.h"
#include "build.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  char=
 */
static int char_eql_check_common(
		addr var, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr pos;
	unicode a, b;

	GetCharacter(var, &a);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetCharacter(pos, &b);
		if (! (call(a, b)))
			return Result(ret, Nil);
		a = b;
	}
	return Result(ret, T);
}

static int call_char_eql(unicode a, unicode b)
{
	return a == b;
}

_g int char_eql_common(addr var, addr list, addr *ret)
{
	return char_eql_check_common(var, list, ret, call_char_eql);
}


/*
 *  char/=
 */
_g int char_not_eql_common(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	for (;;) {
		Return_getcons(list, &left, &list);
		if (list == Nil)
			break;
		GetCharacter(left, &a);
		for (loop = list; loop != Nil; ) {
			Return_getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			if (a == b)
				return Result(ret, Nil);
		}
	}
	return Result(ret, T);
}


/*
 *  char<
 */
static int call_char_less(unicode a, unicode b)
{
	return a < b;
}

_g int char_less_common(addr var, addr list, addr *ret)
{
	return char_eql_check_common(var, list, ret, call_char_less);
}


/*
 *  char>
 */
static int call_char_greater(unicode a, unicode b)
{
	return a > b;
}

_g int char_greater_common(addr var, addr list, addr *ret)
{
	return char_eql_check_common(var, list, ret, call_char_greater);
}


/*
 *  char<=
 */
static int call_char_less_equal(unicode a, unicode b)
{
	return a <= b;
}

_g int char_less_equal_common(addr var, addr list, addr *ret)
{
	return char_eql_check_common(var, list, ret, call_char_less_equal);
}


/*
 *  char>=
 */
static int call_char_greater_equal(unicode a, unicode b)
{
	return a >= b;
}

_g int char_greater_equal_common(addr var, addr list, addr *ret)
{
	return char_eql_check_common(var, list, ret, call_char_greater_equal);
}


/*
 *  char-equal
 */
static int char_equal_check_common(
		addr var, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr pos;
	unicode a, b;

	GetCharacter(var, &a);
	a = toUpperUnicode(a);
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetCharacter(pos, &b);
		b = toUpperUnicode(b);
		if (! (call(a, b)))
			return Result(ret, Nil);
		a = b;
	}
	return Result(ret, T);
}

_g int char_equal_common(addr var, addr list, addr *ret)
{
	return char_equal_check_common(var, list, ret, call_char_eql);
}


/*
 *  char-not-equal
 */
_g int char_not_equal_common(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	for (;;) {
		Return_getcons(list, &left, &list);
		if (list == Nil)
			break;
		GetCharacter(left, &a);
		a = toUpperUnicode(a);
		for (loop = list; loop != Nil; ) {
			Return_getcons(loop, &right, &loop);
			GetCharacter(right, &b);
			b = toUpperUnicode(b);
			if (a == b)
				return Result(ret, Nil);
		}
	}
	return Result(ret, T);
}


/*
 *  char-lessp
 */
_g int char_lessp_common(addr var, addr list, addr *ret)
{
	return char_equal_check_common(var, list, ret, call_char_less);
}


/*
 *  char-greaterp
 */
_g int char_greaterp_common(addr var, addr list, addr *ret)
{
	return char_equal_check_common(var, list, ret, call_char_greater);
}


/*
 *  char-not-lessp
 */
_g int char_not_lessp_common(addr var, addr list, addr *ret)
{
	return char_equal_check_common(var, list, ret, call_char_greater_equal);
}


/*
 *  char-not-greaterp
 */
_g int char_not_greaterp_common(addr var, addr list, addr *ret)
{
	return char_equal_check_common(var, list, ret, call_char_less_equal);
}


/*
 *  character
 */
_g int character_common(addr var, addr *ret)
{
	unicode c;
	size_t size;

	if (symbolp(var)) {
		/* string check */
		GetNameSymbol(var, &var);
		string_length(var, &size);
		if (size != 1)
			return fmte("The length of symbol ~S name must be 1.", NULL);
		string_getc(var, 0, &c);
		character_heap(&var, c);
	}
	else if (stringp(var)) {
		string_length(var, &size);
		if (size != 1)
			return fmte("The length of string ~S name must be 1.", NULL);
		string_getc(var, 0, &c);
		character_heap(&var, c);
	}
	else if (GetType(var) != LISPTYPE_CHARACTER) {
		return TypeError_(var, CHARACTER);
	}

	return Result(ret, var);;
}


/*
 *  alpha-char-p
 */
_g void alpha_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphabetic(c)? T: Nil;
}


/*
 *  alphanumericp
 */
_g void alphanumericp_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphanumeric(c)? T: Nil;
}


/*
 *  digit-char
 */
_g void digit_char_common(addr var, addr opt, addr *ret)
{
	fixnum w, r;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);

	/* digit */
	GetFixnum(var, &w);
	if (0 <= w && w < r) {
		character_heap(&var, (unicode)(w < 10? ('0' + w): (w - 10 + 'A')));
		*ret = var;
	}
	else {
		*ret = Nil;
	}
}


/*
 *  digit-char-p
 */
_g void digit_char_p_common(addr var, addr opt, addr *ret)
{
	fixnum r, w;
	unicode c;

	/* radix */
	if (opt == Unbound)
		r = 10;
	else
		GetFixnum(opt, &r);
	/* character */
	GetCharacter(var, &c);
	if (isDigitCase(c))
		w = (fixnum)(c - '0');
	else if (isLowerCase(c))
		w = (fixnum)(c - 'a' + 10);
	else if (isUpperCase(c))
		w = (fixnum)(c - 'A' + 10);
	else {
		*ret = Nil;
		return;
	}
	if (r <= w) {
		*ret = Nil;
		return;
	}
	else {
		fixnum_heap(ret, w);
		return;
	}
}


/*
 *  graphic-char-p
 */
_g void graphic_char_p_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (c < 0x80) {
		*ret = (c == ' ' || isgraphunicode(c))? T: Nil;
	}
	else {
		*ret = T;
	}
}


/*
 *  standard-char-p
 */
_g void standard_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isStandardType(c)? T: Nil;
}


/*
 *  char-upcase
 */
_g void char_upcase_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (isLowerCase(c))
		character_heap(&var, (c - 'a' + 'A'));
	*ret = var;
}


/*
 *  char-downcase
 */
_g void char_downcase_common(addr var, addr *ret)
{
	unicode c;

	GetCharacter(var, &c);
	if (isUpperCase(c))
		character_heap(&var, (c - 'A' + 'a'));
	*ret = var;
}


/*
 *  upper-case-p
 */
_g void upper_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isUpperCase(c)? T: Nil;
}


/*
 *  lower-case-p
 */
_g void lower_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isLowerCase(c)? T: Nil;
}


/*
 *  both-case-p
 */
_g void both_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = (isUpperCase(c) || isLowerCase(c))? T: Nil;
}


/*
 *  char-code
 */
_g void char_code_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	fixnum_heap(ret, (fixnum)c);
}


/*
 *  code-char
 */
_g void code_char_common(addr var, addr *ret)
{
	fixnum v;

	GetFixnum(var, &v);
	if (0 <= v && v < (fixnum)UnicodeCount)
		character_heap(ret, (unicode)v);
	else
		*ret = Nil;
}


/*
 *  char-name
 */
_g void char_name_common(addr var, addr *ret)
{
	if (! findtable_char_name(ret, var))
		*ret = Nil;
}


/*
 *  name-char
 */
_g void name_char_common(LocalRoot local, addr var, addr *ret)
{
	LocalStack stack;
	unicode c;

	if (GetType(var) == LISPTYPE_CHARACTER) {
		/* character */
		push_local(local, &stack);
		GetCharacter(var, &c);
		strvect_local(local, &var, 1);
		strvect_setc(var, 0, c);
		if (! findtable_name_char(ret, var))
			*ret = Nil;
		rollback_local(local, stack);
	}
	else {
		/* symbol, string */
		if (symbolp(var))
			GetNameSymbol(var, &var);
		if (! findtable_name_char(ret, var))
			*ret = Nil;
	}
}

