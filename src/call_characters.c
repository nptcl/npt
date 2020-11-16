#include "call_characters.h"
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
static int char_eql_check_common_(
		constindex name, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr var, pos;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConstant(name, &pos);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", pos, NULL);
	}
	GetCons(list, &var, &list);

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
#define CharEqlCheckCommon(name, list, ret, call) \
	char_eql_check_common_(CONSTANT_COMMON_##name, list, ret, call)

static int call_char_eql(unicode a, unicode b)
{
	return a == b;
}

int char_eql_common(addr list, addr *ret)
{
	return CharEqlCheckCommon(CHAR_EQL, list, ret, call_char_eql);
}


/*
 *  char/=
 */
int char_not_eql_common(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConst(COMMON_CHAR_NOT_EQL, &left);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", left, NULL);
	}

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

int char_less_common(addr list, addr *ret)
{
	return CharEqlCheckCommon(CHAR_LESS, list, ret, call_char_less);
}


/*
 *  char>
 */
static int call_char_greater(unicode a, unicode b)
{
	return a > b;
}

int char_greater_common(addr list, addr *ret)
{
	return CharEqlCheckCommon(CHAR_GREATER, list, ret, call_char_greater);
}


/*
 *  char<=
 */
static int call_char_less_equal(unicode a, unicode b)
{
	return a <= b;
}

int char_less_equal_common(addr list, addr *ret)
{
	return CharEqlCheckCommon(CHAR_LESS_EQUAL, list, ret, call_char_less_equal);
}


/*
 *  char>=
 */
static int call_char_greater_equal(unicode a, unicode b)
{
	return a >= b;
}

int char_greater_equal_common(addr list, addr *ret)
{
	return CharEqlCheckCommon(CHAR_GREATER_EQUAL, list, ret, call_char_greater_equal);
}


/*
 *  char-equal
 */
static int char_equal_check_common_(
		constindex name, addr list, addr *ret, int (*call)(unicode, unicode))
{
	addr var, pos;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConstant(name, &pos);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", pos, NULL);
	}
	GetCons(list, &var, &list);

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
#define CharEqualCheckCommon(name, list, ret, call) \
	char_equal_check_common_(CONSTANT_COMMON_##name, list, ret, call)

int char_equal_common(addr list, addr *ret)
{
	return CharEqualCheckCommon(CHAR_EQUAL, list, ret, call_char_eql);
}


/*
 *  char-not-equal
 */
int char_not_equal_common(addr list, addr *ret)
{
	addr left, right, loop;
	unicode a, b;

	if (list == Nil) {
		*ret = Nil;
		GetConst(COMMON_CHAR_NOT_EQUAL, &left);
		return call_simple_program_error_va_(NULL,
				"Too few arguemnts ~A.", left, NULL);
	}
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
int char_lessp_common(addr list, addr *ret)
{
	return CharEqualCheckCommon(CHAR_LESSP, list, ret, call_char_less);
}


/*
 *  char-greaterp
 */
int char_greaterp_common(addr list, addr *ret)
{
	return CharEqualCheckCommon(CHAR_GREATERP, list, ret, call_char_greater);
}


/*
 *  char-not-lessp
 */
int char_not_lessp_common(addr list, addr *ret)
{
	return CharEqualCheckCommon(CHAR_NOT_LESSP, list, ret, call_char_greater_equal);
}


/*
 *  char-not-greaterp
 */
int char_not_greaterp_common(addr list, addr *ret)
{
	return CharEqualCheckCommon(CHAR_NOT_GREATERP, list, ret, call_char_less_equal);
}


/*
 *  character
 */
static int character_common_error(addr var)
{
	addr pos, size;

	GetConst(COMMON_STRING, &pos);
	fixnum_heap(&size, 1);
	list_heap(&pos, pos, size, NULL);
	return call_type_error_va_(Execute_Thread, var, pos,
			"The length of symbol ~S name must be 1.", var, NULL);
}

int character_common(addr var, addr *ret)
{
	unicode c;
	size_t size;

	if (symbolp(var)) {
		/* string check */
		GetNameSymbol(var, &var);
		string_length(var, &size);
		if (size != 1) {
			*ret = Nil;
			return character_common_error(var);
		}
		Return(string_getc_(var, 0, &c));
		character_heap(&var, c);
	}
	else if (stringp(var)) {
		string_length(var, &size);
		if (size != 1) {
			*ret = Nil;
			return character_common_error(var);
		}
		Return(string_getc_(var, 0, &c));
		character_heap(&var, c);
	}
	else if (GetType(var) != LISPTYPE_CHARACTER) {
		return TypeError_(var, CHARACTER);
	}

	return Result(ret, var);
}


/*
 *  alpha-char-p
 */
void alpha_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphabetic(c)? T: Nil;
}


/*
 *  alphanumericp
 */
void alphanumericp_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isAlphanumeric(c)? T: Nil;
}


/*
 *  digit-char
 */
void digit_char_common(addr var, addr opt, addr *ret)
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
void digit_char_p_common(addr var, addr opt, addr *ret)
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
void graphic_char_p_common(addr var, addr *ret)
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
void standard_char_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isStandardType(c)? T: Nil;
}


/*
 *  char-upcase
 */
void char_upcase_common(addr var, addr *ret)
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
void char_downcase_common(addr var, addr *ret)
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
void upper_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isUpperCase(c)? T: Nil;
}


/*
 *  lower-case-p
 */
void lower_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = isLowerCase(c)? T: Nil;
}


/*
 *  both-case-p
 */
void both_case_p_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	*ret = (isUpperCase(c) || isLowerCase(c))? T: Nil;
}


/*
 *  char-code
 */
void char_code_common(addr var, addr *ret)
{
	unicode c;
	GetCharacter(var, &c);
	fixnum_heap(ret, (fixnum)c);
}


/*
 *  code-char
 */
void code_char_common(addr var, addr *ret)
{
	fixnum v;
	unicode c;

	GetFixnum(var, &v);
	c = (unicode)v;
	if (isBaseType(c))
		character_heap(ret, c);
	else
		*ret = Nil;
}


/*
 *  char-name
 */
int char_name_common_(addr var, addr *ret)
{
	return findtable_char_name_(ret, var);
}


/*
 *  name-char
 */
int name_char_common_(LocalRoot local, addr var, addr *ret)
{
	LocalStack stack;
	unicode c;

	if (GetType(var) == LISPTYPE_CHARACTER) {
		/* character */
		push_local(local, &stack);
		GetCharacter(var, &c);
		strvect_local(local, &var, 1);
		Return(strvect_setc_(var, 0, c));
		Return(findtable_name_char_(ret, var));
		rollback_local(local, stack);
	}
	else {
		/* symbol, string */
		if (symbolp(var))
			GetNameSymbol(var, &var);
		Return(findtable_name_char_(ret, var));
	}

	return 0;
}

