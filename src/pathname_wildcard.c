#include "cons.h"
#include "constant.h"
#include "pathname_object.h"
#include "pathname_wildcard.h"
#include "strtype.h"

/*
 *  wild_pathname_boolean
 */
static int wild_pathname_string_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos))
		return Result(ret, 0);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == '*' || c == '?')
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

_g int wild_pathname_boolean_(addr file, addr field, int *ret)
{
	int check;
	addr value, pos, wild1, wild2, path;

	Check(! pathnamep(file), "type error");
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);

	/* skip host*/
	/* skip device */

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &value);
	if (field == value || field == Nil) {
		GetDirectoryPathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		while (consp(pos)) {
			GetCons(pos, &path, &pos);
			if (path == wild1)
				return Result(ret, 1);
			if (path == wild2)
				return Result(ret, 1);
			Return(wild_pathname_string_(path, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	/* name */
	GetConst(KEYWORD_NAME, &value);
	if (field == value || field == Nil) {
		GetNamePathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		Return(wild_pathname_string_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	/* type */
	GetConst(KEYWORD_TYPE, &value);
	if (field == value || field == Nil) {
		GetTypePathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
		Return(wild_pathname_string_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	/* version */
	GetConst(KEYWORD_VERSION, &value);
	if (field == value || field == Nil) {
		GetVersionPathname(file, &pos);
		if (pos == wild1)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}


/*
 *  wildcard-pathname-p
 */
static int wildcard_character_pathname_(
		addr p1, size_t n1, size_t s1,
		addr p2, size_t n2, size_t s2,
		int *ret)
{
	int check;
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2)
		return Result(ret, 1);
	if (n1 == s1 || n2 == s2)
		return Result(ret, 0);
	Return(string_getc_(p1, n1, &c1));
	Return(string_getc_(p2, n2, &c2));
	/* (a ?) -> next */
	if (c2 == '?')
		return wildcard_character_pathname_(p1,n1+1,s1,  p2,n2+1,s2,  ret);
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2)
			return Result(ret, 0);
		else
			return wildcard_character_pathname_(p1,n1+1,s1,  p2,n2+1,s2,  ret);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		Return(wildcard_character_pathname_(p1,i,s1,  p2,n2,s2,  &check));
		if (check)
			return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int wildcard_string_p_(addr pos, int *ret)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == '*' || c == '?')
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

_g int wildcard_stringp_p_(addr pos, int *ret)
{
	if (! stringp(pos))
		return Result(ret, 0);
	else
		return wildcard_string_p_(pos, ret);
}

_g int wildcard_string_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	int check, check1, check2;
	addr wild;
	size_t s1, s2;

	GetConst(KEYWORD_WILD, &wild);
	if (a == wild && b == wild)
		return Result(ret, 1);
	check1 = stringp(a);
	check2 = stringp(b);
	if (check1 && b == wild)
		return Result(ret, 1);
	if ((! check1) || (! check2))
		return Result(ret, 0);
	Return((*equal)(a, b, &check));
	if (check)
		return Result(ret, 1);
	Return(wildcard_string_p_(a, &check));
	if (check)
		return Result(ret, 0);
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_character_pathname_(a, 0, s1, b, 0, s2, ret);
}

_g int wildcard_eq_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	if (a == b)
		return Result(ret, 1);
	else
		return wildcard_string_pathname_(a, b, equal, ret);
}

static int wildcard_nil_pathname_(addr a, addr b, int wildp,
		lisp_equal_calltype equal, int *ret)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (wildp && a == Nil)
		a = wild;
	if (wildp && b == Nil)
		b = wild;

	return wildcard_eq_pathname_(a, b, equal, ret);
}

static int wildcard_list_pathname_(addr a, addr b, lisp_equal_calltype equal, int *ret)
{
	int check;
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil)
		return Result(ret, 1);
	if (a != Nil && b == Nil)
		return Result(ret, 0);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	if (a == Nil && b != Nil) {
		while (b != Nil) {
			Return_getcons(b, &pos2, &b);
			if (pos2 != wilds)
				return Result(ret, 0);
		}
		return Result(ret, 1);
	}
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	Return_getcons(a, &pos1, &a1);
	Return_getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild)
		return wildcard_list_pathname_(a1, b1, equal, ret);
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		Return(wildcard_string_pathname_(pos1, pos2, equal, &check));
		if (! check)
			return Result(ret, 0);
		else
			return wildcard_list_pathname_(a1, b1, equal, ret);
	}
	/* ("str" **) */
	for (;;) {
		Return(wildcard_list_pathname_(a, b1, equal, &check));
		if (check)
			return Result(ret, 1);
		if (a == Nil)
			break;
		Return_getcdr(a, &a);
	}

	return Result(ret, 0);
}

static int wildcard_directory_p_(addr pos, int *ret)
{
	addr check;

	GetConst(KEYWORD_WILD, &check);
	if (pos == check)
		return Result(ret, 1);
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (pos == check)
		return Result(ret, 1);

	return wildcard_stringp_p_(pos, ret);
}

static int wildcard_directory_pathname_(addr a, addr b,
		lisp_equal_calltype equal, int *ret)
{
	int check, check1, check2;
	addr car1, car2, cdr1, cdr2;

	/* right is nil */
	if (b == Nil)
		return Result(ret, 1);

	/* compare */
	cdr1 = a;
	cdr2 = b;
	check1 = 0;
	check2 = 1;
	for (;;) {
		if (cdr1 == Nil && cdr2 == Nil)
			return Result(ret, 1);
		if (cdr1 == Nil || cdr2 == Nil)
			break;
		Return_getcons(cdr1, &car1, &cdr1);
		Return_getcons(cdr2, &car2, &cdr2);
		Return(wildcard_directory_p_(car1, &check));
		if (check)
			check1 = 1;
		Return(wildcard_directory_p_(car2, &check));
		if (check)
			check2 = 1;
		Return((*equal)(car1, car2, &check));
		if (! check)
			break;
	}
	if (check1 || (! check2)) {
		return Result(ret, 0);
	}
	else {
		Return_getcdr(a, &a);
		Return_getcdr(b, &b);
		return wildcard_list_pathname_(a, b, equal, ret);
	}
}

static int wildcard_version_pathname(addr a, addr b)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (a == Nil)
		a = wild;
	if (b == Nil)
		b = wild;
	if (eql_function(a, b))
		return 1;

	return b == wild;
}

_g int wildcard_pathname_(addr a, addr b, int wild, int *ret)
{
	int check;
	addr check1, check2;
	lisp_equal_calltype equal;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");
	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return Result(ret, 0);

	/* host */
	GetHostPathname(a, &check1);
	GetHostPathname(b, &check2);
	Return(equalp_function_(check1, check2, &check));
	if (! check)
		return Result(ret, 0);
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &check1);
	GetDevicePathname(b, &check2);
	Return((*equal)(check1, check2, &check));
	if (! check)
		return Result(ret, 0);

	/* directory */
	GetDirectoryPathname(a, &check1);
	GetDirectoryPathname(b, &check2);
	Return(wildcard_directory_pathname_(check1, check2, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* name */
	GetNamePathname(a, &check1);
	GetNamePathname(b, &check2);
	Return(wildcard_nil_pathname_(check1, check2, wild, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* type */
	GetTypePathname(a, &check1);
	GetTypePathname(b, &check2);
	Return(wildcard_nil_pathname_(check1, check2, wild, equal, &check));
	if (! check)
		return Result(ret, 0);

	/* version */
	GetVersionPathname(a, &check1);
	GetVersionPathname(b, &check2);
	if (! wildcard_version_pathname(check1, check2))
		return Result(ret, 0);

	return Result(ret, 1);
}

