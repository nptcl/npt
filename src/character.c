#include <errno.h>
#include "character.h"
#include "constant.h"
#include "hashtable.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  character check
 */
_g int isbasechar(unicode x)
{
	return isBaseChar(x);
}

_g int isuppercase(unicode x)
{
	return isUpperCase(x);
}

_g int islowercase(unicode x)
{
	return isLowerCase(x);
}

_g int isdigitcase(unicode x)
{
	return isDigitCase(x);
}

_g int isalphabetic(unicode x)
{
	return isAlphabetic(x);
}

_g int isalphanumeric(unicode x)
{
	return isAlphanumeric(x);
}

_g int isgraphunicode(unicode x)
{
	if (x < 0x80)
		return _isGraphUnicode(x);
	else
		return isBaseType(x);
}

_g int isspaceunicode(unicode x)
{
	return isSpaceUnicode(x);
}

_g unicode toupperunicode(unicode x)
{
	return toUpperUnicode(x);
}

_g unicode tolowerunicode(unicode x)
{
	return toLowerUnicode(x);
}


/*
 *  character type
 */
_g int issurrogatepair(unicode x)
{
	return isSurrogatePair(x);
}

_g int isbaserange(unicode x)
{
	return isBaseRange(x);
}

_g int isstandardtype(unicode x)
{
	return isStandardType(x);
}

_g int isbasetype(unicode x)
{
	return isBaseType(x);
}

_g int isextendedtype(unicode x)
{
	return isExtendedType(x);
}


/* equal */
_g int character_equal_unicode(addr left, unicode right)
{
	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	return RefCharacter(left) == right;
}

_g int character_equalp_unicode(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter(left, &u);
	return toUpperUnicode(u) == toUpperUnicode(right);
}

_g int character2_equal_unicode(addr left, unicode a, unicode b)
{
	CheckType(left, LISPSYSTEM_CHARACTER2);
	return refcharacter2a(left) == a && refcharacter2b(left) == b;
}

_g int character2_equalp_unicode(addr left, unicode a, unicode b)
{
	unicode c, d;

	CheckType(left, LISPSYSTEM_CHARACTER2);
	getcharacter2a(left, &c);
	getcharacter2b(left, &d);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	c = toUpperUnicode(c);
	d = toUpperUnicode(d);
	return a == c && b == d;
}


/*
 *  character table
 */
static void build_character_cache(void)
{
	addr pos;

	/* character cache */
	heap_array4(&pos, LISPSYSTEM_CHARACTER_CACHE, CHARACTER_CACHE);
	SetConst(CHARACTER_CACHE, pos);
}

static void defnametable(addr getname, addr getchar, unicode u, const char *name)
{
	addr pos, string, cons;

	character_heap(&pos, u);
	strvect_char_heap(&string, name);
	if (intern_hashheap(getname, pos, &cons) != 0) {
		/* not found */
		SetCdr(cons, string);
	}
	if (intern_hashheap(getchar, string, &cons) != 0) {
		/* not found */
		SetCdr(cons, pos);
	}
}

static void build_character_name(void)
{
	addr getname, getchar;

	hashtable_heap(&getname);
	hashtable_heap(&getchar);
	settest_hashtable(getname, HASHTABLE_TEST_EQL);
	settest_hashtable(getchar, HASHTABLE_TEST_EQUALP);
	defnametable(getname, getchar, 0x07, "Bell");
	defnametable(getname, getchar, 0x08, "Backspace");
	defnametable(getname, getchar, 0x09, "Tab");
	defnametable(getname, getchar, 0x0A, "Newline");
	defnametable(getname, getchar, 0x0A, "Linefeed");
	defnametable(getname, getchar, 0x0C, "Page");
	defnametable(getname, getchar, 0x0D, "Return");
	defnametable(getname, getchar, 0x20, "Space");
	defnametable(getname, getchar, 0x7F, "Rubout");
	Root(LISPINDEX_CHAR_NAME) = getname;
	Root(LISPINDEX_NAME_CHAR) = getchar;
}

_g void build_character(void)
{
	build_character_cache();
	build_character_name();
}

_g int findtable_unicode_name(addr *ret, unicode u)
{
	addr table;

	table = Root(LISPINDEX_CHAR_NAME);
	return findvalue_unicode_hashtable(table, u, ret);
}

_g int findtable_char_name(addr *ret, addr pos)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	return findtable_unicode_name(ret, RefCharacter(pos));
}

_g int findtable_name_char(addr *ret, addr name)
{
	addr table;

	Check(! stringp(name), "type error");
	table = Root(LISPINDEX_NAME_CHAR);
	return findvalue_hashtable(table, name, ret);
}

static int unicode_code_p(unicode c)
{
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

static int unicode_code(addr name, size_t size, unicode *ret)
{
	char buffer[16];
	unsigned long ul;
	unicode u;
	size_t i, k;

	if (size < 2) return 1;
	string_getc(name, 0, &u);
	if (toUpperUnicode(u) != 'U') return 1;
	if (12 < size) return 1;
	for (k = 0, i = 1; i < size; i++) {
		string_getc(name, i, &u);
		if (! unicode_code_p(u)) return 1;
		buffer[k++] = (char)u;
	}
	buffer[k] = 0;
	errno = 0;
	ul = strtoul(buffer, NULL, 16);
	if (errno == ERANGE) return 1;
	if (0xFFFFFFFFUL < ul) return 1;
	*ret = (unicode)(0xFFFFFFFFUL & ul);

	return 0;
}

_g int find_name_char(addr *ret, addr name)
{
	unicode u;
	size_t size;

	if (GetType(name) == LISPTYPE_SYMBOL) {
		GetNameSymbol(name, &name);
	}
	Check(! stringp(name), "type error");
	string_length(name, &size);
	if (size == 0) {
		return 1;
	}
	if (size == 1) {
		/* form #\a */
		string_getc(name, 0, &u);
		character_heap(ret, u);
		return 0;
	}
	if (! unicode_code(name, size, &u)) {
		/* for #\u123 */
		character_heap(ret, u);
		return 0;
	}

	return findtable_name_char(ret, name);
}

