#include "character.h"
#include "character_check.h"
#include "character_name.h"
#include "cons.h"
#include "hashtable.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  character table
 */
_g int findtable_unicode_name(addr *ret, unicode u)
{
	addr table;

	table = LispRoot(CHAR_NAME);
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
	table = LispRoot(NAME_CHAR);
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

	if (symbolp(name)) {
		GetNameSymbol(name, &name);
	}
	Check(! stringp(name), "type error");
	string_length(name, &size);
	if (size == 0) {
		return 0;
	}
	if (size == 1) {
		/* form #\a */
		string_getc(name, 0, &u);
		character_heap(ret, u);
		return 1;
	}
	if (! unicode_code(name, size, &u)) {
		/* for #\u123 */
		character_heap(ret, u);
		return 1;
	}

	return findtable_name_char(ret, name);
}


/*
 *  build
 */
static void defnametable(addr getname, addr getchar, unicode u, const char *name)
{
	addr pos, string, cons;

	character_heap(&pos, u);
	strvect_char_heap(&string, name);
	if (intern_hashheap(getname, pos, &cons) == 0) {
		/* not found */
		SetCdr(cons, string);
	}
	if (intern_hashheap(getchar, string, &cons) == 0) {
		/* not found */
		SetCdr(cons, pos);
	}
}

_g void build_character_name(void)
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
	LispRoot(CHAR_NAME) = getname;
	LispRoot(NAME_CHAR) = getchar;
}

