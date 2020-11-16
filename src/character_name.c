#include <errno.h>
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
int findtable_unicode_name_(addr *ret, unicode c)
{
	addr table;
	table = LispRoot(CHAR_NAME);
	return findnil_unicode_hashtable_(table, c, ret);
}

int findtable_char_name_(addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_CHARACTER);
	return findtable_unicode_name_(ret, RefCharacter(pos));
}

int findtable_name_char_(addr *ret, addr name)
{
	addr table;

	Check(! stringp(name), "type error");
	table = LispRoot(NAME_CHAR);
	return findnil_hashtable_(table, name, ret);
}

static int unicode_code_p(unicode c)
{
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

static int unicode_code_(addr name, size_t size, unicode *value, int *ret)
{
	char buffer[16];
	unsigned long ul;
	unicode c;
	size_t i, k;

	if (size < 2)
		return Result(ret, 1);
	Return(string_getc_(name, 0, &c));
	if (toUpperUnicode(c) != 'U')
		return Result(ret, 1);
	if (12 < size)
		return Result(ret, 1);
	for (k = 0, i = 1; i < size; i++) {
		Return(string_getc_(name, i, &c));
		if (! unicode_code_p(c))
			return Result(ret, 1);
		buffer[k++] = (char)c;
	}
	buffer[k] = 0;
	errno = 0;
	ul = strtoul(buffer, NULL, 16);
	if (errno == ERANGE)
		return Result(ret, 1);
	if (0xFFFFFFFFUL < ul)
		return Result(ret, 1);
	*value = (unicode)(0xFFFFFFFFUL & ul);

	return Result(ret, 0);
}

int find_name_char_(addr *ret, addr name)
{
	int check;
	unicode c;
	size_t size;

	if (symbolp(name)) {
		GetNameSymbol(name, &name);
	}
	Check(! stringp(name), "type error");
	string_length(name, &size);
	if (size == 0)
		return Result(ret, Nil);
	if (size == 1) {
		/* form #\a */
		Return(string_getc_(name, 0, &c));
		character_heap(ret, c);
		return 0;
	}
	Return(unicode_code_(name, size, &c, &check));
	if (! check) {
		/* for #\u123 */
		character_heap(ret, c);
		return 0;
	}

	return findtable_name_char_(ret, name);
}


/*
 *  build
 */
static int defnametable_(addr getname, addr getchar, unicode c, const char *name)
{
	int check;
	addr pos, string, cons;

	character_heap(&pos, c);
	strvect_char_heap(&string, name);
	Return(internp_hashheap_(getname, pos, &cons, &check));
	if (check == 0) {
		/* not found */
		SetCdr(cons, string);
	}
	Return(internp_hashheap_(getchar, string, &cons, &check));
	if (check == 0) {
		/* not found */
		SetCdr(cons, pos);
	}

	return 0;
}

static int defnametable_group_(addr getname, addr getchar)
{
	Return(defnametable_(getname, getchar, 0x07, "Bell"));
	Return(defnametable_(getname, getchar, 0x08, "Backspace"));
	Return(defnametable_(getname, getchar, 0x09, "Tab"));
	Return(defnametable_(getname, getchar, 0x0A, "Newline"));
	Return(defnametable_(getname, getchar, 0x0A, "Linefeed"));
	Return(defnametable_(getname, getchar, 0x0C, "Page"));
	Return(defnametable_(getname, getchar, 0x0D, "Return"));
	Return(defnametable_(getname, getchar, 0x20, "Space"));
	Return(defnametable_(getname, getchar, 0x7F, "Rubout"));
	return 0;
}

void build_character_name(void)
{
	addr getname, getchar;

	hashtable_heap(&getname);
	hashtable_heap(&getchar);
	settest_hashtable(getname, HASHTABLE_TEST_EQL);
	settest_hashtable(getchar, HASHTABLE_TEST_EQUALP);
	Error(defnametable_group_(getname, getchar));
	SetLispRoot(CHAR_NAME, getname);
	SetLispRoot(NAME_CHAR, getchar);
}

