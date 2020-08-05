#include "character_name.c"
#include "character.h"
#include "constant.h"
#include "degrade.h"

static int test_defnametable(void)
{
	addr getname, getchar, pos;

	hashtable_heap(&getname);
	hashtable_heap(&getchar);
	settest_hashtable(getname, HASHTABLE_TEST_EQL);
	settest_hashtable(getchar, HASHTABLE_TEST_EQUALP);
	defnametable_(getname, getchar, 0x08, "Backspace");
	defnametable_(getname, getchar, 0x09, "Tab");
	defnametable_(getname, getchar, 0x08, "Backspace");

	find_unicode_hashtable_(getname, 0x08, &pos);
	test(pos != Unbound, "defnametable.1");
	test(string_equalp_char_debug(pos, "Backspace"), "defnametable.2");

	find_unicode_hashtable_(getname, 0x09, &pos);
	test(pos != Unbound, "defnametable.3");
	test(string_equalp_char_debug(pos, "Tab"), "defnametable.4");

	find_unicode_hashtable_(getname, 0xFF, &pos);
	test(pos == Unbound, "defnametable.5");

	find_char_hashtable_(getchar, "baCKspaCE", &pos);
	test(pos != Unbound, "defnametable.6");
	test(RefCharacter(pos) == 0x08, "defnametable.7");

	find_char_hashtable_(getchar, "Hello", &pos);
	test(pos == Unbound, "defnametable.8");

	RETURN;
}

static int test_findtable_unicode_name(void)
{
	addr pos;

	findtable_unicode_name_(&pos, 0x0C);
	test(pos != Nil, "findtable_unicode_name.1");
	test(string_equalp_char_debug(pos, "Page"), "findtable_unicode_name.2");

	findtable_unicode_name_(&pos, 0xF0000000UL);
	test(pos == Nil, "findtable_unicode_name.3");

	RETURN;
}

static int test_findtable_char_name(void)
{
	addr pos;

	character_heap(&pos, 0x0C);
	findtable_char_name_(&pos, pos);
	test(pos != Nil, "findtable_char_name.1");
	test(string_equalp_char_debug(pos, "Page"), "findtable_char_name.2");

	character_heap(&pos, 0xF0000000UL);
	findtable_char_name_(&pos, pos);
	test(pos == Nil, "findtable_char_name.3");

	RETURN;
}

static int test_findtable_name_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "space");
	findtable_name_char_(&pos, pos);
	test(pos != Nil, "findtable_name_char.1");
	test(RefCharacter(pos) == 0x20, "findtable_name_char.2");

	strvect_char_heap(&pos, "Hello");
	findtable_name_char_(&pos, pos);
	test(pos == Nil, "findtable_name_char.3");

	RETURN;
}

static int test_unicode_code(void)
{
	int check;
	unicode u;
	addr pos;
	size_t size;

	strvect_char_heap(&pos, "u");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check, "unicode_code.1");

	strvect_char_heap(&pos, "a10");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check, "unicode_code.2");

	strvect_char_heap(&pos, "u1000000000000000000000");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check, "unicode_code.3");

	strvect_char_heap(&pos, "u99999999999");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check, "unicode_code.4");

	strvect_char_heap(&pos, "u65");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check == 0 && u == 0x65, "unicode_code.5");

	strvect_char_heap(&pos, "uabc");
	strvect_length(pos, &size);
	unicode_code_(pos, size, &u, &check);
	test(check == 0 && u == 0x0ABC, "unicode_code.6");

	RETURN;
}

static int test_find_name_char(void)
{
	addr pos, name;

	strvect_heap(&pos, 0);
	find_name_char_(&pos, pos);
	test(pos == Nil, "find_name_char.1");

	strvect_char_heap(&pos, "e");
	find_name_char_(&pos, pos);
	test(pos != Nil && RefCharacter(pos) == 'e', "find_name_char.2");

	strvect_char_heap(&pos, "U32");
	find_name_char_(&pos, pos);
	test(pos != Nil && RefCharacter(pos) == 0x32, "find_name_char.3");

	strvect_char_heap(&pos, "backspace");
	find_name_char_(&pos, pos);
	test(pos != Nil && RefCharacter(pos) == 0x08, "find_name_char.4");

	strvect_char_heap(&name, "SPACE");
	symbol_heap(&pos);
	SetNameSymbol(pos, name);
	find_name_char_(&pos, pos);
	test(pos != Nil && RefCharacter(pos) == 0x20, "find_name_char.5");

	RETURN;
}


/*
 *  character_name
 */
static int testcase_character_name(void)
{
	TestBreak(test_defnametable);
	TestBreak(test_findtable_unicode_name);
	TestBreak(test_findtable_char_name);
	TestBreak(test_findtable_name_char);
	TestBreak(test_unicode_code);
	TestBreak(test_find_name_char);

	return 0;
}

static void testinit_character_name(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
}

int test_character_name(void)
{
	TITLE;
	return degrade_code(
			testinit_character_name,
			testcase_character_name);
}

