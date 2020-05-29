#include "character_name.c"
#include "character.h"
#include "constant.h"
#include "degrade.h"

static int test_defnametable(void)
{
	int check;
	addr getname, getchar, pos;

	hashtable_heap(&getname);
	hashtable_heap(&getchar);
	settest_hashtable(getname, HASHTABLE_TEST_EQL);
	settest_hashtable(getchar, HASHTABLE_TEST_EQUALP);
	defnametable(getname, getchar, 0x08, "Backspace");
	defnametable(getname, getchar, 0x09, "Tab");
	defnametable(getname, getchar, 0x08, "Backspace");

	check = findvalue_unicode_hashtable(getname, 0x08, &pos);
	test(check, "defnametable.1");
	test(string_equalp_char(pos, "Backspace"), "defnametable.2");

	check = findvalue_unicode_hashtable(getname, 0x09, &pos);
	test(check, "defnametable.3");
	test(string_equalp_char(pos, "Tab"), "defnametable.4");

	check = findvalue_unicode_hashtable(getname, 0xFF, &pos);
	test(check == 0, "defnametable.5");

	check = findvalue_char_hashtable(getchar, "baCKspaCE", &pos);
	test(check, "defnametable.6");
	test(RefCharacter(pos) == 0x08, "defnametable.7");

	check = findvalue_char_hashtable(getchar, "Hello", &pos);
	test(check == 0, "defnametable.8");

	RETURN;
}

static int test_findtable_unicode_name(void)
{
	int check;
	addr pos;

	check = findtable_unicode_name(&pos, 0x0C);
	test(check, "findtable_unicode_name.1");
	test(string_equalp_char(pos, "Page"), "findtable_unicode_name.2");

	check = findtable_unicode_name(&pos, 0xF0000000UL);
	test(check == 0, "findtable_unicode_name.3");

	RETURN;
}

static int test_findtable_char_name(void)
{
	int check;
	addr pos;

	character_heap(&pos, 0x0C);
	check = findtable_char_name(&pos, pos);
	test(check, "findtable_char_name.1");
	test(string_equalp_char(pos, "Page"), "findtable_char_name.2");

	character_heap(&pos, 0xF0000000UL);
	check = findtable_char_name(&pos, pos);
	test(check == 0, "findtable_char_name.3");

	RETURN;
}

static int test_findtable_name_char(void)
{
	int check;
	addr pos;

	strvect_char_heap(&pos, "space");
	check = findtable_name_char(&pos, pos);
	test(check, "findtable_name_char.1");
	test(RefCharacter(pos) == 0x20, "findtable_name_char.2");

	strvect_char_heap(&pos, "Hello");
	check = findtable_name_char(&pos, pos);
	test(check == 0, "findtable_name_char.3");

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
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code.1");

	strvect_char_heap(&pos, "a10");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code.2");

	strvect_char_heap(&pos, "u1000000000000000000000");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code.3");

	strvect_char_heap(&pos, "u99999999999");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code.4");

	strvect_char_heap(&pos, "u65");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check == 0 && u == 0x65, "unicode_code.5");

	strvect_char_heap(&pos, "uabc");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check == 0 && u == 0x0ABC, "unicode_code.6");

	RETURN;
}

static int test_find_name_char(void)
{
	int check;
	addr pos, name;

	strvect_heap(&pos, 0);
	check = find_name_char(&pos, pos);
	test(check == 0, "find_name_char.1");

	strvect_char_heap(&pos, "e");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 'e', "find_name_char.2");

	strvect_char_heap(&pos, "U32");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x32, "find_name_char.3");

	strvect_char_heap(&pos, "backspace");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x08, "find_name_char.4");

	strvect_char_heap(&name, "SPACE");
	symbol_heap(&pos);
	SetNameSymbol(pos, name);
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x20, "find_name_char.5");

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

