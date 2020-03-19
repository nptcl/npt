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
	test(check, "defnametable1");
	test(string_equalp_char(pos, "Backspace"), "defnametable2");

	check = findvalue_unicode_hashtable(getname, 0x09, &pos);
	test(check, "defnametable3");
	test(string_equalp_char(pos, "Tab"), "defnametable4");

	check = findvalue_unicode_hashtable(getname, 0xFF, &pos);
	test(check == 0, "defnametable5");

	check = findvalue_char_hashtable(getchar, "baCKspaCE", &pos);
	test(check, "defnametable6");
	test(RefCharacter(pos) == 0x08, "defnametable7");

	check = findvalue_char_hashtable(getchar, "Hello", &pos);
	test(check == 0, "defnametable8");

	RETURN;
}

static int test_findtable_unicode_name(void)
{
	int check;
	addr pos;

	check = findtable_unicode_name(&pos, 0x0C);
	test(check, "findtable_unicode_name1");
	test(string_equalp_char(pos, "Page"), "findtable_unicode_name2");

	check = findtable_unicode_name(&pos, 0xF0000000UL);
	test(check == 0, "findtable_unicode_name3");

	RETURN;
}

static int test_findtable_char_name(void)
{
	int check;
	addr pos;

	character_heap(&pos, 0x0C);
	check = findtable_char_name(&pos, pos);
	test(check, "findtable_char_name1");
	test(string_equalp_char(pos, "Page"), "findtable_char_name2");

	character_heap(&pos, 0xF0000000UL);
	check = findtable_char_name(&pos, pos);
	test(check == 0, "findtable_char_name3");

	RETURN;
}

static int test_findtable_name_char(void)
{
	int check;
	addr pos;

	strvect_char_heap(&pos, "space");
	check = findtable_name_char(&pos, pos);
	test(check, "findtable_name_char1");
	test(RefCharacter(pos) == 0x20, "findtable_name_char2");

	strvect_char_heap(&pos, "Hello");
	check = findtable_name_char(&pos, pos);
	test(check == 0, "findtable_name_char3");

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
	test(check, "unicode_code1");

	strvect_char_heap(&pos, "a10");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code2");

	strvect_char_heap(&pos, "u1000000000000000000000");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code3");

	strvect_char_heap(&pos, "u99999999999");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check, "unicode_code4");

	strvect_char_heap(&pos, "u65");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check == 0 && u == 0x65, "unicode_code5");

	strvect_char_heap(&pos, "uabc");
	strvect_length(pos, &size);
	check = unicode_code(pos, size, &u);
	test(check == 0 && u == 0x0ABC, "unicode_code6");

	RETURN;
}

static int test_find_name_char(void)
{
	int check;
	addr pos, name;

	strvect_heap(&pos, 0);
	check = find_name_char(&pos, pos);
	test(check == 0, "find_name_char1");

	strvect_char_heap(&pos, "e");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 'e', "find_name_char2");

	strvect_char_heap(&pos, "U32");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x32, "find_name_char3");

	strvect_char_heap(&pos, "backspace");
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x08, "find_name_char4");

	strvect_char_heap(&name, "SPACE");
	symbol_heap(&pos);
	SetNameSymbol(pos, name);
	check = find_name_char(&pos, pos);
	test(check && RefCharacter(pos) == 0x20, "find_name_char5");

	RETURN;
}


/*
 *  main
 */
static int testbreak_character_name(void)
{
	TestBreak(test_defnametable);
	TestBreak(test_findtable_unicode_name);
	TestBreak(test_findtable_char_name);
	TestBreak(test_findtable_name_char);
	TestBreak(test_unicode_code);
	TestBreak(test_find_name_char);

	return 0;
}

int test_character_name(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		lisp_initialize = 1;
		result = testbreak_character_name();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}
