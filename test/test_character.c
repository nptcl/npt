#include "character.c"
#include "constant.h"
#include "degrade.h"

static int test_character_access(void)
{
	addr pos;
	unicode c;
	const unicode *ptr;

	make_character_heap(&pos, 'A');
	ptr = PtrCharacter_Low(pos);
	test(*ptr == 'A', "PtrCharacter_Low.1");
	test(RefCharacter_Low(pos) == 'A', "RefCharacter_Low.1");
	SetCharacter_Low(pos, 'a');
	test(RefCharacter_Low(pos) == 'a', "SetCharacter_Low.1");
	GetCharacter_Low(pos, &c);
	test(c == 'a', "GetCharacter_Low.1");
	test(RefCharacter(pos) == 'a', "RefCharacter.1");
	SetCharacter_unsafe(pos, 'z');
	test(RefCharacter(pos) == 'z', "SetCharacter.1");
	GetCharacter(pos, &c);
	test(c == 'z', "GetCharacter.1");

	RETURN;
}

static int test_character_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	character_alloc(NULL, &pos, 'a');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc.1");
	test(RefCharacter(pos) == 'a', "character_alloc.2");
	test(ref_character_type(pos) == CHARACTER_TYPE_STANDARD, "character_alloc.3");
	test(lenbodyr(pos) == sizeoft(unicode), "character_alloc.4");
	test(! GetStatusDynamic(pos), "character_alloc.5");

	local = Local_Thread;
	push_local(local, &stack);
	character_local(local, &pos, 'b');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc.6");
	test(RefCharacter(pos) == 'b', "character_alloc.7");
	test(GetStatusDynamic(pos), "character_alloc.8");
	rollback_local(local, stack);

	character_heap(&pos, 'c');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc.9");
	test(RefCharacter(pos) == 'c', "character_alloc.10");
	test(! GetStatusDynamic(pos), "character_alloc.11");

	pos = characterh('d');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc.12");
	test(RefCharacter(pos) == 'd', "character_alloc.13");
	test(! GetStatusDynamic(pos), "character_alloc.14");

	RETURN;
}

static int test_ptrcharacter(void)
{
	addr pos;
	unicode u;

	character_heap(&pos, 'a');
	test(*ptrcharacter(pos) == 'a', "ptrcharacter.1");
	test(refcharacter(pos) == 'a', "refcharacter.1");
	getcharacter(pos, &u);
	test(u == 'a', "getcharacter.1");

	RETURN;
}

static int test_setcharacter_unsafe(void)
{
	addr pos;

	make_character_heap(&pos, 0xFF);
	setcharacter_unsafe(pos, 'A');
	test(RefCharacter(pos) == 'A', "setcharacter_unsafe.1");
	test(ref_character_type(pos) == CHARACTER_TYPE_STANDARD, "setcharacter_unsafe.2");
	setcharacter_unsafe(pos, 0x100000);
	test(RefCharacter(pos) == 0x100000, "setcharacter_unsafe.3");
	test(ref_character_type(pos) == CHARACTER_TYPE_BASE, "setcharacter_unsafe.4");

	RETURN;
}

static int test_character_type(void)
{
	test(character_type('a') == CHARACTER_TYPE_STANDARD, "character_type.1");
	test(character_type(' ') == CHARACTER_TYPE_STANDARD, "character_type.2");
	test(character_type('$') == CHARACTER_TYPE_STANDARD, "character_type.3");
	test(character_type(0x0A) == CHARACTER_TYPE_STANDARD, "character_type.4");
	test(character_type(0x0D) == CHARACTER_TYPE_BASE, "character_type.5");
	test(character_type(0xD7FF) == CHARACTER_TYPE_BASE, "character_type.6");
	test(character_type(0xD800) == CHARACTER_TYPE_INVALID, "character_type.7");
	test(character_type(0xDFFF) == CHARACTER_TYPE_INVALID, "character_type.8");
	test(character_type(0xE000) == CHARACTER_TYPE_BASE, "character_type.9");
	test(character_type(0xFFFF) == CHARACTER_TYPE_BASE, "character_type.10");
	test(character_type(0x010000) == CHARACTER_TYPE_BASE, "character_type.11");
	test(character_type(0x010FFFF) == CHARACTER_TYPE_BASE, "character_type.12");
	test(character_type(0x0110000) == CHARACTER_TYPE_INVALID, "character_type.13");
	test(character_type(0x80000000) == CHARACTER_TYPE_EXTENDED, "character_type.14");
	test(character_type(0x80002000) == CHARACTER_TYPE_EXTENDED, "character_type.15");

	RETURN;
}

static int test_standard_char_p(void)
{
	addr x, y, z;

	make_character_heap(&x, 'A');
	make_character_heap(&y, 0x0D);
	make_character_heap(&z, 0x80003000);

	test(standard_char_p(x), "standard_char_p.1");
	test(! standard_char_p(y), "standard_char_p.2");
	test(! standard_char_p(z), "standard_char_p.3");

	test(base_char_p(x), "base_char_p.1");
	test(base_char_p(y), "base_char_p.2");
	test(! base_char_p(z), "base_char_p.3");

	test(! extended_char_p(x), "extended_char_p.1");
	test(! extended_char_p(y), "extended_char_p.2");
	test(extended_char_p(z), "extended_char_p.3");

	test(characterp(x), "characterp.1");
	test(characterp(y), "characterp.2");
	test(characterp(z), "characterp.3");
	test(! characterp(Nil), "characterp.4");
	test(! characterp(T), "characterp.5");

	RETURN;
}

static int test_unicode_equalp(void)
{
	test(unicode_equalp('A', 'a'), "unicode_equalp.1");
	test(unicode_equalp('z', 'Z'), "unicode_equalp.2");
	test(unicode_equalp('b', 'b'), "unicode_equalp.3");
	test(unicode_equalp('C', 'C'), "unicode_equalp.4");
	test(! unicode_equalp('a', '8'), "unicode_equalp.5");
	test(! unicode_equalp('A', '8'), "unicode_equalp.6");
	test(! unicode_equalp(0x10, 0x4000), "unicode_equalp.7");

	RETURN;
}

static int test_unicode_comparep(void)
{
	test(unicode_comparep('A', 'a') == 0, "unicode_comparep.1");
	test(unicode_comparep('z', 'Z') == 0, "unicode_comparep.2");
	test(unicode_comparep('b', 'b') == 0, "unicode_comparep.3");
	test(unicode_comparep('C', 'C') == 0, "unicode_comparep.4");
	test(unicode_comparep('a', 'b') < 0, "unicode_comparep.5");
	test(unicode_comparep('A', 'B') < 0, "unicode_comparep.6");
	test(unicode_comparep('a', 'B') < 0, "unicode_comparep.7");
	test(unicode_comparep('A', 'b') < 0, "unicode_comparep.8");
	test(unicode_comparep('z', 'A') > 0, "unicode_comparep.9");
	test(unicode_comparep('*', 0x1000) < 0, "unicode_comparep.10");

	RETURN;
}

static int test_character_equal(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_equal(left, right), "character_equal.1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'A');
	test(! character_equal(left, right), "character_equal.2");

	SetCharacter_unsafe(left, '*');
	SetCharacter_unsafe(right, 'Z');
	test(! character_equal(left, right), "character_equal.3");

	RETURN;
}

static int test_character_equalp(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_equalp(left, right), "character_equalp.1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'A');
	test(character_equalp(left, right), "character_equalp.2");

	SetCharacter_unsafe(left, '*');
	SetCharacter_unsafe(right, 'Z');
	test(! character_equalp(left, right), "character_equalp.3");

	RETURN;
}

static int test_character_compare(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_compare(left, right) == 0, "character_compare.1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'z');
	test(character_compare(left, right) < 0, "character_compare.2");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'c');
	test(character_compare(left, right) > 0, "character_compare.3");

	RETURN;
}

static int test_character_comparep(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'a');
	test(character_comparep(left, right) == 0, "character_comparep.1");

	SetCharacter_unsafe(left, 'A');
	SetCharacter_unsafe(right, 'z');
	test(character_comparep(left, right) < 0, "character_comparep.2");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'C');
	test(character_comparep(left, right) > 0, "character_comparep.3");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'c');
	test(character_comparep(left, right) > 0, "character_comparep.4");

	RETURN;
}

static int test_character_unicode_equal(void)
{
	addr left;

	character_heap(&left, 'a');
	test(character_unicode_equal(left, 'a'), "character_unicode_equal.1");
	test(! character_unicode_equal(left, 'b'), "character_unicode_equal.2");
	test(! character_unicode_equal(left, 'A'), "character_unicode_equal.3");

	RETURN;
}

static int test_character_unicode_equalp(void)
{
	addr left;

	character_heap(&left, 'a');
	test(character_unicode_equalp(left, 'a'), "character_unicode_equalp.1");
	test(! character_unicode_equalp(left, 'b'), "character_unicode_equalp.2");
	test(character_unicode_equalp(left, 'A'), "character_unicode_equalp.3");

	RETURN;
}

static int test_character_unicode_compare(void)
{
	addr left;

	character_heap(&left, 'g');
	test(character_unicode_compare(left, 'g') == 0, "character_unicode_compare.1");
	test(character_unicode_compare(left, 'a') > 0, "character_unicode_compare.2");
	test(character_unicode_compare(left, 'z') < 0, "character_unicode_compare.3");
	test(character_unicode_compare(left, 'G') != 0, "character_unicode_compare.4");

	RETURN;
}

static int test_character_unicode_comparep(void)
{
	addr left;

	character_heap(&left, 'g');
	test(character_unicode_comparep(left, 'g') == 0, "character_unicode_comparep.1");
	test(character_unicode_comparep(left, 'a') > 0, "character_unicode_comparep.2");
	test(character_unicode_comparep(left, 'z') < 0, "character_unicode_comparep.3");
	test(character_unicode_comparep(left, 'G') == 0, "character_unicode_comparep.4");
	test(character_unicode_comparep(left, 'A') > 0, "character_unicode_comparep.5");
	test(character_unicode_comparep(left, 'Z') < 0, "character_unicode_comparep.6");

	RETURN;
}


/*
 *  character
 */
static int testcase_character(void)
{
	TestBreak(test_character_access);
	TestBreak(test_character_alloc);
	TestBreak(test_ptrcharacter);
	TestBreak(test_setcharacter_unsafe);
	TestBreak(test_character_type);
	TestBreak(test_standard_char_p);
	TestBreak(test_unicode_equalp);
	TestBreak(test_unicode_comparep);
	TestBreak(test_character_equal);
	TestBreak(test_character_equalp);
	TestBreak(test_character_compare);
	TestBreak(test_character_comparep);
	TestBreak(test_character_unicode_equal);
	TestBreak(test_character_unicode_equalp);
	TestBreak(test_character_unicode_compare);
	TestBreak(test_character_unicode_comparep);

	return 0;
}

static void testinit_character(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
}

int test_character(void)
{
	DegradeTitle;
	return DegradeCode(character);
}

