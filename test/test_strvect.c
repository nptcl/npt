#include "strvect.c"
#include "degrade.h"

/*
 *  character
 */
static int test_unicode_macro(void)
{
	enum CHARACTER_TYPE type;
	addr pos;
	unicode u;
	const unicode *ptr;
	size_t size;

	make_character_heap(&pos, 'A');
	ptr = PtrCharacter_Low(pos);
	test(*ptr == 'A', "PtrCharacter_Low1");
	test(RefCharacter_Low(pos) == 'A', "RefCharacter_Low1");
	SetCharacter_Low(pos, 'a');
	test(RefCharacter_Low(pos) == 'a', "SetCharacter_Low1");
	GetCharacter_Low(pos, &u);
	test(u == 'a', "GetCharacter_Low1");
	test(RefCharacter(pos) == 'a', "RefCharacter1");
	SetCharacter_unsafe(pos, 'z');
	test(RefCharacter(pos) == 'z', "SetCharacter1");
	GetCharacter(pos, &u);
	test(u == 'z', "GetCharacter1");

	test(StringBodyLength(5) == IdxSize + sizeoft(unicode) * 5, "StringBodyLength1");
	strvect_heap(&pos, 10);
	test(PtrStringBase(pos) == posbodyr(pos), "PtrStringBase1");
	test(*(size_t *)PtrStringBase(pos) == 10, "PtrStringBase2");
	test(*PtrStringSize(pos) == 10, "PtrStringSize1");
	test(RefStringSize(pos) == 10, "RefStringSize1");
	SetStringSize(pos, 5);
	test(RefStringSize(pos) == 5, "SetStringSize1");
	GetStringSize(pos, &size);
	test(size == 5, "GetStringSize1");
	ptr = PtrStringUnicode(pos);
	test(ptr == (const unicode *)(posbodyr(pos) + IdxSize), "PtrStringUnicode1");
	GetStringUnicode(pos, &ptr);
	test(ptr == (const unicode *)(posbodyr(pos) + IdxSize), "GetStringUnicode1");

	SetUser(pos, (byte)CHARACTER_TYPE_EMPTY);
	test(RefCharacterType(pos) == CHARACTER_TYPE_EMPTY, "RefCharacter1");
	SetCharacterType(pos, CHARACTER_TYPE_INVALID);
	test(RefCharacterType(pos) == CHARACTER_TYPE_INVALID, "SetCharacterType1");
	GetCharacterType(pos, &type);
	test(type == CHARACTER_TYPE_INVALID, "GetCharacterType1");

	RETURN;
}


/*
 *  character
 */
static int test_character_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	character_alloc(NULL, &pos, 'a');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc1");
	test(RefCharacter(pos) == 'a', "character_alloc2");
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD, "character_alloc3");
	test(lenbodyr(pos) == sizeoft(unicode), "character_alloc4");
	test(! GetStatusDynamic(pos), "character_alloc5");

	local = Local_Thread;
	push_local(local, &stack);
	character_local(local, &pos, 'b');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc6");
	test(RefCharacter(pos) == 'b', "character_alloc7");
	test(GetStatusDynamic(pos), "character_alloc8");
	rollback_local(local, stack);

	character_heap(&pos, 'c');
	test(GetType(pos) == LISPTYPE_CHARACTER, "character_alloc9");
	test(RefCharacter(pos) == 'c', "character_alloc10");
	test(! GetStatusDynamic(pos), "character_alloc11");

	RETURN;
}

static int test_ptrcharacter(void)
{
	addr pos;
	unicode u;

	character_heap(&pos, 'a');
	test(*ptrcharacter(pos) == 'a', "ptrcharacter1");
	test(refcharacter(pos) == 'a', "refcharacter1");
	getcharacter(pos, &u);
	test(u == 'a', "getcharacter1");

	RETURN;
}

static int test_character_type(void)
{
	test(character_type('a') == CHARACTER_TYPE_STANDARD, "character_type1");
	test(character_type(' ') == CHARACTER_TYPE_STANDARD, "character_type2");
	test(character_type('$') == CHARACTER_TYPE_STANDARD, "character_type3");
	test(character_type(0x0A) == CHARACTER_TYPE_STANDARD, "character_type4");
	test(character_type(0x0D) == CHARACTER_TYPE_BASE, "character_type5");
	test(character_type(0xD7FF) == CHARACTER_TYPE_BASE, "character_type6");
	test(character_type(0xD800) == CHARACTER_TYPE_INVALID, "character_type7");
	test(character_type(0xDFFF) == CHARACTER_TYPE_INVALID, "character_type8");
	test(character_type(0xE000) == CHARACTER_TYPE_BASE, "character_type9");
	test(character_type(0xFFFF) == CHARACTER_TYPE_BASE, "character_type10");
	test(character_type(0x010000) == CHARACTER_TYPE_BASE, "character_type11");
	test(character_type(0x010FFFF) == CHARACTER_TYPE_BASE, "character_type12");
	test(character_type(0x0110000) == CHARACTER_TYPE_INVALID, "character_type13");
	test(character_type(0x80000000) == CHARACTER_TYPE_EXTENDED, "character_type14");
	test(character_type(0x80002000) == CHARACTER_TYPE_EXTENDED, "character_type15");

	RETURN;
}

static int test_setcharacter_unsafe(void)
{
	addr pos;

	make_character_heap(&pos, 0xFF);
	setcharacter_unsafe(pos, 'A');
	test(RefCharacter(pos) == 'A', "setcharacter_unsafe1");
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD, "setcharacter_unsafe2");
	setcharacter_unsafe(pos, 0x100000);
	test(RefCharacter(pos) == 0x100000, "setcharacter_unsafe3");
	test(RefCharacterType(pos) == CHARACTER_TYPE_BASE, "setcharacter_unsafe4");

	RETURN;
}

static int test_standard_char_p(void)
{
	addr pos;

	make_character_heap(&pos, 'A');
	test(standard_char_p(pos), "standard_char_p1");
	test(base_char_p(pos), "base_char_p1");
	test(! extended_char_p(pos), "extended_char_p1");

	SetCharacter_unsafe(pos, 0x0D);
	test(! standard_char_p(pos), "standard_char_p2");
	test(base_char_p(pos), "base_char_p2");
	test(! extended_char_p(pos), "extended_char_p2");

	SetCharacter_unsafe(pos, 0x80003000);
	test(! standard_char_p(pos), "standard_char_p3");
	test(! base_char_p(pos), "base_char_p3");
	test(extended_char_p(pos), "extended_char_p3");

	RETURN;
}

static int test_unicode_equalp(void)
{
	test(unicode_equalp('A', 'a'), "unicode_equalp1");
	test(unicode_equalp('z', 'Z'), "unicode_equalp2");
	test(unicode_equalp('b', 'b'), "unicode_equalp3");
	test(unicode_equalp('C', 'C'), "unicode_equalp4");
	test(! unicode_equalp('a', '8'), "unicode_equalp5");
	test(! unicode_equalp('A', '8'), "unicode_equalp6");
	test(! unicode_equalp(0x10, 0x4000), "unicode_equalp7");

	RETURN;
}

static int test_unicode_comparep(void)
{
	test(unicode_comparep('A', 'a') == 0, "unicode_comparep1");
	test(unicode_comparep('z', 'Z') == 0, "unicode_comparep2");
	test(unicode_comparep('b', 'b') == 0, "unicode_comparep3");
	test(unicode_comparep('C', 'C') == 0, "unicode_comparep4");
	test(unicode_comparep('a', 'b') < 0, "unicode_comparep5");
	test(unicode_comparep('A', 'B') < 0, "unicode_comparep6");
	test(unicode_comparep('a', 'B') < 0, "unicode_comparep7");
	test(unicode_comparep('A', 'b') < 0, "unicode_comparep8");
	test(unicode_comparep('z', 'A') > 0, "unicode_comparep9");
	test(unicode_comparep('*', 0x1000) < 0, "unicode_comparep10");

	RETURN;
}

static int test_character_equal(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_equal(left, right), "character_equal1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'A');
	test(! character_equal(left, right), "character_equal2");

	SetCharacter_unsafe(left, '*');
	SetCharacter_unsafe(right, 'Z');
	test(! character_equal(left, right), "character_equal3");

	RETURN;
}

static int test_character_equalp(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_equalp(left, right), "character_equalp1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'A');
	test(character_equalp(left, right), "character_equalp2");

	SetCharacter_unsafe(left, '*');
	SetCharacter_unsafe(right, 'Z');
	test(! character_equalp(left, right), "character_equalp3");

	RETURN;
}

static int test_character_compare(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'A');
	test(character_compare(left, right) == 0, "character_compare1");

	SetCharacter_unsafe(left, 'a');
	SetCharacter_unsafe(right, 'z');
	test(character_compare(left, right) < 0, "character_compare2");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'c');
	test(character_compare(left, right) > 0, "character_compare3");

	RETURN;
}

static int test_character_comparep(void)
{
	addr left, right;

	make_character_heap(&left, 'A');
	make_character_heap(&right, 'a');
	test(character_comparep(left, right) == 0, "character_comparep1");

	SetCharacter_unsafe(left, 'A');
	SetCharacter_unsafe(right, 'z');
	test(character_comparep(left, right) < 0, "character_comparep2");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'C');
	test(character_comparep(left, right) > 0, "character_comparep3");

	SetCharacter_unsafe(left, 'g');
	SetCharacter_unsafe(right, 'c');
	test(character_comparep(left, right) > 0, "character_comparep4");

	RETURN;
}

static int test_character_unicode_equal(void)
{
	addr left;

	character_heap(&left, 'a');
	test(character_unicode_equal(left, 'a'), "character_unicode_equal1");
	test(! character_unicode_equal(left, 'b'), "character_unicode_equal2");
	test(! character_unicode_equal(left, 'A'), "character_unicode_equal3");

	RETURN;
}

static int test_character_unicode_equalp(void)
{
	addr left;

	character_heap(&left, 'a');
	test(character_unicode_equalp(left, 'a'), "character_unicode_equalp1");
	test(! character_unicode_equalp(left, 'b'), "character_unicode_equalp2");
	test(character_unicode_equalp(left, 'A'), "character_unicode_equalp3");

	RETURN;
}

static int test_character_unicode_compare(void)
{
	addr left;

	character_heap(&left, 'g');
	test(character_unicode_compare(left, 'g') == 0, "character_unicode_compare1");
	test(character_unicode_compare(left, 'a') > 0, "character_unicode_compare2");
	test(character_unicode_compare(left, 'z') < 0, "character_unicode_compare3");
	test(character_unicode_compare(left, 'G') != 0, "character_unicode_compare4");

	RETURN;
}

static int test_character_unicode_comparep(void)
{
	addr left;

	character_heap(&left, 'g');
	test(character_unicode_comparep(left, 'g') == 0, "character_unicode_comparep1");
	test(character_unicode_comparep(left, 'a') > 0, "character_unicode_comparep2");
	test(character_unicode_comparep(left, 'z') < 0, "character_unicode_comparep3");
	test(character_unicode_comparep(left, 'G') == 0, "character_unicode_comparep4");
	test(character_unicode_comparep(left, 'A') > 0, "character_unicode_comparep5");
	test(character_unicode_comparep(left, 'Z') < 0, "character_unicode_comparep6");

	RETURN;
}


/*
 *  buffer compare
 */
static int test_memu_equal(void)
{
	unicode left[10], right[10];

	test(! memu_equal(left, right, 4, 5), "memu_equal1");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu_equal(left, right, 3, 3), "memu_equal2");
	test(! memu_equal(left, right, 4, 4), "memu_equal3");

	RETURN;
}

static int test_memu_compare(void)
{
	unicode left[10], right[10];

	test(memu_compare(left, right, 4, 5) < 0, "memu_compare1");
	test(memu_compare(left, right, 5, 4) > 0, "memu_compare2");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu_compare(left, right, 3, 3) == 0, "memu_compare3");
	test(memu_compare(left, right, 4, 4) < 0, "memu_compare4");

	RETURN;
}

static int test_memu_equalp(void)
{
	unicode left[10], right[10];

	test(! memu_equalp(left, right, 4, 5), "memu_equalp1");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu_equalp(left, right, 3, 3), "memu_equalp2");
	test(! memu_equalp(left, right, 4, 4), "memu_equalp3");

	RETURN;
}

static int test_memu_comparep(void)
{
	unicode left[10], right[10];

	test(memu_comparep(left, right, 4, 5) < 0, "memu_comparep1");
	test(memu_comparep(left, right, 5, 4) > 0, "memu_comparep2");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu_comparep(left, right, 3, 3) == 0, "memu_comparep3");
	test(memu_comparep(left, right, 4, 4) > 0, "memu_comparep4");

	RETURN;
}

static int test_memu1_equal(void)
{
	unicode left[10];
	byte right[10];

	test(! memu1_equal(left, right, 4, 5), "memu1_equal1");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu1_equal(left, right, 3, 3), "memu1_equal2");
	test(! memu1_equal(left, right, 4, 4), "memu1_equal3");

	RETURN;
}

static int test_memu1_compare(void)
{
	unicode left[10];
	byte right[10];

	test(memu1_compare(left, right, 4, 5) < 0, "memu1_compare1");
	test(memu1_compare(left, right, 5, 4) > 0, "memu1_compare2");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu1_compare(left, right, 3, 3) == 0, "memu1_compare3");
	test(memu1_compare(left, right, 4, 4) < 0, "memu1_compare4");

	RETURN;
}

static int test_memu1_equalp(void)
{
	unicode left[10];
	byte right[10];

	test(! memu1_equalp(left, right, 4, 5), "memu1_equalp1");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu1_equalp(left, right, 3, 3), "memu1_equalp2");
	test(! memu1_equalp(left, right, 4, 4), "memu1_equalp3");

	RETURN;
}

static int test_memu1_comparep(void)
{
	unicode left[10];
	byte right[10];

	test(memu1_comparep(left, right, 4, 5) < 0, "memu1_comparep1");
	test(memu1_comparep(left, right, 5, 4) > 0, "memu1_comparep2");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu1_comparep(left, right, 3, 3) == 0, "memu1_comparep3");
	test(memu1_comparep(left, right, 4, 4) > 0, "memu1_comparep4");

	RETURN;
}


/*
 *  strvect
 */
static int test_strvect_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	strvect_alloc(NULL, &pos, 10);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc1");
	test(RefStringSize(pos) == 10, "strvect_alloc2");
	test(RefCharacterType(pos) == CHARACTER_TYPE_EMPTY, "strvect_alloc3");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_local(local, &pos, 10);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc4");
	test(GetStatusDynamic(pos), "strvect_alloc5");
	rollback_local(local, stack);

	strvect_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc6");
	test(! GetStatusDynamic(pos), "strvect_alloc7");

	RETURN;
}

static int test_strvect_length(void)
{
	addr pos;
	size_t size;

	strvect_heap(&pos, 10);
	strvect_length(pos, &size);
	test(size == 10, "strvect_length1");

	RETURN;
}

static int test_strvect_posbodylen(void)
{
	addr pos;
	const unicode *body;
	size_t size;

	strvect_heap(&pos, 10);
	strvect_posbodylen(pos, &body, &size);
	test(size == 10, "strvect_posbodylen1");
	test(body == (const unicode *)(posbodyr(pos) + IdxSize), "strvect_posbodylen2");

	RETURN;
}

static int test_unicode_character_type(void)
{
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, '$')
			== CHARACTER_TYPE_STANDARD, "unicode_character_type1");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0xFF)
			== CHARACTER_TYPE_BASE, "unicode_character_type2");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0xF0000000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type3");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type4");

	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 'a')
			== CHARACTER_TYPE_STANDARD, "unicode_character_type5");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0)
			== CHARACTER_TYPE_BASE, "unicode_character_type6");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0x80001000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type7");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type8");

	test(unicode_character_type(CHARACTER_TYPE_BASE, 'z')
			== CHARACTER_TYPE_BASE, "unicode_character_type9");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0x0D)
			== CHARACTER_TYPE_BASE, "unicode_character_type10");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0x80000000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type11");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type12");

	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 'a')
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type13");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type14");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0x80000000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type15");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type16");

	test(unicode_character_type(CHARACTER_TYPE_INVALID, 'a')
			== CHARACTER_TYPE_INVALID, "unicode_character_type17");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0x0100)
			== CHARACTER_TYPE_INVALID, "unicode_character_type18");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0xE0000000)
			== CHARACTER_TYPE_INVALID, "unicode_character_type19");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type20");

	RETURN;
}

static int test_strvect_update_character_type(void)
{
	addr pos;
	unicode *body;

	strvect_heap(&pos, 3);
	GetStringUnicode(pos, &body);
	body[0] = 'a';
	body[1] = 0x0A;
	body[2] = '%';
	strvect_update_character_type(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD,
			"strvect_update_character_type1");

	body[0] = 'a';
	body[1] = 0xFF;
	body[2] = 0x0A;
	strvect_update_character_type(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_BASE,
			"strvect_update_character_type2");

	RETURN;
}

static int test_strvect_char_alloc(void)
{
	addr pos;
	unicode *body;
	LocalRoot local;
	LocalStack stack;

	strvect_char_alloc(NULL, &pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc1");
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD, "strvect_char_alloc2");
	test(RefStringSize(pos) == 5, "strvect_char_alloc3");
	GetStringUnicode(pos, &body);
	test(body[0] == 'H', "strvect_char_alloc4");
	test(body[4] == 'o', "strvect_char_alloc5");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc6");
	test(GetStatusDynamic(pos), "strvect_char_alloc7");
	rollback_local(local, stack);

	strvect_char_heap(&pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc8");
	test(! GetStatusDynamic(pos), "strvect_char_alloc9");

	RETURN;
}

static int test_strvect_size1_alloc(void)
{
	addr pos;
	unicode *body;
	LocalRoot local;
	LocalStack stack;

	strvect_size1_alloc(NULL, &pos, "Hello", 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_size1_alloc1");
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD, "strvect_size1_alloc2");
	test(RefStringSize(pos) == 5, "strvect_size1_alloc3");
	GetStringUnicode(pos, &body);
	test(body[0] == 'H', "strvect_size1_alloc4");
	test(body[4] == 'o', "strvect_size1_alloc5");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_size1_local(local, &pos, "Hello", 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_size1_alloc6");
	test(GetStatusDynamic(pos), "strvect_size1_alloc7");
	rollback_local(local, stack);

	strvect_size1_heap(&pos, "Hello", 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_size1_alloc8");
	test(! GetStatusDynamic(pos), "strvect_size1_alloc9");

	RETURN;
}

static int test_strvect_sizeu_alloc(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;
	unicode *body;
	LocalRoot local;
	LocalStack stack;

	strvect_sizeu_alloc(NULL, &pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc1");
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD, "strvect_sizeu_alloc2");
	test(RefStringSize(pos) == 5, "strvect_sizeu_alloc3");
	GetStringUnicode(pos, &body);
	test(body[0] == 'H', "strvect_sizeu_alloc4");
	test(body[4] == 'o', "strvect_sizeu_alloc5");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_sizeu_local(local, &pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc6");
	test(GetStatusDynamic(pos), "strvect_sizeu_alloc7");
	rollback_local(local, stack);

	strvect_sizeu_heap(&pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc8");
	test(! GetStatusDynamic(pos), "strvect_sizeu_alloc9");

	RETURN;
}


/*
 *  strvect_equal
 */
static int test_strvect_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary1");
	strvect_char_heap(&pos, "Helloo");
	test(! strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary2");
	strvect_char_heap(&pos, "HELLO");
	test(! strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary3");

	RETURN;
}

static int test_strvect_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary1");
	strvect_char_heap(&pos, "Helloo");
	test(! strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary3");

	RETURN;
}

static int test_strvect_equal_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equal_char(pos, "Hello"), "strvect_equal_char1");
	test(! strvect_equal_char(pos, "Helloo"), "strvect_equal_char2");
	test(! strvect_equal_char(pos, "HELLO"), "strvect_equal_char3");

	RETURN;
}

static int test_strvect_equalp_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equalp_char(pos, "Hello"), "strvect_equalp_char1");
	test(! strvect_equalp_char(pos, "Helloo"), "strvect_equalp_char2");
	test(strvect_equalp_char(pos, "HELLO"), "strvect_equalp_char3");

	RETURN;
}

static int test_strvect_equal(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_equal(left, right), "strvect_equal1");
	strvect_char_heap(&right, "Helloo");
	test(! strvect_equal(left, right), "strvect_equal2");
	strvect_char_heap(&right, "HELLO");
	test(! strvect_equal(left, right), "strvect_equal3");

	RETURN;
}

static int test_strvect_equalp(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_equalp(left, right), "strvect_equalp1");
	strvect_char_heap(&right, "Helloo");
	test(! strvect_equalp(left, right), "strvect_equalp2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_equalp(left, right), "strvect_equalp3");

	RETURN;
}


/*
 *  strvect_compare
 */
static int test_strvect_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_compare_binary(pos, Hello, 5) == 0, "strvect_compare_binary1");
	strvect_char_heap(&pos, "Helloo");
	test(strvect_compare_binary(pos, Hello, 5) > 0, "strvect_compare_binary2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_compare_binary(pos, Hello, 5) != 0, "strvect_compare_binary3");

	RETURN;
}

static int test_strvect_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_comparep_binary(pos, Hello, 5) == 0, "strvect_comparep_binary1");
	strvect_char_heap(&pos, "Helloo");
	test(strvect_comparep_binary(pos, Hello, 5) > 0, "strvect_comparep_binary2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_comparep_binary(pos, Hello, 5) == 0, "strvect_comparep_binary3");

	RETURN;
}

static int test_strvect_compare_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_compare_char(pos, "Hello") == 0, "strvect_compare_char1");
	test(strvect_compare_char(pos, "Helloo") < 0, "strvect_compare_char2");
	test(strvect_compare_char(pos, "HELLO") != 0, "strvect_compare_char3");

	RETURN;
}

static int test_strvect_comparep_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_comparep_char(pos, "Hello") == 0, "strvect_comparep_char1");
	test(strvect_comparep_char(pos, "Helloo") < 0, "strvect_comparep_char2");
	test(strvect_comparep_char(pos, "HELLO") == 0, "strvect_comparep_char3");

	RETURN;
}

static int test_strvect_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_compare(left, right) == 0, "strvect_compare1");
	strvect_char_heap(&right, "Helloo");
	test(strvect_compare(left, right) < 0, "strvect_compare2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_compare(left, right) != 0, "strvect_compare3");

	RETURN;
}

static int test_strvect_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_comparep(left, right) == 0, "strvect_comparep1");
	strvect_char_heap(&right, "Helloo");
	test(strvect_comparep(left, right) < 0, "strvect_comparep2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_comparep(left, right) == 0, "strvect_comparep3");

	RETURN;
}


/*
 *  getc/setc
 */
static int test_strvect_refc(void)
{
	addr pos;
	unicode u;

	strvect_char_heap(&pos, "Hello");
	test(strvect_refc(pos, 0) == 'H', "strvect_refc1");
	test(strvect_refc(pos, 1) == 'e', "strvect_refc1");
	test(strvect_refc(pos, 4) == 'o', "strvect_refc1");
	strvect_getc(pos, 0, &u);
	test(u == 'H', "strvect_getc1");
	strvect_getc(pos, 1, &u);
	test(u == 'e', "strvect_getc2");
	strvect_getc(pos, 4, &u);
	test(u == 'o', "strvect_getc3");
	strvect_setc(pos, 1, 'E');
	test(strvect_refc(pos, 1) == 'E', "strvect_setc1");

	RETURN;
}


/*
 *  main
 */
static int testbreak_strvect(void)
{
	/* character */
	TestBreak(test_unicode_macro);
	TestBreak(test_character_alloc);
	TestBreak(test_ptrcharacter);
	TestBreak(test_character_type);
	TestBreak(test_setcharacter_unsafe);
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
	/* buffer compare */
	TestBreak(test_memu_equal);
	TestBreak(test_memu_compare);
	TestBreak(test_memu_equalp);
	TestBreak(test_memu_comparep);
	TestBreak(test_memu1_equal);
	TestBreak(test_memu1_compare);
	TestBreak(test_memu1_equalp);
	TestBreak(test_memu1_comparep);
	/* strvect */
	TestBreak(test_strvect_alloc);
	TestBreak(test_strvect_length);
	TestBreak(test_strvect_posbodylen);
	TestBreak(test_unicode_character_type);
	TestBreak(test_strvect_update_character_type);
	TestBreak(test_strvect_char_alloc);
	TestBreak(test_strvect_size1_alloc);
	TestBreak(test_strvect_sizeu_alloc);
	/* strvect_equal */
	TestBreak(test_strvect_equal_binary);
	TestBreak(test_strvect_equalp_binary);
	TestBreak(test_strvect_equal_char);
	TestBreak(test_strvect_equalp_char);
	TestBreak(test_strvect_equal);
	TestBreak(test_strvect_equalp);
	/* strvect_compare */
	TestBreak(test_strvect_compare_binary);
	TestBreak(test_strvect_comparep_binary);
	TestBreak(test_strvect_compare_char);
	TestBreak(test_strvect_comparep_char);
	TestBreak(test_strvect_compare);
	TestBreak(test_strvect_comparep);
	/* getc/setc */
	TestBreak(test_strvect_refc);

	return 0;
}

int test_strvect(void)
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
		result = testbreak_strvect();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

