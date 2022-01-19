#include "strvect.c"
#include "degrade.h"

static int test_strvect_access(void)
{
	addr pos;
	const unicode *ptr;
	size_t size;

	test(StringBodyLength(5) == IdxSize + sizeoft(unicode) * 5, "StringBodyLength.1");
	strvect_heap(&pos, 10);
	test(PtrStringBase(pos) == posbodyr(pos), "PtrStringBase.1");
	test(*(size_t *)PtrStringBase(pos) == 10, "PtrStringBase.2");
	test(*PtrStringSize(pos) == 10, "PtrStringSize.1");
	test(RefStringSize(pos) == 10, "RefStringSize.1");
	SetStringSize(pos, 5);
	test(RefStringSize(pos) == 5, "SetStringSize.1");
	GetStringSize(pos, &size);
	test(size == 5, "GetStringSize.1");
	ptr = PtrStringUnicode(pos);
	test(ptr == (const unicode *)(posbodyr(pos) + IdxSize), "PtrStringUnicode.1");
	GetStringUnicode(pos, &ptr);
	test(ptr == (const unicode *)(posbodyr(pos) + IdxSize), "GetStringUnicode.1");

	RETURN;
}


/*
 *  buffer compare
 */
static int test_memu_equal(void)
{
	unicode left[10], right[10];

	aatype(left);
	aatype(right);
	test(! memu_equal(left, right, 4, 5), "memu_equal.1");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu_equal(left, right, 3, 3), "memu_equal.2");
	test(! memu_equal(left, right, 4, 4), "memu_equal.3");

	RETURN;
}

static int test_memu_compare(void)
{
	unicode left[10], right[10];

	aatype(left);
	aatype(right);
	test(memu_compare(left, right, 4, 5) < 0, "memu_compare.1");
	test(memu_compare(left, right, 5, 4) > 0, "memu_compare.2");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu_compare(left, right, 3, 3) == 0, "memu_compare.3");
	test(memu_compare(left, right, 4, 4) < 0, "memu_compare.4");

	RETURN;
}

static int test_memu_equalp(void)
{
	unicode left[10], right[10];

	aatype(left);
	aatype(right);
	test(! memu_equalp(left, right, 4, 5), "memu_equalp.1");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu_equalp(left, right, 3, 3), "memu_equalp.2");
	test(! memu_equalp(left, right, 4, 4), "memu_equalp.3");

	RETURN;
}

static int test_memu_comparep(void)
{
	unicode left[10], right[10];

	aatype(left);
	aatype(right);
	test(memu_comparep(left, right, 4, 5) < 0, "memu_comparep.1");
	test(memu_comparep(left, right, 5, 4) > 0, "memu_comparep.2");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu_comparep(left, right, 3, 3) == 0, "memu_comparep.3");
	test(memu_comparep(left, right, 4, 4) > 0, "memu_comparep.4");

	RETURN;
}

static int test_memu1_equal(void)
{
	unicode left[10];
	byte right[10];

	aatype(left);
	aatype(right);
	test(! memu1_equal(left, right, 4, 5), "memu1_equal.1");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu1_equal(left, right, 3, 3), "memu1_equal.2");
	test(! memu1_equal(left, right, 4, 4), "memu1_equal.3");

	RETURN;
}

static int test_memu1_compare(void)
{
	unicode left[10];
	byte right[10];

	aatype(left);
	aatype(right);
	test(memu1_compare(left, right, 4, 5) < 0, "memu1_compare.1");
	test(memu1_compare(left, right, 5, 4) > 0, "memu1_compare.2");
	left[0] = right[0] = 10;
	left[1] = right[1] = 20;
	left[2] = right[2] = 30;
	left[3] = 40; right[3] = 50;
	test(memu1_compare(left, right, 3, 3) == 0, "memu1_compare.3");
	test(memu1_compare(left, right, 4, 4) < 0, "memu1_compare.4");

	RETURN;
}

static int test_memu1_equalp(void)
{
	unicode left[10];
	byte right[10];

	aatype(left);
	aatype(right);
	test(! memu1_equalp(left, right, 4, 5), "memu1_equalp.1");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu1_equalp(left, right, 3, 3), "memu1_equalp.2");
	test(! memu1_equalp(left, right, 4, 4), "memu1_equalp.3");

	RETURN;
}

static int test_memu1_comparep(void)
{
	unicode left[10];
	byte right[10];

	aatype(left);
	aatype(right);
	test(memu1_comparep(left, right, 4, 5) < 0, "memu1_comparep.1");
	test(memu1_comparep(left, right, 5, 4) > 0, "memu1_comparep.2");
	left[0] = 'A'; right[0] = 'A';
	left[1] = 'b'; right[1] = 'B';
	left[2] = 'C'; right[2] = 'c';
	left[3] = 'Z'; right[3] = 'Y';
	test(memu1_comparep(left, right, 3, 3) == 0, "memu1_comparep.3");
	test(memu1_comparep(left, right, 4, 4) > 0, "memu1_comparep.4");

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
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc.1");
	test(RefStringSize(pos) == 10, "strvect_alloc.2");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_local(local, &pos, 10);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc.3");
	test(GetStatusDynamic(pos), "strvect_alloc.4");
	rollback_local(local, stack);

	strvect_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_alloc.5");
	test(! GetStatusDynamic(pos), "strvect_alloc.6");

	RETURN;
}

static int test_strvect_length(void)
{
	addr pos;
	size_t size;

	strvect_heap(&pos, 10);
	strvect_length(pos, &size);
	test(size == 10, "strvect_length.1");

	RETURN;
}

static int test_strvect_posbodylen(void)
{
	addr pos;
	const unicode *body;
	size_t size;

	strvect_heap(&pos, 10);
	strvect_posbodylen(pos, &body, &size);
	test(size == 10, "strvect_posbodylen.1");
	test(body == (const unicode *)(posbodyr(pos) + IdxSize), "strvect_posbodylen.2");

	RETURN;
}

static int test_unicode_character_type(void)
{
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, '$')
			== CHARACTER_TYPE_STANDARD, "unicode_character_type.1");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0xFF)
			== CHARACTER_TYPE_BASE, "unicode_character_type.2");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0x00F00000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.3");
	test(unicode_character_type(CHARACTER_TYPE_EMPTY, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.4");

	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 'a')
			== CHARACTER_TYPE_STANDARD, "unicode_character_type.5");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0)
			== CHARACTER_TYPE_BASE, "unicode_character_type.6");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0x00801000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.7");
	test(unicode_character_type(CHARACTER_TYPE_STANDARD, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.8");

	test(unicode_character_type(CHARACTER_TYPE_BASE, 'z')
			== CHARACTER_TYPE_BASE, "unicode_character_type.9");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0x0D)
			== CHARACTER_TYPE_BASE, "unicode_character_type.10");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0x00800000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.11");
	test(unicode_character_type(CHARACTER_TYPE_BASE, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.12");

	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 'a')
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.13");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.14");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0x00800000)
			== CHARACTER_TYPE_EXTENDED, "unicode_character_type.15");
	test(unicode_character_type(CHARACTER_TYPE_EXTENDED, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.16");

	test(unicode_character_type(CHARACTER_TYPE_INVALID, 'a')
			== CHARACTER_TYPE_INVALID, "unicode_character_type.17");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0x0100)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.18");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0x00E00000)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.19");
	test(unicode_character_type(CHARACTER_TYPE_INVALID, 0xD800)
			== CHARACTER_TYPE_INVALID, "unicode_character_type.20");

	RETURN;
}

static int test_strvect_char_alloc(void)
{
	enum CHARACTER_TYPE type;
	addr pos;
	unicode *body;
	LocalRoot local;
	LocalStack stack;

	strvect_char_alloc(NULL, &pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc.1");
	strvect_character_type_(pos, &type);
	test(type == CHARACTER_TYPE_STANDARD, "strvect_char_alloc.2");
	test(RefStringSize(pos) == 5, "strvect_char_alloc.3");
	GetStringUnicode(pos, &body);
	test(body[0] == 'H', "strvect_char_alloc.4");
	test(body[4] == 'o', "strvect_char_alloc.5");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc.6");
	test(GetStatusDynamic(pos), "strvect_char_alloc.7");
	rollback_local(local, stack);

	strvect_char_heap(&pos, "Hello");
	test(GetType(pos) == LISPTYPE_STRING, "strvect_char_alloc.8");
	test(! GetStatusDynamic(pos), "strvect_char_alloc.9");

	RETURN;
}

static int test_strvect_sizeu_alloc(void)
{
	enum CHARACTER_TYPE type;
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;
	unicode *body;
	LocalRoot local;
	LocalStack stack;

	strvect_sizeu_alloc_(NULL, &pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc.1");
	strvect_character_type_(pos, &type);
	test(type == CHARACTER_TYPE_STANDARD, "strvect_sizeu_alloc.2");
	test(RefStringSize(pos) == 5, "strvect_sizeu_alloc.3");
	GetStringUnicode(pos, &body);
	test(body[0] == 'H', "strvect_sizeu_alloc.4");
	test(body[4] == 'o', "strvect_sizeu_alloc.5");

	local = Local_Thread;
	push_local(local, &stack);
	strvect_sizeu_local_(local, &pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc.6");
	test(GetStatusDynamic(pos), "strvect_sizeu_alloc.7");
	rollback_local(local, stack);

	strvect_sizeu_heap_(&pos, Hello, 5);
	test(GetType(pos) == LISPTYPE_STRING, "strvect_sizeu_alloc.8");
	test(! GetStatusDynamic(pos), "strvect_sizeu_alloc.9");

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
	test(strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary.1");
	strvect_char_heap(&pos, "Helloo");
	test(! strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary.2");
	strvect_char_heap(&pos, "HELLO");
	test(! strvect_equal_binary(pos, Hello, 5), "strvect_equal_binary.3");

	RETURN;
}

static int test_strvect_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary.1");
	strvect_char_heap(&pos, "Helloo");
	test(! strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary.2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_equalp_binary(pos, Hello, 5), "strvect_equalp_binary.3");

	RETURN;
}

static int test_strvect_equal_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equal_char(pos, "Hello"), "strvect_equal_char.1");
	test(! strvect_equal_char(pos, "Helloo"), "strvect_equal_char.2");
	test(! strvect_equal_char(pos, "HELLO"), "strvect_equal_char.3");

	RETURN;
}

static int test_strvect_equalp_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_equalp_char(pos, "Hello"), "strvect_equalp_char.1");
	test(! strvect_equalp_char(pos, "Helloo"), "strvect_equalp_char.2");
	test(strvect_equalp_char(pos, "HELLO"), "strvect_equalp_char.3");

	RETURN;
}

static int test_strvect_equal(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_equal(left, right), "strvect_equal.1");
	strvect_char_heap(&right, "Helloo");
	test(! strvect_equal(left, right), "strvect_equal.2");
	strvect_char_heap(&right, "HELLO");
	test(! strvect_equal(left, right), "strvect_equal.3");

	RETURN;
}

static int test_strvect_equalp(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_equalp(left, right), "strvect_equalp.1");
	strvect_char_heap(&right, "Helloo");
	test(! strvect_equalp(left, right), "strvect_equalp.2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_equalp(left, right), "strvect_equalp.3");

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
	test(strvect_compare_binary(pos, Hello, 5) == 0, "strvect_compare_binary.1");
	strvect_char_heap(&pos, "Helloo");
	test(strvect_compare_binary(pos, Hello, 5) > 0, "strvect_compare_binary.2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_compare_binary(pos, Hello, 5) != 0, "strvect_compare_binary.3");

	RETURN;
}

static int test_strvect_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_comparep_binary(pos, Hello, 5) == 0, "strvect_comparep_binary.1");
	strvect_char_heap(&pos, "Helloo");
	test(strvect_comparep_binary(pos, Hello, 5) > 0, "strvect_comparep_binary.2");
	strvect_char_heap(&pos, "HELLO");
	test(strvect_comparep_binary(pos, Hello, 5) == 0, "strvect_comparep_binary.3");

	RETURN;
}

static int test_strvect_compare_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_compare_char(pos, "Hello") == 0, "strvect_compare_char.1");
	test(strvect_compare_char(pos, "Helloo") < 0, "strvect_compare_char.2");
	test(strvect_compare_char(pos, "HELLO") != 0, "strvect_compare_char.3");

	RETURN;
}

static int test_strvect_comparep_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(strvect_comparep_char(pos, "Hello") == 0, "strvect_comparep_char.1");
	test(strvect_comparep_char(pos, "Helloo") < 0, "strvect_comparep_char.2");
	test(strvect_comparep_char(pos, "HELLO") == 0, "strvect_comparep_char.3");

	RETURN;
}

static int test_strvect_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_compare(left, right) == 0, "strvect_compare.1");
	strvect_char_heap(&right, "Helloo");
	test(strvect_compare(left, right) < 0, "strvect_compare.2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_compare(left, right) != 0, "strvect_compare.3");

	RETURN;
}

static int test_strvect_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strvect_comparep(left, right) == 0, "strvect_comparep.1");
	strvect_char_heap(&right, "Helloo");
	test(strvect_comparep(left, right) < 0, "strvect_comparep.2");
	strvect_char_heap(&right, "HELLO");
	test(strvect_comparep(left, right) == 0, "strvect_comparep.3");

	RETURN;
}


/*
 *  strvect
 */
static int testcase_strvect(void)
{
	TestBreak(test_strvect_access);
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
	TestBreak(test_strvect_char_alloc);
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

	return 0;
}

static void testinit_strvect(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
}

int test_strvect(void)
{
	DegradeTitle;
	return DegradeCode(strvect);
}

