#include "strtype.c"
#include "clos.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "type_table.h"

/*
 *  string check
 */
static int test_array_stringp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(array_stringp(pos), "array_stringp1");

	array_va_heap(&pos, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(! array_stringp(pos), "array_stringp2");

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_T;
	test(! array_stringp(pos), "array_stringp3");

	RETURN;
}

static int test_strarrayp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(strarrayp(pos), "strarrayp1");
	test(! strarrayp(Nil), "strarrayp2");

	RETURN;
}

static int test_stringp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(stringp(pos), "stringp1");

	strvect_char_heap(&pos, "Hello");
	test(stringp(pos), "stringp2");
	test(! stringp(T), "stringp3");

	RETURN;
}


/*
 *  strarray
 */
static int test_strarray_allocr(void)
{
	addr pos;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	pos = strarray_allocr(NULL, 10);
	test(stringp(pos), "strarray_allocr1");
	test(strarrayp(pos), "strarray_allocr2");
	str = ArrayInfoStruct(pos);
	test(str->front == 10, "strarray_allocr3");

	local = Local_Thread;
	push_local(local, &stack);
	pos = strarray_localr(local, 10);
	test(stringp(pos), "strarray_allocr4");
	test(GetStatusDynamic(pos), "strarray_allocr5");
	rollback_local(local, stack);

	pos = strarray_heapr(10);
	test(stringp(pos), "strarray_allocr6");
	test(! GetStatusDynamic(pos), "strarray_allocr7");

	RETURN;
}

static int test_strarray_alloc(void)
{
	addr pos;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_alloc(NULL, &pos, 10);
	test(stringp(pos), "strarray_alloc1");
	test(strarrayp(pos), "strarray_alloc2");
	str = ArrayInfoStruct(pos);
	test(str->front == 10, "strarray_alloc3");
	test(RefCharacterType(pos) == CHARACTER_TYPE_EMPTY, "strarray_alloc4");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_local(local, &pos, 10);
	test(stringp(pos), "strarray_alloc5");
	test(GetStatusDynamic(pos), "strarray_alloc6");
	rollback_local(local, stack);

	strarray_heap(&pos, 10);
	test(stringp(pos), "strarray_alloc7");
	test(! GetStatusDynamic(pos), "strarray_alloc8");

	RETURN;
}

static int test_strarray_update_character_type(void)
{
	addr pos;
	unicode *body;

	strarray_heap(&pos, 3);
	GetArrayUnicode(pos, &body);
	body[0] = 'a';
	body[1] = 'b';
	body[2] = 'c';
	strarray_update_character_type(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD,
			"strarray_update_character_type1");

	body[0] = 'a';
	body[1] = 0x0D;
	body[2] = 'c';
	strarray_update_character_type(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_BASE,
			"strarray_update_character_type2");

	RETURN;
}

static int test_strarray_char_allocr(void)
{
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	pos = strarray_char_allocr(NULL, "Hello");
	test(strarrayp(pos), "strarray_char_allocr1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_char_allocr2");
	test(str->front == 5, "strarray_char_allocr3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_char_allocr4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_char_allocr5");
	test(body[1] == 'e', "strarray_char_allocr6");
	test(body[4] == 'o', "strarray_char_allocr7");

	local = Local_Thread;
	push_local(local, &stack);
	pos = strarray_char_localr(local, "Hello");
	test(strarrayp(pos), "strarray_char_allocr8");
	test(GetStatusDynamic(pos), "strarray_char_allocr9");
	rollback_local(local, stack);

	pos = strarray_char_heapr("Hello");
	test(strarrayp(pos), "strarray_char_allocr10");
	test(! GetStatusDynamic(pos), "strarray_char_allocr11");

	RETURN;
}

static int test_strarray_char_alloc(void)
{
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_char_alloc(NULL, &pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_char_alloc2");
	test(str->front == 5, "strarray_char_alloc3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_char_alloc4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_char_alloc5");
	test(body[1] == 'e', "strarray_char_alloc6");
	test(body[4] == 'o', "strarray_char_alloc7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_char_local(local, &pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc8");
	test(GetStatusDynamic(pos), "strarray_char_alloc9");
	rollback_local(local, stack);

	strarray_char_heap(&pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc10");
	test(! GetStatusDynamic(pos), "strarray_char_alloc11");

	RETURN;
}

static int test_strarray_size1_allocr(void)
{
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	pos = strarray_size1_allocr(NULL, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_allocr1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_size1_allocr2");
	test(str->front == 5, "strarray_size1_allocr3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_size1_allocr4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_size1_allocr5");
	test(body[1] == 'e', "strarray_size1_allocr6");
	test(body[4] == 'o', "strarray_size1_allocr7");

	local = Local_Thread;
	push_local(local, &stack);
	pos = strarray_size1_localr(local, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_allocr8");
	test(GetStatusDynamic(pos), "strarray_size1_allocr9");
	rollback_local(local, stack);

	pos = strarray_size1_heapr("Hello", 5);
	test(strarrayp(pos), "strarray_size1_allocr10");
	test(! GetStatusDynamic(pos), "strarray_size1_allocr11");

	RETURN;
}

static int test_strarray_size1_alloc(void)
{
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_size1_alloc(NULL, &pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_size1_alloc2");
	test(str->front == 5, "strarray_size1_alloc3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_size1_alloc4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_size1_alloc5");
	test(body[1] == 'e', "strarray_size1_alloc6");
	test(body[4] == 'o', "strarray_size1_alloc7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_size1_local(local, &pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc8");
	test(GetStatusDynamic(pos), "strarray_size1_alloc9");
	rollback_local(local, stack);

	strarray_size1_heap(&pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc10");
	test(! GetStatusDynamic(pos), "strarray_size1_alloc11");

	RETURN;
}

static int test_strarray_sizeu_allocr(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	pos = strarray_sizeu_allocr(NULL, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_allocr1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_sizeu_allocr2");
	test(str->front == 5, "strarray_sizeu_allocr3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_sizeu_allocr4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_sizeu_allocr5");
	test(body[1] == 'e', "strarray_sizeu_allocr6");
	test(body[4] == 'o', "strarray_sizeu_allocr7");

	local = Local_Thread;
	push_local(local, &stack);
	pos = strarray_sizeu_localr(local, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_allocr8");
	test(GetStatusDynamic(pos), "strarray_sizeu_allocr9");
	rollback_local(local, stack);

	pos = strarray_sizeu_heapr(Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_allocr10");
	test(! GetStatusDynamic(pos), "strarray_sizeu_allocr11");

	RETURN;
}

static int test_strarray_sizeu_alloc(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;
	const unicode *body;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_sizeu_alloc(NULL, &pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_sizeu_alloc2");
	test(str->front == 5, "strarray_sizeu_alloc3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_sizeu_alloc4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_sizeu_alloc5");
	test(body[1] == 'e', "strarray_sizeu_alloc6");
	test(body[4] == 'o', "strarray_sizeu_alloc7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_sizeu_local(local, &pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc8");
	test(GetStatusDynamic(pos), "strarray_sizeu_alloc9");
	rollback_local(local, stack);

	strarray_sizeu_heap(&pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc10");
	test(! GetStatusDynamic(pos), "strarray_sizeu_alloc11");

	RETURN;
}

static int test_strarray_length(void)
{
	addr pos;
	size_t size;

	strarray_char_alloc(NULL, &pos, "Hello");
	strarray_length(pos, &size);
	test(size == 5, "strarray_length1");

	RETURN;
}

static int test_strarray_posbodylen(void)
{
	addr pos;
	const unicode *body;
	size_t size;

	strarray_char_alloc(NULL, &pos, "Hello");
	strarray_posbodylen(pos, &body, &size);
	test(size == 5, "strarray_posbodylen1");
	test(body[0] == 'H', "strarray_posbodylen2");
	test(body[1] == 'e', "strarray_posbodylen3");
	test(body[4] == 'o', "strarray_posbodylen4");

	RETURN;
}

static int test_strarray_refc(void)
{
	addr pos;
	unicode u;

	strarray_char_alloc(NULL, &pos, "Hello");
	test(strarray_refc(pos, 0) == 'H', "strarray_refc1");
	test(strarray_refc(pos, 4) == 'o', "strarray_refc2");
	strarray_setc(pos, 1, 'E');
	test(strarray_refc(pos, 1) == 'E', "strarray_setc1");
	strarray_getc(pos, 2, &u);
	test(u == 'l', "strarray_getc1");

	RETURN;
}

static int test_strarray_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary1");
	strarray_char_heap(&pos, "Helloo");
	test(! strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary2");
	strarray_char_heap(&pos, "HELLO");
	test(! strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary3");

	RETURN;
}

static int test_strarray_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary1");
	strarray_char_heap(&pos, "Helloo");
	test(! strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary3");

	RETURN;
}

static int test_strarray_equal_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equal_char(pos, "Hello"), "strarray_equal_char1");
	test(! strarray_equal_char(pos, "Helloo"), "strarray_equal_char2");
	test(! strarray_equal_char(pos, "HELLO"), "strarray_equal_char3");

	RETURN;
}

static int test_strarray_equalp_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equalp_char(pos, "Hello"), "strarray_equalp_char1");
	test(! strarray_equalp_char(pos, "Helloo"), "strarray_equalp_char2");
	test(strarray_equalp_char(pos, "HELLO"), "strarray_equalp_char3");

	RETURN;
}

static int test_strarray_equal(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_equal(left, right), "strarray_equal1");
	strarray_char_heap(&right, "Helloo");
	test(! strarray_equal(left, right), "strarray_equal2");
	strarray_char_heap(&right, "HELLO");
	test(! strarray_equal(left, right), "strarray_equal3");

	RETURN;
}

static int test_strarray_equalp(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_equalp(left, right), "strarray_equalp1");
	strarray_char_heap(&right, "Helloo");
	test(! strarray_equalp(left, right), "strarray_equalp2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_equalp(left, right), "strarray_equalp3");

	RETURN;
}

static int test_strarray_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_compare_binary(pos, Hello, 5) == 0, "strarray_compare_binary1");
	strarray_char_heap(&pos, "Helloo");
	test(strarray_compare_binary(pos, Hello, 5) > 0, "strarray_compare_binary2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_compare_binary(pos, Hello, 5) != 0, "strarray_compare_binary3");

	RETURN;
}

static int test_strarray_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_comparep_binary(pos, Hello, 5) == 0, "strarray_comparep_binary1");
	strarray_char_heap(&pos, "Helloo");
	test(strarray_comparep_binary(pos, Hello, 5) > 0, "strarray_comparep_binary2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_comparep_binary(pos, Hello, 5) == 0, "strarray_comparep_binary3");

	RETURN;
}

static int test_strarray_compare_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_compare_char(pos, "Hello") == 0, "strarray_compare_char1");
	test(strarray_compare_char(pos, "Helloo") < 0, "strarray_compare_char2");
	test(strarray_compare_char(pos, "HELLO") != 0, "strarray_compare_char3");

	RETURN;
}

static int test_strarray_comparep_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_comparep_char(pos, "Hello") == 0, "strarray_comparep_char1");
	test(strarray_comparep_char(pos, "Helloo") < 0, "strarray_comparep_char2");
	test(strarray_comparep_char(pos, "HELLO") == 0, "strarray_comparep_char3");

	RETURN;
}

static int test_strarray_compare(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_compare(left, right) == 0, "strarray_compare1");
	strarray_char_heap(&right, "Helloo");
	test(strarray_compare(left, right) < 0, "strarray_compare2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_compare(left, right) != 0, "strarray_compare3");

	RETURN;
}

static int test_strarray_comparep(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_comparep(left, right) == 0, "strarray_comparep1");
	strarray_char_heap(&right, "Helloo");
	test(strarray_comparep(left, right) < 0, "strarray_comparep2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_comparep(left, right) == 0, "strarray_comparep3");

	RETURN;
}


/*
 *  string
 */
static int test_string_length(void)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, "Hello");
	string_length(pos, &size);
	test(size == 5, "string_length1");

	strarray_char_heap(&pos, "aaabbb");
	string_length(pos, &size);
	test(size == 6, "string_length2");

	RETURN;
}

static int test_string_posbodylen(void)
{
	addr pos;
	const unicode *body;
	size_t size;

	strvect_char_heap(&pos, "Hello");
	string_posbodylen(pos, &body, &size);
	test(size == 5, "string_posbodylen1");
	test(body[0] == 'H', "string_posbodylen2");

	strarray_char_heap(&pos, "abcdef");
	string_posbodylen(pos, &body, &size);
	test(size == 6, "string_posbodylen3");
	test(body[0] == 'a', "string_posbodylen4");

	RETURN;
}

static int test_string_refc(void)
{
	addr pos;
	unicode u;

	strvect_char_heap(&pos, "Hello");
	test(string_refc(pos, 0) == 'H', "string_refc1");
	string_setc(pos, 1, 'E');
	test(string_refc(pos, 1) == 'E', "string_setc1");
	string_getc(pos, 4, &u);
	test(u == 'o', "string_getc1");

	strarray_char_heap(&pos, "Hello");
	test(string_refc(pos, 0) == 'H', "string_refc2");
	string_setc(pos, 1, 'E');
	test(string_refc(pos, 1) == 'E', "string_setc2");
	string_getc(pos, 4, &u);
	test(u == 'o', "string_getc2");

	RETURN;
}

static int test_string_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_equal_binary(pos, Hello, 5), "string_equal_binary1");
	test(! string_equal_binary(pos, Hello, 4), "string_equal_binary2");

	strarray_char_heap(&pos, "Hello");
	test(string_equal_binary(pos, Hello, 5), "string_equal_binary3");
	test(! string_equal_binary(pos, Hello, 4), "string_equal_binary4");

	RETURN;
}

static int test_string_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_equalp_binary(pos, Hello, 5), "string_equalp_binary1");
	test(! string_equalp_binary(pos, Hello, 4), "string_equalp_binary2");

	strarray_char_heap(&pos, "HELLO");
	test(string_equalp_binary(pos, Hello, 5), "string_equalp_binary3");
	test(! string_equalp_binary(pos, Hello, 4), "string_equalp_binary4");

	RETURN;
}

static int test_string_equal_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_equal_char(pos, "Hello"), "string_equal_char1");
	test(! string_equal_char(pos, "Helloo"), "string_equal_char2");

	strarray_char_heap(&pos, "Hello");
	test(string_equal_char(pos, "Hello"), "string_equal_char3");
	test(! string_equal_char(pos, "Helloo"), "string_equal_char4");

	RETURN;
}

static int test_string_equalp_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_equalp_char(pos, "Hello"), "string_equalp_char1");
	test(! string_equalp_char(pos, "Helloo"), "string_equalp_char2");

	strarray_char_heap(&pos, "HELLO");
	test(string_equalp_char(pos, "Hello"), "string_equalp_char3");
	test(! string_equalp_char(pos, "Helloo"), "string_equalp_char4");

	RETURN;
}

static int test_strvect_strarray_equal(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_equal(left, right), "strvect_strarray_equal1");

	strarray_char_heap(&right, "HELLO");
	test(! strvect_strarray_equal(left, right), "strvect_strarray_equal2");

	RETURN;
}

static int test_string_equal(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(string_equal(left, left), "string_equal1");
	test(string_equal(left, right), "string_equal2");
	test(string_equal(right, left), "string_equal3");
	test(string_equal(right, right), "string_equal4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal6");
	test(! string_equal(right, left), "string_equal7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal8");

	RETURN;
}

static int test_strvect_strarray_equalp(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_equalp(left, right), "strvect_strarray_equalp1");

	strarray_char_heap(&right, "HELLO");
	test(strvect_strarray_equalp(left, right), "strvect_strarray_equalp2");

	strarray_char_heap(&right, "HELLOO");
	test(! strvect_strarray_equalp(left, right), "strvect_strarray_equalp3");

	RETURN;
}

static int test_string_equalp(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(string_equalp(left, left), "string_equalp1");
	test(string_equalp(left, right), "string_equalp2");
	test(string_equalp(right, left), "string_equalp3");
	test(string_equalp(right, right), "string_equalp4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp6");
	test(! string_equalp(right, left), "string_equalp7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp8");

	RETURN;
}

static int test_string_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_compare_binary(pos, Hello, 5) == 0, "string_compare_binary1");
	test(string_compare_binary(pos, Hello, 4) > 0, "string_compare_binary2");

	strarray_char_heap(&pos, "Hello");
	test(string_compare_binary(pos, Hello, 5) == 0, "string_compare_binary3");
	test(string_compare_binary(pos, Hello, 4) > 0, "string_compare_binary4");

	RETURN;
}

static int test_string_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_comparep_binary(pos, Hello, 5) == 0, "string_comparep_binary1");
	test(string_comparep_binary(pos, Hello, 4) > 0, "string_comparep_binary2");

	strarray_char_heap(&pos, "HELLO");
	test(string_comparep_binary(pos, Hello, 5) == 0, "string_comparep_binary3");
	test(string_comparep_binary(pos, Hello, 4) > 0, "string_comparep_binary4");

	RETURN;
}

static int test_string_compare_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_compare_char(pos, "Hello") == 0, "string_compare_char1");
	test(string_compare_char(pos, "Helloo") < 0, "string_compare_char2");

	strarray_char_heap(&pos, "Hello");
	test(string_compare_char(pos, "Hello") == 0, "string_compare_char3");
	test(string_compare_char(pos, "Helloo") < 0, "string_compare_char4");

	RETURN;
}

static int test_string_comparep_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_comparep_char(pos, "Hello") == 0, "string_comparep_char1");
	test(string_comparep_char(pos, "Helloo") < 0, "string_comparep_char2");

	strarray_char_heap(&pos, "HELLO");
	test(string_comparep_char(pos, "Hello") == 0, "string_comparep_char3");
	test(string_comparep_char(pos, "Helloo") < 0, "string_comparep_char4");

	RETURN;
}

static int test_strvect_strarray_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_compare(left, right) == 0, "strvect_strarray_compare1");

	strarray_char_heap(&right, "Helloo");
	test(strvect_strarray_compare(left, right) < 0, "strvect_strarray_compare2");

	RETURN;
}

static int test_strarray_strvect_compare(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_compare(left, right) == 0, "strarray_strvect_compare1");

	strvect_char_heap(&right, "Helloo");
	test(strarray_strvect_compare(left, right) < 0, "strarray_strvect_compare2");

	RETURN;
}

static int test_string_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(string_compare(left, left) == 0, "string_compare1");
	test(string_compare(left, right) == 0, "string_compare2");
	test(string_compare(right, left) == 0, "string_compare3");
	test(string_compare(right, right) == 0, "string_compare4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare6");
	test(string_compare(right, left) > 0, "string_compare7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare8");

	RETURN;
}

static int test_strvect_strarray_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_comparep(left, right) == 0, "strvect_strarray_comparep1");

	strarray_char_heap(&right, "Helloo");
	test(strvect_strarray_comparep(left, right) < 0, "strvect_strarray_comparep2");

	RETURN;
}

static int test_strarray_strvect_comparep(void)
{
	addr left, right;

	strarray_char_heap(&left, "HELLO");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_comparep(left, right) == 0, "strarray_strvect_comparep1");

	strvect_char_heap(&right, "Helloo");
	test(strarray_strvect_comparep(left, right) < 0, "strarray_strvect_comparep2");

	RETURN;
}

static int test_string_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Hello");
	test(string_comparep(left, left) == 0, "string_comparep1");
	test(string_comparep(left, right) == 0, "string_comparep2");
	test(string_comparep(right, left) == 0, "string_comparep3");
	test(string_comparep(right, right) == 0, "string_comparep4");

	strvect_char_heap(&left, "HELLO");
	strvect_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep5");

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep6");
	test(string_comparep(right, left) > 0, "string_comparep7");

	strarray_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep8");

	RETURN;
}



/*
 *  main
 */
static int testbreak_strtype(void)
{
	/* string check */
	TestBreak(test_array_stringp);
	TestBreak(test_strarrayp);
	TestBreak(test_stringp);
	/* strarray */
	TestBreak(test_strarray_allocr);
	TestBreak(test_strarray_alloc);
	TestBreak(test_strarray_update_character_type);
	TestBreak(test_strarray_char_allocr);
	TestBreak(test_strarray_char_alloc);
	TestBreak(test_strarray_size1_allocr);
	TestBreak(test_strarray_size1_alloc);
	TestBreak(test_strarray_sizeu_allocr);
	TestBreak(test_strarray_sizeu_alloc);
	TestBreak(test_strarray_length);
	TestBreak(test_strarray_posbodylen);
	TestBreak(test_strarray_refc);
	TestBreak(test_strarray_equal_binary);
	TestBreak(test_strarray_equalp_binary);
	TestBreak(test_strarray_equal_char);
	TestBreak(test_strarray_equalp_char);
	TestBreak(test_strarray_equal);
	TestBreak(test_strarray_equalp);
	TestBreak(test_strarray_compare_binary);
	TestBreak(test_strarray_comparep_binary);
	TestBreak(test_strarray_compare_char);
	TestBreak(test_strarray_comparep_char);
	TestBreak(test_strarray_compare);
	TestBreak(test_strarray_comparep);
	/* string */
	TestBreak(test_string_length);
	TestBreak(test_string_posbodylen);
	TestBreak(test_string_refc);
	TestBreak(test_string_equal_binary);
	TestBreak(test_string_equalp_binary);
	TestBreak(test_string_equal_char);
	TestBreak(test_string_equalp_char);
	TestBreak(test_strvect_strarray_equal);
	TestBreak(test_string_equal);
	TestBreak(test_strvect_strarray_equalp);
	TestBreak(test_string_equalp);
	TestBreak(test_string_compare_binary);
	TestBreak(test_string_comparep_binary);
	TestBreak(test_string_compare_char);
	TestBreak(test_string_comparep_char);
	TestBreak(test_strvect_strarray_compare);
	TestBreak(test_strarray_strvect_compare);
	TestBreak(test_string_compare);
	TestBreak(test_strvect_strarray_comparep);
	TestBreak(test_strarray_strvect_comparep);
	TestBreak(test_string_comparep);

	return 0;
}

int test_strtype(void)
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
		build_package();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		lisp_initialize = 1;
		result = testbreak_strtype();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

