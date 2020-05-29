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
	test(array_stringp(pos), "array_stringp.1");

	array_va_heap(&pos, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(! array_stringp(pos), "array_stringp.2");

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_T;
	test(! array_stringp(pos), "array_stringp.3");

	RETURN;
}

static int test_strarrayp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(strarrayp(pos), "strarrayp.1");
	test(! strarrayp(Nil), "strarrayp.2");

	RETURN;
}

static int test_stringp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(stringp(pos), "stringp.1");

	strvect_char_heap(&pos, "Hello");
	test(stringp(pos), "stringp.2");
	test(! stringp(T), "stringp.3");

	RETURN;
}


/*
 *  strarray
 */
static int test_strarray_alloc(void)
{
	addr pos;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_alloc(NULL, &pos, 10);
	test(stringp(pos), "strarray_alloc.1");
	test(strarrayp(pos), "strarray_alloc.2");
	str = ArrayInfoStruct(pos);
	test(str->front == 10, "strarray_alloc.3");
	test(RefCharacterType(pos) == CHARACTER_TYPE_EMPTY, "strarray_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_local(local, &pos, 10);
	test(stringp(pos), "strarray_alloc.5");
	test(GetStatusDynamic(pos), "strarray_alloc.6");
	rollback_local(local, stack);

	strarray_heap(&pos, 10);
	test(stringp(pos), "strarray_alloc.7");
	test(! GetStatusDynamic(pos), "strarray_alloc.8");

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
			"strarray_update_character_type.1");

	body[0] = 'a';
	body[1] = 0x0D;
	body[2] = 'c';
	strarray_update_character_type(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_BASE,
			"strarray_update_character_type.2");

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
	test(strarrayp(pos), "strarray_char_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_char_alloc.2");
	test(str->front == 5, "strarray_char_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_char_alloc.4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_char_alloc.5");
	test(body[1] == 'e', "strarray_char_alloc.6");
	test(body[4] == 'o', "strarray_char_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_char_local(local, &pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc.8");
	test(GetStatusDynamic(pos), "strarray_char_alloc.9");
	rollback_local(local, stack);

	strarray_char_heap(&pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_char_alloc.11");

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
	test(strarrayp(pos), "strarray_size1_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_size1_alloc.2");
	test(str->front == 5, "strarray_size1_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_size1_alloc.4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_size1_alloc.5");
	test(body[1] == 'e', "strarray_size1_alloc.6");
	test(body[4] == 'o', "strarray_size1_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_size1_local(local, &pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc.8");
	test(GetStatusDynamic(pos), "strarray_size1_alloc.9");
	rollback_local(local, stack);

	strarray_size1_heap(&pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_size1_alloc.11");

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
	test(strarrayp(pos), "strarray_sizeu_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_sizeu_alloc.2");
	test(str->front == 5, "strarray_sizeu_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_sizeu_alloc.4");
	GetArrayUnicode(pos, &body);
	test(body[0] == 'H', "strarray_sizeu_alloc.5");
	test(body[1] == 'e', "strarray_sizeu_alloc.6");
	test(body[4] == 'o', "strarray_sizeu_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_sizeu_local(local, &pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc.8");
	test(GetStatusDynamic(pos), "strarray_sizeu_alloc.9");
	rollback_local(local, stack);

	strarray_sizeu_heap(&pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_sizeu_alloc.11");

	RETURN;
}

static int test_strarray_length(void)
{
	addr pos;
	size_t size;

	strarray_char_alloc(NULL, &pos, "Hello");
	strarray_length(pos, &size);
	test(size == 5, "strarray_length.1");

	RETURN;
}

static int test_strarray_refc(void)
{
	addr pos;
	unicode u;

	strarray_char_alloc(NULL, &pos, "Hello");
	test(strarray_refc(pos, 0) == 'H', "strarray_refc.1");
	test(strarray_refc(pos, 4) == 'o', "strarray_refc.2");
	strarray_setc(pos, 1, 'E');
	test(strarray_refc(pos, 1) == 'E', "strarray_setc.1");
	strarray_getc(pos, 2, &u);
	test(u == 'l', "strarray_getc.1");

	RETURN;
}

static int test_strarray_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary.1");
	strarray_char_heap(&pos, "Helloo");
	test(! strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary.2");
	strarray_char_heap(&pos, "HELLO");
	test(! strarray_equal_binary(pos, Hello, 5), "strarray_equal_binary.3");

	RETURN;
}

static int test_strarray_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary.1");
	strarray_char_heap(&pos, "Helloo");
	test(! strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary.2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_equalp_binary(pos, Hello, 5), "strarray_equalp_binary.3");

	RETURN;
}

static int test_strarray_equal_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equal_char(pos, "Hello"), "strarray_equal_char.1");
	test(! strarray_equal_char(pos, "Helloo"), "strarray_equal_char.2");
	test(! strarray_equal_char(pos, "HELLO"), "strarray_equal_char.3");

	RETURN;
}

static int test_strarray_equalp_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_equalp_char(pos, "Hello"), "strarray_equalp_char.1");
	test(! strarray_equalp_char(pos, "Helloo"), "strarray_equalp_char.2");
	test(strarray_equalp_char(pos, "HELLO"), "strarray_equalp_char.3");

	RETURN;
}

static int test_strarray_equal(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_equal(left, right), "strarray_equal.1");
	strarray_char_heap(&right, "Helloo");
	test(! strarray_equal(left, right), "strarray_equal.2");
	strarray_char_heap(&right, "HELLO");
	test(! strarray_equal(left, right), "strarray_equal.3");

	RETURN;
}

static int test_strarray_equalp(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_equalp(left, right), "strarray_equalp.1");
	strarray_char_heap(&right, "Helloo");
	test(! strarray_equalp(left, right), "strarray_equalp.2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_equalp(left, right), "strarray_equalp.3");

	RETURN;
}

static int test_strarray_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_compare_binary(pos, Hello, 5) == 0, "strarray_compare_binary.1");
	strarray_char_heap(&pos, "Helloo");
	test(strarray_compare_binary(pos, Hello, 5) > 0, "strarray_compare_binary.2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_compare_binary(pos, Hello, 5) != 0, "strarray_compare_binary.3");

	RETURN;
}

static int test_strarray_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_comparep_binary(pos, Hello, 5) == 0, "strarray_comparep_binary.1");
	strarray_char_heap(&pos, "Helloo");
	test(strarray_comparep_binary(pos, Hello, 5) > 0, "strarray_comparep_binary.2");
	strarray_char_heap(&pos, "HELLO");
	test(strarray_comparep_binary(pos, Hello, 5) == 0, "strarray_comparep_binary.3");

	RETURN;
}

static int test_strarray_compare_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_compare_char(pos, "Hello") == 0, "strarray_compare_char.1");
	test(strarray_compare_char(pos, "Helloo") < 0, "strarray_compare_char.2");
	test(strarray_compare_char(pos, "HELLO") != 0, "strarray_compare_char.3");

	RETURN;
}

static int test_strarray_comparep_char(void)
{
	addr pos;

	strarray_char_heap(&pos, "Hello");
	test(strarray_comparep_char(pos, "Hello") == 0, "strarray_comparep_char.1");
	test(strarray_comparep_char(pos, "Helloo") < 0, "strarray_comparep_char.2");
	test(strarray_comparep_char(pos, "HELLO") == 0, "strarray_comparep_char.3");

	RETURN;
}

static int test_strarray_compare(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_compare(left, right) == 0, "strarray_compare.1");
	strarray_char_heap(&right, "Helloo");
	test(strarray_compare(left, right) < 0, "strarray_compare.2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_compare(left, right) != 0, "strarray_compare.3");

	RETURN;
}

static int test_strarray_comparep(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strarray_comparep(left, right) == 0, "strarray_comparep.1");
	strarray_char_heap(&right, "Helloo");
	test(strarray_comparep(left, right) < 0, "strarray_comparep.2");
	strarray_char_heap(&right, "HELLO");
	test(strarray_comparep(left, right) == 0, "strarray_comparep.3");

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
	test(size == 5, "string_length.1");

	strarray_char_heap(&pos, "aaabbb");
	string_length(pos, &size);
	test(size == 6, "string_length.2");

	RETURN;
}

static int test_string_refc(void)
{
	addr pos;
	unicode u;

	strvect_char_heap(&pos, "Hello");
	test(string_refc(pos, 0) == 'H', "string_refc.1");
	string_setc(pos, 1, 'E');
	test(string_refc(pos, 1) == 'E', "string_setc.1");
	string_getc(pos, 4, &u);
	test(u == 'o', "string_getc.1");

	strarray_char_heap(&pos, "Hello");
	test(string_refc(pos, 0) == 'H', "string_refc.2");
	string_setc(pos, 1, 'E');
	test(string_refc(pos, 1) == 'E', "string_setc.2");
	string_getc(pos, 4, &u);
	test(u == 'o', "string_getc.2");

	RETURN;
}

static int test_string_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_equal_binary(pos, Hello, 5), "string_equal_binary.1");
	test(! string_equal_binary(pos, Hello, 4), "string_equal_binary.2");

	strarray_char_heap(&pos, "Hello");
	test(string_equal_binary(pos, Hello, 5), "string_equal_binary.3");
	test(! string_equal_binary(pos, Hello, 4), "string_equal_binary.4");

	RETURN;
}

static int test_string_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_equalp_binary(pos, Hello, 5), "string_equalp_binary.1");
	test(! string_equalp_binary(pos, Hello, 4), "string_equalp_binary.2");

	strarray_char_heap(&pos, "HELLO");
	test(string_equalp_binary(pos, Hello, 5), "string_equalp_binary.3");
	test(! string_equalp_binary(pos, Hello, 4), "string_equalp_binary.4");

	RETURN;
}

static int test_string_equal_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_equal_char(pos, "Hello"), "string_equal_char.1");
	test(! string_equal_char(pos, "Helloo"), "string_equal_char.2");

	strarray_char_heap(&pos, "Hello");
	test(string_equal_char(pos, "Hello"), "string_equal_char.3");
	test(! string_equal_char(pos, "Helloo"), "string_equal_char.4");

	RETURN;
}

static int test_string_equalp_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_equalp_char(pos, "Hello"), "string_equalp_char.1");
	test(! string_equalp_char(pos, "Helloo"), "string_equalp_char.2");

	strarray_char_heap(&pos, "HELLO");
	test(string_equalp_char(pos, "Hello"), "string_equalp_char.3");
	test(! string_equalp_char(pos, "Helloo"), "string_equalp_char.4");

	RETURN;
}

static int test_strarray_strvect_equal(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_equal(left, right), "strarray_strvect_equal.1");

	strarray_char_heap(&left, "HELLO");
	test(! strarray_strvect_equal(left, right), "strarray_strvect_equal.2");

	RETURN;
}

static int test_string_equal(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(string_equal(left, left), "string_equal.1");
	test(string_equal(left, right), "string_equal.2");
	test(string_equal(right, left), "string_equal.3");
	test(string_equal(right, right), "string_equal.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal.6");
	test(! string_equal(right, left), "string_equal.7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(! string_equal(left, right), "string_equal.8");

	RETURN;
}

static int test_strarray_strvect_equalp(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_equalp(left, right), "strarray_strvect_equalp.1");

	strarray_char_heap(&left, "HELLO");
	test(strarray_strvect_equalp(left, right), "strarray_strvect_equalp.2");

	strarray_char_heap(&left, "HELLOO");
	test(! strarray_strvect_equalp(left, right), "strarray_strvect_equalp.3");

	RETURN;
}

static int test_string_equalp(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLO");
	test(string_equalp(left, left), "string_equalp.1");
	test(string_equalp(left, right), "string_equalp.2");
	test(string_equalp(right, left), "string_equalp.3");
	test(string_equalp(right, right), "string_equalp.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp.6");
	test(! string_equalp(right, left), "string_equalp.7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "HELLOO");
	test(! string_equalp(left, right), "string_equalp.8");

	RETURN;
}

static int test_string_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_compare_binary(pos, Hello, 5) == 0, "string_compare_binary.1");
	test(string_compare_binary(pos, Hello, 4) > 0, "string_compare_binary.2");

	strarray_char_heap(&pos, "Hello");
	test(string_compare_binary(pos, Hello, 5) == 0, "string_compare_binary.3");
	test(string_compare_binary(pos, Hello, 4) > 0, "string_compare_binary.4");

	RETURN;
}

static int test_string_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_comparep_binary(pos, Hello, 5) == 0, "string_comparep_binary.1");
	test(string_comparep_binary(pos, Hello, 4) > 0, "string_comparep_binary.2");

	strarray_char_heap(&pos, "HELLO");
	test(string_comparep_binary(pos, Hello, 5) == 0, "string_comparep_binary.3");
	test(string_comparep_binary(pos, Hello, 4) > 0, "string_comparep_binary.4");

	RETURN;
}

static int test_string_compare_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "Hello");
	test(string_compare_char(pos, "Hello") == 0, "string_compare_char.1");
	test(string_compare_char(pos, "Helloo") < 0, "string_compare_char.2");

	strarray_char_heap(&pos, "Hello");
	test(string_compare_char(pos, "Hello") == 0, "string_compare_char.3");
	test(string_compare_char(pos, "Helloo") < 0, "string_compare_char.4");

	RETURN;
}

static int test_string_comparep_char(void)
{
	addr pos;

	strvect_char_heap(&pos, "HELLO");
	test(string_comparep_char(pos, "Hello") == 0, "string_comparep_char.1");
	test(string_comparep_char(pos, "Helloo") < 0, "string_comparep_char.2");

	strarray_char_heap(&pos, "HELLO");
	test(string_comparep_char(pos, "Hello") == 0, "string_comparep_char.3");
	test(string_comparep_char(pos, "Helloo") < 0, "string_comparep_char.4");

	RETURN;
}

static int test_strvect_strarray_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_compare(left, right) == 0, "strvect_strarray_compare.1");

	strarray_char_heap(&right, "Helloo");
	test(strvect_strarray_compare(left, right) < 0, "strvect_strarray_compare.2");

	RETURN;
}

static int test_strarray_strvect_compare(void)
{
	addr left, right;

	strarray_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_compare(left, right) == 0, "strarray_strvect_compare.1");

	strvect_char_heap(&right, "Helloo");
	test(strarray_strvect_compare(left, right) < 0, "strarray_strvect_compare.2");

	RETURN;
}

static int test_string_compare(void)
{
	addr left, right;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Hello");
	test(string_compare(left, left) == 0, "string_compare.1");
	test(string_compare(left, right) == 0, "string_compare.2");
	test(string_compare(right, left) == 0, "string_compare.3");
	test(string_compare(right, right) == 0, "string_compare.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare.6");
	test(string_compare(right, left) > 0, "string_compare.7");

	strarray_char_heap(&left, "Hello");
	strarray_char_heap(&right, "Helloo");
	test(string_compare(left, right) < 0, "string_compare.8");

	RETURN;
}

static int test_strvect_strarray_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Hello");
	test(strvect_strarray_comparep(left, right) == 0, "strvect_strarray_comparep.1");

	strarray_char_heap(&right, "Helloo");
	test(strvect_strarray_comparep(left, right) < 0, "strvect_strarray_comparep.2");

	RETURN;
}

static int test_strarray_strvect_comparep(void)
{
	addr left, right;

	strarray_char_heap(&left, "HELLO");
	strvect_char_heap(&right, "Hello");
	test(strarray_strvect_comparep(left, right) == 0, "strarray_strvect_comparep.1");

	strvect_char_heap(&right, "Helloo");
	test(strarray_strvect_comparep(left, right) < 0, "strarray_strvect_comparep.2");

	RETURN;
}

static int test_string_comparep(void)
{
	addr left, right;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Hello");
	test(string_comparep(left, left) == 0, "string_comparep.1");
	test(string_comparep(left, right) == 0, "string_comparep.2");
	test(string_comparep(right, left) == 0, "string_comparep.3");
	test(string_comparep(right, right) == 0, "string_comparep.4");

	strvect_char_heap(&left, "HELLO");
	strvect_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep.5");

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep.6");
	test(string_comparep(right, left) > 0, "string_comparep.7");

	strarray_char_heap(&left, "HELLO");
	strarray_char_heap(&right, "Helloo");
	test(string_comparep(left, right) < 0, "string_comparep.8");

	RETURN;
}


/*
 *  strtype
 */
static int testcase_strtype(void)
{
	/* string check */
	TestBreak(test_array_stringp);
	TestBreak(test_strarrayp);
	TestBreak(test_stringp);
	/* strarray */
	TestBreak(test_strarray_alloc);
	TestBreak(test_strarray_update_character_type);
	TestBreak(test_strarray_char_alloc);
	TestBreak(test_strarray_size1_alloc);
	TestBreak(test_strarray_sizeu_alloc);
	TestBreak(test_strarray_length);
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
	TestBreak(test_string_refc);
	TestBreak(test_string_equal_binary);
	TestBreak(test_string_equalp_binary);
	TestBreak(test_string_equal_char);
	TestBreak(test_string_equalp_char);
	TestBreak(test_strarray_strvect_equal);
	TestBreak(test_string_equal);
	TestBreak(test_strarray_strvect_equalp);
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

static void testinit_strtype(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_package();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
}

int test_strtype(void)
{
	TITLE;
	return degrade_code(
			testinit_strtype,
			testcase_strtype);
}

