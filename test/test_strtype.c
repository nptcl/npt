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

	array_va_heap_(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(array_stringp(pos), "array_stringp.1");

	array_va_heap_(&pos, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_CHARACTER;
	test(! array_stringp(pos), "array_stringp.2");

	array_va_heap_(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_T;
	test(! array_stringp(pos), "array_stringp.3");

	RETURN;
}

static int test_strarrayp(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap_(&pos, 10, 0);
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

	array_va_heap_(&pos, 10, 0);
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

	pos = Nil;
	strarray_alloc_(NULL, &pos, 10);
	test(stringp(pos), "strarray_alloc.1");
	test(strarrayp(pos), "strarray_alloc.2");
	str = ArrayInfoStruct(pos);
	test(str->front == 10, "strarray_alloc.3");
	test(RefCharacterType(pos) == CHARACTER_TYPE_EMPTY, "strarray_alloc.4");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_local_(local, &pos, 10);
	test(stringp(pos), "strarray_alloc.5");
	test(GetStatusDynamic(pos), "strarray_alloc.6");
	rollback_local(local, stack);

	strarray_heap_(&pos, 10);
	test(stringp(pos), "strarray_alloc.7");
	test(! GetStatusDynamic(pos), "strarray_alloc.8");

	RETURN;
}

static int test_strarray_update_character_type(void)
{
	addr pos;

	pos = Nil;
	strarray_heap_(&pos, 3);
	strarray_setc_(pos, 0, 'a');
	strarray_setc_(pos, 1, 'b');
	strarray_setc_(pos, 2, 'c');
	strarray_update_character_type_(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_STANDARD,
			"strarray_update_character_type.1");

	strarray_setc_(pos, 0, 'a');
	strarray_setc_(pos, 1, 0x0D);
	strarray_setc_(pos, 2, 'c');
	strarray_update_character_type_(pos);
	test(RefCharacterType(pos) == CHARACTER_TYPE_BASE,
			"strarray_update_character_type.2");

	RETURN;
}

static int test_strarray_char_alloc(void)
{
	addr pos;
	unicode c;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_char_alloc_(NULL, &pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_char_alloc.2");
	test(str->front == 5, "strarray_char_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_char_alloc.4");
	strarray_getc_(pos, 0, &c);
	test(c == 'H', "strarray_char_alloc.5");
	strarray_getc_(pos, 1, &c);
	test(c == 'e', "strarray_char_alloc.6");
	strarray_getc_(pos, 4, &c);
	test(c == 'o', "strarray_char_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_char_local_(local, &pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc.8");
	test(GetStatusDynamic(pos), "strarray_char_alloc.9");
	rollback_local(local, stack);

	strarray_char_heap_(&pos, "Hello");
	test(strarrayp(pos), "strarray_char_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_char_alloc.11");

	RETURN;
}

static int test_strarray_size1_alloc(void)
{
	addr pos;
	unicode c;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_size1_alloc_(NULL, &pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_size1_alloc.2");
	test(str->front == 5, "strarray_size1_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_size1_alloc.4");
	strarray_getc_(pos, 0, &c);
	test(c == 'H', "strarray_size1_alloc.5");
	strarray_getc_(pos, 1, &c);
	test(c == 'e', "strarray_size1_alloc.6");
	strarray_getc_(pos, 4, &c);
	test(c == 'o', "strarray_size1_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_size1_local_(local, &pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc.8");
	test(GetStatusDynamic(pos), "strarray_size1_alloc.9");
	rollback_local(local, stack);

	strarray_size1_heap_(&pos, "Hello", 5);
	test(strarrayp(pos), "strarray_size1_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_size1_alloc.11");

	RETURN;
}

static int test_strarray_sizeu_alloc(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	addr pos;
	unicode c;
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;

	strarray_sizeu_alloc_(NULL, &pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc.1");
	str = ArrayInfoStruct(pos);
	test(str->size == 5, "strarray_sizeu_alloc.2");
	test(str->front == 5, "strarray_sizeu_alloc.3");
	test(str->type == ARRAY_TYPE_CHARACTER, "strarray_sizeu_alloc.4");
	strarray_getc_(pos, 0, &c);
	test(c == 'H', "strarray_sizeu_alloc.5");
	strarray_getc_(pos, 1, &c);
	test(c == 'e', "strarray_sizeu_alloc.6");
	strarray_getc_(pos, 4, &c);
	test(c == 'o', "strarray_sizeu_alloc.7");

	local = Local_Thread;
	push_local(local, &stack);
	strarray_sizeu_local_(local, &pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc.8");
	test(GetStatusDynamic(pos), "strarray_sizeu_alloc.9");
	rollback_local(local, stack);

	strarray_sizeu_heap_(&pos, Hello, 5);
	test(strarrayp(pos), "strarray_sizeu_alloc.10");
	test(! GetStatusDynamic(pos), "strarray_sizeu_alloc.11");

	RETURN;
}

static int test_strarray_length(void)
{
	addr pos;
	size_t size;

	strarray_char_alloc_(NULL, &pos, "Hello");
	strarray_length(pos, &size);
	test(size == 5, "strarray_length.1");

	RETURN;
}

static int test_strarray_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_equal_binary_(pos, Hello, 5, &check);
	test(check, "strarray_equal_binary.1");
	strarray_char_heap_(&pos, "Helloo");
	strarray_equal_binary_(pos, Hello, 5, &check);
	test(! check, "strarray_equal_binary.2");
	strarray_char_heap_(&pos, "HELLO");
	strarray_equal_binary_(pos, Hello, 5, &check);
	test(! check, "strarray_equal_binary.3");

	RETURN;
}

static int test_strarray_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_equalp_binary_(pos, Hello, 5, &check);
	test(check, "strarray_equalp_binary.1");
	strarray_char_heap_(&pos, "Helloo");
	strarray_equalp_binary_(pos, Hello, 5, &check);
	test(! check, "strarray_equalp_binary.2");
	strarray_char_heap_(&pos, "HELLO");
	strarray_equalp_binary_(pos, Hello, 5, &check);
	test(check, "strarray_equalp_binary.3");

	RETURN;
}

static int test_strarray_equal_char(void)
{
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_equal_char_(pos, "Hello", &check);
	test(check, "strarray_equal_char.1");
	strarray_equal_char_(pos, "Helloo", &check);
	test(! check, "strarray_equal_char.2");
	strarray_equal_char_(pos, "HELLO", &check);
	test(! check, "strarray_equal_char.3");

	RETURN;
}

static int test_strarray_equalp_char(void)
{
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_equalp_char_(pos, "Hello", &check);
	test(check, "strarray_equalp_char.1");
	strarray_equalp_char_(pos, "Helloo", &check);
	test(! check, "strarray_equalp_char.2");
	strarray_equalp_char_(pos, "HELLO", &check);
	test(check, "strarray_equalp_char.3");

	RETURN;
}

static int test_strarray_equal(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	strarray_equal_(left, right, &check);
	test(check, "strarray_equal.1");
	strarray_char_heap_(&right, "Helloo");
	strarray_equal_(left, right, &check);
	test(! check, "strarray_equal.2");
	strarray_char_heap_(&right, "HELLO");
	strarray_equal_(left, right, &check);
	test(! check, "strarray_equal.3");

	RETURN;
}

static int test_strarray_equalp(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	strarray_equalp_(left, right, &check);
	test(check, "strarray_equalp.1");
	strarray_char_heap_(&right, "Helloo");
	strarray_equalp_(left, right, &check);
	test(! check, "strarray_equalp.2");
	strarray_char_heap_(&right, "HELLO");
	strarray_equalp_(left, right, &check);
	test(check, "strarray_equalp.3");

	RETURN;
}

static int test_strarray_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_compare_binary_(pos, Hello, 5, &check);
	test(check == 0, "strarray_compare_binary.1");
	strarray_char_heap_(&pos, "Helloo");
	strarray_compare_binary_(pos, Hello, 5, &check);
	test(check > 0, "strarray_compare_binary.2");
	strarray_char_heap_(&pos, "HELLO");
	strarray_compare_binary_(pos, Hello, 5, &check);
	test(check != 0, "strarray_compare_binary.3");

	RETURN;
}

static int test_strarray_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_comparep_binary_(pos, Hello, 5, &check);
	test(check == 0, "strarray_comparep_binary.1");
	strarray_char_heap_(&pos, "Helloo");
	strarray_comparep_binary_(pos, Hello, 5, &check);
	test(check > 0, "strarray_comparep_binary.2");
	strarray_char_heap_(&pos, "HELLO");
	strarray_comparep_binary_(pos, Hello, 5, &check);
	test(check == 0, "strarray_comparep_binary.3");

	RETURN;
}

static int test_strarray_compare_char(void)
{
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_compare_char_(pos, "Hello", &check);
	test(check == 0, "strarray_compare_char.1");
	strarray_compare_char_(pos, "Helloo", &check);
	test(check < 0, "strarray_compare_char.2");
	strarray_compare_char_(pos, "HELLO", &check);
	test(check != 0, "strarray_compare_char.3");

	RETURN;
}

static int test_strarray_comparep_char(void)
{
	int check;
	addr pos;

	check = 0;

	strarray_char_heap_(&pos, "Hello");
	strarray_comparep_char_(pos, "Hello", &check);
	test(check == 0, "strarray_comparep_char.1");
	strarray_comparep_char_(pos, "Helloo", &check);
	test(check < 0, "strarray_comparep_char.2");
	strarray_comparep_char_(pos, "HELLO", &check);
	test(check == 0, "strarray_comparep_char.3");

	RETURN;
}

static int test_strarray_compare(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	strarray_compare_(left, right, &check);
	test(check == 0, "strarray_compare.1");
	strarray_char_heap_(&right, "Helloo");
	strarray_compare_(left, right, &check);
	test(check < 0, "strarray_compare.2");
	strarray_char_heap_(&right, "HELLO");
	strarray_compare_(left, right, &check);
	test(check != 0, "strarray_compare.3");

	RETURN;
}

static int test_strarray_comparep(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	strarray_comparep_(left, right, &check);
	test(check == 0, "strarray_comparep.1");
	strarray_char_heap_(&right, "Helloo");
	strarray_comparep_(left, right, &check);
	test(check < 0, "strarray_comparep.2");
	strarray_char_heap_(&right, "HELLO");
	strarray_comparep_(left, right, &check);
	test(check == 0, "strarray_comparep.3");

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

	strarray_char_heap_(&pos, "aaabbb");
	string_length(pos, &size);
	test(size == 6, "string_length.2");

	RETURN;
}

static int test_string_equal_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "Hello");
	string_equal_binary_(pos, Hello, 5, &check);
	test(check, "string_equal_binary.1");
	string_equal_binary_(pos, Hello, 4, &check);
	test(! check, "string_equal_binary.2");

	strarray_char_heap_(&pos, "Hello");
	string_equal_binary_(pos, Hello, 5, &check);
	test(check, "string_equal_binary.3");
	string_equal_binary_(pos, Hello, 4, &check);
	test(! check, "string_equal_binary.4");

	RETURN;
}

static int test_string_equalp_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "HELLO");
	string_equalp_binary_(pos, Hello, 5, &check);
	test(check, "string_equalp_binary.1");
	string_equalp_binary_(pos, Hello, 4, &check);
	test(! check, "string_equalp_binary.2");

	strarray_char_heap_(&pos, "HELLO");
	string_equalp_binary_(pos, Hello, 5, &check);
	test(check, "string_equalp_binary.3");
	string_equalp_binary_(pos, Hello, 4, &check);
	test(! check, "string_equalp_binary.4");

	RETURN;
}

static int test_string_equal_char(void)
{
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "Hello");
	string_equal_char_(pos, "Hello", &check);
	test(check, "string_equal_char.1");
	string_equal_char_(pos, "Helloo", &check);
	test(! check, "string_equal_char.2");

	strarray_char_heap_(&pos, "Hello");
	string_equal_char_(pos, "Hello", &check);
	test(check, "string_equal_char.3");
	string_equal_char_(pos, "Helloo", &check);
	test(! check, "string_equal_char.4");

	RETURN;
}

static int test_string_equalp_char(void)
{
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "HELLO");
	string_equalp_char_(pos, "Hello", &check);
	test(check, "string_equalp_char.1");
	string_equalp_char_(pos, "Helloo", &check);
	test(! check, "string_equalp_char.2");

	strarray_char_heap_(&pos, "HELLO");
	string_equalp_char_(pos, "Hello", &check);
	test(check, "string_equalp_char.3");
	string_equalp_char_(pos, "Helloo", &check);
	test(! check, "string_equalp_char.4");

	RETURN;
}

static int test_strarray_strvect_equal(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	strarray_strvect_equal_(left, right, &check);
	test(check, "strarray_strvect_equal.1");

	strarray_char_heap_(&left, "HELLO");
	strarray_strvect_equal_(left, right, &check);
	test(! check, "strarray_strvect_equal.2");

	RETURN;
}

static int test_string_equal(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	string_equal_(left, left, &check);
	test(check, "string_equal.1");
	string_equal_(left, right, &check);
	test(check, "string_equal.2");
	string_equal_(right, left, &check);
	test(check, "string_equal.3");
	string_equal_(right, right, &check);
	test(check, "string_equal.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLO");
	string_equal_(left, right, &check);
	test(! check, "string_equal.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "HELLO");
	string_equal_(left, right, &check);
	test(! check, "string_equal.6");
	string_equal_(right, left, &check);
	test(! check, "string_equal.7");

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "HELLO");
	string_equal_(left, right, &check);
	test(! check, "string_equal.8");

	RETURN;
}

static int test_strarray_strvect_equalp(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	strarray_strvect_equalp_(left, right, &check);
	test(check, "strarray_strvect_equalp.1");

	strarray_char_heap_(&left, "HELLO");
	strarray_strvect_equalp_(left, right, &check);
	test(check, "strarray_strvect_equalp.2");

	strarray_char_heap_(&left, "HELLOO");
	strarray_strvect_equalp_(left, right, &check);
	test(! check, "strarray_strvect_equalp.3");

	RETURN;
}

static int test_string_equalp(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "HELLO");
	string_equalp_(left, left, &check);
	test(check, "string_equalp.1");
	string_equalp_(left, right, &check);
	test(check, "string_equalp.2");
	string_equalp_(right, left, &check);
	test(check, "string_equalp.3");
	string_equalp_(right, right, &check);
	test(check, "string_equalp.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "HELLOO");
	string_equalp_(left, right, &check);
	test(! check, "string_equalp.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "HELLOO");
	string_equalp_(left, right, &check);
	test(! check, "string_equalp.6");
	string_equalp_(right, left, &check);
	test(! check, "string_equalp.7");

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "HELLOO");
	string_equalp_(left, right, &check);
	test(! check, "string_equalp.8");

	RETURN;
}

static int test_string_compare_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "Hello");
	string_compare_binary_(pos, Hello, 5, &check);
	test(check == 0, "string_compare_binary.1");
	string_compare_binary_(pos, Hello, 4, &check);
	test(check > 0, "string_compare_binary.2");

	strarray_char_heap_(&pos, "Hello");
	string_compare_binary_(pos, Hello, 5, &check);
	test(check == 0, "string_compare_binary.3");
	string_compare_binary_(pos, Hello, 4, &check);
	test(check > 0, "string_compare_binary.4");

	RETURN;
}

static int test_string_comparep_binary(void)
{
	const unicode Hello[] = {'H', 'e', 'l', 'l', 'o'};
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "HELLO");
	string_comparep_binary_(pos, Hello, 5, &check);
	test(check == 0, "string_comparep_binary.1");
	string_comparep_binary_(pos, Hello, 4, &check);
	test(check > 0, "string_comparep_binary.2");

	strarray_char_heap_(&pos, "HELLO");
	string_comparep_binary_(pos, Hello, 5, &check);
	test(check == 0, "string_comparep_binary.3");
	string_comparep_binary_(pos, Hello, 4, &check);
	test(check > 0, "string_comparep_binary.4");

	RETURN;
}

static int test_string_compare_char(void)
{
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "Hello");
	string_compare_char_(pos, "Hello", &check);
	test(check == 0, "string_compare_char.1");
	string_compare_char_(pos, "Helloo", &check);
	test(check < 0, "string_compare_char.2");

	strarray_char_heap_(&pos, "Hello");
	string_compare_char_(pos, "Hello", &check);
	test(check == 0, "string_compare_char.3");
	string_compare_char_(pos, "Helloo", &check);
	test(check < 0, "string_compare_char.4");

	RETURN;
}

static int test_string_comparep_char(void)
{
	int check;
	addr pos;

	check = 0;

	strvect_char_heap(&pos, "HELLO");
	string_comparep_char_(pos, "Hello", &check);
	test(check == 0, "string_comparep_char.1");
	string_comparep_char_(pos, "Helloo", &check);
	test(check < 0, "string_comparep_char.2");

	strarray_char_heap_(&pos, "HELLO");
	string_comparep_char_(pos, "Hello", &check);
	test(check == 0, "string_comparep_char.3");
	string_comparep_char_(pos, "Helloo", &check);
	test(check < 0, "string_comparep_char.4");

	RETURN;
}

static int test_strvect_strarray_compare(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	strvect_strarray_compare_(left, right, &check);
	test(check == 0, "strvect_strarray_compare.1");

	strarray_char_heap_(&right, "Helloo");
	strvect_strarray_compare_(left, right, &check);
	test(check < 0, "strvect_strarray_compare.2");

	RETURN;
}

static int test_strarray_strvect_compare(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "Hello");
	strvect_char_heap(&right, "Hello");
	strarray_strvect_compare_(left, right, &check);
	test(check == 0, "strarray_strvect_compare.1");

	strvect_char_heap(&right, "Helloo");
	strarray_strvect_compare_(left, right, &check);
	test(check < 0, "strarray_strvect_compare.2");

	RETURN;
}

static int test_string_compare(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "Hello");
	string_compare_(left, left, &check);
	test(check == 0, "string_compare.1");
	string_compare_(left, right, &check);
	test(check == 0, "string_compare.2");
	string_compare_(right, left, &check);
	test(check == 0, "string_compare.3");
	string_compare_(right, right, &check);
	test(check == 0, "string_compare.4");

	strvect_char_heap(&left, "Hello");
	strvect_char_heap(&right, "Helloo");
	string_compare_(left, right, &check);
	test(check < 0, "string_compare.5");

	strvect_char_heap(&left, "Hello");
	strarray_char_heap_(&right, "Helloo");
	string_compare_(left, right, &check);
	test(check < 0, "string_compare.6");
	string_compare_(right, left, &check);
	test(check > 0, "string_compare.7");

	strarray_char_heap_(&left, "Hello");
	strarray_char_heap_(&right, "Helloo");
	string_compare_(left, right, &check);
	test(check < 0, "string_compare.8");

	RETURN;
}

static int test_strvect_strarray_comparep(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap_(&right, "Hello");
	strvect_strarray_comparep_(left, right, &check);
	test(check == 0, "strvect_strarray_comparep.1");

	strarray_char_heap_(&right, "Helloo");
	strvect_strarray_comparep_(left, right, &check);
	test(check < 0, "strvect_strarray_comparep.2");

	RETURN;
}

static int test_strarray_strvect_comparep(void)
{
	int check;
	addr left, right;

	check = 0;

	strarray_char_heap_(&left, "HELLO");
	strvect_char_heap(&right, "Hello");
	strarray_strvect_comparep_(left, right, &check);
	test(check == 0, "strarray_strvect_comparep.1");

	strvect_char_heap(&right, "Helloo");
	strarray_strvect_comparep_(left, right, &check);
	test(check < 0, "strarray_strvect_comparep.2");

	RETURN;
}

static int test_string_comparep(void)
{
	int check;
	addr left, right;

	check = 0;

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap_(&right, "Hello");
	string_comparep_(left, left, &check);
	test(check == 0, "string_comparep.1");
	string_comparep_(left, right, &check);
	test(check == 0, "string_comparep.2");
	string_comparep_(right, left, &check);
	test(check == 0, "string_comparep.3");
	string_comparep_(right, right, &check);
	test(check == 0, "string_comparep.4");

	strvect_char_heap(&left, "HELLO");
	strvect_char_heap(&right, "Helloo");
	string_comparep_(left, right, &check);
	test(check < 0, "string_comparep.5");

	strvect_char_heap(&left, "HELLO");
	strarray_char_heap_(&right, "Helloo");
	string_comparep_(left, right, &check);
	test(check < 0, "string_comparep.6");
	string_comparep_(right, left, &check);
	test(check > 0, "string_comparep.7");

	strarray_char_heap_(&left, "HELLO");
	strarray_char_heap_(&right, "Helloo");
	string_comparep_(left, right, &check);
	test(check < 0, "string_comparep.8");

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

