#include "radix.c"
#include "character.h"
#include "charqueue.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "eval.h"
#include "function.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "readtable.h"
#include "strtype.h"
#include "stream.h"
#include "stream_string.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"
#include "unicode.h"

#define FILE_CASE_LARGE     "test/case_large.txt"
#define FILE_CASE_ROMA      "test/case_roma.txt"

static int equalstream(addr stream, const char *right)
{
	int result;
	addr left;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	string_stream_local(local, stream, &left);
	result = string_equal_char(left, right);
	rollback_local(local, stack);
	clear_output_string_stream(stream);

	return result;
}

static void makeinputstring(LocalRoot local, addr queue, addr list)
{
	addr pos;

	if (list == Nil) return;
	GetCons(list, &pos, &list);
	makeinputstring(local, queue, list);
	pushstring_charqueue_local(local, queue, pos);
}

static int equalinputstring(english input, const char *right)
{
	int result;
	addr queue, left;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	makeinputstring(local, queue, input->string);
	make_charqueue_local(local, queue, &left);
	result = string_equal_char(left, right);
	rollback_local(local, stack);

	return result;
}


/*
 *  english
 */
static int test_name_standard_char(void)
{
	const char *ptr;

	ptr = name_standard_char(0, 0);
	test(strcmp(ptr, "ni") == 0, "name_standard_char1");
	ptr = name_standard_char(2, 0);
	test(strcmp(ptr, "bi") == 0, "name_standard_char2");
	ptr = name_standard_char(3, 0);
	test(strcmp(ptr, "tri") == 0, "name_standard_char3");

	ptr = name_standard_char(0, 1);
	test(strcmp(ptr, "thousand") == 0, "name_standard_char4");
	ptr = name_standard_char(1, 1);
	test(strcmp(ptr, "million") == 0, "name_standard_char5");
	ptr = name_standard_char(9, 1);
	test(strcmp(ptr, "nonillion") == 0, "name_standard_char6");

	RETURN;
}

static int test_english_string(void)
{
	addr pos1, pos2, left, right;
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	input->string = Nil;

	push_local(local, &stack);
	english_string(input, "HELLO");
	pos1 = input->string;
	test(consp(pos1), "english_string1");
	GetCons(pos1, &left, &right);
	test(string_equal_char(left, "HELLO"), "english_string2");
	test(right == Nil, "english_string3");

	english_string(input, "AAA");
	pos2 = input->string;
	GetCons(pos2, &left, &right);
	test(string_equal_char(left, "AAA"), "english_string4");
	test(right == pos1, "english_string5");

	rollback_local(local, stack);

	RETURN;
}

static int test_english_char(void)
{
	addr pos1, pos2, left, right;
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	input->string = Nil;

	push_local(local, &stack);
	english_char(input, 'a');
	pos1 = input->string;
	test(consp(pos1), "english_char1");
	GetCons(pos1, &left, &right);
	test(string_equal_char(left, "a"), "english_char2");
	test(right == Nil, "english_char3");

	english_char(input, 'b');
	pos2 = input->string;
	GetCons(pos2, &left, &right);
	test(string_equal_char(left, "b"), "english_char4");
	test(right == pos1, "english_char5");

	rollback_local(local, stack);

	RETURN;
}

static int test_name_table_1(void)
{
	const char *a, *b;

	name_table_1(&a, &b, 6);
	test(strcmp(a, "se") == 0, "name_table_1-1");
	test(strcmp(b, "sx") == 0, "name_table_1-2");

	RETURN;
}

static int test_name_table_10(void)
{
	const char *a, *b;

	name_table_10(&a, &b, 5);
	test(strcmp(a, "quinquaginta") == 0, "name_table_10-1");
	test(strcmp(b, "ns") == 0, "name_table_10-2");

	RETURN;
}

static int test_name_table_100(void)
{
	const char *a, *b;

	name_table_100(&a, &b, 8);
	test(strcmp(a, "octingenti") == 0, "name_table_100-1");
	test(strcmp(b, "mx") == 0, "name_table_100-2");

	RETURN;
}

static int test_name_concat(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;

	push_local(local, &stack);
	input->string = Nil;
	name_concat(input, "AA", "*", "BB", "bacsdef");
	pos = input->string;
	test(consp(pos), "name_concat1");
	test(length_list_unsafe(pos) == 3, "name_concat2");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "BB"), "name_concat3");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "s"), "name_concat4");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "AA"), "name_concat5");

	input->string = Nil;
	name_concat(input, "AA", "*", "BB", "xbacdef");
	pos = input->string;
	test(length_list_unsafe(pos) == 3, "name_concat6");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "BB"), "name_concat7");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "s"), "name_concat8");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "AA"), "name_concat9");

	input->string = Nil;
	name_concat(input, "AA", "*", "BB", "bacdef");
	pos = input->string;
	test(length_list_unsafe(pos) == 2, "name_concat10");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "BB"), "name_concat11");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "AA"), "name_concat12");

	input->string = Nil;
	name_concat(input, "AA", "cd", "BB", "abdef");
	pos = input->string;
	test(length_list_unsafe(pos) == 3, "name_concat13");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "BB"), "name_concat14");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "d"), "name_concat15");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "AA"), "name_concat16");

	input->string = Nil;
	name_concat(input, "AA", "cd", "BB", "abef");
	pos = input->string;
	test(length_list_unsafe(pos) == 2, "name_concat17");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "BB"), "name_concat18");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "AA"), "name_concat19");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_front(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;

	push_local(local, &stack);
	input->string = Nil;
	number_name_front(input, 400);
	test(length_list_unsafe(input->string) == 1, "number_name_front1");
	test(equalinputstring(input, "quadringenti"), "number_name_front2");

	input->string = Nil;
	number_name_front(input, 30);
	test(length_list_unsafe(input->string) == 1, "number_name_front3");
	test(equalinputstring(input, "triginta"), "number_name_front4");

	input->string = Nil;
	number_name_front(input, 820);
	test(length_list_unsafe(input->string) == 2, "number_name_front5");
	test(equalinputstring(input, "vigintioctingenti"), "number_name_front6");

	input->string = Nil;
	number_name_front(input, 103);
	test(length_list_unsafe(input->string) == 3, "number_name_front7");
	test(equalinputstring(input, "trescenti"), "number_name_front8");

	input->string = Nil;
	number_name_front(input, 76);
	test(length_list_unsafe(input->string) == 2, "number_name_front9");
	test(equalinputstring(input, "seseptuaginta"), "number_name_front10");

	input->string = Nil;
	number_name_front(input, 457);
	test(length_list_unsafe(input->string) == 4, "number_name_front11");
	test(equalinputstring(input, "septenquinquagintaquadringenti"),
			"number_name_front12");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_standard(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;

	push_local(local, &stack);

	input->string = Nil;
	number_name_standard(input, 3, 0);
	test(equalinputstring(input, "tri"), "number_name_standard1");

	input->string = Nil;
	number_name_standard(input, 3, 1);
	test(equalinputstring(input, "trillion"), "number_name_standard2");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_1000(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	push_local(local, &stack);

	input->string = Nil;
	number_name_1000(input, 0);
	test(equalinputstring(input, "ni"), "number_name_1000-1");

	input->string = Nil;
	number_name_1000(input, 2);
	test(equalinputstring(input, "bi"), "number_name_1000-2");

	input->string = Nil;
	number_name_1000(input, 9);
	test(equalinputstring(input, "noni"), "number_name_1000-3");

	input->string = Nil;
	number_name_1000(input, 10);
	test(equalinputstring(input, "deci"), "number_name_1000-4");

	input->string = Nil;
	number_name_1000(input, 19);
	test(equalinputstring(input, "novendeci"), "number_name_1000-5");

	input->string = Nil;
	number_name_1000(input, 809);
	test(equalinputstring(input, "novemoctingenti"), "number_name_1000-6");

	input->string = Nil;
	number_name_1000(input, 889);
	test(equalinputstring(input, "novemoctogintaoctingenti"), "number_name_1000-7");

	input->string = Nil;
	number_name_1000(input, 999);
	test(equalinputstring(input, "novenonagintanongenti"), "number_name_1000-8");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_vowel(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	push_local(local, &stack);

	input->string = Nil;
	english_string(input, "Hello");
	number_name_vowel(input);
	test(equalinputstring(input, "Hell"), "number_name_vowel1");

	input->string = Nil;
	english_string(input, "Hellq");
	number_name_vowel(input);
	test(equalinputstring(input, "Hellq"), "number_name_vowel2");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_extend(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	push_local(local, &stack);

	input->string = Nil;
	number_name_extend(input, 11);
	test(equalinputstring(input, "undecilli"),
			"number_name_extend1");

	input->string = Nil;
	number_name_extend(input, 457);
	test(equalinputstring(input, "septenquinquagintaquadringentilli"),
			"number_name_extend2");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_recursive(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	push_local(local, &stack);

	input->string = Nil;
	number_name_recursive(input, 11);
	test(equalinputstring(input, "undecilli"),
			"number_name_recursive1");

	input->string = Nil;
	number_name_recursive(input, 457);
	test(equalinputstring(input, "septenquinquagintaquadringentilli"),
			"number_name_recursive2");

	input->string = Nil;
	number_name_recursive(input, 1000);
	test(equalinputstring(input, "millinilli"),
			"number_name_recursive3");

	input->string = Nil;
	number_name_recursive(input, 1234);
	test(equalinputstring(input, "milliquattuortrigintaducentilli"),
			"number_name_recursive4");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_index(void)
{
	LocalRoot local;
	LocalStack stack;
	struct english_struct str, *input;

	local = Local_Thread;
	input = &str;
	input->local = local;
	push_local(local, &stack);

	input->string = Nil;
	number_name_index(input, 0);
	test(equalinputstring(input, "thousand"), "number_name_index1");

	input->string = Nil;
	number_name_index(input, 1);
	test(equalinputstring(input, "million"), "number_name_index2");

	input->string = Nil;
	number_name_index(input, 9);
	test(equalinputstring(input, "nonillion"), "number_name_index3");

	input->string = Nil;
	number_name_index(input, 10);
	test(equalinputstring(input, "decillion"), "number_name_index4");

	input->string = Nil;
	number_name_index(input, 11);
	test(equalinputstring(input, "undecillion"), "number_name_index5");

	input->string = Nil;
	number_name_index(input, 36);
	test(equalinputstring(input, "sestrigintillion"), "number_name_index6");

	input->string = Nil;
	number_name_index(input, 457);
	test(equalinputstring(input, "septenquinquagintaquadringentillion"),
			"number_name_index7");

	input->string = Nil;
	number_name_index(input, 1000);
	test(equalinputstring(input, "millinillion"), "number_name_index8");

	input->string = Nil;
	number_name_index(input, 1001);
	test(equalinputstring(input, "millimillion"), "number_name_index9");

	input->string = Nil;
	number_name_index(input, 1002);
	test(equalinputstring(input, "millibillion"), "number_name_index10");

	input->string = Nil;
	number_name_index(input, 1234);
	test(equalinputstring(input, "milliquattuortrigintaducentillion"),
			"number_name_index11");

	input->string = Nil;
	number_name_index(input, 6560);
	test(equalinputstring(input, "sextillisexagintaquingentillion"),
			"number_name_index12");

	input->string = Nil;
	number_name_index(input, 19683);
	test(equalinputstring(input, "novendecillitresoctogintasescentillion"),
			"number_name_index13");

	input->string = Nil;
	number_name_index(input, 1000000);
	test(equalinputstring(input, "millinillinillion"),
			"number_name_index14");

	rollback_local(local, stack);

	RETURN;
}

static int test_radix_table_20(void)
{
	const char *ptr;

	ptr = radix_table_20(0, 1);
	test(strcmp(ptr, "zero") == 0, "radix_table_20-1");
	ptr = radix_table_20(3, 0);
	test(strcmp(ptr, "third") == 0, "radix_table_20-2");
	ptr = radix_table_20(11, 1);
	test(strcmp(ptr, "eleven") == 0, "radix_table_20-3");
	ptr = radix_table_20(19, 0);
	test(strcmp(ptr, "nineteenth") == 0, "radix_table_20-4");

	RETURN;
}

static int test_radix_table_100(void)
{
	const char *ptr;

	ptr = radix_table_100(2, 0);
	test(strcmp(ptr, "twentieth") == 0, "radix_table_100-1");
	ptr = radix_table_100(4, 1);
	test(strcmp(ptr, "forty") == 0, "radix_table_100-2");
	ptr = radix_table_100(9, 1);
	test(strcmp(ptr, "ninety") == 0, "radix_table_100-3");

	RETURN;
}

static int test_push_radix(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);

	input.local = local;
	input.root = Nil;
	input.cardinal = 0;

	push_radix(&input, T);
	test(consp(input.root), "push_radix1");
	test(input.cardinal == 1, "push_radix2");
	GetCons(input.root, &check, &pos);
	test(check == T, "push_radix3");
	test(pos == Nil, "push_radix4");

	push_radix(&input, Nil);
	GetCons(input.root, &check, &pos);
	test(check == Nil, "push_radix5");
	test(pos != Nil, "push_radix6");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_radix_char(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);

	input.local = local;
	input.root = Nil;
	input.cardinal = 0;

	push_radix_char(&input, "HELLO");
	test(consp(input.root), "push_radix_char1");
	test(input.cardinal == 1, "push_radix_char2");
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "HELLO"), "push_radix_char3");
	test(pos == Nil, "push_radix_char4");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_radix_list(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);

	input.local = local;
	input.root = Nil;
	input.cardinal = 0;

	list_local(local, &pos,
			fixnum_localr(local, 10),
			fixnum_localr(local, 20),
			fixnum_localr(local, 30),
			NULL);
	push_radix_list(&input, pos);
	pos = input.root;
	test(consp(pos), "push_radix_list1");
	test(input.cardinal == 1, "push_radix_list2");
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 3, "push_radix_list3");
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 30, "push_radix_list4");
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 20, "push_radix_list5");
	GetCons(pos, &check, &pos);
	test(RefFixnum(check) == 10, "push_radix_list6");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_radix_20(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);

	input.local = local;
	input.root = Nil;
	input.cardinal = 0;

	push_radix_20(&input, 3);
	push_radix_20(&input, 3);
	pos = input.root;
	test(input.cardinal == 1, "push_radix_20-1");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "three"), "push_radix_20-2");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "third"), "push_radix_20-3");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_radix_100(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	input.local = local;

	input.root = Nil;
	input.cardinal = 0;
	push_radix_100(&input, 15);
	GetCar(input.root, &check);
	test(string_equal_char(check, "fifteenth"), "push_radix_100-1");

	input.root = Nil;
	input.cardinal = 1;
	push_radix_100(&input, 16);
	GetCar(input.root, &check);
	test(string_equal_char(check, "sixteen"), "push_radix_100-2");

	input.root = Nil;
	input.cardinal = 0;
	push_radix_100(&input, 34);
	GetCar(input.root, &pos);
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "thirty"), "push_radix_100-3");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "-"), "push_radix_100-4");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "fourth"), "push_radix_100-5");
	test(pos == Nil, "push_radix_100-6");

	input.root = Nil;
	input.cardinal = 1;
	push_radix_100(&input, 91);
	GetCar(input.root, &pos);
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "ninety"), "push_radix_100-7");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "-"), "push_radix_100-8");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "one"), "push_radix_100-9");
	test(pos == Nil, "push_radix_100-10");

	input.root = Nil;
	input.cardinal = 0;
	push_radix_100(&input, 80);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "eightieth"), "push_radix_100-11");
	test(pos == Nil, "push_radix_100-12");

	input.root = Nil;
	input.cardinal = 1;
	push_radix_100(&input, 20);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "twenty"), "push_radix_100-13");
	test(pos == Nil, "push_radix_100-14");

	input.root = Nil;
	input.cardinal = 0;
	push_radix_100(&input, 0);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "zeroth"), "push_radix_100-15");
	test(pos == Nil, "push_radix_100-16");

	input.root = Nil;
	input.cardinal = 1;
	push_radix_100(&input, 9);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "nine"), "push_radix_100-17");
	test(pos == Nil, "push_radix_100-18");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_radix_hundred(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	input.local = local;

	input.root = Nil;
	input.cardinal = 0;
	push_radix_hundred(&input);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "hundredth"), "push_radix_hundred1");
	test(pos == Nil, "push_radix_hundred2");

	input.root = Nil;
	input.cardinal = 1;
	push_radix_hundred(&input);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "hundred"), "push_radix_hundred3");
	test(pos == Nil, "push_radix_hundred4");

	rollback_local(local, stack);

	RETURN;
}

static int test_number_name_cardinal(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	input.local = local;

	input.root = Nil;
	input.index = 0;
	input.cardinal = 0;
	number_name_cardinal(&input);
	GetCar(input.root, &pos);
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "thousand"), "number_name_cardinal1");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "th"), "number_name_cardinal2");
#ifdef ENGLISH_RADIX_MODE1
	test(pos == Nil, "number_name_cardinal-macro1");
#endif

	input.root = T;
	input.index = 1;
	input.cardinal = 0;
	number_name_cardinal(&input);
	GetCar(input.root, &pos);
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "million"), "number_name_cardinal3");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "th"), "number_name_cardinal4");
#ifdef ENGLISH_RADIX_MODE1
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, ENGLISH_RADIX_MODE1),
			"number_name_cardinal-macro2");
#endif

	input.root = T;
	input.index = 4;
	input.cardinal = 1;
	number_name_cardinal(&input);
	GetCar(input.root, &pos);
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "quadrillion"), "number_name_cardinal5");
#ifdef ENGLISH_RADIX_MODE1
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, ENGLISH_RADIX_MODE1),
			"number_name_cardinal-macro3");
#endif

	rollback_local(local, stack);

	RETURN;
}

static int test_english_execute_loop(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	input.local = local;

	input.root = Nil;
	input.cardinal = 1;
	input.index = 0;
	bignum_value_alloc(local, &pos, signplus_bignum, 0);
	input.pos = pos;
	english_execute_loop(&input);
	test(input.root == Nil, "english_execute_loop1");

	input.root = Nil;
	input.cardinal = 1;
	input.index = 7;
	bignum_value_alloc(local, &pos, signplus_bignum, 203);
	input.pos = pos;
	english_execute_loop(&input);
	pos = input.root;
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "two"), "english_execute_loop2");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "hundred"), "english_execute_loop3");
#ifdef ENGLISH_RADIX_MODE2
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, ENGLISH_RADIX_MODE2), "english_execute_loop_macro");
#endif
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "three"), "english_execute_loop4");
	GetCons(pos, &check, &pos);
	GetCar(check, &check);
	test(string_equal_char(check, "septillion"), "english_execute_loop5");

	input.root = Nil;
	input.cardinal = 0;
	input.index = -1;
	bignum_value_alloc(local, &pos, signplus_bignum, 500);
	input.pos = pos;
	english_execute_loop(&input);
	pos = input.root;
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "five"), "english_execute_loop6");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "hundredth"), "english_execute_loop7");

	input.root = Nil;
	input.cardinal = 0;
	input.index = 2;
	bignum_value_alloc(local, &pos, signplus_bignum, 11);
	input.pos = pos;
	english_execute_loop(&input);
	pos = input.root;
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "eleven"), "english_execute_loop8");
	GetCons(pos, &check, &pos);
	GetCons(check, &check, &pos);
	test(string_equal_char(check, "billion"), "english_execute_loop9");
	GetCar(pos, &pos);
	test(string_equal_char(pos, "th"), "english_execute_loop10");

	input.root = Nil;
	input.cardinal = 1;
	input.index = 1;
	bignum_value_alloc(local, &pos, signplus_bignum, 4003);
	input.pos = pos;
	english_execute_loop(&input);
	pos = input.root;
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "four"), "english_execute_loop11");
	GetCons(pos, &check, &pos);
	GetCar(check, &check);
	test(string_equal_char(check, "billion"), "english_execute_loop12");
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "three"), "english_execute_loop13");
	GetCons(pos, &check, &pos);
	GetCar(check, &check);
	test(string_equal_char(check, "million"), "english_execute_loop14");

	rollback_local(local, stack);

	RETURN;
}

static int test_english_execute(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	input.local = local;

	input.root = Nil;
	input.cardinal = 1;
	input.index = 0;
	bignum_value_alloc(local, &pos, signplus_bignum, 0);
	input.pos = pos;
	english_execute(&input);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "zero"), "english_execute1");

	input.root = Nil;
	input.cardinal = 0;
	input.index = 0;
	bignum_value_alloc(local, &pos, signplus_bignum, 0);
	input.pos = pos;
	english_execute(&input);
	GetCons(input.root, &check, &pos);
	test(string_equal_char(check, "zeroth"), "english_execute2");

	input.root = Nil;
	input.cardinal = 1;
	input.index = -1;
	bignum_value_alloc(local, &pos, signplus_bignum, 3);
	input.pos = pos;
	english_execute(&input);
	pos = input.root;
	GetCons(pos, &check, &pos);
	test(string_equal_char(check, "three"), "english_execute3");
	test(pos == Nil, "english_execute4");

	rollback_local(local, stack);

	RETURN;
}

static int test_english_output(void)
{
	addr stream, cons;
	LocalRoot local;
	LocalStack stack;
	struct english_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	list_local(local, &cons,
			strvect_char_localr(local, "cc"),
			strvect_char_localr(local, "dd"),
			strvect_char_localr(local, "ee"),
			NULL);
	list_local(local, &cons,
			strvect_char_localr(local, "aa"),
			strvect_char_localr(local, "bb"),
			cons,
			strvect_char_localr(local, "ff"),
			NULL);
	input.root = cons;
	english_output(stream, &input, 0);
	test(equalstream(stream, "aa bb ccddee ff"), "english_output1");
	english_output(stream, &input, 1);
	test(equalstream(stream, "minus aa bb ccddee ff"), "english_output2");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

#ifdef ENGLISH_RADIX_MODE1
#define TEST_MODE1 " " ENGLISH_RADIX_MODE1
#else
#define TEST_MODE1 ""
#endif

#ifdef ENGLISH_RADIX_MODE2
#define TEST_MODE2 " " ENGLISH_RADIX_MODE2
#else
#define TEST_MODE2 ""
#endif
static int test_english_integer(void)
{
	const char *ptr;
	addr stream;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	english_integer(local, stream, fixnum_localr(local, 0), 1);
	test(equalstream(stream, "zero"), "english_integer1");
	english_integer(local, stream, fixnum_localr(local, 0), 0);
	test(equalstream(stream, "zeroth"), "english_integer2");
	english_integer(local, stream, fixnum_localr(local, 4), 1);
	test(equalstream(stream, "four"), "english_integer3");
	english_integer(local, stream, fixnum_localr(local, 5), 0);
	test(equalstream(stream, "fifth"), "english_integer4");
	english_integer(local, stream, fixnum_localr(local, -9), 1);
	test(equalstream(stream, "minus nine"), "english_integer5");
	english_integer(local, stream, fixnum_localr(local, 11), 1);
	test(equalstream(stream, "eleven"), "english_integer6");
	english_integer(local, stream, fixnum_localr(local, 21), 1);
	test(equalstream(stream, "twenty-one"), "english_integer7");
	english_integer(local, stream, fixnum_localr(local, 123), 1);
	ptr = "one hundred" TEST_MODE2 " twenty-three";
	test(equalstream(stream, ptr), "english_integer8");

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_english_unit_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	english_unit_local(local, &pos, fixnuml(0), 1);
	test(GetType(pos) == LISPTYPE_STRING, "english_unit_local1");
	test(GetStatusDynamic(pos), "english_unit_local2");
	test(string_equal_char(pos, "thousand"), "english_unit_local3");

	english_unit_local(local, &pos, fixnuml(0), 0);
	test(GetType(pos) == LISPTYPE_STRING, "english_unit_local4");
	test(GetStatusDynamic(pos), "english_unit_local5");
	test(string_equal_char(pos, "thousandth"), "english_unit_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_english_unit_heap(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	english_unit_heap(local, &pos, fixnuml(0), 1);
	test(GetType(pos) == LISPTYPE_STRING, "english_unit_heap1");
	test(! GetStatusDynamic(pos), "english_unit_heap2");
	test(string_equal_char(pos, "thousand"), "english_unit_heap3");

	english_unit_heap(local, &pos, fixnuml(0), 0);
	test(GetType(pos) == LISPTYPE_STRING, "english_unit_heap4");
	test(! GetStatusDynamic(pos), "english_unit_heap5");
	test(string_equal_char(pos, "thousandth"), "english_unit_heap6");

	english_unit_heap(local, &pos, fixnuml(1), 1);
	test(string_equal_char(pos, "million"), "english_unit_heap7");
	english_unit_heap(local, &pos, fixnuml(12), 1);
	test(string_equal_char(pos, "duodecillion"), "english_unit_heap8");
	english_unit_heap(local, &pos, fixnuml(40), 1);
	test(string_equal_char(pos, "quadragintillion"), "english_unit_heap9");
	english_unit_heap(local, &pos, fixnuml(1000), 1);
	test(string_equal_char(pos, "millinillion"), "english_unit_heap10");
	english_unit_heap(local, &pos, fixnuml(19683), 1);
	test(string_equal_char(pos, "novendecillitresoctogintasescentillion"),
			"english_unit_heap11");

	rollback_local(local, stack);

	RETURN;
}

static int test_english_unit(void)
{
	char str1[32], str2[256];
	int check, result;
	fixnum i;
	addr pos;
	FILE *file;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	file = fopen(FILE_CASE_LARGE, "r");
	if (file == NULL)
		fmte("file error", NULL);
	check = 0;
	for (i = 0; ! feof(file); i++) {
		result = fscanf(file, "%s %s\n", str1, str2);
		if (result != 2) {
			degrade_printf("fscanf error\n");
			break;
		}
		pos = readr(str1);
		push_local(local, &stack);
		english_unit_local(local, &pos, pos, 1);
		check = string_equal_char(pos, str2);
		rollback_local(local, stack);
		if (! check) {
			degrade_printf("english error: %s, %s\n", str1, str2);
			break;
		}
	}
	fclose(file);
	test(check, "english_unit1");

	RETURN;
}


/*
 *  roma
 */
static int test_roma_integer(void)
{
	char str1[32], str2[32];
	int result;
	FILE *file;
	addr stream, pos;
	fixnum i;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 32);

	file = fopen(FILE_CASE_ROMA, "r");
	if (file == NULL)
		fmte("file error", NULL);
	for (i = 1; ! feof(file); i++) {
		result = fscanf(file, "%s %s\n", str1, str2);
		if (result != 2) {
			degrade_printf("fscanf error\n");
			break;
		}
		/* first */
		clear_output_string_stream(stream);
		roma_integer(stream, i, 0);
		string_stream_local(local, stream, &pos);
		if (! string_equal_char(pos, str1)) {
			degrade_printf("roma(1) check error: %s\n", str1);
			return 1;
		}
		/* second */
		clear_output_string_stream(stream);
		roma_integer(stream, i, 1);
		string_stream_local(local, stream, &pos);
		if (! string_equal_char(pos, str2)) {
			degrade_printf("roma(2) check error: %s\n", str2);
			return 1;
		}
	}
	test(1, "roma_integer1");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_radix(void)
{
	/* english */
	TestBreak(test_name_standard_char);
	TestBreak(test_english_string);
	TestBreak(test_english_char);
	TestBreak(test_name_table_1);
	TestBreak(test_name_table_10);
	TestBreak(test_name_table_100);
	TestBreak(test_name_concat);
	TestBreak(test_number_name_front);
	TestBreak(test_number_name_standard);
	TestBreak(test_number_name_1000);
	TestBreak(test_number_name_vowel);
	TestBreak(test_number_name_extend);
	TestBreak(test_number_name_recursive);
	TestBreak(test_number_name_index);
	TestBreak(test_radix_table_20);
	TestBreak(test_radix_table_100);
	TestBreak(test_push_radix);
	TestBreak(test_push_radix_char);
	TestBreak(test_push_radix_list);
	TestBreak(test_push_radix_20);
	TestBreak(test_push_radix_100);
	TestBreak(test_push_radix_hundred);
	TestBreak(test_number_name_cardinal);
	TestBreak(test_english_execute_loop);
	TestBreak(test_english_execute);
	TestBreak(test_english_output);
	TestBreak(test_english_integer);
	TestBreak(test_english_unit_local);
	TestBreak(test_english_unit_heap);
	TestBreak(test_english_unit);
	/* roma */
	TestBreak(test_roma_integer);

	return 0;
}

int test_radix(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_character();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_eval_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_radix();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

