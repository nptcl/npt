#include "format.c"
#include "character.h"
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
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"
#include "unicode.h"

/*
 *  fmtinput
 */
static int test_fmtinput_init(void)
{
	addr pos;
	struct fmtinput_struct input;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "HELLO");
	fmtinput_init(&input, local, pos);
	test(input.local != NULL, "fmtinput_init1");
	test(input.index == 0, "fmtinput_init2");
	test(input.size == 5, "fmtinput_init3");
	test(input.format == pos, "fmtinput_init4");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtinput_getc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	unicode u;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	test(fmtinput_getc(&input, &u) == 0, "fmtinput_getc1");
	test(u == 'H', "fmtinput_getc2");
	test(input.index == 1, "fmtinput_getc3");
	test(fmtinput_getc(&input, &u) == 0, "fmtinput_getc4");
	test(u == 'e', "fmtinput_getc5");
	test(input.index == 2, "fmtinput_getc6");
	test(fmtinput_getc(&input, &u) == 0, "fmtinput_getc7");
	test(fmtinput_getc(&input, &u) == 0, "fmtinput_getc8");
	test(fmtinput_getc(&input, &u) == 0, "fmtinput_getc9");
	test(u == 'o', "fmtinput_getc10");
	test(fmtinput_getc(&input, &u), "fmtinput_getc11");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtinput_getcheck(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	unicode u;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtinput_getcheck(&input, &u);
	test(u == 'H', "fmtinput_getcheck1");
	test(input.index == 1, "fmtinput_getcheck2");
	fmtinput_getcheck(&input, &u);
	test(u == 'e', "fmtinput_getcheck3");
	test(input.index == 2, "fmtinput_getcheck4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  fmtchar-parse
 */
static int test_fmtchar_init(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	input.index = 3;

	fmtchar_init(&input, &chr);
	test(chr.size == 0, "fmtchar_init1");
	test(chr.args[0].type == fmtargs_nil, "fmtchar_init2");
	test(chr.position == 3, "fmtchar_init3");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtargs_make(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	input.index = 2;
	ptr = fmtargs_make(&input, &chr);
	test(ptr == &(chr.args[0]), "fmtargs_make1");
	test(ptr == chr.root, "fmtargs_make2");
	test(ptr == chr.tail, "fmtargs_make3");
	test(ptr->position == 2, "fmtargs_make4");
	test(chr.size == 1, "fmtargs_make5");

	input.index = 4;
	ptr = fmtargs_make(&input, &chr);
	test(ptr == &(chr.args[1]), "fmtargs_make6");
	test(chr.root == &(chr.args[0]), "fmtargs_make7");
	test(ptr == chr.tail, "fmtargs_make8");
	test(ptr->position == 4, "fmtargs_make9");
	test(chr.size == 2, "fmtargs_make10");

	while (chr.size < FORMAT_ARGUMENT_SIZE)
		fmtargs_make(&input, &chr);
	ptr = fmtargs_make(&input, &chr);
	test(ptr == chr.tail, "fmtargs_make11");
	test(chr.size == FORMAT_ARGUMENT_SIZE + 1, "fmtargs_make12");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_push(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	fmtchar_push(&input, &chr);
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_argument, "fmtchar_push1");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_sharp(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	fmtchar_sharp(&input, &chr);
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_count, "fmtchar_sharp1");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_character(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	input.index = 4;
	fmtchar_character(&input, &chr);
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_character, "fmtchar_character1");
	test(ptr->u.character == 'o', "fmtchar_character2");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_sign(void)
{
	int sign;

	sign = 0;
	fmtchar_sign(NULL, &sign, '+');
	test(sign == '+', "fmtchar_sign1");

	RETURN;
}

static int test_fmtchar_nil(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	fmtchar_nil(&input, &chr);
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_nil, "fmtchar_nil1");

	rollback_local(local, stack);

	RETURN;
}

static int test_parse_fixnum_value(void)
{
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;
	fixnum v;
	struct fmtinput_struct input;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 10, "12345");
	v = parse_fixnum_value(&input, '+', cons);
	test(v == 12345, "parse_fixnum_value1");

	setchar_bigcons(local, cons, 10, "234");
	v = parse_fixnum_value(&input, '-', cons);
	test(v == -234, "parse_fixnum_value2");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_value(void)
{
	int sign;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 10, "12345");
	sign = '-';
	fmtchar_value(&input, &chr, &sign, cons);
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_integer, "fmtchar_value1");
	test(ptr->u.value == -12345, "fmtchar_value2");
	test(sign == 0, "fmtchar_value3");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_nilcheck(void)
{
	addr cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	test(fmtchar_nilcheck(NULL, 0, cons), "fmtchar_nilcheck1");
	setchar_bigcons(local, cons, 10, "12345");
	test(! fmtchar_nilcheck(NULL, '+', cons), "fmtchar_nilcheck2");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_colon(void)
{
	struct fmtinput_struct input;
	struct fmtchar chr;

	cleartype(input);
	input.index = 3;
	chr.colon_pos = 0;
	chr.colon = 0;
	fmtchar_colon(&input, &chr);
	test(chr.colon, "fmtchar_colon1");
	test(chr.colon_pos == 3, "fmtchar_colon2");

	RETURN;
}

static int test_fmtchar_atsign(void)
{
	struct fmtinput_struct input;
	struct fmtchar chr;

	cleartype(input);
	input.index = 3;
	chr.atsign_pos = 0;
	chr.atsign = 0;
	fmtchar_atsign(&input, &chr);
	test(chr.atsign, "fmtchar_atsign1");
	test(chr.atsign_pos == 3, "fmtchar_atsign2");

	RETURN;
}

static int test_fmtchar_parse_function(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "~/car/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	fmtchar_parse_function(&input, &chr);
	internchar(LISP_COMMON, "CAR", &check);
	test(chr.symbol == check, "fmtchar_parse_function1");

	strvect_char_local(local, &pos, "~/cl-User:Hello/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	fmtchar_parse_function(&input, &chr);
	internchar(LISP_COMMON_USER, "HELLO", &check);
	test(chr.symbol == check, "fmtchar_parse_function2");

	strvect_char_local(local, &pos, "~/cl-user::aaa/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	fmtchar_parse_function(&input, &chr);
	internchar(LISP_COMMON_USER, "AAA", &check);
	test(chr.symbol == check, "fmtchar_parse_function3");

	rollback_local(local, stack);

	RETURN;
}

static void testparse(fmtinput input, struct fmtchar *chr, const char *str)
{
	addr pos;
	LocalRoot local;

	local = input->local;
	strvect_char_local(local, &pos, str);
	fmtinput_init(input, local, pos);
	fmtchar_init(input, chr);
	input->index = 1;
	fmtchar_parse(input, chr);
}

static int test_fmtchar_parse(void)
{
	addr check;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	input.local = local;
	push_local(local, &stack);

	testparse(&input, &chr, "~A");
	test(chr.character == 'A', "fmtchar_parse1");
	test(chr.size == 0, "fmtchar_parse2");

	testparse(&input, &chr, "~:S");
	test(chr.character == 'S', "fmtchar_parse3");
	test(! chr.atsign, "fmtchar_parse4");
	test(chr.colon, "fmtchar_parse4");
	test(chr.size == 0, "fmtchar_parse5");

	testparse(&input, &chr, "~@S");
	test(! chr.colon, "fmtchar_parse6");
	test(chr.atsign, "fmtchar_parse7");
	test(chr.size == 0, "fmtchar_parse8");

	testparse(&input, &chr, "~:@S");
	test(chr.colon, "fmtchar_parse9");
	test(chr.atsign, "fmtchar_parse10");
	test(chr.size == 0, "fmtchar_parse11");

	testparse(&input, &chr, "~@:S");
	test(chr.colon, "fmtchar_parse12");
	test(chr.atsign, "fmtchar_parse13");
	test(chr.size == 0, "fmtchar_parse14");

	testparse(&input, &chr, "~,A");
	test(chr.size == 1, "fmtchar_parse15");
	testparse(&input, &chr, "~,vA");
	test(chr.size == 2, "fmtchar_parse16");
	testparse(&input, &chr, "~v,A");
	test(chr.size == 1, "fmtchar_parse17");
	testparse(&input, &chr, "~v,VA");
	test(chr.size == 2, "fmtchar_parse18");
	testparse(&input, &chr, "~v,V,A");
	test(chr.size == 2, "fmtchar_parse19");
	testparse(&input, &chr, "~v,V,vA");
	test(chr.size == 3, "fmtchar_parse20");
	testparse(&input, &chr, "~,,A");
	test(chr.size == 2, "fmtchar_parse18");
	testparse(&input, &chr, "~v,,A");
	test(chr.size == 2, "fmtchar_parse19");

	testparse(&input, &chr, "~v,V,,#,A");
	test(chr.character == 'A', "fmtchar_parse20");
	test(chr.size == 4, "fmtchar_parse21");
	testparse(&input, &chr, "~v,V,,#A");
	test(chr.size == 4, "fmtchar_parse22");
	test(chr.args[0].type == fmtargs_argument, "fmtchar_parse23");
	test(chr.args[1].type == fmtargs_argument, "fmtchar_parse24");
	test(chr.args[2].type == fmtargs_nil, "fmtchar_parse25");
	test(chr.args[3].type == fmtargs_count, "fmtchar_parse26");

	testparse(&input, &chr, "~10A");
	test(chr.character == 'A', "fmtchar_parse_int1");
	test(chr.size == 1, "fmtchar_parse_int2");
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int3");
	test(ptr->u.value == 10, "fmtchar_parse_int4");

	testparse(&input, &chr, "~-10,+20A");
	test(chr.character == 'A', "fmtchar_parse_int5");
	test(chr.size == 2, "fmtchar_parse_int6");
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int7");
	test(ptr->u.value == -10, "fmtchar_parse_int8");
	ptr = &(chr.args[1]);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int9");
	test(ptr->u.value == 20, "fmtchar_parse_int10");

	testparse(&input, &chr, "~'*A");
	test(chr.character == 'A', "fmtchar_parse_char1");
	test(chr.size == 1, "fmtchar_parse_char2");
	ptr = &(chr.args[0]);
	test(ptr->type == fmtargs_character, "fmtchar_parse_char3");
	test(ptr->u.character == '*', "fmtchar_parse_char4");

	testparse(&input, &chr, "~10,20,30:@/car/");
	test(chr.character == '/', "fmtchar_parse_function1");
	test(chr.size == 3, "fmtchar_parse_function2");
	internchar(LISP_COMMON, "CAR", &check);
	test(chr.symbol == check, "fmtchar_parse_function3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  fmtchar-group
 */
static int test_fmtchar_local(void)
{
	LocalRoot local;
	LocalStack stack;
	struct fmtchar *ptr;

	local = Local_Thread;
	push_local(local, &stack);
	ptr = fmtchar_local(local);
	test(ptr->character == 0, "fmtchar_local1");
	rollback_local(local, stack);

	RETURN;
}

static int test_fmtroot_push(void)
{
	LocalRoot local;
	LocalStack stack;
	struct fmtroot root;
	struct fmtchar chr1, chr2;

	local = Local_Thread;
	push_local(local, &stack);
	memset(&root, 0, sizeoft(struct fmtroot));
	fmtroot_push(local, &root, &chr1);
	test(root.root == &chr1, "fmtroot_push1");
	test(root.tail == &chr1, "fmtroot_push2");

	fmtroot_push(local, &root, &chr2);
	test(root.root == &chr1, "fmtroot_push3");
	test(root.tail == &chr2, "fmtroot_push4");
	test(chr1.next == &chr2, "fmtroot_push5");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtroot_text(void)
{
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtroot root;
	struct fmtchar *comm;

	local = Local_Thread;
	push_local(local, &stack);
	memset(&input, 0, sizeoft(struct fmtinput_struct));
	memset(&root, 0, sizeoft(struct fmtroot));
	input.local = local;
	input.index = 10;
	root.start = 10;
	fmtroot_text(&input, &root);
	test(root.root == NULL, "fmtroot_text1");

	input.index = 12;
	root.start = 10;
	fmtroot_text(&input, &root);
	comm = root.root;
	test(comm != NULL, "fmtroot_text2");
	test(comm->character == 0, "fmtroot_text3");
	test(comm->size = 2, "fmtroot_text4");
	test(comm->args[0].type = fmtargs_index, "fmtroot_text5");
	test(comm->args[0].u.index = 10, "fmtroot_text6");
	test(comm->args[1].type = fmtargs_index, "fmtroot_text7");
	test(comm->args[1].u.index = 12, "fmtroot_text8");
	test(comm->position = 10, "fmtroot_text9");
	test(root.start = 12, "fmtroot_text10");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_loop(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar *comm;

	local = Local_Thread;
	input.local = local;
	push_local(local, &stack);

	strvect_char_local(local, &pos, "~A");
	fmtinput_init(&input, local, pos);
	comm = fmtchar_loop(&input);
	test(comm->character == 'A', "fmtchar_loop1");
	test(comm->next == NULL, "fmtchar_loop2");

	strvect_char_local(local, &pos, "Hello~A");
	fmtinput_init(&input, local, pos);
	comm = fmtchar_loop(&input);
	test(comm->character == 0, "fmtchar_loop3");
	test(comm->args[0].u.index == 0, "fmtchar_loop4");
	test(comm->args[1].u.index == 5, "fmtchar_loop5");
	comm = comm->next;
	test(comm->character == 'A', "fmtchar_loop6");
	test(comm->next == NULL, "fmtchar_loop7");

	strvect_char_local(local, &pos, "~10,2,3,'*,'=Fabc");
	fmtinput_init(&input, local, pos);
	comm = fmtchar_loop(&input);
	test(comm->character == 'F', "fmtchar_loop8");
	comm = comm->next;
	test(comm->character == 0, "fmtchar_loop9");
	test(comm->args[0].u.index == 14, "fmtchar_loop10");
	test(comm->args[1].u.index == 17, "fmtchar_loop11");
	test(comm->next == NULL, "fmtchar_loop12");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_make(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtchar *comm, *check;

	local = Local_Thread;
	push_local(local, &stack);

	strvect_char_local(local, &pos, "~(~)");
	comm = fmtchar_make(local, pos);
	test(comm->character == '(', "fmtchar_make1");
	test(comm->next == NULL, "fmtchar_make2");
	comm = comm->option;
	test(comm->character == ')', "fmtchar_make3");
	test(comm->next == NULL, "fmtchar_make4");

	strvect_char_local(local, &pos, "~(Hello~)aaa");
	comm = fmtchar_make(local, pos);
	test(comm->character == '(', "fmtchar_make5");
	check = comm->option;
	test(check->character == 0, "fmtchar_make6");
	test(check->args[0].u.index == 2, "fmtchar_make7");
	test(check->args[1].u.index == 7, "fmtchar_make8");
	check = check->next;
	test(check->character == ')', "fmtchar_make9");
	test(check->next == NULL, "fmtchar_make10");
	check = comm->next;
	test(check->character == 0, "fmtchar_make11");
	test(check->args[0].u.index == 9, "fmtchar_make12");
	test(check->args[1].u.index == 12, "fmtchar_make13");

	strvect_char_local(local, &pos, "~[aaa~;bbb~]ccc");
	comm = fmtchar_make(local, pos);
	test(comm->character == '[', "fmtchar_make14");
	check = comm->option;
	test(check->character == 0, "fmtchar_make15");
	test(check->args[0].u.index == 2, "fmtchar_make16");
	test(check->args[1].u.index == 5, "fmtchar_make17");
	check = check->next;
	test(check->character == ';', "fmtchar_make18");
	check = check->next;
	test(check->character == 0, "fmtchar_make19");
	test(check->args[0].u.index == 7, "fmtchar_make20");
	test(check->args[1].u.index == 10, "fmtchar_make21");
	check = check->next;
	test(check->character == ']', "fmtchar_make22");
	test(check->next == NULL, "fmtchar_make23");
	check = comm->next;
	test(check->character == 0, "fmtchar_make24");
	test(check->args[0].u.index == 12, "fmtchar_make25");
	test(check->args[1].u.index == 15, "fmtchar_make26");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  format execute
 */
static void fmtprint_print(fmtprint print, const char *str)
{
	while (*str)
		fmtprint_putc(print, *(str++));
}

static int test_fmtprint_putc(void)
{
	addr stream, pos;
	struct fmtprint_struct print;

	open_output_string_stream(&stream, 0);
	print.stream = stream;
	/* upcase */
	print.conversion = fmtcase_upcase;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "aBc");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "ABC"), "fmtprint_putc1");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "-aBc-");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "-ABC-"), "fmtprint_putc2");
	clear_output_string_stream(stream);
	/* downcase */
	print.conversion = fmtcase_downcase;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "AbCdE01234");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "abcde01234"), "fmtprint_putc3");
	clear_output_string_stream(stream);
	/* capitalize */
	print.conversion = fmtcase_capitalize_all;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "abcd efgh-ije-0ab cde0 abcd");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Abcd Efgh-Ije-0ab Cde0 Abcd"), "fmtprint_putc4");
	clear_output_string_stream(stream);
	/* capitalize-first */
	print.conversion = fmtcase_capitalize_first;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "abcd efgh-ije-0ab cde0 abcd");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Abcd efgh-ije-0ab cde0 abcd"), "fmtprint_putc5");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "00abcd efgh");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "00abcd Efgh"), "fmtprint_putc6");
	clear_output_string_stream(stream);

	/* normal */
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	fmtprint_print(&print, "aBcD-eFgH1234");
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "aBcD-eFgH1234"), "fmtprint_putc7");
	clear_output_string_stream(stream);

	close_stream(stream);

	RETURN;
}

static int test_fmtprint_pop(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_pop1");
	test(args.index == 1, "fmtprint_pop2");
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_pop3");
	test(args.index == 2, "fmtprint_pop4");
	fmtprint_pop(&print, NULL, &pos);
	test(RefFixnum(pos) == 30, "fmtprint_pop5");
	test(args.index == 3, "fmtprint_pop6");

	RETURN;
}

static int test_fmtprint_peek(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_peek1");
	test(args.index == 0, "fmtprint_peek2");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_peek3");
	test(args.index == 0, "fmtprint_peek4");

	RETURN;
}

static int test_fmtprint_forward(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_forward(&print, NULL, 2);
	test(args.index == 3, "fmtprint_forward1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 40, "fmtprint_forward2");

	RETURN;
}

static int test_fmtprint_rollback(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_rollback(&print, NULL, 3);
	test(args.index == 1, "fmtprint_rollback1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_rollback2");
	fmtprint_rollback(&print, NULL, 1);
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 10, "fmtprint_rollback3");

	RETURN;
}

static int test_fmtprint_absolute(void)
{
	addr rest, pos;
	struct fmtprint_struct print;
	struct fmtstack args;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	cleartype(print);
	print.rest = &args;
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_pop(&print, NULL, &pos);
	fmtprint_absolute(&print, NULL, 3);
	test(args.index == 3, "fmtprint_absolute1");
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 40, "fmtprint_absolute2");
	fmtprint_absolute(&print, NULL, 1);
	fmtprint_peek(&print, NULL, &pos);
	test(RefFixnum(pos) == 20, "fmtprint_absolute3");

	RETURN;
}

static int test_getint_count(void)
{
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	fixnum value;

	list_heap(&rest,
			fixnum_heapr(10),
			fixnum_heapr(20),
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;
	getint_count(&print, &value);
	test(value == 5, "getint_count1");

	RETURN;
}

static int test_getint_argument(void)
{
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	fixnum value;

	list_heap(&rest,
			fixnum_heapr(10),
			Nil,
			fixnum_heapr(30),
			fixnum_heapr(40),
			fixnum_heapr(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	test(! getint_argument(&print, NULL, &value), "getint_argument1");
	test(value == 10, "getint_argument2");
	test(getint_argument(&print, NULL, &value), "getint_argument3");
	test(! getint_argument(&print, NULL, &value), "getint_argument4");
	test(value == 30, "getint_argument5");

	RETURN;
}

static int test_fmtint_nilp(void)
{
	addr rest, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct fmtchar *comm;
	fixnum value;

	local = Local_Thread;
	push_local(local, &stack);

	list_local(local, &rest, fixnum_heapr(10), Nil, fixnum_heapr(20), NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,20A");
	comm = fmtchar_make(local, pos);
	test(fmtint_nilp(&print, comm, 0, &value), "fmtint_nilp1");

	test(! fmtint_nilp(&print, comm, 1, &value), "fmtint_nilp2");
	test(value == 20, "fmtint_nilp3");

	strvect_char_local(local, &pos, "~#,vA");
	comm = fmtchar_make(local, pos);
	test(! fmtint_nilp(&print, comm, 0, &value), "fmtint_nilp4");
	test(value == 3, "fmtint_nilp5");
	test(! fmtint_nilp(&print, comm, 1, &value), "fmtint_nilp6");
	test(value == 10, "fmtint_nilp7");
	test(fmtint_nilp(&print, comm, 1, &value), "fmtint_nilp8");

	rollback_local(local, stack);

	RETURN;
}

static int test_getchar_argument(void)
{
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	unicode value;

	list_heap(&rest,
			character_heapr('a'),
			Nil,
			character_heapr('b'),
			character_heapr('c'),
			character_heapr('d'),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	test(! getchar_argument(&print, NULL, &value), "getchar_argument1");
	test(value == 'a', "getchar_argument2");
	test(getchar_argument(&print, NULL, &value), "getchar_argument3");
	test(! getchar_argument(&print, NULL, &value), "getchar_argument4");
	test(value == 'b', "getchar_argument5");

	RETURN;
}

static int test_fmtchar_nilp(void)
{
	addr rest, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct fmtchar *comm;
	unicode value;

	local = Local_Thread;
	push_local(local, &stack);

	list_local(local, &rest, character_heapr('a'), Nil, character_heapr('b'), NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,'cA");
	comm = fmtchar_make(local, pos);
	test(fmtchar_nilp(&print, comm, 0, &value), "fmtchar_nilp1");

	test(! fmtchar_nilp(&print, comm, 1, &value), "fmtchar_nilp2");
	test(value == 'c', "fmtchar_nilp3");

	strvect_char_local(local, &pos, "~vA");
	comm = fmtchar_make(local, pos);
	test(! fmtchar_nilp(&print, comm, 0, &value), "fmtchar_nilp4");
	test(value == 'a', "fmtchar_nilp5");
	test(fmtchar_nilp(&print, comm, 0, &value), "fmtchar_nilp6");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtint_default(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct fmtchar *comm;
	fixnum value;

	local = Local_Thread;
	push_local(local, &stack);

	args.root = Nil;
	args.front = Nil;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,20A");
	comm = fmtchar_make(local, pos);
	fmtint_default(&print, comm, 0, &value, 99);
	test(value == 99, "fmtint_default1");
	fmtint_default(&print, comm, 1, &value, 99);
	test(value == 20, "fmtint_default2");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_default(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct fmtchar *comm;
	unicode value;

	local = Local_Thread;
	push_local(local, &stack);

	args.root = Nil;
	args.front = Nil;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,'*A");
	comm = fmtchar_make(local, pos);
	fmtchar_default(&print, comm, 0, &value, '=');
	test(value == '=', "fmtchar_default1");
	fmtchar_default(&print, comm, 1, &value, '=');
	test(value == '*', "fmtchar_default2");

	rollback_local(local, stack);

	RETURN;
}

static int test_format_output(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct fmtchar *comm;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	args.root = Nil;
	args.front = Nil;
	args.index = 0;
	print.rest = &args;
	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	strvect_char_local(local, &pos, "Hello");
	print.format = pos;
	comm = fmtchar_make(local, pos);

	comm->args[0].u.index = 2;
	comm->args[1].u.index = 4;
	format_output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "ll"), "format_output1");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	strvect_char_local(local, &pos, "  Hello");
	print.format = pos;
	comm = fmtchar_make(local, pos);
	format_output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "  Hello"), "format_output2");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	print.delete_space = 1;
	strvect_char_local(local, &pos, "  Hello");
	print.format = pos;
	comm = fmtchar_make(local, pos);
	format_output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "format_output3");
	test(print.delete_space == 0, "format_output4");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	print.delete_space = 1;
	strvect_char_local(local, &pos, "    ");
	print.format = pos;
	comm = fmtchar_make(local, pos);
	format_output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, ""), "format_output5");
	test(print.delete_space == 1, "format_output6");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_write_times(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	write_times(&print, 'c', 10);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "cccccccccc"), "write_times1");
	clear_output_string_stream(stream);
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_write_string(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	strvect_char_local(local, &pos, "Hello");
	write_string(&print, pos);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "write_string1");
	clear_output_string_stream(stream);
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_write_margin(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;

	strvect_char_local(local, &pos, "Hello");
	write_margin(&print, pos, 0, 0,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "write_margin1");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	write_margin(&print, pos, 0, 9,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello****"), "write_margin2");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	write_margin(&print, pos, 1, 9,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "****Hello"), "write_margin3");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	write_margin(&print, pos, 0, 10,4,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello********"), "write_margin4");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	write_margin(&print, pos, 0, 4,10,3,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello***"), "write_margin5");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_ascii_parameter(void)
{
	addr stream, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.ptr = ptr;
	print.local = local;
	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;

	format_ascii_parameter(&print, Nil, 0,1,0,'*', 0,0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "NIL"), "format_ascii_parameter1");
	clear_output_string_stream(stream);

	format_ascii_parameter(&print, Nil, 0,1,0,'*', 1,0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "()"), "format_ascii_parameter2");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "HELLO");
	format_ascii_parameter(&print, pos, 10,1,0,'*', 1,0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "HELLO*****"), "format_ascii_parameter3");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_ascii(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~A");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "10"), "format_ascii1");

	strvect_char_heap(&pos, "~10,3,2,'*:a");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO******"), "format_ascii2");

	strvect_char_heap(&pos, "~10,3,2,'*@A");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "******HELLO"), "format_ascii3");

	strvect_char_heap(&pos, "~:a");
	list_heap(&args, Nil, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "()"), "format_ascii4");

	RETURN;
}

static int test_format_s_express(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~S");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "10"), "format_s_express1");

	strvect_char_heap(&pos, "~10,3,2,'*:s");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "\"HELLO\"***"), "format_s_express2");

	strvect_char_heap(&pos, "~10,3,2,'*@S");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "***\"HELLO\""), "format_s_express3");

	strvect_char_heap(&pos, "~:s");
	list_heap(&args, Nil, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "()"), "format_s_express4");

	RETURN;
}

static int test_format_binary(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~B");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1010"), "format_binary1");

	strvect_char_heap(&pos, "~b");
	list_heap(&args, fixnum_heapr(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_binary2");

	strvect_char_heap(&pos, "~@B");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+1010"), "format_binary3");

	strvect_char_heap(&pos, "~@b");
	list_heap(&args, fixnum_heapr(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_binary4");

	strvect_char_heap(&pos, "~:B");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,010"), "format_binary5");

	strvect_char_heap(&pos, "~,,'_,2:@b");
	list_heap(&args, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+10_10"), "format_binary6");

	strvect_char_heap(&pos, "~B");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_binary7");

	RETURN;
}

static int test_format_octal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~O");
	list_heap(&args, fixnum_heapr(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_octal1");

	strvect_char_heap(&pos, "~o");
	list_heap(&args, fixnum_heapr(-0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123"), "format_octal2");

	strvect_char_heap(&pos, "~@O");
	list_heap(&args, fixnum_heapr(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_octal3");

	strvect_char_heap(&pos, "~@o");
	list_heap(&args, fixnum_heapr(-0765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_octal4");

	strvect_char_heap(&pos, "~:O");
	list_heap(&args, fixnum_heapr(01234567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,234,567"), "format_octal5");

	strvect_char_heap(&pos, "~,,'_,2:@o");
	list_heap(&args, fixnum_heapr(0123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_octal6");

	strvect_char_heap(&pos, "~O");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_octal7");

	RETURN;
}

static int test_format_decimal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~D");
	list_heap(&args, fixnum_heapr(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_decimal1");

	strvect_char_heap(&pos, "~d");
	list_heap(&args, fixnum_heapr(-123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123"), "format_decimal2");

	strvect_char_heap(&pos, "~@D");
	list_heap(&args, fixnum_heapr(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_decimal3");

	strvect_char_heap(&pos, "~@d");
	list_heap(&args, fixnum_heapr(-765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_decimal4");

	strvect_char_heap(&pos, "~:D");
	list_heap(&args, fixnum_heapr(1234567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,234,567"), "format_decimal5");

	strvect_char_heap(&pos, "~,,'_,2:@d");
	list_heap(&args, fixnum_heapr(123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_decimal6");

	strvect_char_heap(&pos, "~D");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_decimal7");

	RETURN;
}

static int test_format_hexadecimal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~X");
	list_heap(&args, fixnum_heapr(0x123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_hexadecimal1");

	strvect_char_heap(&pos, "~x");
	list_heap(&args, fixnum_heapr(-0xFED), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-FED"), "format_hexadecimal2");

	strvect_char_heap(&pos, "~@X");
	list_heap(&args, fixnum_heapr(0xabc), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+ABC"), "format_hexadecimal3");

	strvect_char_heap(&pos, "~@x");
	list_heap(&args, fixnum_heapr(-0x765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_hexadecimal4");

	strvect_char_heap(&pos, "~:X");
	list_heap(&args, fixnum_heapr(0xabcdef0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A,BCD,EF0"), "format_hexadecimal5");

	strvect_char_heap(&pos, "~,,'_,2:@x");
	list_heap(&args, fixnum_heapr(0x123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_hexadecimal6");

	strvect_char_heap(&pos, "~X");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_hexadecimal7");

	RETURN;
}

static int test_format_radix(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~10R");
	list_heap(&args, fixnum_heapr(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_radix1");

	strvect_char_heap(&pos, "~16r");
	list_heap(&args, fixnum_heapr(-0xFED), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-FED"), "format_radix2");

	strvect_char_heap(&pos, "~8@R");
	list_heap(&args, fixnum_heapr(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_radix3");

	strvect_char_heap(&pos, "~2@r");
	list_heap(&args, fixnum_heapr(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_radix4");

	strvect_char_heap(&pos, "~16:R");
	list_heap(&args, fixnum_heapr(0xabcdef0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A,BCD,EF0"), "format_radix5");

	strvect_char_heap(&pos, "~16,,,'_,2:@r");
	list_heap(&args, fixnum_heapr(0x123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_radix6");

	strvect_char_heap(&pos, "~8r");
	list_heap(&args, strvect_char_heapr("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_radix7");

	RETURN;
}

static int test_format_radix_text(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~R");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "four"), "format_radix_text1");

	strvect_char_heap(&pos, "~r");
	list_heap(&args, fixnum_heapr(-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "minus four"), "format_radix_text2");

	strvect_char_heap(&pos, "~:R");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "fourth"), "format_radix_text3");

	strvect_char_heap(&pos, "~:r");
	list_heap(&args, fixnum_heapr(-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "minus fourth"), "format_radix_text4");

	strvect_char_heap(&pos, "~@R");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "IV"), "format_radix_text5");

	strvect_char_heap(&pos, "~:@r");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "IIII"), "format_radix_text6");

	RETURN;
}

static int test_format_plural(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~P");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "s"), "format_plural1");

	strvect_char_heap(&pos, "~p");
	list_heap(&args, fixnum_heapr(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, ""), "format_plural2");

	strvect_char_heap(&pos, "~A~:P");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "4s"), "format_plural3");

	strvect_char_heap(&pos, "~A~:p");
	list_heap(&args, fixnum_heapr(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1"), "format_plural4");

	strvect_char_heap(&pos, "~@P");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "ies"), "format_plural5");

	strvect_char_heap(&pos, "~@p");
	list_heap(&args, fixnum_heapr(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "y"), "format_plural6");

	strvect_char_heap(&pos, "~A~:@P");
	list_heap(&args, fixnum_heapr(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "4ies"), "format_plural7");

	strvect_char_heap(&pos, "~A~:@p");
	list_heap(&args, fixnum_heapr(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1y"), "format_plural8");

	RETURN;
}

static int test_format_character(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~C");
	list_heap(&args, character_heapr('b'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "b"), "format_character1");

	strvect_char_heap(&pos, "~:c");
	list_heap(&args, character_heapr('A'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A"), "format_character2");

	strvect_char_heap(&pos, "~:C");
	list_heap(&args, character_heapr('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "Newline"), "format_character3");

	strvect_char_heap(&pos, "~:@c");
	list_heap(&args, character_heapr('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "Newline"), "format_character4");

	strvect_char_heap(&pos, "~@C");
	list_heap(&args, character_heapr('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\Newline"), "format_character5");

	strvect_char_heap(&pos, "~@c");
	list_heap(&args, character_heapr('z'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\z"), "format_character6");

	strvect_char_heap(&pos, "~@C");
	list_heap(&args, character_heapr('Z'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\Z"), "format_character7");

	RETURN;
}

static void test_ratio_alloc(LocalRoot local,
		addr *ret, int sign, bigtype v1, bigtype v2)
{
	addr numer, denom;
	bignum_value_alloc(local, &numer, signplus_bignum, v1);
	bignum_value_alloc(local, &denom, signplus_bignum, v2);
	make_ratio_alloc_unsafe(local, ret, sign, numer, denom);
}

static int test_format_fixed(void)
{
	addr pos, args, value;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~F");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3"), "format_fixed1");

	strvect_char_heap(&pos, "~f");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3"), "format_fixed2");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, single_float_heapr(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0"), "format_fixed3");

	strvect_char_heap(&pos, "~10F");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "     -12.3"), "format_fixed4");

	strvect_char_heap(&pos, "~,4F");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3000"), "format_fixed5");

	strvect_char_heap(&pos, "~,4F");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3000"), "format_fixed6");

	strvect_char_heap(&pos, "~,,4F");
	list_heap(&args, single_float_heapr(1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12300.0"), "format_fixed7");

	strvect_char_heap(&pos, "~,,-4F");
	list_heap(&args, single_float_heapr(1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.000123"), "format_fixed8");

	strvect_char_heap(&pos, "~3,,,'*F");
	list_heap(&args, single_float_heapr(-123456.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "***"), "format_fixed9");

	strvect_char_heap(&pos, "~10,,,,'=F");
	list_heap(&args, single_float_heapr(-1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "=====-1.23"), "format_fixed10");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, double_float_heapr(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4"), "format_fixed11");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, long_float_heapr(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4"), "format_fixed12");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, fixnum_heapr(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "23.0"), "format_fixed13");

	strvect_char_heap(&pos, "~F");
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-34.0"), "format_fixed14");

	strvect_char_heap(&pos, "~F");
	test_ratio_alloc(NULL, &value, signminus_bignum, 1, 4);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-0.25"), "format_fixed15");

	strvect_char_heap(&pos, "~@F");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.3"), "format_fixed16");

	strvect_char_heap(&pos, "~@F");
	list_heap(&args, fixnum_heapr(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.0"), "format_fixed17");

	RETURN;
}

static int test_format_exponent(void)
{
	addr pos, args, value, symbol;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~E");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D+1"), "format_exponent1");

	strvect_char_heap(&pos, "~e");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23E+1"), "format_exponent2");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, single_float_heapr(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0E+0"), "format_exponent3");

	strvect_char_heap(&pos, "~10e");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "  -1.23E+1"), "format_exponent4");

	strvect_char_heap(&pos, "~,4E");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.2300E+1"), "format_exponent5");

	strvect_char_heap(&pos, "~,4e");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.2300E+1"), "format_exponent6");

	strvect_char_heap(&pos, "~,,4E");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+0001"), "format_exponent7");

	strvect_char_heap(&pos, "~,,2E");
	list_heap(&args, single_float_heapr(-1.23e-3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23E-03"), "format_exponent8");

	strvect_char_heap(&pos, "~,,,4E");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1230.0E-2"), "format_exponent9");

	strvect_char_heap(&pos, "~,,,-2E");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-0.00123E+4"), "format_exponent10");

	strvect_char_heap(&pos, "~3,,,,'*E");
	list_heap(&args, single_float_heapr(-123456.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "***"), "format_exponent11");

	strvect_char_heap(&pos, "~10,,,,,'=E");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "===1.23E+1"), "format_exponent12");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, double_float_heapr(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4D+0"), "format_exponent13");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, long_float_heapr(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4L+0"), "format_exponent14");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, fixnum_heapr(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "2.3E+1"), "format_exponent15");

	strvect_char_heap(&pos, "~E");
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-3.4E+1"), "format_exponent16");

	strvect_char_heap(&pos, "~E");
	test_ratio_alloc(NULL, &value, signminus_bignum, 1, 4);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-2.5E-1"), "format_exponent17");

	/* marker */
	push_close_control(ptr, &value);
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_DOUBLE_FLOAT, &pos);
	pushspecial_control(ptr, symbol, pos);

	strvect_char_heap(&pos, "~E");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23F+1"), "format_exponent18");

	strvect_char_heap(&pos, "~,,,,,,'AE");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A+1"), "format_exponent19");

	strvect_char_heap(&pos, "~,,,,,,'EE");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+1"), "format_exponent20");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+1"), "format_exponent21");

	strvect_char_heap(&pos, "~,,,,,,'AE");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A+1"), "format_exponent22");

	strvect_char_heap(&pos, "~,,,,,,'DE");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D+1"), "format_exponent23");

	free_control(ptr, value);

	strvect_char_heap(&pos, "~@E");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+1.23D+1"), "format_exponent24");

	RETURN;
}

static int test_format_general(void)
{
	addr pos, args, value, symbol;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~G");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3    "), "format_general1");

	strvect_char_heap(&pos, "~g");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3    "), "format_general2");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, single_float_heapr(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0    "), "format_general3");

	strvect_char_heap(&pos, "~10G");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, " -12.3    "), "format_general4");

	strvect_char_heap(&pos, "~,4G");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.30    "), "format_general5");

	strvect_char_heap(&pos, "~,4g");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.30    "), "format_general6");

	strvect_char_heap(&pos, "~,,4G");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3      "), "format_general7");

	strvect_char_heap(&pos, "~,,1G");
	list_heap(&args, single_float_heapr(-1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23   "), "format_general8");

	strvect_char_heap(&pos, "~,,,4G");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3    "), "format_general9");

	strvect_char_heap(&pos, "~,,,-2G");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3    "), "format_general10");

	strvect_char_heap(&pos, "~3,,,,'*G");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "    "), "format_general11");

	strvect_char_heap(&pos, "~5,,,,'*G");
	list_heap(&args, single_float_heapr(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "*    "), "format_general12");

	strvect_char_heap(&pos, "~10,,,,,'=G");
	list_heap(&args, single_float_heapr(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "=-12.3    "), "format_general13");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, double_float_heapr(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4    "), "format_general14");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, long_float_heapr(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4    "), "format_general15");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, fixnum_heapr(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	/* clisp, sbcl, ccl -> "23.    " */
	test(string_equal_char(pos, "23.0    "), "format_general16");

	strvect_char_heap(&pos, "~G");
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-34.0    "), "format_general17");

	strvect_char_heap(&pos, "~G");
	test_ratio_alloc(NULL, &value, signminus_bignum, 4, 2);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-2.0    "), "format_general18");

	/* exponent */
	strvect_char_heap(&pos, "~G");
	list_heap(&args, single_float_heapr(0.1f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.1    "), "format_general19");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, single_float_heapr(0.09f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "9.0E-2"), "format_general20");

	strvect_char_heap(&pos, "~,,2G");
	list_heap(&args, single_float_heapr(0.09f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "9.0E-02"), "format_general21");

	/* marker */
	push_close_control(ptr, &value);
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_DOUBLE_FLOAT, &pos);
	pushspecial_control(ptr, symbol, pos);

	strvect_char_heap(&pos, "~G");
	list_heap(&args, single_float_heapr(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23F-4"), "format_general22");

	strvect_char_heap(&pos, "~,,,,,,'AG");
	list_heap(&args, single_float_heapr(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A-4"), "format_general23");

	strvect_char_heap(&pos, "~,,,,,,'EG");
	list_heap(&args, single_float_heapr(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E-4"), "format_general24");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, double_float_heapr(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E-4"), "format_general25");

	strvect_char_heap(&pos, "~,,,,,,'AG");
	list_heap(&args, double_float_heapr(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A-4"), "format_general26");

	strvect_char_heap(&pos, "~,,,,,,'DG");
	list_heap(&args, double_float_heapr(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D-4"), "format_general27");

	free_control(ptr, value);

	strvect_char_heap(&pos, "~@G");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.3    "), "format_general28");

	RETURN;
}

static int test_format_dollars(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~$");
	list_heap(&args, double_float_heapr(0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.00"), "format_dollars1");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, double_float_heapr(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.00"), "format_dollars2");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, double_float_heapr(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.30"), "format_dollars3");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, double_float_heapr(-12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.35"), "format_dollars4");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, double_float_heapr(-123.456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123.46"), "format_dollars5");

	strvect_char_heap(&pos, "~5$");
	list_heap(&args, double_float_heapr(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.56700"), "format_dollars6");

	strvect_char_heap(&pos, "~0$");
	list_heap(&args, double_float_heapr(-1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1235."), "format_dollars7");

	strvect_char_heap(&pos, "~1$");
	list_heap(&args, double_float_heapr(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.6"), "format_dollars8");

	strvect_char_heap(&pos, "~,6$");
	list_heap(&args, double_float_heapr(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "001234.57"), "format_dollars9");

	strvect_char_heap(&pos, "~,0$");
	list_heap(&args, double_float_heapr(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.57"), "format_dollars10");

	strvect_char_heap(&pos, "~,5$");
	list_heap(&args, double_float_heapr(-12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-00012.35"), "format_dollars11");

	strvect_char_heap(&pos, "~4,6$");
	list_heap(&args, double_float_heapr(12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "000012.3450"), "format_dollars12");

	strvect_char_heap(&pos, "~4,6$");
	list_heap(&args, double_float_heapr(12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "000012.3450"), "format_dollars12");

	strvect_char_heap(&pos, "~,,10$");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "     12.30"), "format_dollars13");

	strvect_char_heap(&pos, "~,,10,'=$");
	list_heap(&args, double_float_heapr(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "====-12.30"), "format_dollars14");

	strvect_char_heap(&pos, "~,,10,'=:$");
	list_heap(&args, double_float_heapr(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-====12.30"), "format_dollars15");

	strvect_char_heap(&pos, "~,,10,'=@$");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "====+12.30"), "format_dollars16");

	strvect_char_heap(&pos, "~,,10,'=:@$");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+====12.30"), "format_dollars17");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0012.300"), "format_dollars18");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(0.12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0000.120"), "format_dollars19");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0012.000"), "format_dollars20");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(1.2), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0001.200"), "format_dollars21");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(0.0000123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0000.000"), "format_dollars22");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, double_float_heapr(12300000), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12300000.000"), "format_dollars23");

	RETURN;
}

static int test_format_terpri(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\n"), "format_terpri1");

	strvect_char_heap(&pos, "~5%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\n\n\n\n\n"), "format_terpri2");

	strvect_char_heap(&pos, "~0%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_terpri3");

	RETURN;
}

static int test_format_fresh_line(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_fresh_line1");

	strvect_char_heap(&pos, "A~&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n"), "format_fresh_line2");

	strvect_char_heap(&pos, "A~5&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n\n\n\n\n"), "format_fresh_line3");

	strvect_char_heap(&pos, "A~%~3&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n\n\n"), "format_fresh_line4");

	strvect_char_heap(&pos, "A~0&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A"), "format_fresh_line5");

	RETURN;
}

static int test_format_pageout(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\f"), "format_pageout1");

	strvect_char_heap(&pos, "~5|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\f\f\f\f\f"), "format_pageout2");

	strvect_char_heap(&pos, "~0|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_pageout3");

	RETURN;
}

static int test_format_tilde(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "~"), "format_tilde1");

	strvect_char_heap(&pos, "~5~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "~~~~~"), "format_tilde2");

	strvect_char_heap(&pos, "~0~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_tilde3");

	RETURN;
}

static int test_format_line_escape(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "A~\nB");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "AB"), "format_line_escape1");

	strvect_char_heap(&pos, "A~\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "AB"), "format_line_escape2");

	strvect_char_heap(&pos, "A~:\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A  B"), "format_line_escape3");

	strvect_char_heap(&pos, "A~@\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\nB"), "format_line_escape4");

	strvect_char_heap(&pos, "A~:@\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n  B"), "format_line_escape5");

	RETURN;
}

static int test_format_tabulate(void)
{
	addr pos, stream;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~T");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, " "), "format_tabulate1");

	strvect_char_heap(&pos, "~10T");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "          "), "format_tabulate2");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~10T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "          "), "format_tabulate3");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~5T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_tabulate4");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_tabulate5");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4,3T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "       "), "format_tabulate6");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,8T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "            "), "format_tabulate7");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4,8T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "            "), "format_tabulate8");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,0T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_tabulate9");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "         "), "format_tabulate10");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,0@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "         "), "format_tabulate11");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,8@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "                "), "format_tabulate12");

	RETURN;
}

static int test_format_ignore(void)
{
	addr pos, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~A~*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1030"), "format_ignore1");

	strvect_char_heap(&pos, "~A~2*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1040"), "format_ignore2");

	strvect_char_heap(&pos, "~A~:*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1010"), "format_ignore3");

	strvect_char_heap(&pos, "~A~A~A~2:*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10203020"), "format_ignore4");

	strvect_char_heap(&pos, "~A~A~0@*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "102010"), "format_ignore5");

	strvect_char_heap(&pos, "~A~A~3@*~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "102040"), "format_ignore6");

	RETURN;
}

static int test_format_indirection(void)
{
	addr pos, pos1, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, strvect_char_heapr("AAA"),
			fixnum_heapr(10), NULL);
	list_heap(&list, pos1, list, fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>20"), "format_indirection1");

	strvect_char_heap(&pos, "~?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, strvect_char_heapr("AAA"),
			fixnum_heapr(10), fixnum_heapr(999), NULL);
	list_heap(&list, pos1, list, fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>20"), "format_indirection2");

	strvect_char_heap(&pos, "~@?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, pos1, strvect_char_heapr("AAA"),
			fixnum_heapr(10), fixnum_heapr(999), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>999"), "format_indirection3");

	RETURN;
}

static int test_format_case(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "aBc012-~(dEF345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-def345-GhI678."), "format_case1");

	strvect_char_heap(&pos, "aBc012-~:@(dEf345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-DEF345-GhI678."), "format_case2");

	strvect_char_heap(&pos, "aBc012-~:(dEf345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-Def345-GhI678."), "format_case3");

	strvect_char_heap(&pos, "zzz~:(abc def 012ghi-jkl~)-mno.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "zzzabc Def 012ghi-Jkl-mno."), "format_case4");

	strvect_char_heap(&pos, "zzz~@(abc def 012ghi-jkl~)-mno.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "zzzabc Def 012ghi-jkl-mno."), "format_case5");

	RETURN;
}

static int test_format_conditional(void)
{
	addr pos, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(0), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno20"), "format_conditional1");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(1), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcghimno20"), "format_conditional2");

	strvect_char_heap(&pos, "~Aabc~2[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(1), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcjklmno1"), "format_conditional3");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(5), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_conditional4");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~:;QQQ~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(5), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcQQQmno20"), "format_conditional5");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~:;~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(5), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_conditional6");

	strvect_char_heap(&pos, "~Aabc~:[def~;ghi~]mno~A");
	list_heap(&list, fixnum_heapr(10), Nil, fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno20"), "format_conditional7");

	strvect_char_heap(&pos, "~Aabc~:[def~;ghi~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(5), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcghimno20"), "format_conditional8");

	strvect_char_heap(&pos, "~Aabc~@[def~]mno~A");
	list_heap(&list, fixnum_heapr(10), Nil, fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_conditional9");

	strvect_char_heap(&pos, "~Aabc~@[def~]mno~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(5), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno5"), "format_conditional10");

	RETURN;
}

static int test_format_iteration(void)
{
	addr pos, list, list1, list2;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~{<~A~A>~}~A");
	list_heap(&list,
			fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_iteration1");

	strvect_char_heap(&pos, "~:{<~A~A>~}~A");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_iteration2");

	strvect_char_heap(&pos, "~@{<~A~A>~}");
	list_heap(&list,
			fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_iteration3");

	strvect_char_heap(&pos, "~:@{<~A~A>~}");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_iteration4");

	strvect_char_heap(&pos, "~1{<~A~A>~}~A");
	list_heap(&list,
			fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>50"), "format_iteration5");

	strvect_char_heap(&pos, "~1:{<~A~A>~}");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>"), "format_iteration6");

	strvect_char_heap(&pos, "~1@{<~A~A>~}~A");
	list_heap(&list,
			fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>30"), "format_iteration7");

	strvect_char_heap(&pos, "~1:@{<~A~A>~}");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>"), "format_iteration8");

	strvect_char_heap(&pos, "~{Hello~}~A");
	list_heap(&list, Nil, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_iteration9");

	strvect_char_heap(&pos, "~{Hello~:}~A");
	list_heap(&list, Nil, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "Hello10"), "format_iteration10");

	strvect_char_heap(&pos, "~{~}~A");
	list_heap(&list,
			fixnum_heapr(10), fixnum_heapr(20),
			fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, strvect_char_heapr("<~A~A>"),
			list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_iteration11");

	RETURN;
}

static int test_format_upandout(void)
{
	addr pos, list, list1, list2;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~A~^Hello");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10Hello"), "format_upandout1");

	strvect_char_heap(&pos, "~A~^Hello");
	list_heap(&list, fixnum_heapr(10), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout2");

	strvect_char_heap(&pos, "~A~0^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout3");

	strvect_char_heap(&pos, "~A~1^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_upandout4");

	strvect_char_heap(&pos, "~A~4,4^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout5");

	strvect_char_heap(&pos, "~A~4,5^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_upandout6");

	strvect_char_heap(&pos, "~A~4,4,4^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout7");

	strvect_char_heap(&pos, "~A~3,4,4^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout8");

	strvect_char_heap(&pos, "~A~4,4,5^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout9");

	strvect_char_heap(&pos, "~A~3,4,5^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_upandout10");

	strvect_char_heap(&pos, "~A~3,2,4^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_upandout11");

	strvect_char_heap(&pos, "~A~2,5,4^~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_upandout12");

	strvect_char_heap(&pos, "~:{<~A~^~A>~}~A");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_upandout13");

	strvect_char_heap(&pos, "~:{<~A~:^~A>~}~A");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnum_heapr(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3050"), "format_upandout14");

	strvect_char_heap(&pos, "~:@{<~A~^~A>~}");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_upandout15");

	strvect_char_heap(&pos, "~:@{<~A~:^~A>~}");
	list_heap(&list1, fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(999), NULL);
	list_heap(&list2, fixnum_heapr(30), fixnum_heapr(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><30"), "format_upandout16");

	RETURN;
}

static void test_format_function_test(Execute ptr, addr rest)
{
	char buffer[100];
	addr stream, arg, colon, atsign;
	size_t size;

	GetCons(rest, &stream, &rest);
	GetCons(rest, &arg, &rest);
	GetCons(rest, &colon, &rest);
	GetCons(rest, &atsign, &rest);

	princ_print(ptr, stream, arg);
	if (colon != Nil)
		write_char_stream(stream, ':');
	if (atsign != Nil)
		write_char_stream(stream, '@');
	size = length_list_unsafe(rest);
	snprintc(buffer, 100, "-%d", (int)size);
	print_ascii_stream(stream, buffer);
}

static int test_format_function(void)
{
	addr pos, list, symbol, call;
	Execute ptr;

	ptr = Execute_Thread;

	internchar(LISP_PACKAGE, "FORMAT-FUNCTION-TEST", &symbol);
	compiled_heap(&call, Nil);
	SetPointer(p_debug1, dynamic, test_format_function_test);
	setcompiled_dynamic(call, p_debug1);
	SetFunctionSymbol(symbol, call);
	strvect_char_heap(&pos, "~/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10-0-20"), "format_function1");

	strvect_char_heap(&pos,
			"~1,2,3:@/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10:@-3-20"), "format_function2");

	strvect_char_heap(&pos,
			"~1,2,'*,#,#,2,3,4,5,6@/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnum_heapr(10), fixnum_heapr(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10@-10-20"), "format_function3");

	RETURN;
}


/*
 *  format
 */
static int test_format_stream_args(void)
{
	addr stream, format, list, tail, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);
	format_stream_args(ptr, stream, format, list, &tail);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_stream_args1");
	test(length_list_unsafe(tail) == 1, "format_stream_args2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_stream_args3");

	clear_output_string_stream(stream);
	format_stream_lisp(ptr, stream, format, list);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_stream_lisp1");
	test(length_list_unsafe(tail) == 1, "format_stream_lisp2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_stream_lisp3");

	rollback_local(local, stack);

	RETURN;
}

static int test_format_string_args(void)
{
	addr format, list, tail, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);
	format_string_args(ptr, format, list, &pos, &tail);
	test(string_equal_char(pos, "Hello1020\n"), "format_string_args1");
	test(length_list_unsafe(tail) == 1, "format_string_args2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_string_args3");

	format_string_lisp(ptr, format, list, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_string_lisp1");
	test(length_list_unsafe(tail) == 1, "format_string_lisp2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_string_lisp3");

	rollback_local(local, stack);

	RETURN;
}

static int test_format_args(void)
{
	addr stream, format, list, tail, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	strvect_char_local(local, &format, "Hello~A~A~%");
	list_local(local, &list,
			fixnum_heapr(10), fixnum_heapr(20), fixnum_heapr(30), NULL);
	format_args(ptr, stream, format, list, &pos, &tail);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_args1");
	test(length_list_unsafe(tail) == 1, "format_args2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_args3");

	format_lisp(ptr, Nil, format, list, &pos);
	test(string_equal_char(pos, "Hello1020\n"), "format_lisp1");
	test(length_list_unsafe(tail) == 1, "format_lisp2");
	GetCar(tail, &pos);
	test(RefFixnum(pos) == 30, "format_lisp3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  main
 */
static int testbreak_format(void)
{
	/* fmtinput */
	TestBreak(test_fmtinput_init);
	TestBreak(test_fmtinput_getc);
	TestBreak(test_fmtinput_getcheck);
	/* fmtchar-parse */
	TestBreak(test_fmtchar_init);
	TestBreak(test_fmtargs_make);
	TestBreak(test_fmtchar_push);
	TestBreak(test_fmtchar_sharp);
	TestBreak(test_fmtchar_character);
	TestBreak(test_fmtchar_sign);
	TestBreak(test_fmtchar_nil);
	TestBreak(test_parse_fixnum_value);
	TestBreak(test_fmtchar_value);
	TestBreak(test_fmtchar_nilcheck);
	TestBreak(test_fmtchar_colon);
	TestBreak(test_fmtchar_atsign);
	TestBreak(test_fmtchar_parse_function);
	TestBreak(test_fmtchar_parse);
	/* fmtchar-group */
	TestBreak(test_fmtchar_local);
	TestBreak(test_fmtroot_push);
	TestBreak(test_fmtroot_text);
	TestBreak(test_fmtchar_loop);
	TestBreak(test_fmtchar_make);
	/* format execute */
	TestBreak(test_fmtprint_putc);
	TestBreak(test_fmtprint_pop);
	TestBreak(test_fmtprint_peek);
	TestBreak(test_fmtprint_forward);
	TestBreak(test_fmtprint_rollback);
	TestBreak(test_fmtprint_absolute);
	TestBreak(test_getint_count);
	TestBreak(test_getint_argument);
	TestBreak(test_fmtint_nilp);
	TestBreak(test_getchar_argument);
	TestBreak(test_fmtchar_nilp);
	TestBreak(test_fmtint_default);
	TestBreak(test_fmtchar_default);
	TestBreak(test_format_output);
	TestBreak(test_write_times);
	TestBreak(test_write_string);
	TestBreak(test_write_margin);
	TestBreak(test_format_ascii_parameter);
	TestBreak(test_format_ascii);
	TestBreak(test_format_s_express);
	TestBreak(test_format_binary);
	TestBreak(test_format_octal);
	TestBreak(test_format_decimal);
	TestBreak(test_format_hexadecimal);
	TestBreak(test_format_radix);
	TestBreak(test_format_radix_text);
	TestBreak(test_format_plural);
	TestBreak(test_format_character);
	TestBreak(test_format_fixed);
	TestBreak(test_format_exponent);
	TestBreak(test_format_general);
	TestBreak(test_format_dollars);
	TestBreak(test_format_terpri);
	TestBreak(test_format_fresh_line);
	TestBreak(test_format_pageout);
	TestBreak(test_format_tilde);
	TestBreak(test_format_line_escape);
	TestBreak(test_format_tabulate);
	TestBreak(test_format_ignore);
	TestBreak(test_format_indirection);
	TestBreak(test_format_case);
	TestBreak(test_format_conditional);
	TestBreak(test_format_iteration);
	TestBreak(test_format_upandout);
	TestBreak(test_format_function);
	/* format */
	TestBreak(test_format_stream_args);
	TestBreak(test_format_string_args);
	TestBreak(test_format_args);

	return 0;
}

int test_format(void)
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
		result = testbreak_format();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

