#include "format_parse.c"
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
#include "stream_string.h"
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
	test(chr.type == FormatType_Error, "fmtchar_init2");
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
	struct fmtargs *ptr, *ptr2;

	local = Local_Thread;
	push_local(local, &stack);
	strvect_char_local(local, &pos, "Hello");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);

	input.index = 2;
	ptr = fmtargs_make(&input, &chr);
	test(ptr == chr.root, "fmtargs_make1");
	test(ptr == chr.tail, "fmtargs_make2");
	test(ptr->position == 2, "fmtargs_make3");
	test(chr.size == 1, "fmtargs_make4");

	input.index = 4;
	ptr2 = fmtargs_make(&input, &chr);
	test(ptr == chr.root, "fmtargs_make5");
	test(ptr2 == chr.tail, "fmtargs_make6");
	test(ptr2->position == 4, "fmtargs_make7");
	test(chr.size == 2, "fmtargs_make8");

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
	ptr = chr.root;
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
	ptr = chr.root;
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
	ptr = chr.root;
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
	ptr = chr.root;
	test(ptr->type == fmtargs_nil, "fmtchar_nil1");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_value_parse(void)
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
	v = fmtchar_value_parse(&input, '+', cons);
	test(v == 12345, "fmtchar_value_parse1");

	setchar_bigcons(local, cons, 10, "234");
	v = fmtchar_value_parse(&input, '-', cons);
	test(v == -234, "fmtchar_value_parse2");

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
	ptr = chr.root;
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

static int test_fmtchar_type(void)
{
	struct fmtinput_struct input;
	struct fmtchar chr;

	cleartype(input);
	cleartype(chr);
	chr.character = 'A';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Aesthetic, "fmtchar_type1");

	chr.character = '<';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Justification, "fmtchar_type2");

	chr.character = '>';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Error, "fmtchar_type3");

	chr.character = ')';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Error, "fmtchar_type4");

	chr.character = ']';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Error, "fmtchar_type5");

	chr.character = '}';
	fmtchar_type(&input, &chr);
	test(chr.type == FormatType_Error, "fmtchar_type6");

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
	strvect_char_local(local, &pos, "~/abc/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	chr.intern = NULL;
	fmtchar_parse_function(&input, &chr);
	test(consp(chr.intern), "fmtchar_parse_function1");
	GetCons(chr.intern, &pos, &check);
	test(strvect_equal_char(pos, "COMMON-LISP-USER"), "fmtchar_parse_function2");
	test(strvect_equal_char(check, "ABC"), "fmtchar_parse_function3");

	strvect_char_local(local, &pos, "~/cl-User:Hello/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	chr.intern = NULL;
	fmtchar_parse_function(&input, &chr);
	test(consp(chr.intern), "fmtchar_parse_function4");
	GetCons(chr.intern, &pos, &check);
	test(strvect_equal_char(pos, "CL-USER"), "fmtchar_parse_function5");
	test(strvect_equal_char(check, "HELLO"), "fmtchar_parse_function6");

	strvect_char_local(local, &pos, "~/Hello::Zzz/");
	fmtinput_init(&input, local, pos);
	fmtchar_init(&input, &chr);
	input.index = 2;
	chr.intern = NULL;
	fmtchar_parse_function(&input, &chr);
	test(consp(chr.intern), "fmtchar_parse_function7");
	GetCons(chr.intern, &pos, &check);
	test(strvect_equal_char(pos, "HELLO"), "fmtchar_parse_function8");
	test(strvect_equal_char(check, "ZZZ"), "fmtchar_parse_function9");

	rollback_local(local, stack);

	RETURN;
}

static void testparse(fmtinput input, struct fmtchar *chr, addr queue, const char *str)
{
	addr pos;
	LocalRoot local;

	local = input->local;
	strvect_char_local(local, &pos, str);
	fmtinput_init(input, local, pos);
	fmtchar_init(input, chr);
	input->index = 1;
	fmtchar_parse(input, chr, queue);
}

static struct fmtargs *test_fmtchar_args(struct fmtchar *chr, size_t index)
{
	size_t i;
	struct fmtargs *arg;

	arg = chr->root;
	for (i = 0; i < index; i++)
		arg = arg->next;

	return arg;
}

static int test_fmtchar_parse(void)
{
	addr check, queue;
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtchar chr;
	struct fmtargs *ptr;

	local = Local_Thread;
	input.local = local;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);

	testparse(&input, &chr, queue, "~A");
	test(chr.character == 'A', "fmtchar_parse1");
	test(chr.size == 0, "fmtchar_parse2");

	testparse(&input, &chr, queue, "~:S");
	test(chr.character == 'S', "fmtchar_parse3");
	test(! chr.atsign, "fmtchar_parse4");
	test(chr.colon, "fmtchar_parse4");
	test(chr.size == 0, "fmtchar_parse5");

	testparse(&input, &chr, queue, "~@S");
	test(! chr.colon, "fmtchar_parse6");
	test(chr.atsign, "fmtchar_parse7");
	test(chr.size == 0, "fmtchar_parse8");

	testparse(&input, &chr, queue, "~:@S");
	test(chr.colon, "fmtchar_parse9");
	test(chr.atsign, "fmtchar_parse10");
	test(chr.size == 0, "fmtchar_parse11");

	testparse(&input, &chr, queue, "~@:S");
	test(chr.colon, "fmtchar_parse12");
	test(chr.atsign, "fmtchar_parse13");
	test(chr.size == 0, "fmtchar_parse14");

	testparse(&input, &chr, queue, "~,A");
	test(chr.size == 1, "fmtchar_parse15");
	testparse(&input, &chr, queue, "~,vA");
	test(chr.size == 2, "fmtchar_parse16");
	testparse(&input, &chr, queue, "~v,A");
	test(chr.size == 1, "fmtchar_parse17");
	testparse(&input, &chr, queue, "~v,VA");
	test(chr.size == 2, "fmtchar_parse18");
	testparse(&input, &chr, queue, "~v,V,A");
	test(chr.size == 2, "fmtchar_parse19");
	testparse(&input, &chr, queue, "~v,V,vA");
	test(chr.size == 3, "fmtchar_parse20");
	testparse(&input, &chr, queue, "~,,A");
	test(chr.size == 2, "fmtchar_parse18");
	testparse(&input, &chr, queue, "~v,,A");
	test(chr.size == 2, "fmtchar_parse19");

	testparse(&input, &chr, queue, "~v,V,,#,A");
	test(chr.character == 'A', "fmtchar_parse20");
	test(chr.size == 4, "fmtchar_parse21");
	testparse(&input, &chr, queue, "~v,V,,#A");
	test(chr.size == 4, "fmtchar_parse22");
	test(test_fmtchar_args(&chr, 0)->type == fmtargs_argument, "fmtchar_parse23");
	test(test_fmtchar_args(&chr, 1)->type == fmtargs_argument, "fmtchar_parse24");
	test(test_fmtchar_args(&chr, 2)->type == fmtargs_nil, "fmtchar_parse25");
	test(test_fmtchar_args(&chr, 3)->type == fmtargs_count, "fmtchar_parse26");

	testparse(&input, &chr, queue, "~10A");
	test(chr.character == 'A', "fmtchar_parse_int1");
	test(chr.size == 1, "fmtchar_parse_int2");
	ptr = test_fmtchar_args(&chr, 0);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int3");
	test(ptr->u.value == 10, "fmtchar_parse_int4");

	testparse(&input, &chr, queue, "~-10,+20A");
	test(chr.character == 'A', "fmtchar_parse_int5");
	test(chr.size == 2, "fmtchar_parse_int6");
	ptr = test_fmtchar_args(&chr, 0);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int7");
	test(ptr->u.value == -10, "fmtchar_parse_int8");
	ptr = test_fmtchar_args(&chr, 1);
	test(ptr->type == fmtargs_integer, "fmtchar_parse_int9");
	test(ptr->u.value == 20, "fmtchar_parse_int10");

	testparse(&input, &chr, queue, "~'*A");
	test(chr.character == 'A', "fmtchar_parse_char1");
	test(chr.size == 1, "fmtchar_parse_char2");
	ptr = test_fmtchar_args(&chr, 0);
	test(ptr->type == fmtargs_character, "fmtchar_parse_char3");
	test(ptr->u.character == '*', "fmtchar_parse_char4");

	testparse(&input, &chr, queue, "~10,20,30:@/car/");
	test(chr.character == '/', "fmtchar_parse_function1");
	test(chr.size == 3, "fmtchar_parse_function2");
	test(consp(chr.intern) , "fmtchar_parse_function3");
	GetCar(chr.intern, &check);
	test(strvect_equal_char(check, "COMMON-LISP-USER"), "fmtchar_parse_function4");
	GetCdr(chr.intern, &check);
	test(strvect_equal_char(check, "CAR"), "fmtchar_parse_function5");

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
	struct fmtinput_struct input;
	struct fmtchar *ptr;

	cleartype(input);
	local = Local_Thread;
	input.local = local;
	input.format = T;
	push_local(local, &stack);
	ptr = fmtchar_local(&input);
	test(ptr->character == 0, "fmtchar_local1");
	test(ptr->format == T, "fmtchar_local2");
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

static int test_fmtroot_output(void)
{
	LocalRoot local;
	LocalStack stack;
	struct fmtinput_struct input;
	struct fmtroot root;
	struct fmtchar *comm;
	struct fmtargs *arg;

	local = Local_Thread;
	push_local(local, &stack);
	memset(&input, 0, sizeoft(struct fmtinput_struct));
	memset(&root, 0, sizeoft(struct fmtroot));
	input.local = local;
	input.index = 10;
	root.start = 10;
	fmtroot_output(&input, &root);
	test(root.root == NULL, "fmtroot_output1");

	root.start = 10;
	input.index = 12;
	fmtroot_output(&input, &root);
	comm = root.root;
	test(comm != NULL, "fmtroot_output2");
	test(comm->character == 0, "fmtroot_output3");
	test(comm->size = 2, "fmtroot_output4");

	arg = test_fmtchar_args(comm, 0);
	test(arg->type = fmtargs_index, "fmtroot_output5");
	test(arg->u.index = 2, "fmtroot_output6");

	arg = test_fmtchar_args(comm, 1);
	test(arg->type = fmtargs_index, "fmtroot_output7");
	test(arg->u.index = 10, "fmtroot_output8");

	arg = test_fmtchar_args(comm, 2);
	test(arg->type = fmtargs_index, "fmtroot_output9");
	test(arg->u.index = 12, "fmtroot_output10");

	test(comm->position = 10, "fmtroot_output11");
	test(root.start = 12, "fmtroot_output12");

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
	test(test_fmtchar_args(comm, 0)->u.index == 5, "fmtchar_loop4");
	test(test_fmtchar_args(comm, 1)->u.index == 0, "fmtchar_loop5");
	test(test_fmtchar_args(comm, 2)->u.index == 5, "fmtchar_loop6");
	comm = comm->next;
	test(comm->character == 'A', "fmtchar_loop7");
	test(comm->next == NULL, "fmtchar_loop8");

	strvect_char_local(local, &pos, "~10,2,3,'*,'=Fabc");
	fmtinput_init(&input, local, pos);
	comm = fmtchar_loop(&input);
	test(comm->character == 'F', "fmtchar_loop9");
	comm = comm->next;
	test(comm->character == 0, "fmtchar_loop10");
	test(test_fmtchar_args(comm, 0)->u.index == 3, "fmtchar_loop11");
	test(test_fmtchar_args(comm, 1)->u.index == 14, "fmtchar_loop12");
	test(test_fmtchar_args(comm, 2)->u.index == 17, "fmtchar_loop13");
	test(comm->next == NULL, "fmtchar_loop14");

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
	test(test_fmtchar_args(check, 0)->u.index == 5, "fmtchar_make7");
	test(test_fmtchar_args(check, 1)->u.index == 2, "fmtchar_make8");
	test(test_fmtchar_args(check, 2)->u.index == 7, "fmtchar_make9");
	check = check->next;
	test(check->character == ')', "fmtchar_make10");
	test(check->next == NULL, "fmtchar_make11");
	check = comm->next;
	test(check->character == 0, "fmtchar_make12");
	test(test_fmtchar_args(check, 0)->u.index == 3, "fmtchar_make13");
	test(test_fmtchar_args(check, 1)->u.index == 9, "fmtchar_make14");
	test(test_fmtchar_args(check, 2)->u.index == 12, "fmtchar_make15");

	strvect_char_local(local, &pos, "~[aaa~;bbb~]ccc");
	comm = fmtchar_make(local, pos);
	test(comm->character == '[', "fmtchar_make16");
	check = comm->option;
	test(check->character == 0, "fmtchar_make17");
	test(test_fmtchar_args(check, 0)->u.index == 3, "fmtchar_make18");
	test(test_fmtchar_args(check, 1)->u.index == 2, "fmtchar_make19");
	test(test_fmtchar_args(check, 2)->u.index == 5, "fmtchar_make20");
	check = check->next;
	test(check->character == ';', "fmtchar_make21");
	check = check->next;
	test(check->character == 0, "fmtchar_make22");
	test(test_fmtchar_args(check, 0)->u.index == 3, "fmtchar_make23");
	test(test_fmtchar_args(check, 1)->u.index == 7, "fmtchar_make24");
	test(test_fmtchar_args(check, 2)->u.index == 10, "fmtchar_make25");
	check = check->next;
	test(check->character == ']', "fmtchar_make26");
	test(check->next == NULL, "fmtchar_make27");
	check = comm->next;
	test(check->character == 0, "fmtchar_make28");
	test(test_fmtchar_args(check, 0)->u.index == 3, "fmtchar_make29");
	test(test_fmtchar_args(check, 1)->u.index == 12, "fmtchar_make30");
	test(test_fmtchar_args(check, 2)->u.index == 15, "fmtchar_make31");

	rollback_local(local, stack);

	RETURN;
}


#if 0
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
#endif


/*
 *  main
 */
static int testbreak_format_parse(void)
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
	TestBreak(test_fmtchar_value_parse);
	TestBreak(test_fmtchar_value);
	TestBreak(test_fmtchar_nilcheck);
	TestBreak(test_fmtchar_colon);
	TestBreak(test_fmtchar_atsign);
	TestBreak(test_fmtchar_type);
	TestBreak(test_fmtchar_parse_function);
	TestBreak(test_fmtchar_parse);
	/* fmtchar-group */
	TestBreak(test_fmtchar_local);
	TestBreak(test_fmtroot_push);
	TestBreak(test_fmtroot_output);
	TestBreak(test_fmtchar_loop);
	TestBreak(test_fmtchar_make);
#if 0
	/* format execute */
	TestBreak(test_fmtprint_putc);
	TestBreak(test_fmtprint_pop);
	TestBreak(test_fmtprint_peek);
	TestBreak(test_fmtprint_forward);
	TestBreak(test_fmtprint_rollback);
	TestBreak(test_fmtprint_absolute);
#endif

	return 0;
}

int test_format_parse(void)
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
		result = testbreak_format_parse();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

