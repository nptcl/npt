#include "format_function.c"
#include "c99.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "declare.h"
#include "degrade.h"
#include "eval.h"
#include "function.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_fmtint_count(void)
{
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	fixnum value;

	cleartype(print);
	cleartype(args);
	list_heap(&rest,
			fixnumh(10),
			fixnumh(20),
			fixnumh(30),
			fixnumh(40),
			fixnumh(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;
	value = 0;
	fmtint_count(&print, &value);
	test(value == 5, "fmtint_count1");

	RETURN;
}

static int test_fmtint_argument(void)
{
	int check;
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	fixnum value;

	cleartype(print);
	cleartype(args);
	list_heap(&rest,
			fixnumh(10),
			Nil,
			fixnumh(30),
			fixnumh(40),
			fixnumh(50),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	value = 0;
	fmtint_argument(&print, NULL, &value, &check);
	test(! check, "fmtint_argument1");
	test(value == 10, "fmtint_argument2");
	fmtint_argument(&print, NULL, &value, &check);
	test(check, "fmtint_argument3");
	fmtint_argument(&print, NULL, &value, &check);
	test(! check, "fmtint_argument4");
	test(value == 30, "fmtint_argument5");

	RETURN;
}

static struct format_operator *test_fmtoperator(addr format)
{
	byte *ptr;
	struct format_operator *str;

	ptr = format_pointer(format);
	str = (struct format_operator *)ptr;
	Check(str->type != FormatType_Format, "type error");

	return (struct format_operator *)(ptr + str->size);
}

static int test_fmtint_nilp(void)
{
	int check;
	addr rest, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct format_operator *comm;
	fixnum value;

	local = Local_Thread;
	push_local(local, &stack);
	cleartype(print);
	cleartype(args);

	list_local(local, &rest, fixnumh(10), Nil, fixnumh(20), NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,20A");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	value = 0;
	fmtint_nilp(&print, comm, 0, &value, &check);
	test(! check, "fmtint_nilp1");
	test(value == 0, "fmtint_nilp2");
	fmtint_nilp(&print, comm, 1, &value, &check);
	test(! check, "fmtint_nilp3");
	test(value == 20, "fmtint_nilp4");

	strvect_char_local(local, &pos, "~#,vA");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	fmtint_nilp(&print, comm, 0, &value, &check);
	test(! check, "fmtint_nilp5");
	test(value == 3, "fmtint_nilp6");
	fmtint_nilp(&print, comm, 1, &value, &check);
	test(! check, "fmtint_nilp7");
	test(value == 10, "fmtint_nilp8");
	fmtint_nilp(&print, comm, 1, &value, &check);
	test(check, "fmtint_nilp9");

	strvect_char_local(local, &pos, "~,20^");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	fmtint_nilp(&print, comm, 0, &value, &check);
	test(check, "fmtint_nilp10");
	fmtint_nilp(&print, comm, 1, &value, &check);
	test(! check, "fmtint_nilp11");
	test(value == 20, "fmtint_nilp12");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtchar_argument(void)
{
	addr rest;
	struct fmtprint_struct print;
	struct fmtstack args;
	unicode value;

	cleartype(print);
	cleartype(args);
	list_heap(&rest,
			characterh('a'),
			Nil,
			characterh('b'),
			characterh('c'),
			characterh('d'),
			NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	value = 0;
	test(! fmtchar_argument(&print, NULL, &value), "fmtchar_argument1");
	test(value == 'a', "fmtchar_argument2");
	test(fmtchar_argument(&print, NULL, &value), "fmtchar_argument3");
	test(! fmtchar_argument(&print, NULL, &value), "fmtchar_argument4");
	test(value == 'b', "fmtchar_argument5");

	RETURN;
}

static int test_fmtchar_nilp(void)
{
	addr rest, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct format_operator *comm;
	unicode value;

	cleartype(print);
	cleartype(args);
	local = Local_Thread;
	push_local(local, &stack);

	list_local(local, &rest, characterh('a'), Nil, characterh('b'), NULL);
	args.root = rest;
	args.front = rest;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,'c/A/");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	value = 0;
	test(fmtchar_nilp(&print, comm, 0, &value), "fmtchar_nilp1");
	test(! fmtchar_nilp(&print, comm, 1, &value), "fmtchar_nilp2");
	test(value == 'c', "fmtchar_nilp3");

	strvect_char_local(local, &pos, "~v/A/");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
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
	struct format_operator *comm;
	fixnum value;

	cleartype(print);
	cleartype(args);
	local = Local_Thread;
	push_local(local, &stack);

	args.root = Nil;
	args.front = Nil;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,20^");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	value = 0;
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
	struct format_operator *comm;
	unicode value;

	cleartype(print);
	cleartype(args);
	local = Local_Thread;
	push_local(local, &stack);

	args.root = Nil;
	args.front = Nil;
	args.index = 0;
	print.rest = &args;

	strvect_char_local(local, &pos, "~,'*/A/");
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	value = 0;
	fmtchar_default(&print, comm, 0, &value, '=');
	test(value == '=', "fmtchar_default1");
	fmtchar_default(&print, comm, 1, &value, '=');
	test(value == '*', "fmtchar_default2");

	rollback_local(local, stack);

	RETURN;
}

static int test_fmtprint_putc_times(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	cleartype(print);
	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	fmtprint_putc_times(&print, 'c', 10);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "cccccccccc"), "fmtprint_putc_times1");
	clear_output_string_stream(stream);
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_fmtprint_string(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	cleartype(print);
	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	strvect_char_local(local, &pos, "Hello");
	fmtprint_string(&print, pos);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "fmtprint_string1");
	clear_output_string_stream(stream);
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_call_Output(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;
	struct fmtstack args;
	struct format_operator *comm;

	cleartype(print);
	cleartype(args);
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
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	format_call_Output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "format_output1");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	strvect_char_local(local, &pos, "  Hello");
	print.format = pos;
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	format_call_Output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "  Hello"), "format_output2");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	print.delete_space = 1;
	strvect_char_local(local, &pos, "  Hello");
	print.format = pos;
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	format_call_Output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "format_output3");
	test(print.delete_space == 0, "format_output4");
	clear_output_string_stream(stream);

	print.first = 1;
	print.word = 0;
	print.delete_space = 1;
	strvect_char_local(local, &pos, "    ");
	print.format = pos;
	format_parse_local(local, &pos, pos);
	comm = test_fmtoperator(pos);
	format_call_Output(&print, comm);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, ""), "format_output5");
	test(print.delete_space == 1, "format_output6");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_write_margin(void)
{
	addr stream, pos;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	cleartype(print);
	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;

	strvect_char_local(local, &pos, "Hello");
	format_write_margin(&print, pos, 0, 0,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello"), "format_write_margin1");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	format_write_margin(&print, pos, 0, 9,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello****"), "format_write_margin2");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	format_write_margin(&print, pos, 1, 9,1,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "****Hello"), "format_write_margin3");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	format_write_margin(&print, pos, 0, 10,4,0,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello********"), "format_write_margin4");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "Hello");
	format_write_margin(&print, pos, 0, 4,10,3,'*');
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "Hello***"), "format_write_margin5");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_call_print(void)
{
	addr stream, string, pos;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	struct fmtprint_struct print;

	cleartype(print);
	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	open_output_string_stream(&string, 0);

	print.ptr = ptr;
	print.local = local;
	print.stream = stream;
	print.conversion = fmtcase_normal;
	print.first = 1;
	print.word = 0;
	print.string = string;

	format_call_print(&print, Nil,  0,0,  0,1,0,'*',  0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "NIL"), "format_call_print1");
	clear_output_string_stream(stream);

	format_call_print(&print, Nil,  1,0,  0,1,0,'*',  0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "()"), "format_call_print2");
	clear_output_string_stream(stream);

	strvect_char_local(local, &pos, "HELLO");
	format_call_print(&print, pos,  1,0,  10,1,0,'*',  0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "HELLO*****"), "format_call_print3");
	clear_output_string_stream(stream);

	character_local(local, &pos, 'A');
	format_call_print(&print, pos,  1,1,  10,1,0,'*',  0);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "*********A"), "format_call_print4");
	clear_output_string_stream(stream);

	character_local(local, &pos, 'B');
	format_call_print(&print, pos,  1,1,  10,1,0,'*',  1);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "*******#\\B"), "format_call_print5");
	clear_output_string_stream(stream);

	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_format_call_Aesthetic(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~A");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "10"), "format_call_Aesthetic1");

	strvect_char_heap(&pos, "~10,3,2,'*:a");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO*****"), "format_call_Aesthetic2");

	strvect_char_heap(&pos, "~10,3,2,'*@A");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "*****HELLO"), "format_call_Aesthetic3");

	strvect_char_heap(&pos, "~:a");
	list_heap(&args, Nil, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "()"), "format_call_Aesthetic4");

	RETURN;
}

static int test_format_call_Standard(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~S");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "10"), "format_call_Standard1");

	strvect_char_heap(&pos, "~10,3,2,'*:s");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "\"HELLO\"*****"), "format_call_Standard2");

	strvect_char_heap(&pos, "~10,3,2,'*@S");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "*****\"HELLO\""), "format_call_Standard3");

	strvect_char_heap(&pos, "~:s");
	list_heap(&args, Nil, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "()"), "format_call_Standard4");

	RETURN;
}

static int test_format_call_Binary(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~B");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1010"), "format_call_Binary1");

	strvect_char_heap(&pos, "~b");
	list_heap(&args, fixnumh(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_call_Binary2");

	strvect_char_heap(&pos, "~@B");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+1010"), "format_call_Binary3");

	strvect_char_heap(&pos, "~@b");
	list_heap(&args, fixnumh(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_call_Binary4");

	strvect_char_heap(&pos, "~:B");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,010"), "format_call_Binary5");

	strvect_char_heap(&pos, "~,,'_,2:@b");
	list_heap(&args, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+10_10"), "format_call_Binary6");

	strvect_char_heap(&pos, "~B");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_call_Binary7");

	RETURN;
}

static int test_format_call_Octal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~O");
	list_heap(&args, fixnumh(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_call_Octal1");

	strvect_char_heap(&pos, "~o");
	list_heap(&args, fixnumh(-0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123"), "format_call_Octal2");

	strvect_char_heap(&pos, "~@O");
	list_heap(&args, fixnumh(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_call_Octal3");

	strvect_char_heap(&pos, "~@o");
	list_heap(&args, fixnumh(-0765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_call_Octal4");

	strvect_char_heap(&pos, "~:O");
	list_heap(&args, fixnumh(01234567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,234,567"), "format_call_Octal5");

	strvect_char_heap(&pos, "~,,'_,2:@o");
	list_heap(&args, fixnumh(0123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_call_Octal6");

	strvect_char_heap(&pos, "~O");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_call_Octal7");

	RETURN;
}

static int test_format_call_Decimal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~D");
	list_heap(&args, fixnumh(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_call_Decimal1");

	strvect_char_heap(&pos, "~d");
	list_heap(&args, fixnumh(-123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123"), "format_call_Decimal2");

	strvect_char_heap(&pos, "~@D");
	list_heap(&args, fixnumh(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_call_Decimal3");

	strvect_char_heap(&pos, "~@d");
	list_heap(&args, fixnumh(-765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_call_Decimal4");

	strvect_char_heap(&pos, "~:D");
	list_heap(&args, fixnumh(1234567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1,234,567"), "format_call_Decimal5");

	strvect_char_heap(&pos, "~,,'_,2:@d");
	list_heap(&args, fixnumh(123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_call_Decimal6");

	strvect_char_heap(&pos, "~D");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_call_Decimal7");

	RETURN;
}

static int test_format_call_Hexadecimal(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~X");
	list_heap(&args, fixnumh(0x123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_call_Hexadecimal1");

	strvect_char_heap(&pos, "~x");
	list_heap(&args, fixnumh(-0xFED), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-FED"), "format_call_Hexadecimal2");

	strvect_char_heap(&pos, "~@X");
	list_heap(&args, fixnumh(0xabc), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+ABC"), "format_call_Hexadecimal3");

	strvect_char_heap(&pos, "~@x");
	list_heap(&args, fixnumh(-0x765), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-765"), "format_call_Hexadecimal4");

	strvect_char_heap(&pos, "~:X");
	list_heap(&args, fixnumh(0xabcdef0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A,BCD,EF0"), "format_call_Hexadecimal5");

	strvect_char_heap(&pos, "~,,'_,2:@x");
	list_heap(&args, fixnumh(0x123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_call_Hexadecimal6");

	strvect_char_heap(&pos, "~X");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_call_Hexadecimal7");

	RETURN;
}

static int test_format_call_Radix(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~10R");
	list_heap(&args, fixnumh(123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "123"), "format_call_Radix1");

	strvect_char_heap(&pos, "~16r");
	list_heap(&args, fixnumh(-0xFED), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-FED"), "format_call_Radix2");

	strvect_char_heap(&pos, "~8@R");
	list_heap(&args, fixnumh(0123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+123"), "format_call_Radix3");

	strvect_char_heap(&pos, "~2@r");
	list_heap(&args, fixnumh(-10), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1010"), "format_call_Radix4");

	strvect_char_heap(&pos, "~16:R");
	list_heap(&args, fixnumh(0xabcdef0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A,BCD,EF0"), "format_call_Radix5");

	strvect_char_heap(&pos, "~16,,,'_,2:@r");
	list_heap(&args, fixnumh(0x123456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12_34_56"), "format_call_Radix6");

	strvect_char_heap(&pos, "~8r");
	list_heap(&args, stringh("HELLO"), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "HELLO"), "format_call_Radix7");

	RETURN;
}

static int test_format_call_RadixText(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&pos, "~R");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "four"), "format_call_RadixText1");

	strvect_char_heap(&pos, "~r");
	list_heap(&args, fixnumh(-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "minus four"), "format_call_RadixText2");

	strvect_char_heap(&pos, "~:R");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "fourth"), "format_call_RadixText3");

	strvect_char_heap(&pos, "~:r");
	list_heap(&args, fixnumh(-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "minus fourth"), "format_call_RadixText4");

	strvect_char_heap(&pos, "~@R");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "IV"), "format_call_RadixText5");

	strvect_char_heap(&pos, "~:@r");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "IIII"), "format_call_RadixText6");

	RETURN;
}

static int test_format_call_Plural(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~P");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "s"), "format_call_Plural1");

	strvect_char_heap(&pos, "~p");
	list_heap(&args, fixnumh(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, ""), "format_call_Plural2");

	strvect_char_heap(&pos, "~A~:P");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "4s"), "format_call_Plural3");

	strvect_char_heap(&pos, "~A~:p");
	list_heap(&args, fixnumh(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1"), "format_call_Plural4");

	strvect_char_heap(&pos, "~@P");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "ies"), "format_call_Plural5");

	strvect_char_heap(&pos, "~@p");
	list_heap(&args, fixnumh(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "y"), "format_call_Plural6");

	strvect_char_heap(&pos, "~A~:@P");
	list_heap(&args, fixnumh(4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "4ies"), "format_call_Plural7");

	strvect_char_heap(&pos, "~A~:@p");
	list_heap(&args, fixnumh(1), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1y"), "format_call_Plural8");

	RETURN;
}

static int test_format_call_Character(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~C");
	list_heap(&args, characterh('b'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "b"), "format_call_Character1");

	strvect_char_heap(&pos, "~:c");
	list_heap(&args, characterh('A'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "A"), "format_call_Character2");

	strvect_char_heap(&pos, "~:C");
	list_heap(&args, characterh('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "Newline"), "format_call_Character3");

	strvect_char_heap(&pos, "~:@c");
	list_heap(&args, characterh('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "Newline"), "format_call_Character4");

	strvect_char_heap(&pos, "~@C");
	list_heap(&args, characterh('\n'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\Newline"), "format_call_Character5");

	strvect_char_heap(&pos, "~@c");
	list_heap(&args, characterh('z'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\z"), "format_call_Character6");

	strvect_char_heap(&pos, "~@C");
	list_heap(&args, characterh('Z'), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "#\\Z"), "format_call_Character7");

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

static int test_format_call_Fixed(void)
{
	addr pos, args, value;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~F");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3"), "format_call_Fixed1");

	strvect_char_heap(&pos, "~f");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3"), "format_call_Fixed2");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, singleh(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0"), "format_call_Fixed3");

	strvect_char_heap(&pos, "~10F");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "     -12.3"), "format_call_Fixed4");

	strvect_char_heap(&pos, "~,4F");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3000"), "format_call_Fixed5");

	strvect_char_heap(&pos, "~,4F");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3000"), "format_call_Fixed6");

	strvect_char_heap(&pos, "~,,4F");
	list_heap(&args, singleh(1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12300.0"), "format_call_Fixed7");

	strvect_char_heap(&pos, "~,,-4F");
	list_heap(&args, singleh(1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.000123"), "format_call_Fixed8");

	strvect_char_heap(&pos, "~3,,,'*F");
	list_heap(&args, singleh(-123456.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "***"), "format_call_Fixed9");

	strvect_char_heap(&pos, "~10,,,,'=F");
	list_heap(&args, singleh(-1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "=====-1.23"), "format_call_Fixed10");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, doubleh(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4"), "format_call_Fixed11");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, longh(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4"), "format_call_Fixed12");

	strvect_char_heap(&pos, "~F");
	list_heap(&args, fixnumh(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "23.0"), "format_call_Fixed13");

	strvect_char_heap(&pos, "~F");
	value = 0;
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-34.0"), "format_call_Fixed14");

	strvect_char_heap(&pos, "~F");
	test_ratio_alloc(NULL, &value, signminus_bignum, 1, 4);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-0.25"), "format_call_Fixed15");

	strvect_char_heap(&pos, "~@F");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.3"), "format_call_Fixed16");

	strvect_char_heap(&pos, "~@F");
	list_heap(&args, fixnumh(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.0"), "format_call_Fixed17");

	RETURN;
}

static int test_format_call_Exponent(void)
{
	addr pos, args, value, symbol;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~E");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D+1"), "format_call_Exponent1");

	strvect_char_heap(&pos, "~e");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23E+1"), "format_call_Exponent2");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, singleh(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0E+0"), "format_call_Exponent3");

	strvect_char_heap(&pos, "~10e");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "  -1.23E+1"), "format_call_Exponent4");

	strvect_char_heap(&pos, "~,4E");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.2300E+1"), "format_call_Exponent5");

	strvect_char_heap(&pos, "~,4e");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.2300E+1"), "format_call_Exponent6");

	strvect_char_heap(&pos, "~,,4E");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+0001"), "format_call_Exponent7");

	strvect_char_heap(&pos, "~,,2E");
	list_heap(&args, singleh(-1.23e-3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23E-03"), "format_call_Exponent8");

	strvect_char_heap(&pos, "~,,,4E");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1230.0E-2"), "format_call_Exponent9");

	strvect_char_heap(&pos, "~,,,-2E");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-0.00123E+4"), "format_call_Exponent10");

	strvect_char_heap(&pos, "~3,,,,'*E");
	list_heap(&args, singleh(-123456.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "***"), "format_call_Exponent11");

	strvect_char_heap(&pos, "~10,,,,,'=E");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "===1.23E+1"), "format_call_Exponent12");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, doubleh(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4D+0"), "format_call_Exponent13");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, longh(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4L+0"), "format_call_Exponent14");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, fixnumh(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "2.3E+1"), "format_call_Exponent15");

	strvect_char_heap(&pos, "~E");
	value = 0;
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-3.4E+1"), "format_call_Exponent16");

	strvect_char_heap(&pos, "~E");
	test_ratio_alloc(NULL, &value, signminus_bignum, 1, 4);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-2.5E-1"), "format_call_Exponent17");

	/* marker */
	push_new_control(ptr, &value);
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_DOUBLE_FLOAT, &pos);
	pushspecial_control(ptr, symbol, pos);

	strvect_char_heap(&pos, "~E");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23F+1"), "format_call_Exponent18");

	strvect_char_heap(&pos, "~,,,,,,'AE");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A+1"), "format_call_Exponent19");

	strvect_char_heap(&pos, "~,,,,,,'EE");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+1"), "format_call_Exponent20");

	strvect_char_heap(&pos, "~E");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E+1"), "format_call_Exponent21");

	strvect_char_heap(&pos, "~,,,,,,'AE");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A+1"), "format_call_Exponent22");

	strvect_char_heap(&pos, "~,,,,,,'DE");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D+1"), "format_call_Exponent23");

	free_control_(ptr, value);

	strvect_char_heap(&pos, "~@E");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+1.23D+1"), "format_call_Exponent24");

	RETURN;
}

static int test_format_call_General(void)
{
	addr pos, args, value, symbol;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~G");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3    "), "format_call_General1");

	strvect_char_heap(&pos, "~g");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3    "), "format_call_General2");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, singleh(0.0f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.0    "), "format_call_General3");

	strvect_char_heap(&pos, "~10G");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, " -12.3    "), "format_call_General4");

	strvect_char_heap(&pos, "~,4G");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.30    "), "format_call_General5");

	strvect_char_heap(&pos, "~,4g");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.30    "), "format_call_General6");

	strvect_char_heap(&pos, "~,,4G");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3      "), "format_call_General7");

	strvect_char_heap(&pos, "~,,1G");
	list_heap(&args, singleh(-1.23f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1.23   "), "format_call_General8");

	strvect_char_heap(&pos, "~,,,4G");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.3    "), "format_call_General9");

	strvect_char_heap(&pos, "~,,,-2G");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.3    "), "format_call_General10");

	strvect_char_heap(&pos, "~3,,,,'*G");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "    "), "format_call_General11");

	strvect_char_heap(&pos, "~5,,,,'*G");
	list_heap(&args, singleh(12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "*    "), "format_call_General12");

	strvect_char_heap(&pos, "~10,,,,,'=G");
	list_heap(&args, singleh(-12.3f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "=-12.3    "), "format_call_General13");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, doubleh(3.4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4    "), "format_call_General14");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, longh(3.4L), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "3.4    "), "format_call_General15");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, fixnumh(23), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	/* clisp, sbcl, ccl -> "23.    " */
	test(string_equal_char(pos, "23.0    "), "format_call_General16");

	strvect_char_heap(&pos, "~G");
	value = 0;
	bignum_value_alloc(NULL, &value, signminus_bignum, 34);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-34.0    "), "format_call_General17");

	strvect_char_heap(&pos, "~G");
	test_ratio_alloc(NULL, &value, signminus_bignum, 4, 2);
	list_heap(&args, value, NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-2.0    "), "format_call_General18");

	/* exponent */
	strvect_char_heap(&pos, "~G");
	list_heap(&args, singleh(0.1f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.1    "), "format_call_General19");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, singleh(0.09f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "9.0E-2"), "format_call_General20");

	strvect_char_heap(&pos, "~,,2G");
	list_heap(&args, singleh(0.09f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "9.0E-02"), "format_call_General21");

	/* marker */
	push_new_control(ptr, &value);
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	GetConst(COMMON_DOUBLE_FLOAT, &pos);
	pushspecial_control(ptr, symbol, pos);

	strvect_char_heap(&pos, "~G");
	list_heap(&args, singleh(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23F-4"), "format_call_General22");

	strvect_char_heap(&pos, "~,,,,,,'AG");
	list_heap(&args, singleh(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A-4"), "format_call_General23");

	strvect_char_heap(&pos, "~,,,,,,'EG");
	list_heap(&args, singleh(1.23e-4f), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E-4"), "format_call_General24");

	strvect_char_heap(&pos, "~G");
	list_heap(&args, doubleh(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23E-4"), "format_call_General25");

	strvect_char_heap(&pos, "~,,,,,,'AG");
	list_heap(&args, doubleh(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23A-4"), "format_call_General26");

	strvect_char_heap(&pos, "~,,,,,,'DG");
	list_heap(&args, doubleh(1.23e-4), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1.23D-4"), "format_call_General27");

	free_control_(ptr, value);

	strvect_char_heap(&pos, "~@G");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+12.3    "), "format_call_General28");

	RETURN;
}

static int test_format_call_Monetary(void)
{
	addr pos, args;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~$");
	list_heap(&args, doubleh(0), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "0.00"), "format_call_Monetary1");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, doubleh(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12.00"), "format_call_Monetary2");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, doubleh(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.30"), "format_call_Monetary3");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, doubleh(-12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-12.35"), "format_call_Monetary4");

	strvect_char_heap(&pos, "~$");
	list_heap(&args, doubleh(-123.456), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-123.46"), "format_call_Monetary5");

	strvect_char_heap(&pos, "~5$");
	list_heap(&args, doubleh(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.56700"), "format_call_Monetary6");

	strvect_char_heap(&pos, "~0$");
	list_heap(&args, doubleh(-1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-1235."), "format_call_Monetary7");

	strvect_char_heap(&pos, "~1$");
	list_heap(&args, doubleh(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.6"), "format_call_Monetary8");

	strvect_char_heap(&pos, "~,6$");
	list_heap(&args, doubleh(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "001234.57"), "format_call_Monetary9");

	strvect_char_heap(&pos, "~,0$");
	list_heap(&args, doubleh(1234.567), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "1234.57"), "format_call_Monetary10");

	strvect_char_heap(&pos, "~,5$");
	list_heap(&args, doubleh(-12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-00012.35"), "format_call_Monetary11");

	strvect_char_heap(&pos, "~4,6$");
	list_heap(&args, doubleh(12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "000012.3450"), "format_call_Monetary12");

	strvect_char_heap(&pos, "~4,6$");
	list_heap(&args, doubleh(12.345), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "000012.3450"), "format_call_Monetary12");

	strvect_char_heap(&pos, "~,,10$");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "     12.30"), "format_call_Monetary13");

	strvect_char_heap(&pos, "~,,10,'=$");
	list_heap(&args, doubleh(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "====-12.30"), "format_call_Monetary14");

	strvect_char_heap(&pos, "~,,10,'=:$");
	list_heap(&args, doubleh(-12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "-====12.30"), "format_call_Monetary15");

	strvect_char_heap(&pos, "~,,10,'=@$");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "====+12.30"), "format_call_Monetary16");

	strvect_char_heap(&pos, "~,,10,'=:@$");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "+====12.30"), "format_call_Monetary17");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(12.3), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0012.300"), "format_call_Monetary18");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(0.12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0000.120"), "format_call_Monetary19");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(12), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0012.000"), "format_call_Monetary20");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(1.2), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0001.200"), "format_call_Monetary21");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(0.0000123), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "==0000.000"), "format_call_Monetary22");

	strvect_char_heap(&pos, "~3,4,10,'=$");
	list_heap(&args, doubleh(12300000), NULL);
	format_string_lisp(ptr, pos, args, &pos);
	test(string_equal_char(pos, "12300000.000"), "format_call_Monetary23");

	RETURN;
}

static int test_format_call_Newline(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\n"), "format_call_Newline1");

	strvect_char_heap(&pos, "~5%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\n\n\n\n\n"), "format_call_Newline2");

	strvect_char_heap(&pos, "~0%");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_call_Newline3");

	RETURN;
}

static int test_format_call_FreshLine(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_call_FreshLine1");

	strvect_char_heap(&pos, "A~&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n"), "format_call_FreshLine2");

	strvect_char_heap(&pos, "A~5&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n\n\n\n\n"), "format_call_FreshLine3");

	strvect_char_heap(&pos, "A~%~3&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n\n\n"), "format_call_FreshLine4");

	strvect_char_heap(&pos, "A~0&");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A"), "format_call_FreshLine5");

	RETURN;
}

static int test_format_call_Page(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\f"), "format_call_Page1");

	strvect_char_heap(&pos, "~5|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "\f\f\f\f\f"), "format_call_Page2");

	strvect_char_heap(&pos, "~0|");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_call_Page3");

	RETURN;
}

static int test_format_call_Tilde(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "~"), "format_call_Tilde1");

	strvect_char_heap(&pos, "~5~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "~~~~~"), "format_call_Tilde2");

	strvect_char_heap(&pos, "~0~");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, ""), "format_call_Tilde3");

	RETURN;
}

static int test_format_call_IgnoredNewline(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "A~\nB");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "AB"), "format_call_IgnoredNewline1");

	strvect_char_heap(&pos, "A~\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "AB"), "format_call_IgnoredNewline2");

	strvect_char_heap(&pos, "A~:\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A  B"), "format_call_IgnoredNewline3");

	strvect_char_heap(&pos, "A~@\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\nB"), "format_call_IgnoredNewline4");

	strvect_char_heap(&pos, "A~:@\n  B");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "A\n  B"), "format_call_IgnoredNewline5");

	RETURN;
}

static int test_format_call_Tabulate(void)
{
	addr pos, stream;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~T");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, " "), "format_call_Tabulate1");

	strvect_char_heap(&pos, "~10T");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "          "), "format_call_Tabulate2");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~10T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "          "), "format_call_Tabulate3");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~5T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_call_Tabulate4");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_call_Tabulate5");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4,3T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "       "), "format_call_Tabulate6");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,8T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "            "), "format_call_Tabulate7");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "    ");
	strvect_char_heap(&pos, "~4,8T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "            "), "format_call_Tabulate8");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,0T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "     "), "format_call_Tabulate9");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "         "), "format_call_Tabulate10");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,0@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "         "), "format_call_Tabulate11");

	open_output_string_stream(&stream, 0);
	print_ascii_stream(stream, "     ");
	strvect_char_heap(&pos, "~4,8@T");
	format_stream_lisp(ptr, stream, pos, Nil);
	string_stream_heap(stream, &pos);
	test(string_equal_char(pos, "                "), "format_call_Tabulate12");

	RETURN;
}

static int test_format_call_GoTo(void)
{
	addr pos, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~A~*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1030"), "format_call_GoTo1");

	strvect_char_heap(&pos, "~A~2*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1040"), "format_call_GoTo2");

	strvect_char_heap(&pos, "~A~:*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1010"), "format_call_GoTo3");

	strvect_char_heap(&pos, "~A~A~A~2:*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10203020"), "format_call_GoTo4");

	strvect_char_heap(&pos, "~A~A~0@*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "102010"), "format_call_GoTo5");

	strvect_char_heap(&pos, "~A~A~3@*~A");
	list_heap(&list, fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "102040"), "format_call_GoTo6");

	RETURN;
}

static int test_format_call_Recursive(void)
{
	addr pos, pos1, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, stringh("AAA"),
			fixnumh(10), NULL);
	list_heap(&list, pos1, list, fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>20"), "format_call_Recursive1");

	strvect_char_heap(&pos, "~?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, stringh("AAA"),
			fixnumh(10), fixnumh(999), NULL);
	list_heap(&list, pos1, list, fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>20"), "format_call_Recursive2");

	strvect_char_heap(&pos, "~@?~D");
	strvect_char_heap(&pos1, "<~A~D>");
	list_heap(&list, pos1, stringh("AAA"),
			fixnumh(10), fixnumh(999), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<AAA10>999"), "format_call_Recursive3");

	RETURN;
}

static int test_format_call_Case(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "aBc012-~(dEF345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-def345-GhI678."), "format_call_Case1");

	strvect_char_heap(&pos, "aBc012-~:@(dEf345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-DEF345-GhI678."), "format_call_Case2");

	strvect_char_heap(&pos, "aBc012-~:(dEf345-~)GhI678.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "aBc012-Def345-GhI678."), "format_call_Case3");

	strvect_char_heap(&pos, "zzz~:(abc def 012ghi-jkl~)-mno.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "zzzAbc Def 012ghi-Jkl-mno."), "format_call_Case4");

	strvect_char_heap(&pos, "zzz~@(abc def 012ghi-jkl~)-mno.");
	format_string_lisp(ptr, pos, Nil, &pos);
	test(string_equal_char(pos, "zzzAbc def 012ghi-jkl-mno."), "format_call_Case5");

	RETURN;
}

static int test_format_call_Condition(void)
{
	addr pos, list;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(0), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno20"), "format_call_Condition1");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(1), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcghimno20"), "format_call_Condition2");

	strvect_char_heap(&pos, "~Aabc~2[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(1), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcjklmno1"), "format_call_Condition3");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(5), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_call_Condition4");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~:;QQQ~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(5), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcQQQmno20"), "format_call_Condition5");

	strvect_char_heap(&pos, "~Aabc~[def~;ghi~;jkl~:;~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(5), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_call_Condition6");

	strvect_char_heap(&pos, "~Aabc~:[def~;ghi~]mno~A");
	list_heap(&list, fixnumh(10), Nil, fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno20"), "format_call_Condition7");

	strvect_char_heap(&pos, "~Aabc~:[def~;ghi~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(5), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcghimno20"), "format_call_Condition8");

	strvect_char_heap(&pos, "~Aabc~@[def~]mno~A");
	list_heap(&list, fixnumh(10), Nil, fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcmno20"), "format_call_Condition9");

	strvect_char_heap(&pos, "~Aabc~@[def~]mno~A");
	list_heap(&list, fixnumh(10), fixnumh(5), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10abcdefmno5"), "format_call_Condition10");

	RETURN;
}

static int test_format_call_Iteration(void)
{
	addr pos, list, list1, list2;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~{<~A~A>~}~A");
	list_heap(&list,
			fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_call_Iteration1");

	strvect_char_heap(&pos, "~:{<~A~A>~}~A");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_call_Iteration2");

	strvect_char_heap(&pos, "~@{<~A~A>~}");
	list_heap(&list,
			fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_call_Iteration3");

	strvect_char_heap(&pos, "~:@{<~A~A>~}");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_call_Iteration4");

	strvect_char_heap(&pos, "~1{<~A~A>~}~A");
	list_heap(&list,
			fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>50"), "format_call_Iteration5");

	strvect_char_heap(&pos, "~1:{<~A~A>~}");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>"), "format_call_Iteration6");

	strvect_char_heap(&pos, "~1@{<~A~A>~}~A");
	list_heap(&list,
			fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>30"), "format_call_Iteration7");

	strvect_char_heap(&pos, "~1:@{<~A~A>~}");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020>"), "format_call_Iteration8");

	strvect_char_heap(&pos, "~{Hello~}~A");
	list_heap(&list, Nil, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_Iteration9");

	strvect_char_heap(&pos, "~{Hello~A~:}~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	list_heap(&list, list, fixnumh(30), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "Hello10Hello2030"), "format_call_Iteration10");

	strvect_char_heap(&pos, "~{~}~A");
	list_heap(&list,
			fixnumh(10), fixnumh(20),
			fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, stringh("<~A~A>"),
			list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_call_Iteration11");

	RETURN;
}

static int test_format_call_EscapeUpward(void)
{
	addr pos, list, list1, list2;
	Execute ptr;

	ptr = Execute_Thread;

	strvect_char_heap(&pos, "~A~^Hello");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10Hello"), "format_call_EscapeUpward1");

	strvect_char_heap(&pos, "~A~^Hello");
	list_heap(&list, fixnumh(10), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward2");

	strvect_char_heap(&pos, "~A~0^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward3");

	strvect_char_heap(&pos, "~A~1^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_call_EscapeUpward4");

	strvect_char_heap(&pos, "~A~4,4^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward5");

	strvect_char_heap(&pos, "~A~4,5^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_call_EscapeUpward6");

	strvect_char_heap(&pos, "~A~4,4,4^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward7");

	strvect_char_heap(&pos, "~A~3,4,4^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward8");

	strvect_char_heap(&pos, "~A~4,4,5^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward9");

	strvect_char_heap(&pos, "~A~3,4,5^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10"), "format_call_EscapeUpward10");

	strvect_char_heap(&pos, "~A~3,2,4^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_call_EscapeUpward11");

	strvect_char_heap(&pos, "~A~2,5,4^~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "1020"), "format_call_EscapeUpward12");

	strvect_char_heap(&pos, "~:{<~A~^~A>~}~A");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>50"), "format_call_EscapeUpward13");

	strvect_char_heap(&pos, "~:{<~A~:^~A>~}~A");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	list_heap(&list, list, fixnumh(50), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3050"), "format_call_EscapeUpward14");

	strvect_char_heap(&pos, "~:@{<~A~^~A>~}");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><3040>"), "format_call_EscapeUpward15");

	strvect_char_heap(&pos, "~:@{<~A~:^~A>~}");
	list_heap(&list1, fixnumh(10), fixnumh(20), fixnumh(999), NULL);
	list_heap(&list2, fixnumh(30), fixnumh(40), NULL);
	list_heap(&list, list1, list2, NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "<1020><30"), "format_call_EscapeUpward16");

	RETURN;
}

static int test_format_call_CallFunction_test(Execute ptr, addr rest)
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

	return 0;
}

static int test_format_call_CallFunction(void)
{
	addr pos, list, symbol, call;
	Execute ptr;

	ptr = Execute_Thread;

	internchar(LISP_PACKAGE, "FORMAT-FUNCTION-TEST", &symbol);
	compiled_heap(&call, Nil);
	SetPointer(p_debug1, dynamic, test_format_call_CallFunction_test);
	setcompiled_dynamic(call, p_debug1);
	SetFunctionSymbol(symbol, call);
	strvect_char_heap(&pos, "~/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10-0-20"), "format_call_CallFunction1");

	strvect_char_heap(&pos,
			"~1,2,3:@/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10:@-3-20"), "format_call_CallFunction2");

	strvect_char_heap(&pos,
			"~1,2,'*,#,#,2,3,4,5,6@/" LISP_PACKAGE "::format-function-test/-~A");
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	format_string_lisp(ptr, pos, list, &pos);
	test(string_equal_char(pos, "10@-10-20"), "format_call_CallFunction3");

	RETURN;
}


/*
 *  main
 */
static int testbreak_format_function(void)
{
	TestBreak(test_fmtint_count);
	TestBreak(test_fmtint_argument);
	TestBreak(test_fmtint_nilp);
	TestBreak(test_fmtchar_argument);
	TestBreak(test_fmtchar_nilp);
	TestBreak(test_fmtint_default);
	TestBreak(test_fmtchar_default);
	TestBreak(test_fmtprint_putc_times);
	TestBreak(test_fmtprint_string);
	TestBreak(test_format_call_Output);
	TestBreak(test_format_write_margin);
	TestBreak(test_format_call_print);
	TestBreak(test_format_call_Aesthetic);
	TestBreak(test_format_call_Standard);
	TestBreak(test_format_call_Binary);
	TestBreak(test_format_call_Octal);
	TestBreak(test_format_call_Decimal);
	TestBreak(test_format_call_Hexadecimal);
	TestBreak(test_format_call_Radix);
	TestBreak(test_format_call_RadixText);
	TestBreak(test_format_call_Plural);
	TestBreak(test_format_call_Character);
	TestBreak(test_format_call_Fixed);
	TestBreak(test_format_call_Exponent);
	TestBreak(test_format_call_General);
	TestBreak(test_format_call_Monetary);
	TestBreak(test_format_call_Newline);
	TestBreak(test_format_call_FreshLine);
	TestBreak(test_format_call_Page);
	TestBreak(test_format_call_Tilde);
	TestBreak(test_format_call_IgnoredNewline);
	TestBreak(test_format_call_Tabulate);
	TestBreak(test_format_call_GoTo);
	TestBreak(test_format_call_Recursive);
	TestBreak(test_format_call_Case);
	TestBreak(test_format_call_Condition);
	TestBreak(test_format_call_Iteration);
	TestBreak(test_format_call_EscapeUpward);
	TestBreak(test_format_call_CallFunction);

	return 0;
}

int test_format_function(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
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
		build_reader();
		build_pathname();
		build_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_format_function();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

