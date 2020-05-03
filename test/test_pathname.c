#include "pathname.c"
#include "clos.h"
#include "common.h"
#include "control.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "reader.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#if 0
static int test_pathname_heap(void)
{
	addr pos;

	pos = 0;
	pathname_heap(&pos, Nil,Nil,Nil,Nil,Nil,Nil);
	test(GetType(pos) == LISPTYPE_PATHNAME, "pathname_heap1");
	pos = 0;
	pathname_local(Local_Thread, &pos, Nil,Nil,Nil,Nil,Nil,Nil);
	test(GetType(pos) == LISPTYPE_PATHNAME, "pathname_local1");

	RETURN;
}

static int test_pathname_fileparse_heap(void)
{
	struct fileparse pa;

	init_fileparse(&pa);
	pathname_fileparse_heap(&pa, LISPTYPE_PATHNAME);
	test(GetType(pa.result) == LISPTYPE_PATHNAME, "pathname_fileparse_heap1");

	RETURN;
}

/* logical-pathname-table */
static int test_sethost_pathname(void)
{
	addr key, value;

	strvect_char_heap(&key, "hello");
	test(gethost_pathname(key, &value) == 0, "gethost_pathname1");
	test(gethost_pathname(key, &value) == 0, "gethost_pathname2");
	sethost_pathname(key, T);
	test(gethost_pathname(key, &value) != 0, "gethost_pathname3");
	test(value == T, "gethost_pathname4");
	sethost_pathname(key, Nil);
	test(gethost_pathname(key, &value) != 0, "gethost_pathname5");
	test(value == Nil, "gethost_pathname6");

	RETURN;
}

/* parser */
static int keywordcheck(addr pos, const char *name)
{
	addr package;
	if (GetType(pos) != LISPTYPE_SYMBOL) return 0;
	GetPackageSymbol(pos, &package);
	if (package == Nil) return 0;
	getname_package(package, &package);
	if (string_compare_char(package, "KEYWORD")) return 0;
	GetNameSymbol(pos, &pos);
	return string_compare_char(pos, name) == 0;
}

static int test_pushconstant(void)
{
	addr queue, check;

	queue_heap(&queue);
	pushconstant(queue, CONSTANT_KEYWORD_ABSOLUTE);
	pushconstant(queue, CONSTANT_KEYWORD_WILD);
	rootqueue(queue, &queue);
	GetCons(queue, &check, &queue);
	test(keywordcheck(check, "ABSOLUTE"), "pushconstant1");
	GetCons(queue, &check, &queue);
	test(keywordcheck(check, "WILD"), "pushconstant2");
	test(queue == Nil, "pushconstant3");

	RETURN;
}

static int left_equalp_test(addr left, const char *right)
{
	addr pos;
	strvect_char_heap(&pos, right);
	return stringp_equalp(left, pos);
}
static int right_equalp_test(addr right, const char *left)
{
	addr pos;
	strvect_char_heap(&pos, left);
	return stringp_equalp(pos, right);
}
static int test_stringp_equalp(void)
{
	addr check;

	test(left_equalp_test(T, "T") == 0, "stringp_equalp_char1");
	strvect_char_heap(&check, "Hello");
	test(left_equalp_test(check, "Hello"), "stringp_equalp_char2");
	test(left_equalp_test(check, "hello"), "stringp_equalp_char3");
	strvect_char_heap(&check, "Hello");
	test(left_equalp_test(check, "Hello"), "stringp_equalp_char4");
	test(left_equalp_test(check, "HELLO"), "stringp_equalp_char5");
	test(left_equalp_test(check, "HELLOa") == 0, "stringp_equalp_char6");
	test(left_equalp_test(check, "HELL") == 0, "stringp_equalp_char7");

	test(right_equalp_test(T, "T") == 0, "stringp_equalp_char1r");
	strvect_char_heap(&check, "Hello");
	test(right_equalp_test(check, "Hello"), "stringp_equalp_char2r");
	test(right_equalp_test(check, "hello"), "stringp_equalp_char3r");
	strvect_char_heap(&check, "Hello");
	test(right_equalp_test(check, "Hello"), "stringp_equalp_char4r");
	test(right_equalp_test(check, "HELLO"), "stringp_equalp_char5r");
	test(right_equalp_test(check, "HELLOa") == 0, "stringp_equalp_char6r");
	test(right_equalp_test(check, "HELL") == 0, "stringp_equalp_char7r");

	RETURN;
}

static int test_wild_value(void)
{
	addr check;

	wild_value(T, &check);
	test(check == T, "wild_value1");

	strvect_char_heap(&check, "*");
	wild_value(check, &check);
	test(keywordcheck(check, "WILD"), "wild_value2");

	strvect_char_heap(&check, "**");
	wild_value(check, &check);
	test(keywordcheck(check, "WILD-INFERIORS") == 0, "wild_value3");

	RETURN;
}

static int test_wild_newest_value(void)
{
	addr check;

	wild_newest_value(T, &check);
	test(check == T, "wild_newest_value1");

	strvect_char_heap(&check, "*");
	wild_newest_value(check, &check);
	test(keywordcheck(check, "WILD"), "wild_newest_value2");

	strvect_char_heap(&check, "newEst");
	wild_newest_value(check, &check);
	test(keywordcheck(check, "NEWEST"), "wild_newest_value3");

	strvect_char_heap(&check, "**");
	wild_newest_value(check, &check);
	test(keywordcheck(check, "WILD-INFERIORS") == 0, "wild_newest_value4");

	RETURN;
}

static int test_relative_empty_p(addr pos)
{
	addr a, b;

	if (! consp(pos)) return 0;
	GetCons(pos, &a, &b);
	GetConst(KEYWORD_RELATIVE, &pos);
	return a == pos && b == Nil;
}

static int test_makelogical(void)
{
	addr host;
	struct fileparse pa;

	init_fileparse(&pa);
	strvect_char_heap(&host, "test");
	strvect_char_heap(&pa.host, "Test");
	strvect_char_heap(&pa.name, "file");
	strvect_char_heap(&pa.type, "*");
	strvect_char_heap(&pa.version, "newest");
	makelogical(&pa, host);
	GetArrayA2(pa.result, PATHNAME_INDEX_HOST, &host);
	test(string_equalp_char(host, "teST"), "makelogical1");
	GetArrayA2(pa.result, PATHNAME_INDEX_DEVICE, &host);
	test(keywordcheck(host, "UNSPECIFIC"), "makelogical2");
	GetArrayA2(pa.result, PATHNAME_INDEX_DIRECTORY, &host);
	test(test_relative_empty_p(host), "makelogical3");
	GetArrayA2(pa.result, PATHNAME_INDEX_NAME, &host);
	test(string_equal_char(host, "file"), "makelogical4");
	GetArrayA2(pa.result, PATHNAME_INDEX_TYPE, &host);
	test(keywordcheck(host, "WILD"), "makelogical5");
	GetArrayA2(pa.result, PATHNAME_INDEX_VERSION, &host);
	test(keywordcheck(host, "NEWEST"), "makelogical6");
	test(GetType(pa.result) == LISPTYPE_PATHNAME, "makelogical7");
	test(RefLogicalPathname(pa.result), "makelogical8");

	RETURN;
}

static int test_pushrange_local(void)
{
	addr thing, queue, control;
	const unicode *body;
	Execute ptr = Execute_Thread;

	push_close_control(ptr, &control);
	strvect_char_heap(&thing, "abcHello1234567890");
	charqueue_heap(&queue, 0);
	GetStringUnicode(thing, &body);
	pushrange_local(ptr->local, queue, body, 3, 8);
	make_charqueue_local(ptr->local, queue, &thing);
	test(string_equal_char(thing, "Hello"), "pushrange_local1");
	free_control(ptr, control);

	RETURN;
}

static int test_splitlogical(void)
{
	struct fileparse pa;

	init_fileparse(&pa);
	charqueue_heap(&pa.queue, 0);
	pa.local = Local_Thread;
	strvect_char_heap(&pa.name, "fileAtypeBversion");
	splitlogical(&pa, 4, 9);
	test(string_equal_char(pa.name, "file"), "splitlogical1");
	test(string_equal_char(pa.type, "type"), "splitlogical2");
	test(string_equal_char(pa.version, "version"), "splitlogical3");

	strvect_char_heap(&pa.name, "aaabbbccc");
	splitlogical(&pa, 3, 4);
	test(string_equal_char(pa.name, "aaa"), "splitlogical4");
	test(string_equal_char(pa.type, ""), "splitlogical5");
	test(string_equal_char(pa.version, "bccc"), "splitlogical6");

	RETURN;
}

static int test_splitnametype(void)
{
	struct fileparse pa;

	init_fileparse(&pa);
	charqueue_heap(&pa.queue, 0);
	pa.local = Local_Thread;
	strvect_char_heap(&pa.name, "fileAtypeBversion");
	splitnametype(&pa, 4);
	test(string_equal_char(pa.name, "file"), "splitnametype1");
	test(string_equal_char(pa.type, "typeBversion"), "splitnametype2");

	strvect_char_heap(&pa.name, "aaa");
	splitnametype(&pa, 2);
	test(string_equal_char(pa.name, "aa"), "splitnametype3");
	test(string_equal_char(pa.type, ""), "splitnametype4");

	RETURN;
}

static int test_pushqueue_directory(void)
{
	addr queue, pos;

	queue_heap(&queue);
	internchar("KEYWORD", "ABSOLUTE", &pos);
	pushqueue_directory(queue, pos);
	strvect_char_heap(&pos, ".");
	pushqueue_directory(queue, pos);
	strvect_char_heap(&pos, "..");
	pushqueue_directory(queue, pos);
	strvect_char_heap(&pos, "*");
	pushqueue_directory(queue, pos);
	strvect_char_heap(&pos, "**");
	pushqueue_directory(queue, pos);
	strvect_char_heap(&pos, "Hello");
	pushqueue_directory(queue, pos);

	rootqueue(queue, &queue);
	GetCons(queue, &pos, &queue);
	test(keywordcheck(pos, "ABSOLUTE"), "pushqueue_directory1");
	GetCons(queue, &pos, &queue);
	test(GetType(pos) == LISPTYPE_STRING, "pushqueue_directory2");
	test(string_compare_char(pos, ".") == 0, "pushqueue_directory3");
	GetCons(queue, &pos, &queue);
	test(keywordcheck(pos, "UP"), "pushqueue_directory4");
	GetCons(queue, &pos, &queue);
	test(keywordcheck(pos, "WILD"), "pushqueue_directory5");
	GetCons(queue, &pos, &queue);
	test(keywordcheck(pos, "WILD-INFERIORS"), "pushqueue_directory6");
	GetCons(queue, &pos, &queue);
	test(string_compare_char(pos, "Hello") == 0, "pushqueue_directory7");

	RETURN;
}

static int test_check_logical(void)
{
	addr queue, name, control;
	Execute ptr = Execute_Thread;
	LocalRoot local;

	local = ptr->local;
	push_close_control(ptr, &control);
	strvect_char_heap(&name, "hello");
	sethost_pathname(name, T);
	charqueue_local(local, &queue, 0);
	pushchar_charqueue_local(ptr->local, queue, "HELLO");
	test(check_logical(local, queue) == 0, "check_logical1");

	clear_charqueue(queue);
	pushchar_charqueue_local(ptr->local, queue, "aaa");
	test(check_logical(local, queue) != 0, "check_logical2");

	/* close */
	sethost_pathname(name, Nil);
	free_control(ptr, control);

	RETURN;
}

static int test_check_logical_drive(void)
{
	addr name;
	LocalRoot local = Local_Thread;

	test(check_logical_drive(local, 'C') != 0, "check_logical_drive1");
	strvect_char_heap(&name, "c");
	sethost_pathname(name, Nil);
	test(check_logical_drive(local, 'C') == 0, "check_logical_drive2");

	RETURN;
}

static int test_makeunix(void)
{
	addr host, device, check;
	struct fileparse pa;

	init_fileparse(&pa);
	strvect_char_heap(&host, "host");
	strvect_char_heap(&device, "device");
	pathname_heap(&pa.path, host, device, Nil, Nil, Nil, Nil);
	strvect_char_heap(&pa.host, "aaa");
	strvect_char_heap(&pa.device, "bbb");
	strvect_char_heap(&pa.name, "file");
	strvect_char_heap(&pa.type, "*");
	strvect_char_heap(&pa.version, "newest");
	makeunix(&pa);
	GetArrayA2(pa.result, PATHNAME_INDEX_HOST, &host);
	GetConst(SYSTEM_UNIX, &check);
	test(host == check, "makeunix1");
	GetArrayA2(pa.result, PATHNAME_INDEX_DEVICE, &host);
	test(string_equal_char(host, "device"), "makeunix2");
	GetArrayA2(pa.result, PATHNAME_INDEX_DIRECTORY, &host);
	test(test_relative_empty_p(host), "makeunix3");
	GetArrayA2(pa.result, PATHNAME_INDEX_NAME, &host);
	test(string_equal_char(host, "file"), "makeunix4");
	GetArrayA2(pa.result, PATHNAME_INDEX_TYPE, &host);
	test(keywordcheck(host, "WILD"), "makeunix5");
	GetArrayA2(pa.result, PATHNAME_INDEX_VERSION, &host);
	test(host == Nil, "makeunix6");
	test(GetType(pa.result) == LISPTYPE_PATHNAME, "makeunix7");

	RETURN;
}

static int test_makewindows(void)
{
	addr host, device, check;
	struct fileparse pa;

	init_fileparse(&pa);
	strvect_char_heap(&host, "host");
	strvect_char_heap(&device, "device");
	pathname_heap(&pa.path, host, device, Nil, Nil, Nil, Nil);
	strvect_char_heap(&pa.host, "aaa");
	strvect_char_heap(&pa.device, "bbb");
	strvect_char_heap(&pa.name, "file");
	strvect_char_heap(&pa.type, "*");
	strvect_char_heap(&pa.version, "newest");
	makewindows(&pa);
	GetArrayA2(pa.result, PATHNAME_INDEX_HOST, &host);
	GetConst(SYSTEM_WINDOWS, &check);
	test(host == check, "makewindows1");
	GetArrayA2(pa.result, PATHNAME_INDEX_DEVICE, &host);
	test(string_equal_char(host, "bbb"), "makewindows2");
	GetArrayA2(pa.result, PATHNAME_INDEX_DIRECTORY, &host);
	test(test_relative_empty_p(host), "makewindows3");
	GetArrayA2(pa.result, PATHNAME_INDEX_NAME, &host);
	test(string_equal_char(host, "file"), "makewindows4");
	GetArrayA2(pa.result, PATHNAME_INDEX_TYPE, &host);
	test(keywordcheck(host, "WILD"), "makewindows5");
	GetArrayA2(pa.result, PATHNAME_INDEX_VERSION, &host);
	test(host == Nil, "makewindows6");
	test(GetType(pa.result) == LISPTYPE_PATHNAME, "makewindows7");

	RETURN;
}


/* parse test */
struct fileparse padata;
static void (*parser)(struct fileparse *) = NULL;
static void (*init)(void) = NULL;

static void init_default(void)
{
	init_fileparse(&padata);
	padata.ptr = Execute_Thread;
	padata.local = Local_Thread;
	pathname_heap(&padata.path, Nil,Nil,Nil,Nil,Nil,Nil);
}

static void init_logical(void)
{
	addr host;

	init_fileparse(&padata);
	padata.ptr = Execute_Thread;
	padata.local = Local_Thread;
	strvect_char_heap(&host, "logical");
	padata.host = host;
}

static void parser_replace(struct fileparse *pa,
		unicode r, void (*call)(struct fileparse *))
{
	addr queue, control;
	unicode c;
	size_t i, size;
	LocalRoot local;

	local = pa->local;
	push_close_control(pa->ptr, &control);
	string_length(pa->thing, &size);
	charqueue_local(local, &queue, 0);
	for (i = 0; i < size; i++) {
		string_getc(pa->thing, i, &c);
		push_charqueue_local(local, queue, (c == '/'? r: c));
	}
	make_charqueue_heap(queue, &pa->thing);
	call(pa);
	free_control(pa->ptr, control);
}

static void parser_logicalsemi(struct fileparse *pa)
{
	parser_replace(pa, ';', parser_logical);
}

static void parser_backslash(struct fileparse *pa)
{
	parser_replace(pa, '\\', parser_windows);
}

static void parse1(const char *name)
{
	init();
	strvect_char_heap(&padata.thing, name);
	string_length(padata.thing, &padata.end);
	parser(&padata);
}

static void parseu(const char *name)
{
	init();
	strvect_char_heap(&padata.thing, name);
	string_length(padata.thing, &padata.end);
	parser(&padata);
}

/* check */
static int emptycheck(void)
{
	addr check;

	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (! test_relative_empty_p(check)) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (check != Nil) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;

	return 1;
}

static int namecheck(const char *name)
{
	addr check;

	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (! test_relative_empty_p(check)) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (string_compare_char(check, name)) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;

	return 1;
}

static int absoluteonly(void)
{
	addr pos, check;

	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (check != Nil) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &pos);
	if (keywordcheck(check, "ABSOLUTE") == 0) return 0;
	if (pos != Nil) return 0;

	return 1;
}

static int absolute_namecheck(const char *name)
{
	addr next, check;

	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;
	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &next);
	if (keywordcheck(check, "ABSOLUTE") == 0) return 0;
	if (next != Nil) return 0;
	/* name */
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (string_compare_char(check, name)) return 0;

	return 1;
}

static int nametype_check(const char *name, const char *type)
{
	addr check;

	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (! test_relative_empty_p(check)) return 0;
	/* name */
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (string_compare_char(check, name)) return 0;
	/* type */
	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (string_compare_char(check, type)) return 0;

	return 1;
}

static int testparse_empty(void)
{
	const char *name = "";

	parse1(name);
	if (emptycheck() == 0) return 0;
	parseu(name);
	if (emptycheck() == 0) return 0;

	return 1;
}

static int parser_namecheck(const char *name)
{
	parse1(name);
	if (namecheck(name) == 0) return 0;
	parseu(name);
	if (namecheck(name) == 0) return 0;

	return 1;
}

static int relative_check(const char *name)
{
	addr next, check;

	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (check != Nil) return 0;
	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &next);
	if (keywordcheck(check, "RELATIVE") == 0) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, name)) return 0;
	if (next != Nil) return 0;

	return 1;
}

static int relativename_check(const char *directory, const char *name)
{
	addr next, check;

	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;
	/* name */
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (string_compare_char(check, name)) return 0;
	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &next);
	if (keywordcheck(check, "RELATIVE") == 0) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, directory)) return 0;
	if (next != Nil) return 0;

	return 1;
}

static int paths_check(const char *directory, const char *check1, const char *check2)
{
	addr next, check;

	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (check != Nil) return 0;
	/* name */
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (check != Nil) return 0;
	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &next);
	if (keywordcheck(check, directory) == 0) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, check1)) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, check2)) return 0;
	if (next != Nil) return 0;

	return 1;
}

static int paths3_check(const char *path0,
		const char *path1, const char *path2, const char *path3,
		const char *name, const char *type)
{
	addr next, check;

	/* directory */
	GetArrayA2(padata.result, PATHNAME_INDEX_DIRECTORY, &check);
	if (test_relative_empty_p(check)) return 0;
	GetCons(check, &check, &next);
	if (keywordcheck(check, path0) == 0) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, path1)) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, path2)) return 0;
	if (next == Nil) return 0;
	GetCons(next, &check, &next);
	if (string_compare_char(check, path3)) return 0;
	if (next != Nil) return 0;
	/* name */
	GetArrayA2(padata.result, PATHNAME_INDEX_NAME, &check);
	if (string_compare_char(check, name)) return 0;
	/* type */
	GetArrayA2(padata.result, PATHNAME_INDEX_TYPE, &check);
	if (string_compare_char(check, type)) return 0;

	return 1;
}

static int testparse_name(void)
{
	return parser_namecheck("hello");
}

static int testparse_absolute(void)
{
	const char *name = "/";

	parse1(name);
	if (absoluteonly() == 0) return 0;
	parseu(name);
	if (absoluteonly() == 0) return 0;

	return 1;
}

static int absolute_name(const char *name, const char *check)
{
	parse1(name);
	if (absolute_namecheck(check) == 0) return 0;
	parseu(name);
	if (absolute_namecheck(check) == 0) return 0;

	return 1;
}

static int testparse_absolutename(void)
{
	return absolute_name("/hello", "hello");
}

static int testparse_absolutes_name(void)
{
	return absolute_name("////hello", "hello");
}

static int testparse_dot(void)
{
	return parser_namecheck(".");
}

static int testparse_dotname(void)
{
	return parser_namecheck(".hello");
}

static int parser_namedot(const char *thing, const char *name, const char *type)
{
	parse1(thing);
	if (nametype_check(name, type) == 0) return 0;
	parseu(thing);
	if (nametype_check(name, type) == 0) return 0;

	return 1;
}

static int testparse_namedot(void)
{
	return parser_namedot("hello.lisp", "hello", "lisp");
}

static int testparse_namedotempty(void)
{
	return parser_namedot("hello.", "hello", "");
}

static int testparse_dotdot(void)
{
	return parser_namedot("..", ".", "");
}

static int parser_relative(const char *name, const char *check)
{
	parse1(name);
	if (relative_check(check) == 0) return 0;
	parseu(name);
	if (relative_check(check) == 0) return 0;

	return 1;
}

static int testparse_relative(void)
{
	return parser_relative("hello/", "hello");
}

static int testparse_relative1(void)
{
	return parser_relative(".hello/", ".hello");
}

static int testparse_relative2(void)
{
	return parser_relative("abc.def/", "abc.def");
}

static int testparse_relative3(void)
{
	return parser_relative("abc./", "abc.");
}

static int testparse_relative4(void)
{
	return parser_relative("abc/////", "abc");
}

static int parser_relative_name(const char *name, const char *str1, const char *str2)
{
	parse1(name);
	if (relativename_check(str1, str2) == 0) return 0;
	parseu(name);
	if (relativename_check(str1, str2) == 0) return 0;

	return 1;
}

static int testparse_relative_name(void)
{
	return parser_relative_name("abc/hello", "abc", "hello");
}

static int testparse_relative_name1(void)
{
	return parser_relative_name("abc/.", "abc", ".");
}

static int testparse_relative_name2(void)
{
	return parser_relative_name("ab.c/hello", "ab.c", "hello");
}

static int testparse_relative_name3(void)
{
	return parser_relative_name(".abc/hello", ".abc", "hello");
}

static int testparse_relative_name4(void)
{
	return parser_relative_name("abc/////.hello", "abc", ".hello");
}

static int parser_absolute_paths(const char *name,
		const char *check1, const char *check2, const char *check3)
{
	parse1(name);
	if (paths_check(check1, check2, check3) == 0) return 0;
	parseu(name);
	if (paths_check(check1, check2, check3) == 0) return 0;

	return 1;
}

static int testparse_absolute_paths(void)
{
	return parser_absolute_paths("/abc/hello/", "ABSOLUTE", "abc", "hello");
}

static int testparse_absolute_paths2(void)
{
	return parser_absolute_paths("///abc////hello///", "ABSOLUTE", "abc", "hello");
}

static int testparse_relative_paths(void)
{
	return parser_absolute_paths("abc/hello/", "RELATIVE", "abc", "hello");
}

static int testparse_relative_paths2(void)
{
	return parser_absolute_paths("abc////hello////", "RELATIVE", "abc", "hello");
}

static int parser_paths3(const char *pathname,
		const char *path0, const char *path1, const char *path2, const char *path3,
		const char *name, const char *type)
{
	parse1(pathname);
	if (paths3_check(path0, path1, path2, path3, name, type) == 0) return 0;
	parseu(pathname);
	if (paths3_check(path0, path1, path2, path3, name, type) == 0) return 0;

	return 1;
}

static int testparse_absolute_paths3(void)
{
	return parser_paths3("/usr/local/bin/hello.txt",
			"ABSOLUTE", "usr", "local", "bin", "hello", "txt");
}

static int testparse_relative_paths3(void)
{
	return parser_paths3("usr/local/bin/hello.txt",
			"RELATIVE", "usr", "local", "bin", "hello", "txt");
}

static int parser_test(void)
{
	init_fileparse(&padata);

	test(testparse_empty(), "testparse_empty");
	test(testparse_name(), "testparse_name");
	test(testparse_absolute(), "testparse_absolute");
	test(testparse_absolutename(), "testparse_absolutename");
	test(testparse_absolutes_name(), "testparse_absolutes_name");
	test(testparse_dot(), "testparse_dot");
	test(testparse_dotname(), "testparse_dotname");
	test(testparse_namedot(), "testparse_namedot");
	test(testparse_namedotempty(), "testparse_namedotempty");
	test(testparse_dotdot(), "testparse_dotdot");
	test(testparse_relative(), "testparse_relative");
	test(testparse_relative1(), "testparse_relative1");
	test(testparse_relative2(), "testparse_relative2");
	test(testparse_relative3(), "testparse_relative3");
	test(testparse_relative4(), "testparse_relative4");
	test(testparse_relative_name(), "testparse_relative_name");
	test(testparse_relative_name1(), "testparse_relative_name1");
	test(testparse_relative_name2(), "testparse_relative_name2");
	test(testparse_relative_name3(), "testparse_relative_name3");
	test(testparse_relative_name4(), "testparse_relative_name4");
	test(testparse_absolute_paths(), "testparse_absolute_paths");
	test(testparse_absolute_paths2(), "testparse_absolute_paths2");
	test(testparse_relative_paths(), "testparse_relative_paths");
	test(testparse_relative_paths2(), "testparse_relative_paths2");
	test(testparse_absolute_paths3(), "testparse_absolute_paths3");
	test(testparse_relative_paths3(), "testparse_relative_paths3");

	RETURN;
}

static int test_parser_logical(void)
{
	parser = parser_logicalsemi;
	init = init_logical;
	return 0; /* TODO */
	return parser_test();
}

static int test_parser_unix(void)
{
	parser = parser_unix;
	init = init_default;
	return parser_test();
}

static int test_parser_windows(void)
{
	parser = parser_windows;
	init = init_default;
	return parser_test();
}

static int test_parser_backslash(void)
{
	parser = parser_backslash;
	init = init_default;
	return parser_test();
}

static int test_name_pathname(void)
{
	addr file, pos;
	Execute ptr = Execute_Thread;

	strvect_char_heap(&file, "/usr/local/temp/hello.lisp");
	//setunixpath(ptr);
	parsepath(ptr, file, &pos);
	name_pathname(ptr, pos, &pos);

#if (defined LISP_WINDOWS) || (defined LISP_ANSIC_WINDOWS)
	test(string_equal_char(pos, "\\usr\\local\\temp\\hello.lisp"), "name_pathname1");
#else
	test(string_equal(file, pos), "name_pathname1");
#endif

	RETURN;
}


/*
 *  merge-pathnames
 */
static int test_merge_pathnames_clang(void)
{
	addr name1, name2, path1, path2, path;
	Execute ptr;

	ptr = Execute_Thread;
	strvect_char_heap(&name1, "/usr/local/");
	strvect_char_heap(&name2, "share/file.txt");
	parsepath(ptr, name1, &path1);
	parsepath(ptr, name2, &path2);
	merge_pathnames_clang(ptr, path2, path1, Nil, &path);
	name_pathname(ptr, path, &path);
#if (defined LISP_WINDOWS) || (defined LISP_ANSIC_WINDOWS)
	test(string_equal_char(path, "\\usr\\local\\share\\file.txt"), "merge-pathnames1");
#else
	test(string_equal_char(path, "/usr/local/share/file.txt"), "merge-pathnames1");
#endif

	RETURN;
}
#endif


/*
 *  Main
 */
static int testbreak_pathname(void)
{
#if 0
	/* parse-namestring */
	TestBreak(test_pathname_heap);
	TestBreak(test_pathname_fileparse_heap);
	TestBreak(test_sethost_pathname);
	TestBreak(test_pushconstant);
	TestBreak(test_stringp_equalp);
	TestBreak(test_wild_value);
	TestBreak(test_wild_newest_value);
	TestBreak(test_makelogical);
	TestBreak(test_pushrange_local);
	TestBreak(test_splitlogical);
	TestBreak(test_splitnametype);
	TestBreak(test_pushqueue_directory);
	TestBreak(test_check_logical);
	TestBreak(test_check_logical_drive);
	TestBreak(test_makeunix);
	TestBreak(test_makewindows);
	TestBreak(test_parser_logical);
	TestBreak(test_parser_unix);
	TestBreak(test_parser_windows);
	TestBreak(test_parser_backslash);

	/* namestring */
	TestBreak(test_name_pathname);

	/* merge-pathnames*/
	TestBreak(test_merge_pathnames_clang);
#endif

	return 0;
}

int test_pathname(void)
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
		build_reader();
		build_pathname();
		lisp_initialize = 1;
		result = testbreak_pathname();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

