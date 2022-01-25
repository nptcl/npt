#include "type_function.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "copy.h"
#include "degrade.h"
#include "hashtable.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "random_state.h"
#include "reader.h"
#include "stream.h"
#include "subtypep_optimize.h"
#include "symbol.h"
#include "syscall.h"
#include "type_parse.h"
#include "type_table.h"

static void test_parse_type(addr *ret, addr pos)
{
	if (parse_type_(Execute_Thread, ret, pos, Nil)) {
		Error(fmte_("parse-type error.", NULL));
	}
}

static void parse_type_string(addr *ret, const char *code)
{
	readstring_debug(ret, code);
	test_parse_type(ret, *ret);
}

static void parse_args(addr *ret, const char *str)
{
	parse_type_string(ret, str);
	GetArrayType(*ret, 0, ret);
}

static int test_make_ordargs(void)
{
	addr pos, a, b, c, d;
	ordargs str;

	parse_args(&pos, "(function "
			"(integer nil nil &optional t null &rest real &key (hello string)))");
	GetArrayA2(pos, 0, &a);
	GetArrayA2(pos, 1, &b);
	GetArrayA2(pos, 2, &c);
	GetArrayA2(pos, 3, &d);
	make_ordargs(&str, pos);
	test(str.var == a, "make_ordargs1");
	test(str.opt == b, "make_ordargs2");
	test(str.rest == c, "make_ordargs3");
	test(str.key == d, "make_ordargs4");
	test(length_list_unsafe(a) == 3, "make_ordargs5");
	test(length_list_unsafe(b) == 2, "make_ordargs6");
	test(RefLispDecl(c) == LISPDECL_REAL, "make_ordargs7");
	test(length_list_unsafe(d) == 1, "make_ordargs8");
	test(str.size_var == 3, "make_ordargs9");
	test(str.size_opt == 2, "make_ordargs10");
	test(str.size_key == 1, "make_ordargs11");
	test(str.pos_rest == 5, "make_ordargs12");
	test(str.size == 7, "make_ordargs13");

	parse_args(&pos, "(function (nil &rest real))");
	make_ordargs(&str, pos);
	test(str.size == 2, "make_ordargs14");

	parse_args(&pos, "(function (nil &optional real))");
	make_ordargs(&str, pos);
	test(str.size == 2, "make_ordargs15");

	parse_args(&pos, "(function (nil &key (name integer)))");
	make_ordargs(&str, pos);
	test(str.size == 3, "make_ordargs16");

	parse_args(&pos, "(function (nil &optional t null))");
	make_ordargs(&str, pos);
	test(str.size == 3, "make_ordargs17");

	/* &allow-other-keys */
	parse_args(&pos, "(function (nil &key (name integer)))");
	SetArrayType(pos, 3, T);
	make_ordargs(&str, pos);
	test(str.key == T, "make_ordargs18");
	test(str.size_key == 0, "make_ordargs19");
	test(str.size == 3, "make_ordargs20");

	RETURN;
}

static int test_gettype_ordargs(void)
{
	addr pos;
	ordargs str;
	ordtype type;

	parse_args(&pos, "(function (null atom))");
	make_ordargs(&str, pos);

	gettype_ordargs_(&str, 0, &type);
	test(type.var, "gettype_ordargs1");
	test(RefLispDecl(type.type) == LISPDECL_NULL, "gettype_ordargs2");

	gettype_ordargs_(&str, 1, &type);
	test(type.var, "gettype_ordargs3");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordargs4");

	gettype_ordargs_(&str, 2, &type);
	test(! type.var, "gettype_ordargs4");
	test(type.nil, "gettype_ordargs5");
	test(type.type == Nil, "gettype_ordargs6");

	parse_args(&pos, "(function (null atom &optional integer))");
	make_ordargs(&str, pos);

	gettype_ordargs_(&str, 1, &type);
	test(type.var, "gettype_ordargs7");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordargs8");

	gettype_ordargs_(&str, 2, &type);
	test(type.var, "gettype_ordargs9");
	test(RefLispDecl(type.type) == LISPDECL_INTEGER, "gettype_ordargs10");

	gettype_ordargs_(&str, 3, &type);
	test(! type.var, "gettype_ordargs11");
	test(type.nil, "gettype_ordargs12");
	test(type.type == Nil, "gettype_ordargs13");

	parse_args(&pos, "(function (&optional integer &rest cons))");
	make_ordargs(&str, pos);

	gettype_ordargs_(&str, 0, &type);
	test(type.var, "gettype_ordargs14");
	test(RefLispDecl(type.type) == LISPDECL_INTEGER, "gettype_ordargs15");

	gettype_ordargs_(&str, 1, &type);
	test(type.var, "gettype_ordargs15");
	test(type.rest, "gettype_ordargs16");
	test(RefLispDecl(type.type) == LISPDECL_CONS, "gettype_ordargs17");

	parse_args(&pos, "(function (atom &rest cons &key (hello real)))");
	make_ordargs(&str, pos);

	gettype_ordargs_(&str, 0, &type);
	test(type.var, "gettype_ordargs18");
	test(RefLispDecl(type.type) == LISPDECL_ATOM, "gettype_ordargs19");

	gettype_ordargs_(&str, 1, &type);
	test(type.var, "gettype_ordargs20");
	test(type.key, "gettype_ordargs21");
	test(! type.value, "gettype_ordargs22");
	test(type.type != Nil, "gettype_ordargs23");

	gettype_ordargs_(&str, 2, &type);
	test(type.var, "gettype_ordargs24");
	test(! type.key, "gettype_ordargs25");
	test(type.value, "gettype_ordargs26");
	test(type.type != Nil, "gettype_ordargs27");

	gettype_ordargs_(&str, 1, &type);
	test(type.var, "gettype_ordargs28");
	test(type.key, "gettype_ordargs29");
	test(! type.value, "gettype_ordargs30");
	test(type.type != Nil, "gettype_ordargs31");

	parse_args(&pos, "(function (atom &key (hello real)))");
	make_ordargs(&str, pos);

	gettype_ordargs_(&str, 1, &type);
	test(! type.var, "gettype_ordargs32");
	test(type.key, "gettype_ordargs33");
	test(! type.value, "gettype_ordargs34");
	test(type.type == Nil, "gettype_ordargs35");

	/* allow-other-keys */
	parse_args(&pos, "(function (atom &key (hello real)))");
	SetArrayType(pos, 3, T);
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	test(! type.var, "gettype_ordargs36");
	test(type.key, "gettype_ordargs37");
	test(! type.value, "gettype_ordargs38");
	test(type.type == Nil, "gettype_ordargs39");

	RETURN;
}

static int test_simple_p_ordargs(void)
{
	addr pos;
	ordargs str;

	parse_args(&pos, "(function (atom t))");
	make_ordargs(&str, pos);
	test(simple_p_ordargs(&str), "simple_p_ordargs1");

	parse_args(&pos, "(function (atom &rest t))");
	make_ordargs(&str, pos);
	test(! simple_p_ordargs(&str), "simple_p_ordargs2");

	parse_args(&pos, "(function (atom &key (a t)))");
	make_ordargs(&str, pos);
	test(! simple_p_ordargs(&str), "simple_p_ordargs3");

	RETURN;
}

static int test_merge_key_ordargs(void)
{
	addr pos, check, array;
	ordargs str;
	LocalRoot local = Local_Thread;

	/* &allow-other-keys */
	parse_args(&pos, "(function (atom &key (a t)))");
	SetArrayType(pos, 3, T);
	make_ordargs(&str, pos);
	merge_key_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_SYMBOL, "merge_key_ordargs1");

	/* (eql key) */
	parse_args(&pos, "(function (atom &key (name integer)))");
	make_ordargs(&str, pos);
	merge_key_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_EQL, "merge_key_ordargs2");
	GetArrayType(pos, 0, &pos);
	readstring_debug(&check, "name");
	test(pos == check, "merge_key_ordargs3");

	/* (or (eql key) ...) */
	parse_args(&pos, "(function (atom &key (aa real) (bb t) (cc atom)))");
	make_ordargs(&str, pos);
	merge_key_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_OR, "merge_key_ordargs4");
	GetArrayType(pos, 0, &array);
	test(lenarrayr(array) == 3, "merge_key_ordargs5");
	getarray(array, 0, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "merge_key_ordargs6");
	GetArrayType(pos, 0, &pos);
	readstring_debug(&check, "aa");
	test(pos == check, "merge_key_ordargs7");

	getarray(array, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "merge_key_ordargs8");
	GetArrayType(pos, 0, &pos);
	readstring_debug(&check, "bb");
	test(pos == check, "merge_key_ordargs9");

	getarray(array, 2, &pos);
	test(RefLispDecl(pos) == LISPDECL_EQL, "merge_key_ordargs10");
	GetArrayType(pos, 0, &pos);
	readstring_debug(&check, "cc");
	test(pos == check, "merge_key_ordargs11");

	RETURN;
}

static int test_merge_value_ordargs(void)
{
	addr pos, array;
	ordargs str;
	LocalRoot local = Local_Thread;

	/* &allow-other-keys */
	parse_args(&pos, "(function (atom &key (a t)))");
	SetArrayType(pos, 3, T);
	make_ordargs(&str, pos);
	merge_value_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_T, "merge_value_ordargs1");

	/* (eql type) */
	parse_args(&pos, "(function (atom &key (name integer)))");
	make_ordargs(&str, pos);
	merge_value_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "merge_value_ordargs2");

	/* (or type ...) */
	parse_args(&pos, "(function (atom &key (aa real) (bb t) (cc atom)))");
	make_ordargs(&str, pos);
	merge_value_ordargs(local, &pos, &str);
	test(RefLispDecl(pos) == LISPDECL_OR, "merge_value_ordargs3");
	GetArrayType(pos, 0, &array);
	test(lenarrayr(array) == 3, "merge_value_ordargs4");
	getarray(array, 0, &pos);
	test(RefLispDecl(pos) == LISPDECL_REAL, "merge_value_ordargs5");
	getarray(array, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "merge_value_ordargs6");
	getarray(array, 2, &pos);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "merge_value_ordargs7");

	RETURN;
}

static int test_merge_ordargs(void)
{
	addr pos, check;
	ordargs str;
	ordtype type;
	LocalRoot local = Local_Thread;

	parse_args(&pos, "(function (atom list))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 0, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "gettype_ordargs1");

	parse_args(&pos, "(function (atom &optional list))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_LIST, "gettype_ordargs2");

	parse_args(&pos, "(function (atom &rest real))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_REAL, "gettype_ordargs3");

	parse_args(&pos, "(function (atom &key (name real)))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_EQL, "gettype_ordargs4");

	parse_args(&pos, "(function (atom &key (name real)))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 2, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_REAL, "gettype_ordargs5");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordargs6");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordargs7");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_EQL, "gettype_ordargs8");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 2, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordargs9");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordargs10");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_REAL, "gettype_ordargs11");

	parse_args(&pos, "(function (atom &rest cons &key (name real)))");
	SetArrayType(pos, 3, T);
	make_ordargs(&str, pos);
	gettype_ordargs_(&str, 1, &type);
	merge_ordargs(local, &pos, &str, &type);
	test(RefLispDecl(pos) == LISPDECL_AND, "gettype_ordargs12");
	GetArrayType(pos, 0, &pos);
	getarray(pos, 0, &check);
	test(RefLispDecl(check) == LISPDECL_CONS, "gettype_ordargs13");
	getarray(pos, 1, &check);
	test(RefLispDecl(check) == LISPDECL_SYMBOL, "gettype_ordargs14");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_function(void)
{
	/* function */
	TestBreak(test_make_ordargs);
	TestBreak(test_gettype_ordargs);
	TestBreak(test_simple_p_ordargs);
	TestBreak(test_merge_key_ordargs);
	TestBreak(test_merge_value_ordargs);
	TestBreak(test_merge_ordargs);

	return 0;
}

static void testinit_type_function(Execute ptr)
{
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
}

int test_type_function(void)
{
	DegradeTitle;
	return DegradeCode(type_function);
}

