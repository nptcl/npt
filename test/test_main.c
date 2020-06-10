#include "main.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "eval.h"
#include "degrade.h"
#include "equal.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "syscall.h"
#include "type.h"

#if 0
static int test_make_unimem(void)
{
	struct unimem *ptr;

	ptr = make_unimem(10);
	test(ptr, "make_unimem1");
	test(ptr->ptr, "make_unimem2");
	test(ptr->size == 10, "make_unimem3");
	free_unimem(ptr);
	free_unimem(NULL);
	test(1, "free_unimem1");

	RETURN;
}

static int test_make_string_list(void)
{
	struct unimem *u1, *u2;
	struct string_list *ptr;
	struct string_data *d1, *d2;

	ptr = make_string_list();
	test(ptr, "make_string_list1");
	test(ptr->root == NULL, "make_string_list2");
	test(ptr->tail == NULL, "make_string_list3");

	u1 = make_unimem(10);
	if (u1 == NULL) return 1;
	test(! push_string_list(ptr, u1), "push_string_list1");
	d1 = ptr->root;
	test(ptr->tail == d1, "push_string_list2");
	test(d1->data == u1, "push_string_list3");
	test(d1->next == NULL, "push_string_list4");

	u2 = make_unimem(10);
	if (u2 == NULL) return 1;
	test(! push_string_list(ptr, u2), "push_string_list5");
	test(ptr->root == d1, "push_string_list6");
	test(ptr->tail != d1, "push_string_list7");
	d2 = ptr->tail;
	test(d1->next == d2, "push_string_list8");
	test(d2->next == NULL, "push_string_list9");
	test(d2->data == u2, "push_string_list10");

	free_string_list(ptr);
	test(1, "free_string_list1");

	RETURN;
}

static int test_copy_unicode_char(void)
{
	unicode u[100];
	byte c;

	copy_unicode_char(u, "Hello", 5);
	test(u[0] == 'H', "copy_unicode_char1");
	test(u[1] == 'e', "copy_unicode_char2");
	test(u[3] == 'l', "copy_unicode_char3");
	test(u[4] == 'o', "copy_unicode_char4");

	c = 0xFF;
	copy_unicode_char(u, (const char *)&c, 1);
	test(u[0] = 0xFF, "copy_unicode_char5");

	RETURN;
}

static struct unimem *test_unimem(const char *str)
{
	struct unimem *ptr;
	size_t size;

	size = strlen(str);
	ptr = make_unimem(size);
	Check(ptr == NULL, "make-unimem error");
	copy_unicode_char(ptr->ptr, str, size);
	ptr->ptr[size] = 0;

	return ptr;
}

static int test_unimem_equal(struct unimem *ptr, const char *str)
{
	unicode *left, a, b;
	byte *right;
	size_t size, i;

	size = strlen(str);
	if (ptr->size != size) return 0;
	left = ptr->ptr;
	right = (byte *)str;
	for (i = 0; i < size; i++) {
		a = left[i];
		b = (unicode)right[i];
		if (a != b) return 0;
	}

	return 1;
}

static int test_filename_uc(void)
{
	struct unimem *ptr;
	struct string_list *list;

	ptr = test_unimem("Hello");
	list = make_string_list();
	Check(list == NULL, "make-string-list error");

	test(! filename_uc(list, NULL, "-aaa"), "filename_uc1");
	test(list->tail == NULL, "filename_uc2");
	test(! filename_uc(list, ptr, "-aaa"), "filename_uc3");
	test(test_unimem_equal(list->tail->data, "Hello-aaa"), "filename_uc4");

	RETURN;
}

#ifndef LISP_WINDOWS_WIDE
static int test_filename_cc(void)
{
	struct string_list *list;

	list = make_string_list();
	Check(list == NULL, "make-string-list error");

	test(! filename_cc(list, NULL, "-aaa"), "filename_cc1");
	test(list->tail == NULL, "filename_cc2");
	test(! filename_cc(list, "Hello", "-aaa"), "filename_cc3");
	test(test_unimem_equal(list->tail->data, "Hello-aaa"), "filename_cc4");

	RETURN;
}
#endif

static int test_filename_core(void)
{
	FileName_Core = NULL;
	setenvvar();
	filename_core();
	test(FileName_Core, "filename_core1");
	free_envvar();

	RETURN;
}

static int test_filename_init(void)
{
	FileName_Init = NULL;
	setenvvar();
	filename_init();
	test(FileName_Init, "filename_init1");
	free_envvar();

	RETURN;
}

static int test_equal_unimem_char(void)
{
	struct unimem *ptr;

	ptr = test_unimem("Hello");
	test(equal_unimem_char(ptr, "Hello"), "equal_unimem_char1");
	test(! equal_unimem_char(ptr, "Helloa"), "equal_unimem_char2");
	test(! equal_unimem_char(ptr, "hello"), "equal_unimem_char3");
	free_unimem(ptr);

	RETURN;
}

static int test_args_check(void)
{
	struct unimem *ptr;

	ptr = test_unimem("--");
	test(args_argument(ptr), "args_check1");
	test(! args_help(ptr), "args_check2");
	free_unimem(ptr);

	RETURN;
}


/*
 *  setmemory
 */
static int test_strtosize(void)
{
	struct unimem *ptr;
	const unicode *endp;
	size_t size;

	ptr = test_unimem("");
	test(strtosize(ptr->ptr, &endp, &size) == 1, "strtosize1");
	free_unimem(ptr);

	ptr = test_unimem("0");
	test(strtosize(ptr->ptr, &endp, &size) == 0, "strtosize2");
	test(size == 0, "strtosize3");
	test(*endp == 0, "strtosize4");
	free_unimem(ptr);

	ptr = test_unimem("3A");
	test(strtosize(ptr->ptr, &endp, &size) == 0, "strtosize5");
	test(size == 3, "strtosize6");
	test(*endp == 'A', "strtosize7");
	free_unimem(ptr);

	ptr = test_unimem("34");
	test(strtosize(ptr->ptr, &endp, &size) == 0, "strtosize8");
	test(size == 34, "strtosize9");
	test(*endp == 0, "strtosize10");
	free_unimem(ptr);

	ptr = test_unimem("1024M");
	test(strtosize(ptr->ptr, &endp, &size) == 0, "strtosize11");
	test(size == 1024, "strtosize12");
	test(*endp == 'M', "strtosize13");
	free_unimem(ptr);

	ptr = test_unimem("99999999999999999999999999");
	test(strtosize(ptr->ptr, &endp, &size) == 2, "strtosize14");
	test(size == SIZE_MAX, "strtosize15");
	free_unimem(ptr);

	RETURN;
}

static int test_getunitstring(void)
{
	int type;
	unicode data[10];

	data[0] = 0;
	test(getunitstring(data, &type) == 0, "getunitstring1");
	test(type == 0, "getunitstring2");

	data[0] = 'k';
	data[1] = 0;
	test(getunitstring(data, &type) == 0, "getunitstring3");
	test(type == 'K', "getunitstring4");

	data[0] = 'G';
	data[1] = data[2] = data[3] = ' ';
	data[4] = 0;
	test(getunitstring(data, &type) == 0, "getunitstring5");
	test(type == 'G', "getunitstring6");

	data[0] = 'G';
	data[1] = data[2] = data[3] = ' ';
	data[4] = 'A';
	data[5] = 0;
	test(getunitstring(data, &type), "getunitstring7");

	data[0] = 'A';
	data[1] = 0;
	test(getunitstring(data, &type), "getunitstring8");

	RETURN;
}

static int test_unitloop(void)
{
	size_t size;

	size = 34;
	test(unitloop(&size, 0) == 0, "unitloop1");
	test(size == 34, "unitloop2");

	size = 8;
	test(unitloop(&size, 1) == 0, "unitloop3");
	test(size == 8UL * 1024UL, "unitloop4");

	size = 10;
	test(unitloop(&size, 2) == 0, "unitloop5");
	test(size == 10UL * 1024UL * 1024UL, "unitloop6");

	size = 10;
	test(unitloop(&size, 100), "unitloop7");

	RETURN;
}

static int test_getsizestring(void)
{
	unicode u[100];
	size_t size;

	copy_unicode_char(u, "123", 3);
	u[3] = 0;
	test(! getsizestring(u, &size), "getsizestring1");
	test(size == 123UL, "getsizestring2");

	copy_unicode_char(u, "12M", 3);
	u[3] = 0;
	test(! getsizestring(u, &size), "getsizestring3");
	test(size == 12UL * 1024UL * 1024UL, "getsizestring4");

	RETURN;
}

static int test_setheap(void)
{
	struct unimem *ptr;

	ptr = test_unimem("1G");
	SizeHeap = 0;
	setheap(ptr);
	test(SizeHeap == 1UL * 1024 * 1024 * 1024, "setheap1");
	free_unimem(ptr);

	RETURN;
}

static int test_setlocal(void)
{
	struct unimem *ptr;

	ptr = test_unimem("1G");
	SizeLocal = 0;
	setlocal(ptr);
	test(SizeLocal == 1UL * 1024 * 1024 * 1024, "setlocal1");
	free_unimem(ptr);

	RETURN;
}


/*
 *  main
 */
#ifdef LISP_WINDOWS_WIDE
static int test_make_unimem_ctype(void)
{
	return 0;
}
#else
static int test_make_unimem_ctype(void)
{
	struct unimem *ptr;
	unicode *u;

	ptr = make_unimem_ctype("abc");
	test(ptr, "make_unimem_ctype1");
	test(ptr->size == 3, "make_unimem_ctype2");
	u = ptr->ptr;
	test(u[0] == 'a', "make_unimem_ctype3");
	test(u[1] == 'b', "make_unimem_ctype4");
	test(u[2] == 'c', "make_unimem_ctype5");
	test(u[3] == 0, "make_unimem_ctype6");
	free_unimem(ptr);

	/* a i u */
	ptr = make_unimem_ctype("\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86");
	test(ptr, "make_unimem_ctype7");
	test(ptr->size == 3, "make_unimem_ctype8");
	u = ptr->ptr;
	test(u[0] == 0x3042, "make_unimem_ctype9");
	test(u[1] == 0x3044, "make_unimem_ctype10");
	test(u[2] == 0x3046, "make_unimem_ctype11");
	test(u[3] == 0, "make_unimem_ctype12");
	free_unimem(ptr);

	RETURN;
}
#endif

#ifdef LISP_WINDOWS_WIDE
static int test_make_envmemory(void)
{
	struct envmemory *ptr;

	ptr = make_envmemory(L"aaa=bbbb");
	test(ptr, "make_envmemory1");
	test(test_unimem_equal(ptr->key, "aaa"), "make_envmemory2");
	test(ptr->key->size == 3, "make_envmemory3");
	test(test_unimem_equal(ptr->value, "bbbb"), "make_envmemory4");
	test(ptr->value->size == 4, "make_envmemory5");
	free_envmemory(ptr);

	ptr = make_envmemory(L"aaabbbb");
	test(ptr, "make_envmemory6");
	test(test_unimem_equal(ptr->key, "aaabbbb"), "make_envmemory7");
	test(ptr->key->size == 7, "make_envmemory8");
	test(test_unimem_equal(ptr->value, ""), "make_envmemory9");
	test(ptr->value->size == 0, "make_envmemory10");
	free_envmemory(ptr);

	RETURN;
}

static int test_make_envarray(void)
{
	return 0;
}

static int test_setenvvar(void)
{
	return 0;
}

#else

static int test_make_envmemory(void)
{
	struct envmemory *ptr;

	ptr = make_envmemory("aaa=bbbb");
	test(ptr, "make_envmemory1");
	test(test_unimem_equal(ptr->key, "aaa"), "make_envmemory2");
	test(ptr->key->size == 3, "make_envmemory3");
	test(test_unimem_equal(ptr->value, "bbbb"), "make_envmemory4");
	test(ptr->value->size == 4, "make_envmemory5");
	free_envmemory(ptr);

	ptr = make_envmemory("aaabbbb");
	test(ptr, "make_envmemory6");
	test(test_unimem_equal(ptr->key, "aaabbbb"), "make_envmemory7");
	test(ptr->key->size == 7, "make_envmemory8");
	test(test_unimem_equal(ptr->value, ""), "make_envmemory9");
	test(ptr->value->size == 0, "make_envmemory10");
	free_envmemory(ptr);

	RETURN;
}

static int test_setenvvar(void)
{
	struct envmemory **ptr;
	size_t size;

	ptr = make_envarray(DegradeEnv, &size);
	Check(ptr == NULL, "make_envarray error.");
	Envc = size;
	Envv = ptr;
	EnvHome = NULL;
	setenvvar();
	test(EnvHome, "setenvvar1");
	free_envvar();

	RETURN;
}

static int test_make_envarray(void)
{
	size_t size, i;
	struct envmemory **ptr;

	ptr = make_envarray(DegradeEnv, &size);
	test(ptr, "make_envarray1");

	for (i = 0; DegradeEnv[i]; i++) continue;
	test(size == i, "make_envarray2");

	RETURN;
}
#endif
#endif


/*
 *  main
 */
static int testbreak_main(void)
{
#if 0
	TestBreak(test_make_unimem);
	TestBreak(test_make_string_list);
	TestBreak(test_copy_unicode_char);
	TestBreak(test_filename_uc);
#ifndef LISP_WINDOWS_WIDE
	TestBreak(test_filename_cc);
#endif
	TestBreak(test_filename_core);
	TestBreak(test_filename_init);
	TestBreak(test_equal_unimem_char);
	TestBreak(test_args_check);
	/* setmemory */
	TestBreak(test_strtosize);
	TestBreak(test_getunitstring);
	TestBreak(test_unitloop);
	TestBreak(test_getsizestring);
	TestBreak(test_setheap);
	TestBreak(test_setlocal);
	/* main */
	TestBreak(test_make_unimem_ctype);
	TestBreak(test_make_envmemory);
	TestBreak(test_setenvvar);
	TestBreak(test_make_envarray);
#endif

	return 0;
}

int test_main(void)
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
		buildlisp(ptr);
		lisp_initialize = 1;
		result = testbreak_main();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

