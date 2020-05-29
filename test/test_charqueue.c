#include "charqueue.c"
#include "degrade.h"

/*
 *  charqueue
 */
static int test_charbit_macro(void)
{
	addr pos, check;
	unicode u;
	size_t size;

	charbit_alloc(NULL, &pos, 100);
	PtrArrayAB(pos)[0] = T;
	check = 0;
	GetCharBitNext(pos, &check);
	test(check == T, "charbit_macro.1");
	SetCharBitNext(pos, Nil);
	GetCharBitNext(pos, &check);
	test(check == Nil, "charbit_macro.2");
	test(PtrCharBitBody(pos) == PtrByte4P(pos) + PtrSize, "charbit_macro.3");
	test((void *)PtrCharBitChar(pos) ==
			(void *)(PtrCharBitBody(pos) + IdxSize), "charbit_macro.4");
	size = 999;
	GetCharBitSize(pos, &size);
	test(size == 0, "charbit_macro.5");
	SetCharBitSize(pos, 200);
	GetCharBitSize(pos, &size);
	test(size == 200, "charbit_macro.6");
	PtrCharBitChar(pos)[0] = 100;
	PtrCharBitChar(pos)[1] = 200;
	u = 0;
	GetCharBitChar(pos, 0, &u);
	test(u == 100, "charbit_macro.7");
	GetCharBitChar(pos, 1, &u);
	test(u == 200, "charbit_macro.8");
	SetCharBitChar(pos, 1, 333);
	GetCharBitChar(pos, 1, &u);
	test(u == 333, "charbit_macro.9");

	RETURN;
}

static int test_charbit_alloc(void)
{
	addr pos;
	size_t size;

	charbit_alloc(NULL, &pos, 12);
	test(GetType(pos) == LISPSYSTEM_CHARBIT, "charbit_alloc.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAYBODY, "charbit_alloc.2");
	test(GetLenArrayAB(pos) == 1, "charbit_alloc.3");
	size = IdxSize + 12*sizeoft(unicode);
	test(GetLenBodyAB(pos) == size, "charbit_alloc.4");
	test(RefBodyAB(pos, size_t) == 0, "charbit_alloc.5");

	RETURN;
}

static int test_charbit_push(void)
{
	addr pos;
	unicode u;
	size_t size;

	charbit_alloc(NULL, &pos, 100);

	charbit_push(pos, 100);
	GetCharBitSize(pos, &size);
	test(size == 1, "charbit_push.1");
	GetCharBitChar(pos, 0, &u);
	test(u == 100, "charbit_push.2");

	charbit_push(pos, 200);
	GetCharBitSize(pos, &size);
	test(size == 2, "charbit_push.3");
	GetCharBitChar(pos, 1, &u);
	test(u == 200, "charbit_push.4");

	RETURN;
}

static int test_charqueue_alloc(void)
{
	addr pos, root, tail;
	size_t size;

	charqueue_alloc(NULL, &pos, 14);
	test(GetType(pos) == LISPSYSTEM_CHARQUEUE, "charqueue_alloc.1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "charqueue_alloc.2");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "charqueue_alloc.3");
	GetCharBitSize(root, &size);
	test(size == 0, "charqueue_alloc.4");
	size = GetLenBodyAB(root);
	test(size == IdxSize + sizeoft(unicode) * 14, "charqueue_alloc.5");

	size = 100;
	GetCharQueueSize(pos, &size);
	test(size == 0, "charqueue_alloc.6");
	GetCharQueueMax(pos, &size);
	test(size == 14, "charqueue_alloc.7");

	SetCharQueueSize(pos, 100);
	GetCharQueueSize(pos, &size);
	test(size == 100, "charqueue_alloc.8");
	IncCharQueueSize(pos);
	GetCharQueueSize(pos, &size);
	test(size == 101, "charqueue_alloc.9");
	SetCharQueueMax(pos, 200);
	GetCharQueueMax(pos, &size);
	test(size == 200, "charqueue_alloc.10");

	SetCharQueueRoot(pos, T);
	GetCharQueueRoot(pos, &root);
	test(root == T, "charqueue_alloc.11");
	SetCharQueueTail(pos, Nil);
	GetCharQueueTail(pos, &root);
	test(root == Nil, "charqueue_alloc.12");

	RETURN;
}

static int test_getsize_charqueue(void)
{
	addr pos;
	size_t size;

	charqueue_alloc(NULL, &pos, 0);
	getsize_charqueue(pos, &size);
	test(size == 0, "getsize_charqueue.1");
	IncCharQueueSize(pos);
	getsize_charqueue(pos, &size);
	test(size == 1, "getsize_charqueue.2");

	RETURN;
}

static int test_getchar_charqueue(void)
{
	int check;
	addr pos;
	unicode i, u;

	charqueue_alloc(NULL, &pos, 3);
	getchar_charqueue(pos, 0, &u);
	test(u == 0, "getchar_charqueue.1");

	push_charqueue_heap(pos, 10);
	push_charqueue_heap(pos, 20);
	getchar_charqueue(pos, 0, &u);
	test(u == 10, "getchar_charqueue.1");
	getchar_charqueue(pos, 1, &u);
	test(u == 20, "getchar_charqueue.2");
	getchar_charqueue(pos, 2, &u);
	test(u == 0, "getchar_charqueue.3");

	for (i = 3; i <= 100; i++)
		push_charqueue_heap(pos, i * 10);
	check = 1;
	for (i = 0; i < 100; i++) {
		getchar_charqueue(pos, i, &u);
		if (u != ((i + 1) * 10)) {
			check = 0;
			break;
		}
	}
	test(check, "getchar_charqueue.4");

	RETURN;
}

static int test_push_charqueue(void)
{
	addr pos, root, tail;
	size_t size;
	unicode u;

	charqueue_alloc(NULL, &pos, 5);

	push_charqueue_heap(pos, 10);
	GetCharQueueSize(pos, &size);
	test(size == 1, "push_charqueue.1");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 1, "push_charqueue.2");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "push_charqueue.3");

	push_charqueue_heap(pos, 20);
	GetCharQueueSize(pos, &size);
	test(size == 2, "push_charqueue.4");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 2, "push_charqueue.5");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "push_charqueue.6");
	GetCharBitChar(tail, 1, &u);
	test(u == 20, "push_charqueue.7");

	push_charqueue_heap(pos, 30);
	push_charqueue_heap(pos, 40);
	push_charqueue_heap(pos, 50);
	GetCharQueueSize(pos, &size);
	test(size == 5, "push_charqueue.8");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "push_charqueue.9");
	GetCharBitSize(tail, &size);
	test(size == 5, "push_charqueue.10");
	GetCharBitChar(tail, 4, &u);
	test(u == 50, "push_charqueue.11");

	push_charqueue_heap(pos, 60);
	GetCharQueueSize(pos, &size);
	test(size == 6, "push_charqueue.12");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root != tail, "push_charqueue.13");
	GetCharBitSize(root, &size);
	test(size == 5, "push_charqueue.14");
	GetCharBitSize(tail, &size);
	test(size == 1, "push_charqueue.15");
	GetCharBitChar(tail, 0, &u);
	test(u == 60, "push_charqueue.16");

	RETURN;
}

static int test_make_charqueue(void)
{
	int check;
	addr pos, str;
	size_t size;
	unicode i, u;

	/* stringu */
	charqueue_alloc(NULL, &pos, 33);
	for (i = 1; i <= 100; i++)
		push_charqueue_heap(pos, i * 10);
	make_charqueue_heap(pos, &str);
	string_length(str, &size);
	test(size == 100, "make_charqueue.1");
	check = 1;
	for (i = 0; i < 100; i++) {
		string_getc(str, i, &u);
		if (u != ((i + 1) * 10)) {
			check = 0;
			break;
		}
	}
	test(check, "make_charqueue.2");

	/* string1 */
	charqueue_alloc(NULL, &pos, 22);
	for (i = 1; i <= 100; i++)
		push_charqueue_heap(pos, i * 2);
	make_charqueue_heap(pos, &str);
	string_length(str, &size);
	test(size == 100, "make_charqueue.3");
	check = 1;
	for (i = 0; i < 100; i++) {
		string_getc(str, i, &u);
		if (u != ((i + 1) * 2)) {
			check = 0;
			break;
		}
	}
	test(check, "make_charqueue.4");

	/* empty */
	charqueue_alloc(NULL, &pos, 11);
	make_charqueue_heap(pos, &str);
	string_length(str, &size);
	test(size == 0, "make_charqueue.5");

	RETURN;
}

static int test_clear_charqueue(void)
{
	int i;
	addr pos, root, tail;
	size_t size;

	charqueue_alloc(NULL, &pos, 5);
	for (i = 0; i < 100; i++)
		push_charqueue_heap(pos, i * 10);
	clear_charqueue(pos);
	GetCharQueueSize(pos, &size);
	test(size == 0, "clear_charqueue.1");
	GetCharQueueMax(pos, &size);
	test(size == 5, "clear_charqueue.2");

	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "clear_charqueue.4");
	GetCharBitSize(root, &size);
	test(size == 0, "clear_charqueue.5");
	GetCharBitNext(root, &tail);
	test(tail != Nil, "clear_charqueue.6");

	RETURN;
}

static int test_free_charqueue(void)
{
	int i;
	addr pos, root, tail;
	size_t size;

	charqueue_alloc(NULL, &pos, 5);
	for (i = 0; i < 100; i++)
		push_charqueue_heap(pos, i * 10);
	free_charqueue(pos);
	GetCharQueueSize(pos, &size);
	test(size == 0, "free_charqueue.1");
	GetCharQueueMax(pos, &size);
	test(size == 5, "free_charqueue.2");

	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "free_charqueue.4");
	GetCharBitSize(root, &size);
	test(size == 0, "free_charqueue.5");
	GetCharBitNext(root, &tail);
	test(tail == Nil, "free_charqueue.6");

	RETURN;
}

static int test_reuse_charqueue(void)
{
	addr pos, root, tail;
	size_t size;
	unicode u;

	charqueue_alloc(NULL, &pos, 5);
	for (u = 0; u < 9999; u++)
		push_charqueue_heap(pos, u);
	clear_charqueue(pos);

	push_charqueue_heap(pos, 10);
	GetCharQueueSize(pos, &size);
	test(size == 1, "reuse_charqueue.1");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 1, "reuse_charqueue.2");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "reuse_charqueue.3");

	push_charqueue_heap(pos, 20);
	GetCharQueueSize(pos, &size);
	test(size == 2, "reuse_charqueue.4");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 2, "reuse_charqueue.5");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "reuse_charqueue.6");
	GetCharBitChar(tail, 1, &u);
	test(u == 20, "reuse_charqueue.7");

	push_charqueue_heap(pos, 30);
	push_charqueue_heap(pos, 40);
	push_charqueue_heap(pos, 50);
	GetCharQueueSize(pos, &size);
	test(size == 5, "reuse_charqueue.8");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "reuse_charqueue.9");
	GetCharBitSize(tail, &size);
	test(size == 5, "reuse_charqueue.10");
	GetCharBitChar(tail, 4, &u);
	test(u == 50, "reuse_charqueue.11");

	push_charqueue_heap(pos, 60);
	GetCharQueueSize(pos, &size);
	test(size == 6, "reuse_charqueue.12");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root != tail, "reuse_charqueue.13");
	GetCharBitSize(root, &size);
	test(size == 5, "reuse_charqueue.14");
	GetCharBitSize(tail, &size);
	test(size == 1, "reuse_charqueue.15");
	GetCharBitChar(tail, 0, &u);
	test(u == 60, "reuse_charqueue.16");

	RETURN;
}

static int test_pushstring_charqueue(void)
{
	addr pos, str;

	strvect_char_heap(&str, "1234567");
	charqueue_heap(&pos, 5);
	push_charqueue_heap(pos, 'a');
	push_charqueue_heap(pos, 'b');
	push_charqueue_heap(pos, 'c');
	pushstring_charqueue_heap(pos, str);
	str = Nil;
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abc1234567"), "pushstring_charqueue.1");
	clear_charqueue(pos);

	pushchar_charqueue_heap(pos, "abc");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abc"), "pushstring_charqueue.2");

	pushchar_charqueue_heap(pos, "de");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcde"), "pushstring_charqueue.3");

	pushchar_charqueue_heap(pos, "f");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcdef"), "pushstring_charqueue.4");

	pushchar_charqueue_heap(pos, "gh");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcdefgh"), "pushstring_charqueue.5");

	RETURN;
}

static int test_charqueue_heap(void)
{
	int i, c;
	addr pos, str, check;
	size_t size;
	unicode *u;
	char buffer[LISP_CHARQUEUESIZE * 2];

	charqueue_heap(&pos, 0);
	push_charqueue_heap(pos, 'a');
	push_charqueue_heap(pos, 'b');
	push_charqueue_heap(pos, 'c');
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue.1");
	test(string_equal_char(str, "abc"), "charqueue.2");
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue.3");
	test(string_equal_char(str, "abc"), "charqueue.4");
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue.5");
	strvect_char_heap(&check, "abc");
	test(string_equal(str, check), "charqueue.6");

	clear_charqueue(pos);
	for (i = 0; i < LISP_CHARQUEUESIZE * 2 - 1; i++) {
		c = 'A' + (i % 20);
		buffer[i] = c;
		push_charqueue_heap(pos, c);
	}
	buffer[LISP_CHARQUEUESIZE * 2 - 1] = 0;
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue.7");
	test(string_equal_char(str, buffer), "charqueue.8");

	clear_charqueue(pos);
	push_charqueue_heap(pos, 1000);
	push_charqueue_heap(pos, 2000);
	push_charqueue_heap(pos, 3000);
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue.9");
	string_length(str, &size);
	test(size == 3, "charqueue.10");
	GetStringUnicode(str, &u);
	test(u[0] == 1000, "charqueue.11");
	test(u[1] == 2000, "charqueue.12");
	test(u[2] == 3000, "charqueue.13");

	RETURN;
}


/*
 *  charqueue
 */
static int testcase_charqueue(void)
{
	TestBreak(test_charbit_macro);
	TestBreak(test_charbit_alloc);
	TestBreak(test_charbit_push);
	TestBreak(test_charqueue_alloc);
	TestBreak(test_getsize_charqueue);
	TestBreak(test_getchar_charqueue);
	TestBreak(test_push_charqueue);
	TestBreak(test_make_charqueue);
	TestBreak(test_clear_charqueue);
	TestBreak(test_free_charqueue);
	TestBreak(test_reuse_charqueue);
	TestBreak(test_pushstring_charqueue);
	TestBreak(test_charqueue_heap);

	return 0;
}

static void testinit_charqueue(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_package();
}

int test_charqueue(void)
{
	TITLE;
	return degrade_code(
			testinit_charqueue,
			testcase_charqueue);
}

