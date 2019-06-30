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
	test(check == T, "charbit_macro1");
	SetCharBitNext(pos, Nil);
	GetCharBitNext(pos, &check);
	test(check == Nil, "charbit_macro2");
	test(PtrCharBitBody(pos) == PtrByte4P(pos) + PtrSize, "charbit_macro3");
	test((void *)PtrCharBitChar(pos) ==
			(void *)(PtrCharBitBody(pos) + IdxSize), "charbit_macro4");
	size = 999;
	GetCharBitSize(pos, &size);
	test(size == 0, "charbit_macro5");
	SetCharBitSize(pos, 200);
	GetCharBitSize(pos, &size);
	test(size == 200, "charbit_macro6");
	PtrCharBitChar(pos)[0] = 100;
	PtrCharBitChar(pos)[1] = 200;
	u = 0;
	GetCharBitChar(pos, 0, &u);
	test(u == 100, "charbit_macro7");
	GetCharBitChar(pos, 1, &u);
	test(u == 200, "charbit_macro8");
	SetCharBitChar(pos, 1, 333);
	GetCharBitChar(pos, 1, &u);
	test(u == 333, "charbit_macro9");

	RETURN;
}

static int test_charbit_alloc(void)
{
	addr pos;
	size_t size;

	charbit_alloc(NULL, &pos, 12);
	test(GetType(pos) == LISPSYSTEM_CHARBIT, "charbit_alloc1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAYBODY, "charbit_alloc2");
	test(GetLenArrayAB(pos) == 1, "charbit_alloc3");
	size = IdxSize + 12*sizeoft(unicode);
	test(GetLenBodyAB(pos) == size, "charbit_alloc4");
	test(RefBodyAB(pos, size_t) == 0, "charbit_alloc5");

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
	test(size == 1, "charbit_push1");
	GetCharBitChar(pos, 0, &u);
	test(u == 100, "charbit_push2");

	charbit_push(pos, 200);
	GetCharBitSize(pos, &size);
	test(size == 2, "charbit_push3");
	GetCharBitChar(pos, 1, &u);
	test(u == 200, "charbit_push4");

	RETURN;
}

static int test_charqueue_alloc(void)
{
	addr pos, root, tail;
	size_t size;

	charqueue_alloc(NULL, &pos, 14);
	test(GetType(pos) == LISPSYSTEM_CHARQUEUE, "charqueue_alloc1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "charqueue_alloc2");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "charqueue_alloc3");
	GetCharBitSize(root, &size);
	test(size == 0, "charqueue_alloc4");
	size = GetLenBodyAB(root);
	test(size == IdxSize + sizeoft(unicode) * 14, "charqueue_alloc5");

	size = 100;
	GetCharQueueSize(pos, &size);
	test(size == 0, "charqueue_alloc6");
	GetCharQueueMax(pos, &size);
	test(size == 14, "charqueue_alloc7");

	SetCharQueueSize(pos, 100);
	GetCharQueueSize(pos, &size);
	test(size == 100, "charqueue_alloc8");
	IncCharQueueSize(pos);
	GetCharQueueSize(pos, &size);
	test(size == 101, "charqueue_alloc9");
	SetCharQueueMax(pos, 200);
	GetCharQueueMax(pos, &size);
	test(size == 200, "charqueue_alloc10");

	SetCharQueueRoot(pos, T);
	GetCharQueueRoot(pos, &root);
	test(root == T, "charqueue_alloc11");
	SetCharQueueTail(pos, Nil);
	GetCharQueueTail(pos, &root);
	test(root == Nil, "charqueue_alloc12");

	RETURN;
}

static int test_refsize_charqueue(void)
{
	addr pos;

	charqueue_alloc(NULL, &pos, 0);
	test(refsize_charqueue(pos) == 0, "refsize_charqueue1");
	IncCharQueueSize(pos);
	test(refsize_charqueue(pos) == 1, "refsize_charqueue2");

	RETURN;
}

static int test_getsize_charqueue(void)
{
	addr pos;
	size_t size;

	charqueue_alloc(NULL, &pos, 0);
	getsize_charqueue(pos, &size);
	test(size == 0, "getsize_charqueue1");
	IncCharQueueSize(pos);
	getsize_charqueue(pos, &size);
	test(size == 1, "getsize_charqueue2");

	RETURN;
}

static int test_refchar_charqueue(void)
{
	addr pos;

	charqueue_alloc(NULL, &pos, 3);
	test(refchar_charqueue(pos, 0) == 0, "refchar_charqueue1");
	push_charqueue_heap(pos, 10);
	push_charqueue_heap(pos, 20);
	test(refchar_charqueue(pos, 0) == 10, "refchar_charqueue1");

	RETURN;
}

static int test_getchar_charqueue(void)
{
	int check;
	addr pos;
	unicode i, u;

	charqueue_alloc(NULL, &pos, 3);
	getchar_charqueue(pos, 0, &u);
	test(u == 0, "getchar_charqueue1");

	push_charqueue_heap(pos, 10);
	push_charqueue_heap(pos, 20);
	getchar_charqueue(pos, 0, &u);
	test(u == 10, "getchar_charqueue1");
	getchar_charqueue(pos, 1, &u);
	test(u == 20, "getchar_charqueue2");
	getchar_charqueue(pos, 2, &u);
	test(u == 0, "getchar_charqueue3");

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
	test(check, "getchar_charqueue4");

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
	test(size == 1, "push_charqueue1");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 1, "push_charqueue2");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "push_charqueue3");

	push_charqueue_heap(pos, 20);
	GetCharQueueSize(pos, &size);
	test(size == 2, "push_charqueue4");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 2, "push_charqueue5");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "push_charqueue6");
	GetCharBitChar(tail, 1, &u);
	test(u == 20, "push_charqueue7");

	push_charqueue_heap(pos, 30);
	push_charqueue_heap(pos, 40);
	push_charqueue_heap(pos, 50);
	GetCharQueueSize(pos, &size);
	test(size == 5, "push_charqueue8");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "push_charqueue9");
	GetCharBitSize(tail, &size);
	test(size == 5, "push_charqueue10");
	GetCharBitChar(tail, 4, &u);
	test(u == 50, "push_charqueue11");

	push_charqueue_heap(pos, 60);
	GetCharQueueSize(pos, &size);
	test(size == 6, "push_charqueue12");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root != tail, "push_charqueue13");
	GetCharBitSize(root, &size);
	test(size == 5, "push_charqueue14");
	GetCharBitSize(tail, &size);
	test(size == 1, "push_charqueue15");
	GetCharBitChar(tail, 0, &u);
	test(u == 60, "push_charqueue16");

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
	test(size == 100, "make_charqueue1");
	check = 1;
	for (i = 0; i < 100; i++) {
		string_getc(str, i, &u);
		if (u != ((i + 1) * 10)) {
			check = 0;
			break;
		}
	}
	test(check, "make_charqueue2");

	/* string1 */
	charqueue_alloc(NULL, &pos, 22);
	for (i = 1; i <= 100; i++)
		push_charqueue_heap(pos, i * 2);
	make_charqueue_heap(pos, &str);
	string_length(str, &size);
	test(size == 100, "make_charqueue3");
	check = 1;
	for (i = 0; i < 100; i++) {
		string_getc(str, i, &u);
		if (u != ((i + 1) * 2)) {
			check = 0;
			break;
		}
	}
	test(check, "make_charqueue4");

	/* empty */
	charqueue_alloc(NULL, &pos, 11);
	make_charqueue_heap(pos, &str);
	string_length(str, &size);
	test(size == 0, "make_charqueue5");

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
	test(size == 0, "clear_charqueue1");
	GetCharQueueMax(pos, &size);
	test(size == 5, "clear_charqueue2");

	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "clear_charqueue4");
	GetCharBitSize(root, &size);
	test(size == 0, "clear_charqueue5");
	GetCharBitNext(root, &tail);
	test(tail != Nil, "clear_charqueue6");

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
	test(size == 0, "free_charqueue1");
	GetCharQueueMax(pos, &size);
	test(size == 5, "free_charqueue2");

	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "free_charqueue4");
	GetCharBitSize(root, &size);
	test(size == 0, "free_charqueue5");
	GetCharBitNext(root, &tail);
	test(tail == Nil, "free_charqueue6");

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
	test(size == 1, "reuse_charqueue1");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 1, "reuse_charqueue2");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "reuse_charqueue3");

	push_charqueue_heap(pos, 20);
	GetCharQueueSize(pos, &size);
	test(size == 2, "reuse_charqueue4");
	GetCharQueueTail(pos, &tail);
	GetCharBitSize(tail, &size);
	test(size == 2, "reuse_charqueue5");
	GetCharBitChar(tail, 0, &u);
	test(u == 10, "reuse_charqueue6");
	GetCharBitChar(tail, 1, &u);
	test(u == 20, "reuse_charqueue7");

	push_charqueue_heap(pos, 30);
	push_charqueue_heap(pos, 40);
	push_charqueue_heap(pos, 50);
	GetCharQueueSize(pos, &size);
	test(size == 5, "reuse_charqueue8");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root == tail, "reuse_charqueue9");
	GetCharBitSize(tail, &size);
	test(size == 5, "reuse_charqueue10");
	GetCharBitChar(tail, 4, &u);
	test(u == 50, "reuse_charqueue11");

	push_charqueue_heap(pos, 60);
	GetCharQueueSize(pos, &size);
	test(size == 6, "reuse_charqueue12");
	GetCharQueueRoot(pos, &root);
	GetCharQueueTail(pos, &tail);
	test(root != tail, "reuse_charqueue13");
	GetCharBitSize(root, &size);
	test(size == 5, "reuse_charqueue14");
	GetCharBitSize(tail, &size);
	test(size == 1, "reuse_charqueue15");
	GetCharBitChar(tail, 0, &u);
	test(u == 60, "reuse_charqueue16");

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
	test(string_equal_char(str, "abc1234567"), "pushstring_charqueue1");
	clear_charqueue(pos);

	pushchar_charqueue_heap(pos, "abc");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abc"), "pushstring_charqueue2");

	pushchar_charqueue_heap(pos, "de");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcde"), "pushstring_charqueue3");

	pushchar_charqueue_heap(pos, "f");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcdef"), "pushstring_charqueue4");

	pushchar_charqueue_heap(pos, "gh");
	make_charqueue_heap(pos, &str);
	test(string_equal_char(str, "abcdefgh"), "pushstring_charqueue5");

	RETURN;
}

static int test_charqueue_heap(void)
{
	int i, c;
	addr pos, str, check;
	size_t size;
	unicode *u;
	char buffer[CHARQUEUESIZE * 2];

	charqueue_heap(&pos, 0);
	push_charqueue_heap(pos, 'a');
	push_charqueue_heap(pos, 'b');
	push_charqueue_heap(pos, 'c');
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue1");
	test(string_equal_char(str, "abc"), "charqueue2");
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue3");
	test(string_equal_char(str, "abc"), "charqueue4");
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue5");
	strvect_char_heap(&check, "abc");
	test(string_equal(str, check), "charqueue6");

	clear_charqueue(pos);
	for (i = 0; i < CHARQUEUESIZE * 2 - 1; i++) {
		c = 'A' + (i % 20);
		buffer[i] = c;
		push_charqueue_heap(pos, c);
	}
	buffer[CHARQUEUESIZE * 2 - 1] = 0;
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue7");
	test(string_equal_char(str, buffer), "charqueue8");

	clear_charqueue(pos);
	push_charqueue_heap(pos, 1000);
	push_charqueue_heap(pos, 2000);
	push_charqueue_heap(pos, 3000);
	make_charqueue_heap(pos, &str);
	test(GetType(str) == LISPTYPE_STRING, "charqueue9");
	string_length(str, &size);
	test(size == 3, "charqueue10");
	GetStringUnicode(str, &u);
	test(u[0] == 1000, "charqueue11");
	test(u[1] == 2000, "charqueue12");
	test(u[2] == 3000, "charqueue13");

	RETURN;
}

static int testbreak_charqueue(void)
{
	TestBreak(test_charbit_macro);
	TestBreak(test_charbit_alloc);
	TestBreak(test_charbit_push);
	TestBreak(test_charqueue_alloc);
	TestBreak(test_refsize_charqueue);
	TestBreak(test_getsize_charqueue);
	TestBreak(test_refchar_charqueue);
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

int test_charqueue(void)
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
		lisp_initialize = 1;
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		result = testbreak_charqueue();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

