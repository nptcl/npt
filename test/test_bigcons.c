#include "bigcons.c"
#include "constant.h"
#include "degrade.h"

/*
 *  Bigcons
 */
struct bbuffer_struct {
	addr next;
	size_t count;
	bigtype *buffer;
};
static struct bbuffer_struct bbuffer(addr pos)
{
	struct bbuffer_struct result;
	struct bigbuffer *ptr;

	GetNextBigbuffer(pos, &result.next);
	ptr = StructBigbuffer(pos);
	result.count = ptr->count;
	result.buffer = ptr->buffer;

	return result;
}

static int test_bigbuffer_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigbuffer_local(local, &pos, 100);
	test(GetType(pos) == LISPTYPE_SYSTEM, "bigbuffer1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAYBODY, "bigbuffer2");
	test(bbuffer(pos).count == 1, "bigbuffer3");
	test(bbuffer(pos).buffer[0] == 100, "bigbuffer4");

	test(bbuffer(pos).next == Nil, "bigbuffer5");
	SetNextBigbuffer(pos, T);
	test(bbuffer(pos).next == T, "bigbuffer6");

	rollback_local(local, stack);

	RETURN;
}

struct bcons_struct {
	size_t count, bcount;
	addr root, bnext;
	bigtype *bbuffer;
};
static struct bcons_struct bcons(addr pos)
{
	addr root;
	struct bigbuffer *ptr;
	struct bcons_struct result;

	GetRootBigcons(pos, &root);
	GetCountBigcons(pos, &result.count);
	result.root = root;
	GetNextBigbuffer(root, &result.bnext);
	ptr = StructBigbuffer(root);
	result.bcount = ptr->count;
	result.bbuffer = ptr->buffer;

	return result;
}

static int test_bigcons_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	test(GetType(pos) == LISPTYPE_SYSTEM, "bigcons1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "bigcons2");
	GetCountBigcons(pos, &size);
	test(size == 1, "bigcons3");
	IncCountBigcons(pos, 100);
	GetCountBigcons(pos, &size);
	test(size == 101, "bigcons3");
	SetCountBigcons(pos, 1);
	GetCountBigcons(pos, &size);
	test(size == 1, "bigcons4");

	GetRootBigcons(pos, &check);
	test(check != Nil, "bigcons5");
	test(StructBigbuffer(check)->count == 1, "bigcons6");
	test(StructBigbuffer(check)->buffer[0] == 0, "bigcons7");
	SetRootBigcons(pos, T);
	GetRootBigcons(pos, &check);
	test(check == T, "bigcons8");

	rollback_local(local, stack);

	RETURN;
}

static int test_clear_bigcons(void)
{
	addr pos, next;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	SetCountBigcons(pos, 2);
	bcons(pos).bbuffer[0] = 100;
	bcons(pos).bbuffer[1] = 200;
	bcons(pos).bbuffer[2] = 300;
	clear_bigcons(pos);
	test(bcons(pos).count == 1, "clear_bigcons1");
	test(bcons(pos).bbuffer[0] == 0, "clear_bigcons2");

	bigbuffer_local(local, &next, 999);
	SetNextBigbuffer(bcons(pos).root, next);
	test(bbuffer(next).count == 1, "clear_bigcons3");
	clear_bigcons(pos);
	test(bbuffer(next).count == 0, "clear_bigcons4");

	rollback_local(local, stack);

	RETURN;
}

static addr cons_root(addr pos)
{
	GetRootBigcons(pos, &pos);
	return pos;
}
static size_t cons_count(addr pos)
{
	size_t size;
	GetCountBigcons(pos, &size);
	return size;
}
static void set_cons_count(addr pos, size_t count)
{
	SetCountBigcons(pos, count);
}
static size_t cons_root_count(addr pos)
{
	return StructBigbuffer(cons_root(pos))->count;
}
static void set_cons_root_count(addr pos, size_t count)
{
	StructBigbuffer(cons_root(pos))->count = count;
}
static addr cons_root_next(addr pos)
{
	GetNextBigbuffer(cons_root(pos), &pos);
	return pos;
}
static bigtype *cons_root_buffer(addr pos)
{
	return StructBigbuffer(cons_root(pos))->buffer;
}

static size_t cons_root_next_count(addr pos)
{
	return StructBigbuffer(cons_root_next(pos))->count;
}
static void set_cons_root_next_count(addr pos, size_t count)
{
	StructBigbuffer(cons_root_next(pos))->count = count;
}
static bigtype *cons_root_next_buffer(addr pos)
{
	return StructBigbuffer(cons_root_next(pos))->buffer;
}
static addr cons_root_next_next(addr pos)
{
	GetNextBigbuffer(cons_root_next(pos), &pos);
	return pos;
}
static void alloc_cons_root_next_next(LocalRoot local, addr pos)
{
	addr next;
	bigbuffer_local(local, &next, 0);
	SetNextBigbuffer(cons_root_next(pos), next);
}
static size_t cons_root_next_next_count(addr pos)
{
	return StructBigbuffer(cons_root_next_next(pos))->count;
}

static int test_carrynext(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	test(cons_root(pos) != Nil, "carrynext1");
	test(cons_root_count(pos) == 1, "carrynext2");
	test(cons_root_next(pos) == Nil, "carrynext3");

	carrynext(local, pos, cons_root(pos), cons_root_count(pos), 100);
	test(cons_root_count(pos) == 2, "carrynext4");
	test(cons_count(pos) == 2, "carrynext5");
	test(cons_root_buffer(pos)[0] == 0, "carrynext6");
	test(cons_root_buffer(pos)[1] == 100, "carrynext7");
	test(cons_root_next(pos) == Nil, "carrynext8");

	set_cons_root_count(pos, BIGCONS_SIZE);
	set_cons_count(pos, BIGCONS_SIZE);
	cons_root_buffer(pos)[BIGCONS_SIZE - 1] = 99;
	carrynext(local, pos, cons_root(pos), cons_root_count(pos), 200);
	test(cons_root_count(pos) == BIGCONS_SIZE, "carrynext9");
	test(cons_count(pos) == BIGCONS_SIZE + 1, "carrynext10");
	test(cons_root_buffer(pos)[BIGCONS_SIZE - 1] == 99, "carrynext11");
	test(cons_root_next(pos) != Nil, "carrynext12");
	test(cons_root_next_count(pos) == 1, "carrynext13");
	test(cons_root_next_buffer(pos)[0] == 200, "carrynext14");
	test(cons_root_next_next(pos) == Nil, "carrynext15");

	set_cons_root_count(pos, BIGCONS_SIZE);
	set_cons_count(pos, BIGCONS_SIZE);
	cons_root_buffer(pos)[BIGCONS_SIZE - 1] = 99;
	test(cons_root_next(pos) != Nil, "carrynext16");
	cons_root_next_buffer(pos)[0] = 999;
	carrynext(local, pos, cons_root(pos), cons_root_count(pos), 300);
	test(cons_root_next(pos) != Nil, "carrynext17");
	test(cons_root_next_buffer(pos)[0] == 300, "carrynext18");
	test(cons_root_next_next(pos) == Nil, "carrynext19");

	alloc_cons_root_next_next(local, pos);
	set_cons_root_count(pos, BIGCONS_SIZE);
	set_cons_count(pos, BIGCONS_SIZE);
	cons_root_buffer(pos)[BIGCONS_SIZE - 1] = 99;
	test(cons_root_next(pos) != Nil, "carrynext20");
	cons_root_next_buffer(pos)[0] = 999;
	test(cons_root_next_next_count(pos) == 1, "carrynext21");
	carrynext(local, pos, cons_root(pos), cons_root_count(pos), 300);
	test(cons_root_next(pos) != Nil, "carrynext22");
	test(cons_root_next_buffer(pos)[0] == 300, "carrynext23");
	test(cons_root_next_next(pos) != Nil, "carrynext24");
	test(cons_root_next_next_count(pos) == 0, "carrynext25");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bigcons(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	size_t i;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	cons_root_buffer(pos)[0] = 10;
	plus_bigcons(local, pos, 22);
	test(cons_root_buffer(pos)[0] == 32, "plus_bigcons1");

	cons_root_buffer(pos)[0] = BIGNUM_FULL;
	plus_bigcons(local, pos, 0);
	test(cons_root_buffer(pos)[0] == BIGNUM_FULL, "plus_bigcons2");
	plus_bigcons(local, pos, 3);
	test(cons_count(pos) == 2, "plus_bigcons3");
	test(cons_root_count(pos) == 2, "plus_bigcons4");
	test(cons_root_buffer(pos)[0] == 2, "plus_bigcons5");
	test(cons_root_buffer(pos)[1] == 1, "plus_bigcons6");

	for (i = 0; i < BIGCONS_SIZE; i++)
		cons_root_buffer(pos)[i] = BIGNUM_FULL;
	test(cons_root_next(pos) == Nil, "plus_bigcons7");
	set_cons_count(pos, BIGCONS_SIZE);
	set_cons_root_count(pos, BIGCONS_SIZE);
	plus_bigcons(local, pos, 4);
	test(cons_count(pos) == BIGCONS_SIZE + 1, "plus_bigcons8");
	test(cons_root_count(pos) == BIGCONS_SIZE, "plus_bigcons9");
	test(cons_root_next(pos) != Nil, "plus_bigcons10");
	test(cons_root_next_count(pos) == 1, "plus_bigcons11");
	test(cons_root_next_buffer(pos)[0] == 1, "plus_bigcons12");
	test(cons_root_buffer(pos)[0] == 3, "plus_bigcons13");

	for (i = 0; i < BIGCONS_SIZE; i++)
		cons_root_buffer(pos)[i] = BIGNUM_FULL;
	test(cons_root_next(pos) != Nil, "plus_bigcons14");
	set_cons_count(pos, BIGCONS_SIZE);
	set_cons_root_count(pos, BIGCONS_SIZE);
	set_cons_root_next_count(pos, 0);
	plus_bigcons(local, pos, 5);
	test(cons_count(pos) == BIGCONS_SIZE + 1, "plus_bigcons15");
	test(cons_root_count(pos) == BIGCONS_SIZE, "plus_bigcons16");
	test(cons_root_next(pos) != Nil, "plus_bigcons17");
	test(cons_root_next_count(pos) == 1, "plus_bigcons18");
	test(cons_root_next_buffer(pos)[0] == 1, "plus_bigcons19");
	test(cons_root_buffer(pos)[0] == 4, "plus_bigcons20");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bigcons(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	size_t i;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	multi_bigcons(local, pos, 100);
	test(cons_root_buffer(pos)[0] == 0, "multi_bigcons1");
	plus_bigcons(local, pos, 12);
	multi_bigcons(local, pos, 3);
	test(cons_root_buffer(pos)[0] == 36, "multi_bigcons2");

	cons_root_buffer(pos)[0] = BIGNUM_FULL;
	cons_root_buffer(pos)[1] = 0x0A;
	set_cons_count(pos, 2);
	set_cons_root_count(pos, 2);
	multi_bigcons(local, pos, 0x10);
	test(cons_root_buffer(pos)[1] == 0xAF, "multi_bigcons3");

	for (i = 0; i < BIGCONS_SIZE; i++)
		cons_root_buffer(pos)[i] = BIGNUM_FULL;
	set_cons_count(pos, BIGCONS_SIZE);
	set_cons_root_count(pos, BIGCONS_SIZE);
	test(cons_root_next(pos) == Nil, "multi_bigcons4");
	multi_bigcons(local, pos, 0x10);
	test(cons_root_next(pos) != Nil, "multi_bigcons5");
	test(cons_root_next_buffer(pos)[0] == 0x0F, "multi_bigcons6");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_bigcons(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	push_bigcons(local, pos, 10, 5);
	test(cons_root_buffer(pos)[0] == 5, "push_bigcons1");
	test(cons_root_count(pos) == 1, "push_bigcons2");
	push_bigcons(local, pos, 10, 1);
	test(cons_root_buffer(pos)[0] == 51, "push_bigcons3");

	rollback_local(local, stack);

	RETURN;
}

static int test_getnumber(void)
{
	unsigned ret;

	test(getnumber(10, '0', &ret) == 0, "getnumber1");
	test(ret == 0, "getnumber1ret");
	test(getnumber(10, '9', &ret) == 0, "getnumber2");
	test(ret == 9, "getnumber2ret");
	test(getnumber(8, '7', &ret) == 0, "getnumber3");
	test(ret == 7, "getnumber3ret");
	test(getnumber(8, '8', &ret) != 0, "getnumber4");
	test(getnumber(13, 'a', &ret) == 0, "getnumber5");
	test(ret == 0x0a, "getnumber5ret");
	test(getnumber(13, 'A', &ret) == 0, "getnumber6");
	test(ret == 0x0a, "getnumber6ret");
	test(getnumber(13, 'c', &ret) == 0, "getnumber7");
	test(ret == 0x0c, "getnumber7ret");
	test(getnumber(13, 'C', &ret) == 0, "getnumber8");
	test(ret == 0x0c, "getnumber8ret");
	test(getnumber(13, 'd', &ret) != 0, "getnumber9");
	test(getnumber(13, 'D', &ret) != 0, "getnumber10");
	test(getnumber(13, '*', &ret) != 0, "getnumber11");
	test(getnumber(16, 'f', &ret) == 0, "getnumber12");
	test(ret == 0x0f, "getnumber12ret");
	test(getnumber(16, 'F', &ret) == 0, "getnumber13");
	test(ret == 0x0f, "getnumber13ret");

	RETURN;
}

static int test_setchar_bigcons(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	const char *str1 = "129876545123678900001";
	const char *str2 = "9129876545123678900001";
	const char *str3 = "9129876abcdef54ABCDEF5123678900001";
	const char *str4 = "5";

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &pos);
	setchar_bigcons(local, pos, 10, str1);
	setchar_bigcons(local, pos, 10, str2);
	setchar_bigcons(local, pos, 16, str3);
	setchar_bigcons(local, pos, 10, str4);

	test(cons_count(pos) == 1, "setchar_bigcons1");
	test(cons_root_buffer(pos)[0] == 5, "setchar_bigcons2");
	test(cons_root_count(pos) == 1, "setchar_bigcons3");
	if (cons_root_next(pos) != Nil)
		test(cons_root_next_count(pos) == 0, "setchar_bigcons4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bigcons_char_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_char_local(local, &pos, 10, "11");
	test(cons_count(pos) == 1, "bigcons_char_local1");
	test(cons_root_buffer(pos)[0] == 11, "bigcons_char_local2");

	bigcons_char_local(local, &pos, 16, "1234567890abcdef01234567890ABCDEF");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_bigcons(void)
{
	/* Bigcons */
	TestBreak(test_bigbuffer_local);
	TestBreak(test_bigcons_local);
	TestBreak(test_clear_bigcons);
	TestBreak(test_carrynext);
	TestBreak(test_plus_bigcons);
	TestBreak(test_multi_bigcons);
	TestBreak(test_push_bigcons);
	TestBreak(test_getnumber);
	TestBreak(test_setchar_bigcons);
	TestBreak(test_bigcons_char_local);

	return 0;
}

int test_bigcons(void)
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
		lisp_init = 1;
		result = testbreak_bigcons();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

