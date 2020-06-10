#include "bigdata.c"
#include "bigcons.h"
#include "constant.h"
#include "degrade.h"


/*
 *  Calculate
 */
static int test_multinumber1(void)
{
	bigtype a, b;

	a = 10; b = 0;
	multinumber(&a, &b);
	test(a == 0, "multinumber1-1");
	test(b == 0, "multinumber1-2");

	a = 0; b = 20;
	multinumber(&a, &b);
	test(a == 0, "multinumber1-3");
	test(b == 0, "multinumber1-4");

	a = 11; b = 1;
	multinumber(&a, &b);
	test(a == 11, "multinumber1-5");
	test(b == 0,  "multinumber1-6");

	a = 1; b = 22;
	multinumber(&a, &b);
	test(a == 22, "multinumber1-7");
	test(b == 0,  "multinumber1-8");

	RETURN;
}

static int test_multinumber2(void)
{
	bigtype a, b;

	a = 4; b = 8;
	multinumber(&a, &b);
	test(a == 32, "multinumber2-1");
	test(b == 0,  "multinumber2-2");

	a = 0xF; b = 0x10;
	multinumber(&a, &b);
	test(a == 0xF0, "multinumber2-3");
	test(b == 0x00, "multinumber2-4");

	a = 0xFF; b = 0x10;
	multinumber(&a, &b);
#if defined BIGNUM_CODE_64BIT
	test(a == 0x0FF0, "multinumber2-5");
	test(b == 0x0000, "multinumber2-6");
#elif defined BIGNUM_CODE_32BIT
	test(a == 0x0FF0, "multinumber2-5");
	test(b == 0x0000, "multinumber2-6");
#else
	test(a == 0xF0, "multinumber2-5");
	test(b == 0x0F, "multinumber2-6");
#endif

	a = 0x0FFFFFFF; b = 0x10;
	multinumber(&a, &b);
#if defined BIGNUM_CODE_64BIT
	test(a == 0xFFFFFFF0, "multinumber2-7");
	test(b == 0x00000000, "multinumber2-8");
#elif defined BIGNUM_CODE_32BIT
	test(a == 0xFFFFFFF0, "multinumber2-7");
	test(b == 0x00000000, "multinumber2-8");
#endif

	a = 0xFFFFFFBB; b = 0x10;
	multinumber(&a, &b);
#if defined BIGNUM_CODE_64BIT
	test(a == 0x0FFFFFFBB0, "multinumber2-7");
	test(b == 0x0000000000, "multinumber2-8");
#elif defined BIGNUM_CODE_32BIT
	test(a == 0xFFFFFBB0, "multinumber2-7");
	test(b == 0x0000000F, "multinumber2-8");
#endif

	a = 0xFFFFFFBB; b = 0x100000;
	multinumber(&a, &b);
#if defined BIGNUM_CODE_64BIT
	test(a == 0x0FFFFFFBB00000L, "multinumber2-9");
	test(b == 0x00000000000000L, "multinumber2-10");
#elif defined BIGNUM_CODE_32BIT
	test(a == 0xFBB00000, "multinumber2-9");
	test(b == 0x000FFFFF, "multinumber2-10");
#endif

#ifdef BIGNUM_CODE_64BIT
	a = 0xFFFFFFAAFFFFFFBBL; b = 0x10;
	multinumber(&a, &b);
	test(a == 0xFFFFFAAFFFFFFBB0L, "multinumber2-11");
	test(b == 0x000000000000000FL, "multinumber2-12");

	a = 0xFFFFFFAAFFFFFFBBL; b = 0x100000;
	multinumber(&a, &b);
	test(a == 0xFAAFFFFFFBB00000L, "multinumber2-13");
	test(b == 0x00000000000FFFFFL, "multinumber2-14");
#endif

	RETURN;
}

static int test_plusnumber(void)
{
	bigtype a, b;

	a = 10; b = 20;
	plusnumber(&a, &b);
	test(a == 30, "plusnumber1");
	test(b == 0, "plusnumber2");

#if defined BIGNUM_CODE_64BIT
	a = 0xFFFFFFFFFFFFFFFEL; b = 1;
	plusnumber(&a, &b);
	test(a == 0xFFFFFFFFFFFFFFFFL, "plusnumber2");
	test(b == 0, "plusnumber3");

	a = 0xFFFFFFFFFFFFFFFEL; b = 2;
	plusnumber(&a, &b);
	test(a == 0x00, "plusnumber4");
	test(b == 0x01, "plusnumber5");

	a = 0x8000000000000000L; b = 0x8000000000000001L;
	plusnumber(&a, &b);
	test(a == 0x01, "plusnumber6");
	test(b == 0x01, "plusnumber7");

	a = 0xFFFFFFFFFFFFFFFFL; b = 0xFFFFFFFFFFFFFFFFL;
	plusnumber(&a, &b);
	test(a == 0xFFFFFFFFFFFFFFFEL, "plusnumber8");
	test(b == 0x01, "plusnumber9");

#elif defined BIGNUM_CODE_32BIT
	a = 0xFFFFFFFE; b = 1;
	plusnumber(&a, &b);
	test(a == 0xFFFFFFFF, "plusnumber2");
	test(b == 0, "plusnumber3");

	a = 0xFFFFFFFE; b = 2;
	plusnumber(&a, &b);
	test(a == 0x00, "plusnumber4");
	test(b == 0x01, "plusnumber5");

	a = 0x80000000; b = 0x80000001;
	plusnumber(&a, &b);
	test(a == 0x01, "plusnumber6");
	test(b == 0x01, "plusnumber7");

	a = 0xFFFFFFFF; b = 0xFFFFFFFF;
	plusnumber(&a, &b);
	test(a == 0xFFFFFFFE, "plusnumber8");
	test(b == 0x01, "plusnumber9");

#else
	a = 0xFE; b = 1;
	plusnumber(&a, &b);
	test(a == 0xFF, "plusnumber2");
	test(b == 0, "plusnumber3");

	a = 0xFE; b = 2;
	plusnumber(&a, &b);
	test(a == 0x00, "plusnumber4");
	test(b == 0x01, "plusnumber5");

	a = 0x80; b = 0x81;
	plusnumber(&a, &b);
	test(a == 0x01, "plusnumber6");
	test(b == 0x01, "plusnumber7");

	a = 0xFF; b = 0xFF;
	plusnumber(&a, &b);
	test(a == 0xFE, "plusnumber8");
	test(b == 0x01, "plusnumber9");
#endif

	RETURN;
}

static int test_plusnumber3(void)
{
	bigtype a, b, r;

	r = 0xFF; a = 10; b = 20;
	plusnumber3(&r, a, &b);
	test(r == 30, "plusnumber3-1");
	test(b == 0, "plusnumber3-2");

#if defined BIGNUM_CODE_64BIT
	r = 0xAA; a = 0xFFFFFFFFFFFFFFFEL; b = 1;
	plusnumber3(&r, a, &b);
	test(r == 0xFFFFFFFFFFFFFFFFL, "plusnumber3-2");
	test(b == 0, "plusnumber3-3");

	r = 0xAA; a = 0xFFFFFFFFFFFFFFFEL; b = 2;
	plusnumber3(&r, a, &b);
	test(r == 0x00, "plusnumber3-4");
	test(b == 0x01, "plusnumber3-5");

	r = 0xAA; a = 0x8000000000000000L; b = 0x8000000000000001L;
	plusnumber3(&r, a, &b);
	test(r == 0x01, "plusnumber3-6");
	test(b == 0x01, "plusnumber3-7");

	r = 0xAA; a = 0xFFFFFFFFFFFFFFFFL; b = 0xFFFFFFFFFFFFFFFFL;
	plusnumber3(&r, a, &b);
	test(r == 0xFFFFFFFFFFFFFFFEL, "plusnumber3-8");
	test(b == 0x01, "plusnumber3-9");

#elif defined BIGNUM_CODE_32BIT
	r = 0xAA; a = 0xFFFFFFFE; b = 1;
	plusnumber3(&r, a, &b);
	test(r == 0xFFFFFFFF, "plusnumber3-2");
	test(b == 0, "plusnumber3-3");

	r = 0xAA; a = 0xFFFFFFFE; b = 2;
	plusnumber3(&r, a, &b);
	test(r == 0x00, "plusnumber3-4");
	test(b == 0x01, "plusnumber3-5");

	r = 0xAA; a = 0x80000000; b = 0x80000001;
	plusnumber3(&r, a, &b);
	test(r == 0x01, "plusnumber3-6");
	test(b == 0x01, "plusnumber3-7");

	r = 0xAA; a = 0xFFFFFFFF; b = 0xFFFFFFFF;
	plusnumber3(&r, a, &b);
	test(r == 0xFFFFFFFE, "plusnumber3-8");
	test(b == 0x01, "plusnumber3-9");

#else
	r = 0xAA; a = 0xFE; b = 1;
	plusnumber3(&r, a, &b);
	test(r == 0xFF, "plusnumber3-2");
	test(b == 0, "plusnumber3-3");

	r = 0xAA; a = 0xFE; b = 2;
	plusnumber3(&r, a, &b);
	test(r == 0x00, "plusnumber3-4");
	test(b == 0x01, "plusnumber3-5");

	r = 0xAA; a = 0x80; b = 0x81;
	plusnumber3(&r, a, &b);
	test(r == 0x01, "plusnumber3-6");
	test(b == 0x01, "plusnumber3-7");

	r = 0xAA; a = 0xFF; b = 0xFF;
	plusnumber3(&r, a, &b);
	test(r == 0xFE, "plusnumber3-8");
	test(b == 0x01, "plusnumber3-9");
#endif

	RETURN;
}

static int test_pluscarry(void)
{
	bigtype a, b, c;

	a = 10, b = 20, c = 0;
	pluscarry(&a, b, &c);
	test(a == 30, "pluscarry1");
	test(c == 0, "pluscarry2");

#if defined BIGNUM_CODE_64BIT
	a = 0x8000000000000000L;
	b = 0x7FFFFFFFFFFFFFFFL;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0xFFFFFFFFFFFFFFFFL, "pluscarry3");
	test(c == 0, "pluscarry4");

	a = 0x8000000000000000L;
	b = 0x8000000000000000L;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry5");
	test(c == 0x01, "pluscarry6");

	a = 0x8000000000000000L;
	b = 0x7FFFFFFFFFFFFFFFL;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry7");
	test(c == 0x01, "pluscarry8");

	a = 0x8000000000000000L;
	b = 0x8000000000000000L;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x01, "pluscarry9");
	test(c == 0x01, "pluscarry10");

#elif defined BIGNUM_CODE_32BIT
	a = 0x80000000;
	b = 0x7FFFFFFF;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0xFFFFFFFF, "pluscarry3");
	test(c == 0, "pluscarry4");

	a = 0x80000000;
	b = 0x80000000;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry5");
	test(c == 0x01, "pluscarry6");

	a = 0x80000000;
	b = 0x7FFFFFFF;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry7");
	test(c == 0x01, "pluscarry8");

	a = 0x80000000;
	b = 0x80000000;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x01, "pluscarry9");
	test(c == 0x01, "pluscarry10");

#else
	a = 0x80;
	b = 0x7F;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0xFF, "pluscarry3");
	test(c == 0, "pluscarry4");

	a = 0x80;
	b = 0x80;
	c = 0;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry5");
	test(c == 0x01, "pluscarry6");

	a = 0x80;
	b = 0x7F;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x00, "pluscarry7");
	test(c == 0x01, "pluscarry8");

	a = 0x80;
	b = 0x80;
	c = 1;
	pluscarry(&a, b, &c);
	test(a == 0x01, "pluscarry9");
	test(c == 0x01, "pluscarry10");
#endif

	RETURN;
}

static int test_pluscarry4(void)
{
	bigtype r, a, b, c;

	r = 0xAA;
	a = 10, b = 20, c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 30, "pluscarry4-1");
	test(c == 0, "pluscarry4-2");

#if defined BIGNUM_CODE_64BIT
	r = 0xAA;
	a = 0x8000000000000000L;
	b = 0x7FFFFFFFFFFFFFFFL;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0xFFFFFFFFFFFFFFFFL, "pluscarry4-3");
	test(c == 0, "pluscarry4-4");

	r = 0xAA;
	a = 0x8000000000000000L;
	b = 0x8000000000000000L;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-5");
	test(c == 0x01, "pluscarry4-6");

	r = 0xAA;
	a = 0x8000000000000000L;
	b = 0x7FFFFFFFFFFFFFFFL;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-7");
	test(c == 0x01, "pluscarry4-8");

	r = 0xAA;
	a = 0x8000000000000000L;
	b = 0x8000000000000000L;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x01, "pluscarry4-9");
	test(c == 0x01, "pluscarry4-10");

#elif defined BIGNUM_CODE_32BIT
	r = 0xAA;
	a = 0x80000000;
	b = 0x7FFFFFFF;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0xFFFFFFFF, "pluscarry4-3");
	test(c == 0, "pluscarry4-4");

	r = 0xAA;
	a = 0x80000000;
	b = 0x80000000;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-5");
	test(c == 0x01, "pluscarry4-6");

	r = 0xAA;
	a = 0x80000000;
	b = 0x7FFFFFFF;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-7");
	test(c == 0x01, "pluscarry4-8");

	r = 0xAA;
	a = 0x80000000;
	b = 0x80000000;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x01, "pluscarry4-9");
	test(c == 0x01, "pluscarry4-10");

#else
	r = 0xAA;
	a = 0x80;
	b = 0x7F;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0xFF, "pluscarry4-3");
	test(c == 0, "pluscarry4-4");

	r = 0xAA;
	a = 0x80;
	b = 0x80;
	c = 0;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-5");
	test(c == 0x01, "pluscarry4-6");

	r = 0xAA;
	a = 0x80;
	b = 0x7F;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x00, "pluscarry4-7");
	test(c == 0x01, "pluscarry4-8");

	r = 0xAA;
	a = 0x80;
	b = 0x80;
	c = 1;
	pluscarry4(&r, a, b, &c);
	test(r == 0x01, "pluscarry4-9");
	test(c == 0x01, "pluscarry4-10");
#endif

	RETURN;
}

static int test_multicarry(void)
{
	bigtype a, b, c;

	a = 0x0F; b = 0x02; c = 0;
	multicarry(&a, b, &c);
	test(a == 0x1E, "multicarry1");
	test(c == 0x00, "multicarry2");

	a = 0x0F; b = 0x02; c = 1;
	multicarry(&a, b, &c);
	test(a == 0x1F, "multicarry3");
	test(c == 0x00, "multicarry4");

#if defined BIGNUM_CODE_64BIT
	a = 0x88FFFFFFFFFFFFFFL; b = 0x10; c = 0;
	multicarry(&a, b, &c);
	test(a == 0x8FFFFFFFFFFFFFF0L, "multicarry5");
	test(c == 0x08, "multicarry6");

	a = 0x88FFFFFFFFFFFFFFL; b = 0x10; c = 1;
	multicarry(&a, b, &c);
	test(a == 0x8FFFFFFFFFFFFFF1L, "multicarry6");
	test(c == 0x08, "multicarry7");

	a = 0xFFFFFFFFFFFFFFFFL;
	b = 0xFFFFFFFFFFFFFFFFL;
	c = 0;
	multicarry(&a, b, &c);
	test(a == 0x0000000000000001L, "multicarry7");
	test(c == 0xFFFFFFFFFFFFFFFEL, "multicarry8");

	a = 0xFFFFFFFFFFFFFFFFL;
	b = 0xFFFFFFFFFFFFFFFFL;
	c = 0xFFFFFFFFFFFFFFFFL;
	multicarry(&a, b, &c);
	test(a == 0x0000000000000000L, "multicarry7");
	test(c == 0xFFFFFFFFFFFFFFFFL, "multicarry8");

#elif defined BIGNUM_CODE_32BIT
	a = 0x88FFFFFF; b = 0x10; c = 0;
	multicarry(&a, b, &c);
	test(a == 0x8FFFFFF0, "multicarry5");
	test(c == 0x08, "multicarry6");

	a = 0x88FFFFFF; b = 0x10; c = 1;
	multicarry(&a, b, &c);
	test(a == 0x8FFFFFF1, "multicarry6");
	test(c == 0x08, "multicarry7");

	a = 0xFFFFFFFF;
	b = 0xFFFFFFFF;
	c = 0;
	multicarry(&a, b, &c);
	test(a == 0x00000001, "multicarry7");
	test(c == 0xFFFFFFFE, "multicarry8");

	a = 0xFFFFFFFF;
	b = 0xFFFFFFFF;
	c = 0xFFFFFFFF;
	multicarry(&a, b, &c);
	test(a == 0x00000000, "multicarry7");
	test(c == 0xFFFFFFFF, "multicarry8");

#else
	a = 0x88; b = 0x10; c = 0;
	multicarry(&a, b, &c);
	test(a == 0x80, "multicarry5");
	test(c == 0x08, "multicarry6");

	a = 0x88; b = 0x10; c = 1;
	multicarry(&a, b, &c);
	test(a == 0x81, "multicarry6");
	test(c == 0x08, "multicarry7");

	a = 0xFF;
	b = 0xFF;
	c = 0;
	multicarry(&a, b, &c);
	test(a == 0x01, "multicarry7");
	test(c == 0xFE, "multicarry8");

	a = 0xFF;
	b = 0xFF;
	c = 0xFF;
	multicarry(&a, b, &c);
	test(a == 0x00, "multicarry7");
	test(c == 0xFF, "multicarry8");
#endif

	RETURN;
}

static int test_multicarry_bigdata(void)
{
	bigtype a, b, c;

	a = 0x0F; b = 0x02; c = 0;
	multicarry_bigdata(&a, b, &c);
	test(a == 0x1E, "multicarry_bigdata1");
	test(c == 0x00, "multicarry_bigdata2");

	a = 0x0F; b = 0x02; c = 1;
	multicarry_bigdata(&a, b, &c);
	test(a == 0x1F, "multicarry_bigdata3");
	test(c == 0x00, "multicarry_bigdata4");

	RETURN;
}

static int test_multicarry4(void)
{
	bigtype r, c;

	r = c = 10;
	multicarry4(&r, 20ULL, 30ULL, &c);
	test(r == 610ULL, "multicarry4-1");
	test(c == 0ULL, "multicarry4-2");

	RETURN;
}


/*
 *  compare
 */
static int test_equal_bigdata(void)
{
	int check;
	addr left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignPlus, 3);
	bignum_alloc(local, &right, SignMinus, 3);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 3);
	bigset(data2, 0xAA, 3);
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 3);
	check = equal_bigdata(left, right);
	test(check, "equal_bigdata1");

	SetSizeBignum(left, 2);
	check = equal_bigdata(left, right);
	test(!check, "equal_bigdata2");

	SetSizeBignum(left, 3);
	data2[1] = 10;
	check = equal_bigdata(left, right);
	test(!check, "equal_bigdata3");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bigdata(void)
{
	int check;
	addr left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignPlus, 3);
	bignum_alloc(local, &right, SignMinus, 3);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 3);
	bigset(data2, 0xAA, 3);
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 3);
	check = compare_bigdata(left, right);
	test(check == 0, "compare_bigdata1");

	SetSizeBignum(left, 2);
	check = compare_bigdata(left, right);
	test(check < 0, "compare_bigdata2");

	SetSizeBignum(right, 1);
	check = compare_bigdata(left, right);
	test(check > 0, "compare_bigdata3");

	SetSizeBignum(left, 3);
	SetSizeBignum(right, 3);

	data1[1] = 10;
	data2[1] = 20;
	check = compare_bigdata(left, right);
	test(check < 0, "compare_bigdata4");

	data1[1] = 40;
	data2[1] = 20;
	check = compare_bigdata(left, right);
	test(check > 0, "compare_bigdata5");

	SetSizeBignum(left, 2);
	SetSizeBignum(right, 2);
	data1[0] = 10;
	data2[0] = 20;
	data1[1] = 80;
	data2[1] = 70;
	check = compare_bigdata(left, right);
	test(check > 0, "compare_bigdata6");

	data1[0] = 20;
	data2[0] = 10;
	data1[1] = 70;
	data2[1] = 80;
	check = compare_bigdata(left, right);
	test(check < 0, "compare_bigdata7");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  plus / minus
 */
static int test_minusnumber(void)
{
	bigtype a, b;

	a = 10; b = 4;
	minusnumber(&a, &b);
	test(a == 6, "minusnumber1");
	test(b == 0, "minusnumber2");

	a = 10; b = 18;
	minusnumber(&a, &b);
	test(a == BIGNUM_FULL - 8 + 1, "minusnumber3");
	test(b == 1, "minusnumber4");

	a = 22; b = 22;
	minusnumber(&a, &b);
	test(a == 0, "minusnumber5");
	test(b == 0, "minusnumber6");

	RETURN;
}

static int test_minusnumber3(void)
{
	bigtype r, a, b;

	r = 0xAA; a = 10; b = 4;
	minusnumber3(&r, a, &b);
	test(r == 6, "minusnumber1");
	test(b == 0, "minusnumber2");

	r = 0xAA; a = 10; b = 18;
	minusnumber3(&r, a, &b);
	test(r == BIGNUM_FULL - 8 + 1, "minusnumber3");
	test(b == 1, "minusnumber4");

	r = 0xAA; a = 22; b = 22;
	minusnumber3(&r, a, &b);
	test(r == 0, "minusnumber5");
	test(b == 0, "minusnumber6");

	RETURN;
}

static int test_minuscarry(void)
{
	bigtype a, b, c;

	a = 33; b = 20; c = 0;
	minuscarry(&a, b, &c);
	test(a == 13, "minuscarry1");
	test(c == 0, "minuscarry2");

	a = 33; b = 20; c = 1;
	minuscarry(&a, b, &c);
	test(a == 12, "minuscarry3");
	test(c == 0, "minuscarry4");

	a = 33; b = 20; c = 2;
	minuscarry(&a, b, &c);
	test(a == 11, "minuscarry5");
	test(c == 0, "minuscarry6");

	a = 22; b = 24; c = 0;
	minuscarry(&a, b, &c);
	test(a == BIGNUM_FULL - 1, "minuscarry7");
	test(c == 1, "minuscarry6");

	a = 22; b = 24; c = 1;
	minuscarry(&a, b, &c);
	test(a == BIGNUM_FULL - 2, "minuscarry8");
	test(c == 1, "minuscarry9");

	a = 22; b = 22; c = 0;
	minuscarry(&a, b, &c);
	test(a == 0, "minuscarry10");
	test(c == 0, "minuscarry11");

	a = 22; b = 22; c = 1;
	minuscarry(&a, b, &c);
	test(a == BIGNUM_FULL, "minuscarry12");
	test(c == 1, "minuscarry13");

	a = 22; b = 20; c = 2;
	minuscarry(&a, b, &c);
	test(a == 0, "minuscarry14");
	test(c == 0, "minuscarry15");

	RETURN;
}

static int test_minuscarry4(void)
{
	bigtype r, a, b, c;

	r = 0xAA; a = 33; b = 20; c = 0;
	minuscarry4(&r, a, b, &c);
	test(r == 13, "minuscarry1");
	test(c == 0, "minuscarry2");

	r = 0xAA; a = 33; b = 20; c = 1;
	minuscarry4(&r, a, b, &c);
	test(r == 12, "minuscarry3");
	test(c == 0, "minuscarry4");

	r = 0xAA; a = 33; b = 20; c = 2;
	minuscarry4(&r, a, b, &c);
	test(r == 11, "minuscarry5");
	test(c == 0, "minuscarry6");

	r = 0xAA; a = 22; b = 24; c = 0;
	minuscarry4(&r, a, b, &c);
	test(r == BIGNUM_FULL - 1, "minuscarry7");
	test(c == 1, "minuscarry6");

	r = 0xAA; a = 22; b = 24; c = 1;
	minuscarry4(&r, a, b, &c);
	test(r == BIGNUM_FULL - 2, "minuscarry8");
	test(c == 1, "minuscarry9");

	r = 0xAA; a = 22; b = 22; c = 0;
	minuscarry4(&r, a, b, &c);
	test(r == 0, "minuscarry10");
	test(c == 0, "minuscarry11");

	r = 0xAA; a = 22; b = 22; c = 1;
	minuscarry4(&r, a, b, &c);
	test(r == BIGNUM_FULL, "minuscarry12");
	test(c == 1, "minuscarry13");

	r = 0xAA; a = 22; b = 20; c = 2;
	minuscarry4(&r, a, b, &c);
	test(r == 0, "minuscarry14");
	test(c == 0, "minuscarry15");

	RETURN;
}

static int test_TailCopy(void)
{
	size_t i;
	bigtype array[20], data[20];

	for (i = 0; i < 20; i++) {
		array[i] = 0x10 + i;
		data[i] = 0xAA;
	}
	i = 3;
	TailCopy(data, array, 10, i);
	test(array[0] == 0x10, "TailCopy1");
	test(data[0] == 0xAA, "TailCopy2");
	test(data[2] == 0xAA, "TailCopy3");
	test(data[3] == 0x13, "TailCopy4");
	test(data[9] == 0x19, "TailCopy5");
	test(data[10] == 0xAA, "TailCopy6");

	RETURN;
}

static int test_setplusvalue_bigdata(void)
{
	int sign;
	addr set, left, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &set, SignPlus, 10);
	bignum_alloc(local, &left, SignMinus, 10);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);

	data1[0] = 10;
	data2[0] = 20;
	setplusvalue_bigdata(set, left, SignMinus, 30);
	GetSizeBignum(set, &size);
	test(size == 1, "setplusvalue_bigdata1");
	GetSignBignum(set, &sign);
	test(sign == SignMinus, "setplusvalue_bigdata2");
	test(data1[0] == 50, "setplusvalue_bigdata3");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data2[0] = 10;
	data2[1] = 20;
	data2[2] = 30;
	data2[3] = 40;
	data2[4] = 50;
	SetSizeBignum(set, 1);
	SetSizeBignum(left, 5);
	setplusvalue_bigdata(set, left, SignPlus, 22);
	GetSizeBignum(set, &size);
	test(size == 5, "setplusvalue_bigdata4");
	test(data1[0] == 32, "setplusvalue_bigdata5");
	test(data1[1] == 20, "setplusvalue_bigdata6");
	test(data1[4] == 50, "setplusvalue_bigdata7");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data2[0] = data2[1] = data2[2] = BIGNUM_FULL;
	data2[3] = 10;
	SetSizeBignum(set, 1);
	SetSizeBignum(left, 4);
	setplusvalue_bigdata(set, left, SignMinus, 31);
	GetSizeBignum(set, &size);
	test(size == 4, "setplusvalue_bigdata8");
	test(data1[0] == 30, "setplusvalue_bigdata9");
	test(data1[1] == 0, "setplusvalue_bigdata10");
	test(data1[3] == 11, "setplusvalue_bigdata11");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data2[0] = data2[1] = data2[2] = BIGNUM_FULL;
	SetSizeBignum(set, 1);
	SetSizeBignum(left, 3);
	setplusvalue_bigdata(set, left, SignMinus, 21);
	GetSizeBignum(set, &size);
	test(size == 4, "setplusvalue_bigdata12");
	test(data1[0] == 20, "setplusvalue_bigdata13");
	test(data1[1] == 0, "setplusvalue_bigdata14");
	test(data1[2] == 0, "setplusvalue_bigdata15");
	test(data1[3] == 1, "setplusvalue_bigdata16");

	rollback_local(local, stack);

	RETURN;
}

static int test_plusvalue_bigdata_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value2_local(local, &left, signplus_bignum, 10ULL, 20ULL);
	plusvalue_bigdata_alloc(local, left, signminus_bignum, 3ULL, &left);
	bignum_value2_local(local, &right, signminus_bignum, 10ULL, 23ULL);
	test(equal_bb_real(left, right), "plusvalue_bigdata_alloc1");
	rollback_local(local, stack);

	RETURN;
}

static int test_setminusvalue_bigdata(void)
{
	int sign;
	addr set, left, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &set, SignPlus, 10);
	bignum_alloc(local, &left, SignMinus, 10);
	GetRootDataBignum(set, &root, &data1);
	GetRootDataBignum(left, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 1);
	data2[0] = 100;
	setminusvalue_bigdata(set, left, SignPlus, 10);
	GetSizeBignum(set, &size);
	test(size == 1, "setminusvalue_bigdata1");
	GetSignBignum(set, &sign);
	test(sign == SignPlus, "setminusvalue_bigdata2");
	test(data1[0] == 90, "setminusvalue_bigdata3");

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 1);
	data2[0] = 10;
	setminusvalue_bigdata(set, left, SignMinus, 10);
	GetSizeBignum(set, &size);
	test(size == 1, "setminusvalue_bigdata4");
	GetSignBignum(set, &sign);
	test(sign == SignPlus, "setminusvalue_bigdata5");
	test(data1[0] == 0, "setminusvalue_bigdata6");
	test(zerop_bignum(set), "setminusvalue_bigdata7");

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 1);
	data2[0] = 10;
	setminusvalue_bigdata(set, left, SignPlus, 50);
	GetSizeBignum(set, &size);
	test(size == 1, "setminusvalue_bigdata8");
	GetSignBignum(set, &sign);
	test(sign == SignMinus, "setminusvalue_bigdata9");
	test(data1[0] == 40, "setminusvalue_bigdata10");

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 3);
	data2[0] = 10;
	data2[1] = 20;
	data2[2] = 30;
	setminusvalue_bigdata(set, left, SignPlus, 5);
	GetSizeBignum(set, &size);
	test(size == 3, "setminusvalue_bigdata11");
	GetSignBignum(set, &sign);
	test(sign == SignPlus, "setminusvalue_bigdata12");
	test(data1[0] == 5, "setminusvalue_bigdata13");
	test(data1[1] == 20, "setminusvalue_bigdata14");
	test(data1[2] == 30, "setminusvalue_bigdata15");

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 3);
	data2[0] = 0;
	data2[1] = 0;
	data2[2] = 30;
	setminusvalue_bigdata(set, left, SignPlus, 5);
	GetSizeBignum(set, &size);
	test(size == 3, "setminusvalue_bigdata16");
	GetSignBignum(set, &sign);
	test(sign == SignPlus, "setminusvalue_bigdata17");
	test(data1[0] == BIGNUM_FULL - 4, "setminusvalue_bigdata18");
	test(data1[1] == BIGNUM_FULL, "setminusvalue_bigdata19");
	test(data1[2] == 29, "setminusvalue_bigdata20");

	SetSizeBignum(set, 5);
	SetSizeBignum(left, 3);
	data2[0] = 0;
	data2[1] = 0;
	data2[2] = 1;
	setminusvalue_bigdata(set, left, SignMinus, 5);
	GetSizeBignum(set, &size);
	test(size == 2, "setminusvalue_bigdata21");
	GetSignBignum(set, &sign);
	test(sign == SignMinus, "setminusvalue_bigdata22");
	test(data1[0] == BIGNUM_FULL - 4, "setminusvalue_bigdata23");
	test(data1[1] == BIGNUM_FULL, "setminusvalue_bigdata24");

	rollback_local(local, stack);

	RETURN;
}

static int test_minusvalue_bigdata_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value2_local(local, &left, signplus_bignum, 10ULL, 20ULL);
	minusvalue_bigdata_alloc(local, left, signminus_bignum, 3ULL, &left);
	bignum_value2_local(local, &right, signminus_bignum, 10ULL, 17ULL);
	test(equal_bb_real(left, right), "minusvalue_bigdata_alloc1");
	rollback_local(local, stack);

	RETURN;
}

static int test_plusloop(void)
{
	addr set, left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data, *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &set, SignPlus, 10);
	bignum_alloc(local, &left, SignMinus, 10);
	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(set, &root, &data);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data, 0xAA, 10);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 1;
	data1[1] = 2;
	data1[2] = 3;
	data2[0] = 2;
	data2[1] = 3;
	data2[2] = 4;
	data2[3] = 4;
	plusloop(set, 3, 4, data1, data2);
	GetSizeBignum(set, &size);
	test(size == 4, "plusloop1");
	test(data[0] == 3, "plusloop2");
	test(data[1] == 5, "plusloop3");
	test(data[2] == 7, "plusloop4");
	test(data[3] == 4, "plusloop5");

	data1[0] = 1;
	data1[1] = 1;
	data1[2] = 1;
	data2[0] = BIGNUM_FULL;
	data2[1] = BIGNUM_FULL;
	data2[2] = BIGNUM_FULL;
	data2[3] = BIGNUM_FULL;
	plusloop(set, 3, 4, data1, data2);
	GetSizeBignum(set, &size);
	test(size == 5, "plusloop6");
	test(data[0] == 0, "plusloop7");
	test(data[1] == 1, "plusloop8");
	test(data[2] == 1, "plusloop9");
	test(data[3] == 0, "plusloop10");
	test(data[4] == 1, "plusloop11");

	data1[0] = 1;
	data2[0] = BIGNUM_FULL;
	data2[1] = 10;
	data2[2] = 20;
	data2[3] = 30;
	plusloop(set, 1, 4, data1, data2);
	GetSizeBignum(set, &size);
	test(size == 4, "plusloop12");
	test(data[0] == 0, "plusloop13");
	test(data[1] == 11, "plusloop14");
	test(data[2] == 20, "plusloop15");
	test(data[3] == 30, "plusloop16");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bigdata_alloc(void)
{
	addr pos, left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data, *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignPlus, 10);
	bignum_alloc(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 1;
	data1[1] = 2;
	data1[2] = 3;
	data2[0] = 4;
	data2[1] = 5;
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 2);
	plus_bigdata_alloc(local, left, right, &pos);
	GetSizeBignum(pos, &size);
	test(size == 3, "plus_bigdata_alloc1");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 5, "plus_bigdata_alloc2");
	test(data[1] == 7, "plus_bigdata_alloc3");
	test(data[2] == 3, "plus_bigdata_alloc4");

	plus_bigdata_alloc(local, right, left, &pos);
	GetSizeBignum(pos, &size);
	test(size == 3, "plus_bigdata_alloc5");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 5, "plus_bigdata_alloc6");
	test(data[1] == 7, "plus_bigdata_alloc7");
	test(data[2] == 3, "plus_bigdata_alloc8");

	rollback_local(local, stack);

	RETURN;
}

static int test_letplus_noexpand_bigdata(void)
{
	addr left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignPlus, 10);
	bignum_alloc(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 1;
	data1[1] = 2;
	data1[2] = 3;
	data2[0] = 1;
	data2[1] = 2;
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 2);
	letplus_noexpand_bigdata(left, right);
	GetSizeBignum(left, &size);
	test(size == 3, "letplus_noexpand_bigdata1");
	test(data1[0] == 2, "letplus_noexpand_bigdata2");
	test(data1[1] == 4, "letplus_noexpand_bigdata3");
	test(data1[2] == 3, "letplus_noexpand_bigdata4");
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);

	data2[0] = 1;
	data2[1] = 2;
	data2[2] = 3;
	data1[0] = 1;
	data1[1] = 2;
	SetSizeBignum(left, 2);
	SetSizeBignum(right, 3);
	letplus_noexpand_bigdata(left, right);
	GetSizeBignum(left, &size);
	test(size == 3, "letplus_noexpand_bigdata5");
	test(data1[0] == 2, "letplus_noexpand_bigdata6");
	test(data1[1] == 4, "letplus_noexpand_bigdata7");
	test(data1[2] == 3, "letplus_noexpand_bigdata8");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bigdata_alloc(void)
{
	addr pos, left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data, *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignPlus, 10);
	bignum_alloc(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 30;
	data1[1] = 20;
	data1[2] = 10;
	data2[0] = 10;
	data2[1] = 15;
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 2);
	minus_bigdata_alloc(local, left, right, &pos);
	GetSizeBignum(pos, &size);
	test(size == 3, "minus_bigdata_alloc1");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 20, "minus_bigdata_alloc2");
	test(data[1] == 5, "minus_bigdata_alloc3");
	test(data[2] == 10, "minus_bigdata_alloc4");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 10;
	data2[0] = 10;
	SetSizeBignum(left, 1);
	SetSizeBignum(right, 1);
	minus_bigdata_alloc(local, left, right, &pos);
	GetSizeBignum(pos, &size);
	test(size == 1, "minus_bigdata_alloc5");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 0, "minus_bigdata_alloc6");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 0;
	data1[1] = 0;
	data1[2] = 0;
	data1[3] = 1;
	data2[0] = 10;
	SetSizeBignum(left, 4);
	SetSizeBignum(right, 1);
	minus_bigdata_alloc(local, left, right, &pos);
	GetSizeBignum(pos, &size);
	test(size == 3, "minus_bigdata_alloc7");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == BIGNUM_FULL - 9, "minus_bigdata_alloc8");
	test(data[1] == BIGNUM_FULL, "minus_bigdata_alloc9");
	test(data[2] == BIGNUM_FULL, "minus_bigdata_alloc10");

	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 20;
	data1[1] = 30;
	data1[2] = 42;
	data1[3] = 50;
	data2[0] = 21;
	data2[1] = 30;
	data2[2] = 40;
	data2[3] = 50;
	SetSizeBignum(left, 4);
	SetSizeBignum(right, 4);
	minus_bigdata_alloc(local, left, right, &pos);
	GetSizeBignum(pos, &size);
	test(size == 3, "minus_bigdata_alloc11");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == BIGNUM_FULL, "minus_bigdata_alloc12");
	test(data[1] == BIGNUM_FULL, "minus_bigdata_alloc13");
	test(data[2] == 1, "minus_bigdata_alloc14");

	rollback_local(local, stack);

	RETURN;
}

static int test_minuscheck_bigdata_alloc(void)
{
	int check;
	addr left, right, root;
	bigtype *data1, *data2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 10);
	check = minuscheck_bigdata_alloc(local, left, right, &left);
	test(check == 0, "minuscheck_bigdata_alloc1");

	bignum_local(local, &left, SignPlus, 10);
	bignum_local(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 29;
	data1[1] = 30;
	data1[2] = 40;
	data1[3] = 50;
	data2[0] = 20;
	data2[1] = 30;
	data2[2] = 40;
	data2[3] = 50;
	SetSizeBignum(left, 4);
	SetSizeBignum(right, 4);
	check = minuscheck_bigdata_alloc(local, left, right, &left);
	test(check == 0, "minuscheck_bigdata_alloc2");
	bignum_value_local(local, &right, SignPlus, 9);
	test(equal_bb_real(left, right), "minuscheck_bigdata_alloc3");

	bignum_local(local, &left, SignPlus, 10);
	bignum_local(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 20;
	data1[1] = 30;
	data1[2] = 40;
	data1[3] = 50;
	data2[0] = 29;
	data2[1] = 30;
	data2[2] = 40;
	data2[3] = 50;
	SetSizeBignum(left, 4);
	SetSizeBignum(right, 4);
	check = minuscheck_bigdata_alloc(local, left, right, &left);
	test(check, "minuscheck_bigdata_alloc4");
	bignum_value_local(local, &right, SignPlus, 9);
	test(equal_bb_real(left, right), "minuscheck_bigdata_alloc5");

	rollback_local(local, stack);

	RETURN;
}

static int test_setminus_noexpand(void)
{
	addr left, right, root;
	bigtype *data1, *data2;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_local(local, &left, SignPlus, 10);
	bignum_local(local, &right, SignMinus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[0] = 29;
	data1[1] = 30;
	data1[2] = 40;
	data1[3] = 50;
	data2[0] = 20;
	data2[1] = 30;
	data2[2] = 40;
	data2[3] = 50;
	SetSizeBignum(left, 4);
	SetSizeBignum(right, 4);
	setminus_noexpand(left, left, right);
	bignum_value_local(local, &right, SignPlus, 9);
	test(equal_bb_real(left, right), "setminus_noexpand1");

	rollback_local(local, stack);

	RETURN;
}

static int test_letminus_noexpand_bigdata(void)
{
	int check;
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 10);
	check = letminus_noexpand_bigdata(left, right);
	test(check == 0, "letminus_noexpand_bigdata1");
	test(zerop_bignum(left), "letminus_noexpand_bigdata2");

	bignum_value_local(local, &left, SignPlus, 999);
	bignum_value_local(local, &right, SignMinus, 10);
	check = letminus_noexpand_bigdata(left, right);
	test(check == 0, "letminus_noexpand_bigdata3");
	bignum_value_local(local, &right, SignPlus, 989);
	test(equal_bb_real(left, right), "letminus_noexpand_bigdata4");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 999);
	check = letminus_noexpand_bigdata(left, right);
	test(check, "letminus_noexpand_bigdata5");
	bignum_value_local(local, &right, SignPlus, 989);
	test(equal_bb_real(left, right), "letminus_noexpand_bigdata6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  multiple
 */
static int test_multicarry_fixnum(void)
{
	int sign;
	addr pos;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	multicarry_fixnum(local, 10, 3, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "multicarry_fixnum1");
	test(RefFixnum(pos) == 30, "multicarry_fixnum2");

	multicarry_fixnum(local, -3, 4, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "multicarry_fixnum3");
	test(RefFixnum(pos) == -12, "multicarry_fixnum4");

	multicarry_fixnum(local, 5, -6, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "multicarry_fixnum5");
	test(RefFixnum(pos) == -30, "multicarry_fixnum6");

	multicarry_fixnum(local, -1, -6, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "multicarry_fixnum7");
	test(RefFixnum(pos) == 6, "multicarry_fixnum8");

	multicarry_fixnum(local, FIXNUM_MAX, 2, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "multicarry_fixnum9");
	multicarry_fixnum(local, FIXNUM_MAX, -2, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "multicarry_fixnum10");
	GetSizeBignum(pos, &size);
	test(size == 1, "multicarry_fixnum11");

	multicarry_fixnum(local, FIXNUM_MAX, FIXNUM_MAX, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "multicarry_fixnum12");
	GetSizeBignum(pos, &size);
	test(size == 2, "multicarry_fixnum13");
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "multicarry_fixnum14");

	multicarry_fixnum(local, FIXNUM_MAX, FIXNUM_MIN, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "multicarry_fixnum15");
	GetSizeBignum(pos, &size);
	test(size == 2, "multicarry_fixnum16");
	GetSignBignum(pos, &sign);
	test(IsMinus(sign), "multicarry_fixnum17");

	rollback_local(local, stack);

	RETURN;
}

static int test_multicarry_bignum(void)
{
	int sign;
	addr left, right;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);

	multicarry_bignum(local, 3, 4, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "multicarry_bignum1");
	bignum_value_local(local, &right, SignPlus, 12);
	test(equal_bb_real(left, right), "multicarry_bignum2");

	multicarry_bignum(local, -3, 4, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "multicarry_bignum3");
	bignum_value_local(local, &right, SignMinus, 12);
	test(equal_bb_real(left, right), "multicarry_bignum4");

	multicarry_bignum(local, FIXNUM_MAX, FIXNUM_MAX, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "multicarry_bignum5");
	GetSizeBignum(left, &size);
	test(size == 2, "multicarry_bignum6");
	GetSignBignum(left, &sign);
	test(IsPlus(sign), "multicarry_bignum7");

	rollback_local(local, stack);

	RETURN;
}

static int test_setmultivalue_bigdata(void)
{
	addr pos, left, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignPlus, 10);
	bignum_alloc(local, &left, SignMinus, 10);
	GetRootDataBignum(pos, &root, &data1);
	GetRootDataBignum(left, &root, &data2);
	data2[0] = 10;
	data2[1] = 11;
	data2[2] = 12;
	data2[3] = 22;
	SetSizeBignum(left, 4);
	setmultivalue_bigdata(pos, left, 2);
	GetSizeBignum(pos, &size);
	test(size == 4, "setmultivalue_bigdata1");
	test(data1[0] == 20, "setmultivalue_bigdata2");
	test(data1[1] == 22, "setmultivalue_bigdata3");
	test(data1[2] == 24, "setmultivalue_bigdata4");
	test(data1[3] == 44, "setmultivalue_bigdata5");

	data2[0] = BIGNUM_FULL;
	data2[1] = BIGNUM_FULL;
	SetSizeBignum(left, 2);
	setmultivalue_bigdata(pos, left, 2);
	GetSizeBignum(pos, &size);
	test(size == 3, "setmultivalue_bigdata6");
	test(data1[0] == BIGNUM_FULL - 1, "setmultivalue_bigdata7");
	test(data1[1] == BIGNUM_FULL, "setmultivalue_bigdata8");
	test(data1[2] == 1, "setmultivalue_bigdata9");

	rollback_local(local, stack);

	RETURN;
}

static int test_setmulti_bigdata(void)
{
	addr pos, left, right, root;
	LocalRoot local;
	LocalStack stack;
	bigtype *data, *data1, *data2;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignPlus, 20);
	bignum_alloc(local, &left, SignPlus, 10);
	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);

	data1[0] = 2;
	data1[1] = 3;
	data2[0] = 4;
	data2[1] = 5;
	data2[2] = 6;
	SetSizeBignum(left, 2);
	SetSizeBignum(right, 3);
	setmulti_bigdata(pos, left, right);
	GetSizeBignum(pos, &size);
	test(size == 4, "setmulti_bigdata1");
	/*
	 *     6  5  4
	 *        3  2
	 * -----------
	 *    12 10  8
	 * 18 15 12
	 * -----------
	 */
	test(data[0] == 8, "setmulti_bigdata2");
	test(data[1] == 10+12, "setmulti_bigdata3");
	test(data[2] == 12+15, "setmulti_bigdata4");
	test(data[3] == 18, "setmulti_bigdata5");

	data1[0] = BIGNUM_FULL;
	data1[1] = BIGNUM_FULL;
	data1[2] = BIGNUM_FULL;
	data2[0] = BIGNUM_FULL;
	data2[1] = BIGNUM_FULL;
	SetSizeBignum(left, 3);
	SetSizeBignum(right, 2);
	setmulti_bigdata(pos, left, right);
	GetSizeBignum(pos, &size);
	test(size == 5, "setmulti_bigdata6");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bigdata_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignMinus, 5);
	bignum_value_local(local, &right, SignMinus, 6);
	multi_bigdata_alloc(local, left, right, &left);
	bignum_value_local(local, &right, SignPlus, 30);
	test(equal_bb_real(left, right), "multi_bigdata_alloc1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  division
 */
static int divhalftest(
		bigtype a1, bigtype a2, bigtype b,
		bigtype q1, bigtype q2, bigtype r)
{
	bigtype c;
	divhalf(&a1, &a2, b, &c);
	return a1 == q1 && a2 == q2 && c == r;
}

#define cdivhalf(a,b,c,d,e,f,g) test(divhalftest(a,b,c,d,e,f),g)
static int test_divhalf(void)
{
#if defined BIGNUM_CODE_64BIT
	cdivhalf(0x28E04EA2EB2ABA88LL, 0x347F69D298A362E5LL, 0x68F3D324LL,
			0x63B489ECLL, 0xBC06BAC73284F39FLL, 0x11381389LL, "divhalf1");
	cdivhalf(0xC4E65E25CCF2340ALL, 0x9B0DDFE985CF25C5LL, 0xB8FCDAFLL,
			0x1107C256EFLL, 0xF313AB9EC58D1F72LL, 0x17A5CD7LL, "divhalf2");
#elif defined BIGNUM_CODE_32BIT
	cdivhalf(0x5D3B5F95LL, 0x428FC0FALL, 0x2AE7LL,
			0x22C51LL, 0x9226E45FLL, 0x1941LL, "divhalf1");
	cdivhalf(0x7D75DF62LL, 0x6F90937FLL, 0xBA5CLL,
			0xAC57LL, 0xFFAB6E0CLL, 0x4F2FLL, "divhalf2");
#else
	cdivhalf(0x1ALL, 0x1FLL, 0x9LL, 0x2LL, 0xE7LL, 0x0LL, "divhalf1");
	cdivhalf(0xB2LL, 0x1DLL, 0x8LL, 0x16LL, 0x43LL, 0x5LL, "divhalf2");
#endif

	RETURN;
}

static int test_getshiftvalue(void)
{
	bigtype a, b;

	a = 1;
	test(getshiftvalue(&a) == BIGNUM_FULLBIT - 1, "getshiftvalue1");
	b = BIGNUM_FULLBIT - 1ULL;
	b = 1ULL << b;
	test(a == b, "getshiftvalue2");
	a = 2;
	test(getshiftvalue(&a) == BIGNUM_FULLBIT - 2, "getshiftvalue2");
	a = 3;
	test(getshiftvalue(&a) == BIGNUM_FULLBIT - 2, "getshiftvalue3");
	a = 4;
	test(getshiftvalue(&a) == BIGNUM_FULLBIT - 3, "getshiftvalue4");
	test((a >> (BIGNUM_FULLBIT - 3)) == 4, "getshiftvalue5");

	RETURN;
}

static int divfulltest(
		bigtype a1, bigtype a2, bigtype b,
		bigtype q1, bigtype q2, bigtype r)
{
	bigtype c;
	divfull(&a1, &a2, b, &c);
	/*
	   printf("%16lx\n", a1);
	   printf("%16lx\n", a2);
	   printf("%16lx\n", b);
	   printf("%16lx\n", c);
	   printf("%16lx\n", q1);
	   printf("%16lx\n", q2);
	   printf("%16lx\n", r);
	 */
	return a1 == q1 && a2 == q2 && c == r;
}

#define cdivfull(a,b,c,d,e,f,g) test(divfulltest(a,b,c,d,e,f),g)
static int test_divfull(void)
{
#if defined BIGNUM_CODE_64BIT
	/* right 18 */
	cdivfull(0x62AEB171B90CDF41LL, 0x6FB0A4F79C2D40FALL, 0xF09B048B57LL,
			0x68FF0ELL, 0x7912702A85AE270LL, 0x21B2977CEALL,
			"divfull1");
	cdivfull(0x25E5EBD59B86B180LL, 0xAA56D82D96C94C63LL, 0x22F9CE6758LL,
			0x11563B6LL, 0x35F0B19C133B0F9ELL, 0x16F80F5C13LL,
			"divfull2");
	cdivfull(0x79A1110A984DA515LL, 0x9DDFE1141B8D5EF6LL, 0xE88209304ALL,
			0x85EB14LL, 0xCEE64EF74286A464LL, 0x4B124B1A0ELL,
			"divfull3");
	cdivfull(0x966D2D6CBB6EC739LL, 0xD0E71E484DF35564LL, 0xC95B1D4003LL,
			0xBF3FD2LL, 0x1503698C33276C0LL, 0x3B2EBF124LL,
			"divfull4");
	cdivfull(0x5D5DA8E392CA318ELL, 0xECE8B2DDC8FD4C9ELL, 0xBA79E8A7CALL,
			0x802CE6LL, 0x18EF8D8AA566D98BLL, 0x8B54F3F7F0LL,
			"divfull5");
	/* right 32 */
	cdivfull(0xE43E7F49E5688BE0LL, 0x5113AAAB42B23213LL, 0x6EA80AD37D9767F6LL,
			0x2LL, 0x10090BB00B719A4DLL, 0x33103314F207F115LL,
			"divfull6");
	cdivfull(0x40AA08CAAAF294F2LL, 0x4741DFCBB2A17E26LL, 0xBAAF4FB4B56B770FLL,
			0x0LL, 0x58AC782208F2B04BLL, 0x727195D3A61D4CC1LL,
			"divfull7");
	cdivfull(0xC617857A5DACCAACLL, 0x1E49119AA5D8A346LL, 0x51C44ACFF726F1E2LL,
			0x2LL, 0x6C327D16AC780ABBLL, 0x15D823D85D031F30LL,
			"divfull8");
	cdivfull(0x6809AD5225582A1ELL, 0x304AF663D3A2AEECLL, 0x4353FC20754D56FBLL,
			0x1LL, 0x8B94A285ACA3BB31LL, 0x283C5167DB7AAFE1LL,
			"divfull9");

#elif defined BIGNUM_CODE_32BIT
	/* right 5 */
	cdivfull(0x257FE7AALL, 0x8CF58A77LL, 0x178C5LL, 0x197ALL, 0xC17FC27BLL, 0x39D0LL,
			"divfull1");
	cdivfull(0xEDD8CD39LL, 0x38D3664CLL, 0xF259ALL, 0xFB3LL, 0xE3F00707LL, 0x22916LL,
			"divfull2");
	cdivfull(0x9CAE4E5BLL, 0xC82321C1LL, 0xE5DF6LL, 0xAE7LL, 0xD43F9F4BLL, 0x7D0AFLL,
			"divfull4");
	cdivfull(0xB723EAF1LL, 0xE9D18BBELL, 0xA52E1LL, 0x11BDLL, 0x5DF11C93LL, 0x2588BLL,
			"divfull5");

	/* right 8 */
	cdivfull(0x12017193LL, 0x621598CDLL, 0x4AA84AD4LL, 0x0LL, 0x3DBDC529LL, 0x290878D9LL,
			"divfull6");
	cdivfull(0xBCAF1478LL, 0xDDAF49E6LL, 0xB102A438LL, 0x1LL, 0x10E20265LL, 0x88EC0FCELL,
			"divfull7");
	cdivfull(0x4677FA56LL, 0xA3576931LL, 0x9A2870ECLL, 0x0LL, 0x7505CDD1LL, 0x554A3C85LL,
			"divfull8");
	cdivfull(0x24434C8CLL, 0x5444202FLL, 0xE3851D8FLL, 0x0LL, 0x28CD5823LL, 0x7364EDA2LL,
			"divfull9");
	cdivfull(0xA27DA953LL, 0xFB8F3A1DLL, 0x688FC54LL, 0x18LL, 0xDD447EC4LL, 0x42CB1CDLL,
			"divfull10");
#else
	/* right 2 */
	cdivfull(0xCFLL, 0xA0LL, 0x72LL, 0x1LL, 0xD2LL, 0x1CLL, "divfull6");
	cdivfull(0x65LL, 0x8LL, 0x8CLL, 0x0LL, 0xB8LL, 0x68LL, "divfull7");
	cdivfull(0x9FLL, 0x38LL, 0x99LL, 0x1LL, 0xALL, 0x3ELL, "divfull8");
	cdivfull(0xAFLL, 0xCLL, 0xEBLL, 0x0LL, 0xBELL, 0xA2LL, "divfull9");
	cdivfull(0xC3LL, 0xADLL, 0xDDLL, 0x0LL, 0xE2LL, 0x93LL, "divfull10");
#endif

	RETURN;
}

static int divdoubletest(
		bigtype a1, bigtype a2, bigtype b,
		bigtype q1, bigtype q2, bigtype r)
{
	bigtype c;
	divdouble(&a1, &a2, b, &c);
	/*
	   printf("%16lx\n", a1);
	   printf("%16lx\n", a2);
	   printf("%16lx\n", b);
	   printf("%16lx\n", c);
	   printf("%16lx\n", q1);
	   printf("%16lx\n", q2);
	   printf("%16lx\n", r);
	 */
	return a1 == q1 && a2 == q2 && c == r;
}

#define cdivdouble(a,b,c,d,e,f,g) test(divdoubletest(a,b,c,d,e,f),g)
static int test_divdouble(void)
{
#if defined BIGNUM_CODE_64BIT
	cdivdouble(0x0LL, 0x8LL, 0x8LL,
			0x0LL, 0x1LL, 0x0LL, "divdouble1");
	cdivdouble(0x0LL, 0xD5LL, 0x4LL,
			0x0LL, 0x35LL, 0x1LL, "divdouble2");
	cdivdouble(0x0LL, 0x66LL, 0x9FLL,
			0x0LL, 0x0LL, 0x66LL, "divdouble3");
	cdivdouble(0x0LL, 0x4F2LL, 0x8LL,
			0x0LL, 0x9ELL, 0x2LL, "divdouble4");
	cdivdouble(0x0LL, 0x76LL, 0x23LL,
			0x0LL, 0x3LL, 0xDLL, "divdouble5");
	cdivdouble(0x0LL, 0x1D5LL, 0x3A4LL,
			0x0LL, 0x0LL, 0x1D5LL, "divdouble6");
	cdivdouble(0x0LL, 0xD1D8LL, 0x7LL,
			0x0LL, 0x1DFALL, 0x2LL, "divdouble7");
	cdivdouble(0x0LL, 0xA666LL, 0xC2LL,
			0x0LL, 0xDBLL, 0x70LL, "divdouble8");
	cdivdouble(0x0LL, 0xED72LL, 0xC26LL,
			0x0LL, 0x13LL, 0x6A0LL, "divdouble9");
	cdivdouble(0x0LL, 0xA53ALL, 0xBB34LL,
			0x0LL, 0x0LL, 0xA53ALL, "divdouble10");
	cdivdouble(0x0LL, 0xFAAE5LL, 0xALL,
			0x0LL, 0x19116LL, 0x9LL, "divdouble11");
	cdivdouble(0x0LL, 0x541ALL, 0x34LL,
			0x0LL, 0x19ELL, 0x2LL, "divdouble12");
	cdivdouble(0x0LL, 0x447ABLL, 0xB82LL,
			0x0LL, 0x5FLL, 0x26DLL, "divdouble13");
	cdivdouble(0x0LL, 0x35310LL, 0x6D08LL,
			0x0LL, 0x7LL, 0x57D8LL, "divdouble14");
	cdivdouble(0x0LL, 0xE8326LL, 0x45DEALL,
			0x0LL, 0x3LL, 0x16968LL, "divdouble15");
	cdivdouble(0x0LL, 0x32EFF0LL, 0x8LL,
			0x0LL, 0x65DFELL, 0x0LL, "divdouble16");
	cdivdouble(0x0LL, 0xECE370LL, 0xA4LL,
			0x0LL, 0x171C6LL, 0x98LL, "divdouble17");
	cdivdouble(0x0LL, 0xD3C08CLL, 0x491LL,
			0x0LL, 0x2E5FLL, 0xBDLL, "divdouble18");
	cdivdouble(0x0LL, 0x5BD26FLL, 0xE7CELL,
			0x0LL, 0x65LL, 0x5E29LL, "divdouble19");
	cdivdouble(0x0LL, 0x48D832LL, 0x524D0LL,
			0x0LL, 0xELL, 0xD4D2LL, "divdouble20");
	cdivdouble(0x0LL, 0xDAB79BLL, 0xF527B7LL,
			0x0LL, 0x0LL, 0xDAB79BLL, "divdouble21");
	cdivdouble(0x0LL, 0xD7EBE1FLL, 0xELL,
			0x0LL, 0xF6C46FLL, 0xDLL, "divdouble22");
	cdivdouble(0x0LL, 0x32D51A9LL, 0xEBLL,
			0x0LL, 0x375FFLL, 0x94LL, "divdouble23");
	cdivdouble(0x0LL, 0x71C3AECLL, 0x24BLL,
			0x0LL, 0x319D4LL, 0x1D0LL, "divdouble24");
	cdivdouble(0x0LL, 0x585523LL, 0xB439LL,
			0x0LL, 0x7DLL, 0x554ELL, "divdouble25");
	cdivdouble(0x0LL, 0xC2FBAB3LL, 0xE63A3LL,
			0x0LL, 0xD8LL, 0xBA92BLL, "divdouble26");
	cdivdouble(0x0LL, 0x43781FELL, 0x606F23LL,
			0x0LL, 0xBLL, 0x12BB7DLL, "divdouble27");
	cdivdouble(0x0LL, 0xA8D752DLL, 0xAA16E94LL,
			0x0LL, 0x0LL, 0xA8D752DLL, "divdouble28");
	cdivdouble(0x0LL, 0xA558C984LL, 0xBLL,
			0x0LL, 0xF081251LL, 0x9LL, "divdouble29");
	cdivdouble(0x0LL, 0x22D34A91LL, 0x4CLL,
			0x0LL, 0x754E74LL, 0x21LL, "divdouble30");
	cdivdouble(0x0LL, 0xB53F555DLL, 0x2ELL,
			0x0LL, 0x3F0AE60LL, 0x1DLL, "divdouble31");
	cdivdouble(0x0LL, 0x51588400LL, 0xAC82LL,
			0x0LL, 0x78B7LL, 0x4312LL, "divdouble32");
	cdivdouble(0x0LL, 0xC03DD67CLL, 0x3BC8LL,
			0x0LL, 0x3373BLL, 0x1764LL, "divdouble33");
	cdivdouble(0x0LL, 0x42597A21LL, 0xFF8A3ELL,
			0x0LL, 0x42LL, 0x77D625LL, "divdouble34");
	cdivdouble(0x0LL, 0x6149AD4FLL, 0xA3D0211LL,
			0x0LL, 0x9LL, 0x5249AB6LL, "divdouble35");
	cdivdouble(0x0LL, 0xB5B1FA7FLL, 0xE5773D1ALL,
			0x0LL, 0x0LL, 0xB5B1FA7FLL, "divdouble36");
	cdivdouble(0x0LL, 0x17CA64F94LL, 0xFLL,
			0x0LL, 0x19606BB4LL, 0x8LL, "divdouble37");
	cdivdouble(0x0LL, 0xB36860F90LL, 0xD9LL,
			0x0LL, 0xD3A6C9BLL, 0x2DLL, "divdouble38");
	cdivdouble(0x0LL, 0xB10154829LL, 0x1FDLL,
			0x0LL, 0x5906338LL, 0x1D1LL, "divdouble39");
	cdivdouble(0x0LL, 0xC2FB1974DLL, 0xCF14LL,
			0x0LL, 0xF10B7LL, 0x5001LL, "divdouble40");
	cdivdouble(0x0LL, 0x80D99B52BLL, 0xA48EALL,
			0x0LL, 0xC873LL, 0x4240DLL, "divdouble41");
	cdivdouble(0x0LL, 0xE22D5FB8ALL, 0x748414LL,
			0x0LL, 0x1F0FLL, 0x3D25ELL, "divdouble42");
	cdivdouble(0x0LL, 0x259B27126LL, 0x9E68157LL,
			0x0LL, 0x3CLL, 0x7AC20C2LL, "divdouble43");
	cdivdouble(0x0LL, 0x18EDA55ELL, 0x264C9C47LL,
			0x0LL, 0x0LL, 0x18EDA55ELL, "divdouble44");
	cdivdouble(0x0LL, 0x1CB80E42BLL, 0x3D6E7F519LL,
			0x0LL, 0x0LL, 0x1CB80E42BLL, "divdouble45");
	cdivdouble(0x0LL, 0xC38FC7790LL, 0xELL,
			0x0LL, 0xDF7FBF65LL, 0xALL, "divdouble46");
	cdivdouble(0x0LL, 0x72C333EBB6LL, 0x15LL,
			0x0LL, 0x5770278F0LL, 0x6LL, "divdouble47");
	cdivdouble(0x0LL, 0x80E20E3DC8LL, 0x241LL,
			0x0LL, 0x392E9C47LL, 0x1C1LL, "divdouble48");
	cdivdouble(0x0LL, 0x83F4D2143FLL, 0xEAB5LL,
			0x0LL, 0x8FED77LL, 0x691CLL, "divdouble49");
	cdivdouble(0x0LL, 0xBBC5B00E0CLL, 0x30814LL,
			0x0LL, 0x3DF071LL, 0x2BD38LL, "divdouble50");
	cdivdouble(0x0LL, 0x89F362ADE9LL, 0x81D75ELL,
			0x0LL, 0x10FFDLL, 0x145403LL, "divdouble51");
	cdivdouble(0x0LL, 0x66721A876ELL, 0x241D97ELL,
			0x0LL, 0x2D62LL, 0x1AA1F32LL, "divdouble52");
	cdivdouble(0x0LL, 0xA5D5FCC507LL, 0x98C65F3DLL,
			0x0LL, 0x115LL, 0x8757B806LL, "divdouble53");
	cdivdouble(0x0LL, 0x7DD254D2B7LL, 0x2908F1B04LL,
			0x0LL, 0x31LL, 0x26F0A6F3LL, "divdouble54");
	cdivdouble(0x0LL, 0x9AEFD53716LL, 0xD05E86840DLL,
			0x0LL, 0x0LL, 0x9AEFD53716LL, "divdouble55");
	cdivdouble(0x0LL, 0x1A159EE0B98LL, 0x8LL,
			0x0LL, 0x342B3DC173LL, 0x0LL, "divdouble56");
	cdivdouble(0x0LL, 0xDCD8D172532LL, 0x4FLL,
			0x0LL, 0x2CBA7EAC37LL, 0x39LL, "divdouble57");
	cdivdouble(0x0LL, 0x5300A314C40LL, 0x8DDLL,
			0x0LL, 0x95D60D02LL, 0x186LL, "divdouble58");
	cdivdouble(0x0LL, 0x2CF90A01EF5LL, 0x9C8ELL,
			0x0LL, 0x498A408LL, 0x4285LL, "divdouble59");
	cdivdouble(0x0LL, 0x7FC9BA2E9CDLL, 0x92F08LL,
			0x0LL, 0xDEA251LL, 0x7F845LL, "divdouble60");
	cdivdouble(0x0LL, 0xD57613ADE4BLL, 0x5A4972LL,
			0x0LL, 0x25D3FCLL, 0x579C13LL, "divdouble61");
	cdivdouble(0x0LL, 0x865225889FBLL, 0xF6BF1ABLL,
			0x0LL, 0x8B5BLL, 0xAC1C932LL, "divdouble62");
	cdivdouble(0x0LL, 0x43CE5F7A483LL, 0xD3E7187ELL,
			0x0LL, 0x51ELL, 0x8D684FBFLL, "divdouble63");
	cdivdouble(0x0LL, 0x74DF07CF04DLL, 0xC39DDA0B0LL,
			0x0LL, 0x98LL, 0xB94E587CDLL, "divdouble64");
	cdivdouble(0x0LL, 0xD16A8DE763BLL, 0x36F4B5DDA0LL,
			0x0LL, 0x3CLL, 0x354E3E84BBLL, "divdouble65");
	cdivdouble(0x0LL, 0xD311189F9AFLL, 0x6524BB51E62LL,
			0x0LL, 0x2LL, 0x8C7A1FBCEBLL, "divdouble66");
	cdivdouble(0x0LL, 0xD956A4C222EFLL, 0xBLL,
			0x0LL, 0x13C20EFA6044LL, 0x3LL, "divdouble67");
	cdivdouble(0x0LL, 0xB513B6A0EB23LL, 0xCELL,
			0x0LL, 0xE10719A2B1LL, 0xB5LL, "divdouble68");
	cdivdouble(0x0LL, 0x63902939E5DDLL, 0x916LL,
			0x0LL, 0xAF53B58EALL, 0x7C1LL, "divdouble69");
	cdivdouble(0x0LL, 0x3DD01C17F905LL, 0xFD3ELL,
			0x0LL, 0x3E7C6F42LL, 0xCD09LL, "divdouble70");
	cdivdouble(0x0LL, 0x531018E7A2C1LL, 0x2D248LL,
			0x0LL, 0x1D70B04BLL, 0x287A9LL, "divdouble71");
	cdivdouble(0x0LL, 0xD7DEFEFBE894LL, 0x24E542LL,
			0x0LL, 0x5D9D2C2LL, 0x40890LL, "divdouble72");
	cdivdouble(0x0LL, 0x72631B566399LL, 0x90C3B1BLL,
			0x0LL, 0xCA47FLL, 0x323C534LL, "divdouble73");
	cdivdouble(0x0LL, 0x5AC176FFB39ELL, 0xC4CDBDLL,
			0x0LL, 0x760DD4LL, 0x74BA1ALL, "divdouble74");
	cdivdouble(0x0LL, 0x99B06B8C0A05LL, 0xDC95C55DLL,
			0x0LL, 0xB25DLL, 0x25E8AD3CLL, "divdouble75");
	cdivdouble(0x0LL, 0x9A6225034B26LL, 0x6DE2BA6C06LL,
			0x0LL, 0x167LL, 0x493195CEBCLL, "divdouble76");
	cdivdouble(0x0LL, 0x910FEED0C209LL, 0x7C306B8F4A8LL,
			0x0LL, 0x12LL, 0x55975CF8E39LL, "divdouble77");
	cdivdouble(0x0LL, 0x4753F905CE4CLL, 0x305F12776EE2LL,
			0x0LL, 0x1LL, 0x16F4E68E5F6ALL, "divdouble78");
	cdivdouble(0x0LL, 0x8067B33CBF51FLL, 0x3LL,
			0x0LL, 0x2ACD3BBEEA70ALL, 0x1LL, "divdouble79");
	cdivdouble(0x0LL, 0xD70E037C163E7LL, 0x2LL,
			0x0LL, 0x6B8701BE0B1F3LL, 0x1LL, "divdouble80");
	cdivdouble(0x0LL, 0x49854412F7C9ELL, 0xE3ELL,
			0x0LL, 0x52983DDF1ALL, 0x852LL, "divdouble81");
	cdivdouble(0x0LL, 0x2ABFA4E37E784LL, 0xC006LL,
			0x0LL, 0x38FDBE96BLL, 0x2F02LL, "divdouble82");
	cdivdouble(0x0LL, 0x93D2A99AE8C86LL, 0x1CD40LL,
			0x0LL, 0x520B30C2CLL, 0x4586LL, "divdouble83");
	cdivdouble(0x0LL, 0xC6F0E88FB5B52LL, 0x39F7A2LL,
			0x0LL, 0x36E94690LL, 0x37C432LL, "divdouble84");
	cdivdouble(0x0LL, 0x97380DC74A8BELL, 0xAF9BD10LL,
			0x0LL, 0xDC71ECLL, 0x8064DFELL, "divdouble85");
	cdivdouble(0x0LL, 0xA7E72CAE0EA5BLL, 0x544E293ELL,
			0x0LL, 0x1FDD9ELL, 0x4A86F017LL, "divdouble86");
	cdivdouble(0x0LL, 0x3F85503C2A351LL, 0x929DAB6A1LL,
			0x0LL, 0x6EE9LL, 0x1F1393CC8LL, "divdouble87");
	cdivdouble(0x0LL, 0xBAD7EC1CA3F38LL, 0x133CFD3BEALL,
			0x0LL, 0x9B64LL, 0xD9B9829D0LL, "divdouble88");
	cdivdouble(0x0LL, 0xDE2A5604DBAC3LL, 0x44AE24BBD0DLL,
			0x0LL, 0x33CLL, 0x71735644B7LL, "divdouble89");
	cdivdouble(0x0LL, 0xEBBC9B4B98C75LL, 0xF882D6985042LL,
			0x0LL, 0xFLL, 0x2C1F21CCD897LL, "divdouble90");
	cdivdouble(0x0LL, 0x22B5DF6F25D4CLL, 0xF3A49BED185A6LL,
			0x0LL, 0x0LL, 0x22B5DF6F25D4CLL, "divdouble91");
	cdivdouble(0x0LL, 0xD55810462F8722LL, 0xBLL,
			0x0LL, 0x136518C08FF503LL, 0x1LL, "divdouble92");
	cdivdouble(0x0LL, 0x7E19E920E47030LL, 0xF4LL,
			0x0LL, 0x844D8BAD0075LL, 0xACLL, "divdouble93");
	cdivdouble(0x0LL, 0x4D7C865384383FLL, 0x9C1LL,
			0x0LL, 0x7F1B358A202LL, 0x2BDLL, "divdouble94");
	cdivdouble(0x0LL, 0x348406BAE6BF73LL, 0x8536LL,
			0x0LL, 0x64EC3F51E4LL, 0x55BLL, "divdouble95");
	cdivdouble(0x0LL, 0x7135A8071B8D5ALL, 0x12B5FLL,
			0x0LL, 0x60CEF7A125LL, 0x899FLL, "divdouble96");
	cdivdouble(0x0LL, 0xADA0DA06E04E91LL, 0x929B21LL,
			0x0LL, 0x12F2F91C8LL, 0x6A6BC9LL, "divdouble97");
	cdivdouble(0x0LL, 0x84E20F1FF6D2F6LL, 0xDE5272DLL,
			0x0LL, 0x9903347LL, 0x4AFE7BLL, "divdouble98");
	cdivdouble(0x0LL, 0xB589EF2DA0123BLL, 0xE6EE1AEALL,
			0x0LL, 0xC93F2BLL, 0xA947F6EDLL, "divdouble99");
	cdivdouble(0x0LL, 0x2E945C2B4EEC0ALL, 0x40BFDE02BLL,
			0x0LL, 0xB8295LL, 0x1A2DC9D03LL, "divdouble100");
	cdivdouble(0x0LL, 0xBA55E6EA150DA1LL, 0x34869BEBFDLL,
			0x0LL, 0x38C29LL, 0xC5216E61CLL, "divdouble101");
	cdivdouble(0x0LL, 0xC5B022C41195B1LL, 0x8E5FC089B8ELL,
			0x0LL, 0x1637LL, 0x310E0D9F62FLL, "divdouble102");
	cdivdouble(0x0LL, 0x5B060D25E19BB6LL, 0xFA124BD1385CLL,
			0x0LL, 0x5DLL, 0x2D679AE0224ALL, "divdouble103");
	cdivdouble(0x0LL, 0xDCFA51AE05225BLL, 0x7B7CF70E06854LL,
			0x0LL, 0x1CLL, 0x4DFA15579B92BLL, "divdouble104");
	cdivdouble(0x0LL, 0x5BB4D24B88DF8FLL, 0x43F1751B333EEELL,
			0x0LL, 0x1LL, 0x17C35D3055A0A1LL, "divdouble105");
	cdivdouble(0x0LL, 0x51AF0F9D8C53FFBLL, 0x7LL,
			0x0LL, 0xBAB4B5FA655248LL, 0x3LL, "divdouble106");
	cdivdouble(0x0LL, 0x413575DB72D9B2CLL, 0xBCLL,
			0x0LL, 0x58CB85409C645LL, 0x80LL, "divdouble107");
	cdivdouble(0x0LL, 0x392496AC82B6684LL, 0xDD9LL,
			0x0LL, 0x42065011EBE1LL, 0x7CBLL, "divdouble108");
	cdivdouble(0x0LL, 0xE8A241BA3023712LL, 0x27B7LL,
			0x0LL, 0x5DB8BEA6695BLL, 0xA05LL, "divdouble109");
	cdivdouble(0x0LL, 0xBEEF13C16CDFF83LL, 0x871F8LL,
			0x0LL, 0x169BCCEBD9ALL, 0x55853LL, "divdouble110");
	cdivdouble(0x0LL, 0xCD8C08ECD3B8EB2LL, 0xF512B3LL,
			0x0LL, 0xD6B62DF5CLL, 0x58E95ELL, "divdouble111");
	cdivdouble(0x0LL, 0x2F707FB5622E729LL, 0x5622184LL,
			0x0LL, 0x8CFF2B0DLL, 0x20A0775LL, "divdouble112");
	cdivdouble(0x0LL, 0x3A17762E405D026LL, 0xE892480CLL,
			0x0LL, 0x3FF1921LL, 0xB82D5A9ALL, "divdouble113");
	cdivdouble(0x0LL, 0xB558B9FAAF09AC0LL, 0x61E6FEF77LL,
			0x0LL, 0x1DA31BELL, 0x44B1196ELL, "divdouble114");
	cdivdouble(0x0LL, 0x95BEEC21477501ALL, 0x5A8668B1A8LL,
			0x0LL, 0x1A7792LL, 0x4B71CE64ALL, "divdouble115");
	cdivdouble(0x0LL, 0xEFFF250697EBCD5LL, 0xBAF5837861LL,
			0x0LL, 0x1489FALL, 0x13E41B451BLL, "divdouble116");
	cdivdouble(0x0LL, 0xA8BE140B68021B2LL, 0x8CC2E1ABAE18LL,
			0x0LL, 0x132ELL, 0x1F7C6BBB1162LL, "divdouble117");
	cdivdouble(0x0LL, 0xF7BFC7DBAF95F21LL, 0x74E07D7E51851LL,
			0x0LL, 0x21ELL, 0x4C7E2A3EFE3A3LL, "divdouble118");
	cdivdouble(0x0LL, 0x4FA61196DB5FD96LL, 0x13D7D80EBBD33ALL,
			0x0LL, 0x40LL, 0x46B15BEC12F16LL, "divdouble119");
	cdivdouble(0x0LL, 0x6EC0FEF23F50460LL, 0xAC97AD1D467CB57LL,
			0x0LL, 0x0LL, 0x6EC0FEF23F50460LL, "divdouble120");
	cdivdouble(0xE6E0C6B5A3E8386CLL, 0x5B5E0F3F494DD540LL, 0xELL,
			0x107DC50CF96C0407LL, 0xBD6225A917858F3BLL, 0x6LL, "divdouble121");
	cdivdouble(0x522E978AC0D63D4LL, 0x1118C5B8DF181D63LL, 0xD5LL,
			0x62C5D1C737171LL, 0xF8DE76B659FC81F0LL, 0xB3LL, "divdouble122");
	cdivdouble(0x31E2CBD050D8A3C0LL, 0xE9334B2C4530F096LL, 0xE82LL,
			0x3704530F5FEAALL, 0x5FBC41D8CAAB0644LL, 0xA0ELL, "divdouble123");
	cdivdouble(0xC29170CA8B79A8A5LL, 0x9881FCBBA56C571DLL, 0xF5B3LL,
			0xCAB9AB545F96LL, 0x47A59D7914F8AAE4LL, 0xA5B1LL, "divdouble124");
	cdivdouble(0x8D68715C6EB895CELL, 0x5F1EC5D0004F7216LL, 0xD214ALL,
			0xAC5123BF4D5LL, 0x4170C3558F5A9B3DLL, 0x2B574LL, "divdouble125");
	cdivdouble(0x499E0AE7232B436BLL, 0xD2C4D87E73C761AFLL, 0x57A800LL,
			0xD6FFD9BFF7LL, 0xBB6844D8898AFAD7LL, 0x1949AFLL, "divdouble126");
	cdivdouble(0x4B5279BEAB553189LL, 0x99C3E0A9DFE20FDFLL, 0x85799DALL,
			0x90770AA44LL, 0x4D6293F05733E7BALL, 0x7FA917BLL, "divdouble127");
	cdivdouble(0x792989F96353DA7ALL, 0x35899E241F33D55BLL, 0x35F7C547LL,
			0x23EBDB2E9LL, 0xB558F2E392C3BD78LL, 0x2354F113LL, "divdouble128");
	cdivdouble(0xA58A6F0D08DF61FBLL, 0xFC1308C5C10E8407LL, 0xB9410789LL,
			0xE4C22DAFLL, 0x54AC79D6836CC2CALL, 0x33CBFEDLL, "divdouble129");
	cdivdouble(0xFD3795F1DBFC7FF3LL, 0x6BD0EF193479654DLL, 0x4B1D72BE69LL,
			0x35EFDA8LL, 0xA023F2163251498FLL, 0x3947DC17A6LL, "divdouble130");
	cdivdouble(0xF2B57158BC550261LL, 0xF0EC7D02FCF682D4LL, 0xE5A377F59D8LL,
			0x10E921LL, 0x39E7570D38B1621FLL, 0x42B25CDF1ACLL, "divdouble131");
	cdivdouble(0xE58CA918454B46CFLL, 0xB0D09388A4450341LL, 0x23CC3A9BF78FLL,
			0x66992LL, 0xC08E50330085C7EELL, 0x15964C89B34FLL, "divdouble132");
	cdivdouble(0x6CDA258D9F8AA69CLL, 0xFCBB146C0A4B4C6ELL, 0x9C6A341795F12LL,
			0xB22LL, 0x7C649E3B091379CALL, 0x808285240C63ALL, "divdouble133");
	cdivdouble(0xA3660A3EF7A7CE7LL, 0x7019E0614E7AAB25LL, 0xA28CF238B5E6CELL,
			0x10LL, 0x155E660090964933LL, 0x19198B06B8F21BLL, "divdouble134");
	cdivdouble(0x862DD0DF52FCB080LL, 0x43F2E4DE9D4B818ALL, 0x9D53822A94CD1C6LL,
			0xDLL, 0xA55C21FD427C14F1LL, 0x48B10A93AE8E24LL, "divdouble135");
	cdivdouble(0xEB418051DAEDD559LL, 0xCD22ED8E053BC4B0LL, 0x3F3FD56EFD8C5C75LL,
			0x3LL, 0xB8310DBE4B3B0CD2LL, 0x1EA48659DDCB70B6LL, "divdouble136");
#elif defined BIGNUM_CODE_32BIT
	cdivdouble(0x0LL, 0xCLL, 0xALL,
			0x0LL, 0x1LL, 0x2LL, "divdouble1");
	cdivdouble(0x0LL, 0x36LL, 0xFLL,
			0x0LL, 0x3LL, 0x9LL, "divdouble2");
	cdivdouble(0x0LL, 0xABLL, 0x4BLL,
			0x0LL, 0x2LL, 0x15LL, "divdouble3");
	cdivdouble(0x0LL, 0xB7DLL, 0x6LL,
			0x0LL, 0x1EALL, 0x1LL, "divdouble4");
	cdivdouble(0x0LL, 0xB78LL, 0xCCLL,
			0x0LL, 0xELL, 0x50LL, "divdouble5");
	cdivdouble(0x0LL, 0x26ELL, 0x9E8LL,
			0x0LL, 0x0LL, 0x26ELL, "divdouble6");
	cdivdouble(0x0LL, 0x7938LL, 0xFLL,
			0x0LL, 0x814LL, 0xCLL, "divdouble7");
	cdivdouble(0x0LL, 0xEE5ALL, 0x4BLL,
			0x0LL, 0x32DLL, 0x2BLL, "divdouble8");
	cdivdouble(0x0LL, 0x81F6LL, 0x135LL,
			0x0LL, 0x6BLL, 0xCFLL, "divdouble9");
	cdivdouble(0x0LL, 0xFB0CLL, 0x47EBLL,
			0x0LL, 0x3LL, 0x234BLL, "divdouble10");
	cdivdouble(0x0LL, 0xA723LL, 0xALL,
			0x0LL, 0x10B6LL, 0x7LL, "divdouble11");
	cdivdouble(0x0LL, 0x436A4LL, 0xCELL,
			0x0LL, 0x53CLL, 0x5CLL, "divdouble12");
	cdivdouble(0x0LL, 0xB299FLL, 0x7FFLL,
			0x0LL, 0x165LL, 0x304LL, "divdouble13");
	cdivdouble(0x0LL, 0x978B9LL, 0xBB0DLL,
			0x0LL, 0xCLL, 0xB41DLL, "divdouble14");
	cdivdouble(0x0LL, 0xE4A44LL, 0x2C6B3LL,
			0x0LL, 0x5LL, 0x68C5LL, "divdouble15");
	cdivdouble(0x0LL, 0x5A1388LL, 0x9LL,
			0x0LL, 0xA022BLL, 0x5LL, "divdouble16");
	cdivdouble(0x0LL, 0xE48925LL, 0xEBLL,
			0x0LL, 0xF8F5LL, 0x3ELL, "divdouble17");
	cdivdouble(0x0LL, 0xE9C55BLL, 0x7F6LL,
			0x0LL, 0x1D5DLL, 0x2FDLL, "divdouble18");
	cdivdouble(0x0LL, 0x337DFFLL, 0xE43LL,
			0x0LL, 0x39CLL, 0x42BLL, "divdouble19");
	cdivdouble(0x0LL, 0xBEE1C7LL, 0x61ECCLL,
			0x0LL, 0x1FLL, 0x12713LL, "divdouble20");
	cdivdouble(0x0LL, 0xE58D23LL, 0x65A140LL,
			0x0LL, 0x2LL, 0x1A4AA3LL, "divdouble21");
	cdivdouble(0x0LL, 0x381A8BCLL, 0xFLL,
			0x0LL, 0x3BD80CLL, 0x8LL, "divdouble22");
	cdivdouble(0x0LL, 0x957B4C4LL, 0x56LL,
			0x0LL, 0x1BCF7FLL, 0x1ALL, "divdouble23");
	cdivdouble(0x0LL, 0x2C5ECC7LL, 0xD75LL,
			0x0LL, 0x34C1LL, 0x392LL, "divdouble24");
	cdivdouble(0x0LL, 0x16384ELL, 0xAF08LL,
			0x0LL, 0x20LL, 0x574ELL, "divdouble25");
	cdivdouble(0x0LL, 0xB3BB4BALL, 0x6D51ALL,
			0x0LL, 0x1A4LL, 0x61612LL, "divdouble26");
	cdivdouble(0x0LL, 0xB970396LL, 0x55D39CLL,
			0x0LL, 0x22LL, 0x30E8DELL, "divdouble27");
	cdivdouble(0x0LL, 0x47D9F01LL, 0x8252C72LL,
			0x0LL, 0x0LL, 0x47D9F01LL, "divdouble28");
	cdivdouble(0xE8A58CA5LL, 0x6AFF1904LL, 0xBLL,
			0x1526529ALL, 0xACA2D3BALL, 0x6LL, "divdouble29");
	cdivdouble(0xCBDA6DA2LL, 0x7E9E592ALL, 0x28LL,
			0x518A924LL, 0xFF728A1LL, 0x2LL, "divdouble30");
	cdivdouble(0x4FB2D08FLL, 0xD0D6DE5FLL, 0x1D0LL,
			0x2BF8B9LL, 0xB07338C1LL, 0x8FLL, "divdouble31");
	cdivdouble(0xFD62F5CCLL, 0x829CB4CLL, 0x1B02LL,
			0x961C8LL, 0xF8AB8310LL, 0x152CLL, "divdouble32");
	cdivdouble(0x8A468776LL, 0x8936BA53LL, 0x816F8LL,
			0x1117LL, 0xBDC4F8A1LL, 0x85BLL, "divdouble33");
	cdivdouble(0xD1B938FLL, 0x7DEFFCE6LL, 0x23078ELL,
			0x5FLL, 0xCAF7C675LL, 0x13B500LL, "divdouble34");
	cdivdouble(0x3C874CA9LL, 0x29093F6CLL, 0x7D01604LL,
			0x7LL, 0xBF502401LL, 0xE09968LL, "divdouble35");
	cdivdouble(0xA06893FALL, 0x70072AACLL, 0x522FE9CALL,
			0x1LL, 0xF3A5A96BLL, 0x778193ELL, "divdouble36");
#else
	cdivdouble(0x0LL, 0x6LL, 0x1LL,
			0x0LL, 0x6LL, 0x0LL, "divdouble1");
	cdivdouble(0x5ELL, 0x7DLL, 0x1LL,
			0x5ELL, 0x7DLL, 0x0LL, "divdouble2");
	cdivdouble(0x7DLL, 0xFFLL, 0x3CLL,
			0x2LL, 0x19LL, 0x23LL, "divdouble3");
#endif

	RETURN;
}

static int test_divdouble_all(void)
{
#ifdef BIGNUM_CODE_8BIT
	bigtype a1, a2, b, c, a, r, q;

	for (a = 0; a <= 0xFFFF; a++) {
		for (b = 1; b <= 0xFF; b++) {
			a1 = a >> 8;
			a2 = a & 0xFF;
			c = 0;
			divdouble(&a1, &a2, b, &c);
			q = a / b;
			r = a % b;
			if (((a1 << 8) | a2) != q || c != r) {
				fprintf(stderr, "divdouble_all error\n");
				fprintf(stderr, "%04x / %02x\n", (unsigned)a, (unsigned)b);
				fprintf(stderr, "divdouble: quot = %02x-%02x\n",
						(unsigned)a1, (unsigned)a2);
				fprintf(stderr, "divdouble: rem  = %02x\n", (unsigned)c);
				fprintf(stderr, "    check: quot = %04x\n", (unsigned)q);
				fprintf(stderr, "    check: rem  = %02x\n", (unsigned)r);
				return 1;
			}
		}
	}
#endif

	return 0;
}

static int test_divcarry4_half(void)
{
	bigtype r, a, b, c;

	a = 100;
	b = 6;
	r = c = 0;
	divcarry4_half(&r, a, b, &c);
	test(r == 16, "divcarry4_half1");
	test(c == 4, "divcarry4_half2");

#if defined BIGNUM_CODE_64BIT
	c = 0x300C33AALL;
	a = 0x213CD54F92AFF976LL;
	b = 0xED9574E9LL;
	r = 0;
	divcarry4_half(&r, a, b, &c);
	test(r == 0x33C5A54005A005C8LL, "divcarry4_half3");
	test(c == 0x6C04166ELL, "divcarry4_half4");
#elif defined BIGNUM_CODE_32BIT
	c = 0x5E06LL;
	a = 0x759C7C8BLL;
	b = 0x9A2DLL;
	r = 0;
	divcarry4_half(&r, a, b, &c);
	test(r == 0x9C1F9C59LL, "divcarry4_half3");
	test(c == 0x76E6LL, "divcarry4_half4");
#else
	c = 0x05LL;
	a = 0x8DLL;
	b = 0x0ALL;
	r = 0;
	divcarry4_half(&r, a, b, &c);
	test(r == 0x8ELL, "divcarry4_half5");
	test(c == 0x01LL, "divcarry4_half6");
#endif

	RETURN;
}

static int test_divcarry4_full(void)
{
	bigtype r, a, b, c;

	a = 100;
	b = 30;
	r = c = 0;
	divcarry4_full(&r, a, b, &c);
	test(r == 3, "divcarry4_full1");
	test(c == 10, "divcarry4_full2");

#if defined BIGNUM_CODE_64BIT
	c = 0x2D7744F431BDF497LL;
	a = 0xF29BA2714420DFECLL;
	b = 0x858F4F7434A61EB1LL;
	divcarry4_full(&r, a, b, &c);
	test(r == 0x572580E156F6E691LL, "divcarry4_full3");
	test(c == 0x3BC45AD03E6077ABLL, "divcarry4_full4");
#elif defined BIGNUM_CODE_32BIT
	c = 0xB0F02E23LL;
	a = 0x41484775LL;
	b = 0xBA164537LL;
	divcarry4_full(&r, a, b, &c);
	test(r == 0xF369FC2BLL, "divcarry4_full3");
	test(c == 0x7AD98338LL, "divcarry4_full4");
#else
	c = 0x22LL;
	a = 0xDCLL;
	b = 0xBELL;
	divcarry4_full(&r, a, b, &c);
	test(r == 0x2ELL, "divcarry4_full3");
	test(c == 0xB8LL, "divcarry4_full4");
#endif

	RETURN;
}


/*
 *  division
 */
static int test_div_noexpand(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, quot, rem, cons;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_alloc(local, &left, SignPlus, 100);
	bignum_value_alloc(local, &right, SignPlus, 30);
	div_noexpand(local, &quot, &rem, left, right);
	test(equal_value_bignum(quot, SignPlus, 3), "div_noexpand1");
	test(equal_value_bignum(rem, SignPlus, 10), "div_noexpand2");

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1"
			"fbc8730bdecb6d07b35e0b6ef1afad9bb80b2885934cb296513a351650f17914"
			"311a68bd1425297af0ebc4c7412aa3d0544ac65ba3d62912244f7126efe7c7f3"
			"b6f5873f9800d96c1f1f2cd5ec5fa24d1dade512d4c28c589723eac902940291"
			"6179d96c9ae2a65fdd9ae069df581675b2bef285f083fecaa346cce54e7a3f8f"
			"0e15ce9c4b8e824a8beab21a4fb349c9c828566e3cae9eda4cb7d18c9739a7a4"
			"0dcae5702352fdec5a2085b068fb5e2d883353c33e1bfda9abc56cdb6dbdbafc");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"27193fb1dc622e9bac9d7f2f5d0960e192ecd39c548afae22126b12e6d661da2"
			"05aa3734578797d8114560c3359d52a74b7c04b4b61d2b041ac0219c258bd240"
			"390d57eff6764b1062899b6057cd0d86e8621b3fd9ddb9253b47e799b7fd5211");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	div_noexpand(local, &quot, &rem, left, right);

	setchar_bigcons(local, cons, 16,
			"38e4dc55042132062efde19fc3de51b672efb9cc835e7618245b9df3c7f0be62"
			"2c6baa99f2e630f44435c5dbc6bd49897153ed487fbe0319b2607adce0312797"
			"f33ae76140651edf76c646956499012e491c53f071b93f192a9722fdeff7f853"
			"51e3a8223f74674c54123c0eda30a8b5bd50cab3ae8993ea0c4b16c28f87e293"
			"c");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"20a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902"
			"a65a4d7a70a7aa382db3b74725abc0b0c0cd31fc31e5ecab4db0a5c1cc5bb8b4"
			"8ab331aa037fbfea6038f1b3c383504ce9d38d4c42493171faf89185dfdbc600");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(quot, left), "div_noexpand3");
	test(equal_bb_real(rem, right), "div_noexpand4");

	rollback_local(local, stack);

	RETURN;
}

static int test_letdiv_noexpand(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1"
			"fbc8730bdecb6d07b35e0b6ef1afad9bb80b2885934cb296513a351650f17914"
			"311a68bd1425297af0ebc4c7412aa3d0544ac65ba3d62912244f7126efe7c7f3"
			"b6f5873f9800d96c1f1f2cd5ec5fa24d1dade512d4c28c589723eac902940291"
			"6179d96c9ae2a65fdd9ae069df581675b2bef285f083fecaa346cce54e7a3f8f"
			"0e15ce9c4b8e824a8beab21a4fb349c9c828566e3cae9eda4cb7d18c9739a7a4"
			"0dcae5702352fdec5a2085b068fb5e2d883353c33e1bfda9abc56cdb6dbdbafc");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"27193fb1dc622e9bac9d7f2f5d0960e192ecd39c548afae22126b12e6d661da2"
			"05aa3734578797d8114560c3359d52a74b7c04b4b61d2b041ac0219c258bd240"
			"390d57eff6764b1062899b6057cd0d86e8621b3fd9ddb9253b47e799b7fd5211");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	letdiv_noexpand(local, left, right);

	setchar_bigcons(local, cons, 16,
			"38e4dc55042132062efde19fc3de51b672efb9cc835e7618245b9df3c7f0be62"
			"2c6baa99f2e630f44435c5dbc6bd49897153ed487fbe0319b2607adce0312797"
			"f33ae76140651edf76c646956499012e491c53f071b93f192a9722fdeff7f853"
			"51e3a8223f74674c54123c0eda30a8b5bd50cab3ae8993ea0c4b16c28f87e293"
			"c");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "letdiv_noexpand1");

	rollback_local(local, stack);

	RETURN;
}

static int test_letdivvalue_buffer(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	letdivvalue_buffer(left, 0xB1);
	setchar_bigcons(local, cons, 16,
			"c91528b4ea24e11535de9b14a20a3eda0a9449606350e69de4b7c25ad81a26");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "letdivvalue_buffer1");

	rollback_local(local, stack);

	RETURN;
}

static int test_letdiv_noexpand_bigdata(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);

	/* zero */
	bignum_zero_alloc(local, &left);
	bignum_value_alloc(local, &right, SignPlus, 10);
	letdiv_noexpand_bigdata(local, left, right);
	test(zerop_bignum(left), "letdiv_noexpand_bigdata1");

	/* size1 == size2 == 1 */
	bignum_value_alloc(local, &left, SignPlus, 20);
	bignum_value_alloc(local, &right, SignPlus, 3);
	letdiv_noexpand_bigdata(local, left, right);
	test(equal_value_bignum(left, SignPlus, 6), "letdiv_noexpand_bigdata2");

	/* right == 1 */
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_value_alloc(local, &right, SignPlus, 1);
	letdiv_noexpand_bigdata(local, left, right);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "letdiv_noexpand_bigdata4");

	/* size2 == 1 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_value_alloc(local, &right, SignPlus, 0xB1);
	letdiv_noexpand_bigdata(local, left, right);
	setchar_bigcons(local, cons, 16,
			"c91528b4ea24e11535de9b14a20a3eda0a9449606350e69de4b7c25ad81a26");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "letdiv_noexpand_bigdata5");

	/* compare == 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_cons_alloc(local, &right, SignPlus, cons);
	letdiv_noexpand_bigdata(local, left, right);
	test(equal_value_bignum(left, SignPlus, 1), "letdiv_noexpand_bigdata6");

	/* compare < 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	letdiv_noexpand_bigdata(local, left, right);
	test(zerop_bignum(left), "letdiv_noexpand_bigdata7");

	/* compare > 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16, "27193fb1dc622e9bac9d7f2f5d0960e1");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	letdiv_noexpand_bigdata(local, left, right);
	setchar_bigcons(local, cons, 16, "38e4dc55042132062efde19fc3de51b74");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "letdiv_noexpand_bigdata8");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  setrem
 */
static int test_setrem_noexpand(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1"
			"fbc8730bdecb6d07b35e0b6ef1afad9bb80b2885934cb296513a351650f17914"
			"311a68bd1425297af0ebc4c7412aa3d0544ac65ba3d62912244f7126efe7c7f3"
			"b6f5873f9800d96c1f1f2cd5ec5fa24d1dade512d4c28c589723eac902940291"
			"6179d96c9ae2a65fdd9ae069df581675b2bef285f083fecaa346cce54e7a3f8f"
			"0e15ce9c4b8e824a8beab21a4fb349c9c828566e3cae9eda4cb7d18c9739a7a4"
			"0dcae5702352fdec5a2085b068fb5e2d883353c33e1bfda9abc56cdb6dbdbafc");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"27193fb1dc622e9bac9d7f2f5d0960e192ecd39c548afae22126b12e6d661da2"
			"05aa3734578797d8114560c3359d52a74b7c04b4b61d2b041ac0219c258bd240"
			"390d57eff6764b1062899b6057cd0d86e8621b3fd9ddb9253b47e799b7fd5211");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	setrem_noexpand(local, left, left, right);

	setchar_bigcons(local, cons, 16,
			"20a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902"
			"a65a4d7a70a7aa382db3b74725abc0b0c0cd31fc31e5ecab4db0a5c1cc5bb8b4"
			"8ab331aa037fbfea6038f1b3c383504ce9d38d4c42493171faf89185dfdbc600");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "setrem_noexpand1");

	rollback_local(local, stack);

	RETURN;
}

static int test_remvalue_buffer(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, cons;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	test(remvalue_buffer(left, 0xB1) == 0x5B, "remvalue_buffer1");

	rollback_local(local, stack);

	RETURN;
}

static int test_setrem_noexpand_bigdata(void)
{
	LocalRoot local;
	LocalStack stack;
	addr set, left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);

	/* left == zero */
	bignum_zero_alloc(local, &left);
	bignum_value_alloc(local, &right, SignPlus, 10);
	alloc_bignum(local, &set, 10);
	setrem_noexpand_bigdata(local, set, left, right);
	test(zerop_bignum(set), "setrem_noexpand_bigdata1");

	/* size1 == size2 == 0 */
	bignum_value_alloc(local, &left, SignPlus, 100);
	bignum_value_alloc(local, &right, SignPlus, 23);
	setzero_bignum(set);
	setrem_noexpand_bigdata(local, set, left, right);
	test(equal_value_bignum(set, SignPlus, 8), "setrem_noexpand_bigdata2");

	/* right == 1 */
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_value_alloc(local, &right, SignPlus, 1);
	setrem_noexpand_bigdata(local, set, left, right);
	test(zerop_bignum(set), "setrem_noexpand_bigdata3");

	/* size2 == 1 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_value_alloc(local, &right, SignPlus, 0xB1);
	setrem_noexpand_bigdata(local, set, left, right);
	test(equal_value_bignum(set, SignPlus, 0x5B), "setrem_noexpand_bigdata4");

	/* compare == 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	bignum_cons_alloc(local, &right, SignPlus, cons);
	setrem_noexpand_bigdata(local, set, left, right);
	test(zerop_bignum(set), "setrem_noexpand_bigdata5");

	/* compare < 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	setrem_noexpand_bigdata(local, set, left, right);
	test(equal_bb_real(set, left), "setrem_noexpand_bigdata6");

	/* compare > 0 */
	setchar_bigcons(local, cons, 16,
			"8b07a12515e37f9faa3ee93944091574c15086bba4aaef732b230d60cf6a14a1");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	setchar_bigcons(local, cons, 16, "27193fb1dc622e9bac9d7f2f5d0960e1");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	setrem_noexpand_bigdata(local, set, left, right);
	setchar_bigcons(local, cons, 16, "155735ccd816fd17b702a12563ad73ad");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	test(equal_bb_real(set, left), "setrem_noexpand_bigdata7");

	rollback_local(local, stack);

	RETURN;
}

static int test_divrem_bigdata_local(void)
{
	addr left, right, quot, rem;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 0);
	bignum_value_local(local, &right, SignMinus, 10);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	test(zerop_bignum(quot), "divrem_bigdata_local1");
	test(zerop_bignum(rem), "divrem_bigdata_local2");

	bignum_value_local(local, &left, SignPlus, 26);
	bignum_value_local(local, &right, SignMinus, 3);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	bignum_value_local(local, &left, SignPlus, 8);
	test(equal_bb_real(left, quot), "divrem_bigdata_local3");
	bignum_value_local(local, &left, SignPlus, 2);
	test(equal_bb_real(left, rem), "divrem_bigdata_local4");

	bignum_value2_local(local, &left, SignMinus, 11, 22);
	bignum_value_local(local, &right, SignPlus, 1);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	bignum_value2_local(local, &left, SignPlus, 11, 22);
	test(equal_bb_real(left, quot), "divrem_bigdata_local5");
	test(zerop_bignum(rem), "divrem_bigdata_local6");

	bignum_value2_local(local, &left, SignMinus, 8, 32+3);
	bignum_value_local(local, &right, SignPlus, 4);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	bignum_value2_local(local, &left, SignPlus, 2, 8);
	test(equal_bb_real(left, quot), "divrem_bigdata_local7");
	bignum_value_local(local, &left, SignPlus, 3);
	test(equal_bb_real(left, rem), "divrem_bigdata_local8");

	bignum_value2_local(local, &left, SignMinus, 11, 22);
	bignum_value2_local(local, &right, SignPlus, 11, 22);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	bignum_value_local(local, &left, SignPlus, 1);
	test(equal_bb_real(left, quot), "divrem_bigdata_local9");
	test(zerop_bignum(rem), "divrem_bigdata_local10");

	bignum_value2_local(local, &left, SignMinus, 11, 12);
	bignum_value2_local(local, &right, SignPlus, 11, 22);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	test(zerop_bignum(quot), "divrem_bigdata_local11");
	bignum_value2_local(local, &left, SignPlus, 11, 12);
	test(equal_bb_real(left, rem), "divrem_bigdata_local12");

	bignum_value2_local(local, &left, SignMinus, 11, 22);
	bignum_value2_local(local, &right, SignPlus,  1, 22);
	divrem_bigdata_local(local, &quot, &rem, left, right);
	GetSizeBignum(quot, &size);
	test(size == 1, "divrem_bigdata_local13");
	GetSizeBignum(rem, &size);
	test(size, "divrem_bigdata_local14");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  shift
 */
static int test_power2_bigdata_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, cons;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);

	power2_bigdata_alloc(local, &left, 0);
	test(equal_value_nosign_bignum(left, 1), "power2_bigdata_alloc1");

	power2_bigdata_alloc(local, &left, 1);
	test(equal_value_nosign_bignum(left, 2), "power2_bigdata_alloc2");

	power2_bigdata_alloc(local, &left, 2);
	test(equal_value_nosign_bignum(left, 4), "power2_bigdata_alloc3");

	power2_bigdata_alloc(local, &left, 3);
	test(equal_value_nosign_bignum(left, 8), "power2_bigdata_alloc4");

	power2_bigdata_alloc(local, &left, 400);
	setchar_bigcons(local, cons, 16,
			"100000000000000000000000000000000000000000000000000"
			"00000000000000000000000000000000000000000000000000");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "power2_bigdata_alloc5");

	power2_bigdata_alloc(local, &left, 399);
	setchar_bigcons(local, cons, 16,
			"80000000000000000000000000000000000000000000000000"
			"00000000000000000000000000000000000000000000000000");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "power2_bigdata_alloc6");

	power2_bigdata_alloc(local, &left, 398);
	setchar_bigcons(local, cons, 16,
			"40000000000000000000000000000000000000000000000000"
			"00000000000000000000000000000000000000000000000000");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "power2_bigdata_alloc7");

	rollback_local(local, stack);

	RETURN;
}

static int test_shiftup_bigdata_const(LocalRoot local, size_t shift, const char *str)
{
	int result;
	addr left, right, cons;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);

	setchar_bigcons(local, cons, 16,
			"20a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902"
			"a65a4d7a70a7aa382db3b74725abc0b0c0cd31fc31e5ecab4db0a5c1cc5bb8b4"
			"8ab331aa037fbfea6038f1b3c383504ce9d38d4c42493171faf89185dfdbc600");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	shiftup_bigdata_alloc(local, &left, left, shift);
	setchar_bigcons(local, cons, 16, str);
	bignum_cons_alloc(local, &right, SignPlus, cons);
	result = equal_bb_real(left, right);

	rollback_local(local, stack);

	return result;
}

static int test_shiftup_bigdata_alloc(void)
{
	addr left;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 11);
	shiftup_bigdata_alloc(local, &left, left, 0);
	test(equal_value_nosign_bignum(left, 11), "shiftup_bigdata_alloc1");

	bignum_zero_local(local, &left);
	shiftup_bigdata_alloc(local, &left, left, 100);
	test(zerop_bignum(left), "shiftup_bigdata_alloc2");

	test(test_shiftup_bigdata_const(local, 1,
				"414dc5be548e57fbd53009c412d5f8d3ee7a2c756aa7b2fc91677605681e3205"
				"4cb49af4e14f54705b676e8e4b578161819a63f863cbd9569b614b8398b77169"
				"1566635406ff7fd4c071e3678706a099d3a71a98849262e3f5f1230bbfb78c00"),
			"shiftup_bigdata_alloc3");
	test(test_shiftup_bigdata_const(local, 2,
				"829b8b7ca91caff7aa60138825abf1a7dcf458ead54f65f922ceec0ad03c640a"
				"996935e9c29ea8e0b6cedd1c96af02c30334c7f0c797b2ad36c29707316ee2d2"
				"2accc6a80dfeffa980e3c6cf0e0d4133a74e35310924c5c7ebe246177f6f1800"),
			"shiftup_bigdata_alloc4");
	test(test_shiftup_bigdata_const(local, 8,
				"20a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902"
				"a65a4d7a70a7aa382db3b74725abc0b0c0cd31fc31e5ecab4db0a5c1cc5bb8b4"
				"8ab331aa037fbfea6038f1b3c383504ce9d38d4c42493171faf89185dfdbc600"
				"00"),
			"shiftup_bigdata_alloc5");
	test(test_shiftup_bigdata_const(local, 254,
				"829b8b7ca91caff7aa60138825abf1a7dcf458ead54f65f922ceec0ad03c640a"
				"996935e9c29ea8e0b6cedd1c96af02c30334c7f0c797b2ad36c29707316ee2d2"
				"2accc6a80dfeffa980e3c6cf0e0d4133a74e35310924c5c7ebe246177f6f1800"
				"000000000000000000000000000000000000000000000000000000000000000"),
			"shiftup_bigdata_alloc6");
	test(test_shiftup_bigdata_const(local, 255,
				"1053716f952395fef54c027104b57e34fb9e8b1d5aa9ecbf2459dd815a078c81"
				"532d26bd3853d51c16d9dba392d5e058606698fe18f2f655a6d852e0e62ddc5a"
				"455998d501bfdff5301c78d9e1c1a82674e9c6a6212498b8fd7c48c2efede300"
				"0000000000000000000000000000000000000000000000000000000000000000"),
			"shiftup_bigdata_alloc7");
	test(test_shiftup_bigdata_const(local, 256,
				"20a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902"
				"a65a4d7a70a7aa382db3b74725abc0b0c0cd31fc31e5ecab4db0a5c1cc5bb8b4"
				"8ab331aa037fbfea6038f1b3c383504ce9d38d4c42493171faf89185dfdbc600"
				"0000000000000000000000000000000000000000000000000000000000000000"),
			"shiftup_bigdata_alloc8");
	test(test_shiftup_bigdata_const(local, 257,
				"414dc5be548e57fbd53009c412d5f8d3ee7a2c756aa7b2fc91677605681e3205"
				"4cb49af4e14f54705b676e8e4b578161819a63f863cbd9569b614b8398b77169"
				"1566635406ff7fd4c071e3678706a099d3a71a98849262e3f5f1230bbfb78c00"
				"0000000000000000000000000000000000000000000000000000000000000000"),
			"shiftup_bigdata_alloc9");
	test(test_shiftup_bigdata_const(local, 258,
				"829b8b7ca91caff7aa60138825abf1a7dcf458ead54f65f922ceec0ad03c640a"
				"996935e9c29ea8e0b6cedd1c96af02c30334c7f0c797b2ad36c29707316ee2d2"
				"2accc6a80dfeffa980e3c6cf0e0d4133a74e35310924c5c7ebe246177f6f1800"
				"0000000000000000000000000000000000000000000000000000000000000000"),
			"shiftup_bigdata_alloc10");
	rollback_local(local, stack);

	RETURN;
}

static int test_division2_bigdata_alloc(void)
{
	addr left, right, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);

	bignum_zero_local(local, &left);
	division2_bigdata_alloc(local, &left, left);
	test(equal_value_nosign_bignum(left, 0), "division2_bigdata_alloc1");

	bignum_value_local(local, &left, SignMinus, 1);
	division2_bigdata_alloc(local, &left, left);
	test(equal_value_nosign_bignum(left, 0), "division2_bigdata_alloc2");

	setchar_bigcons(local, cons, 16,
			"320a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f1902");
	bignum_cons_alloc(local, &left, SignPlus, cons);
	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"19053716f952395fef54c027104b57e34fb9e8b1d5aa9ecbf2459dd815a078c81");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc3");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"c829b8b7ca91caff7aa60138825abf1a7dcf458ead54f65f922ceec0ad03c640");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc4");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"6414dc5be548e57fbd53009c412d5f8d3ee7a2c756aa7b2fc91677605681e320");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc5");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"320a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f190");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc6");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"19053716f952395fef54c027104b57e34fb9e8b1d5aa9ecbf2459dd815a078c8");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc7");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"c829b8b7ca91caff7aa60138825abf1a7dcf458ead54f65f922ceec0ad03c64");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc8");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"6414dc5be548e57fbd53009c412d5f8d3ee7a2c756aa7b2fc91677605681e32");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc9");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"320a6e2df2a472bfdea9804e2096afc69f73d163ab553d97e48b3bb02b40f19");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc10");

	division2_bigdata_alloc(local, &left, left);
	setchar_bigcons(local, cons, 16,
			"19053716f952395fef54c027104b57e34fb9e8b1d5aa9ecbf2459dd815a078c");
	bignum_cons_alloc(local, &right, SignPlus, cons);
	test(equal_bb_real(left, right), "division2_bigdata_alloc11");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  output
 */
static int test_letdiv_half_bigdata(void)
{
	addr pos, cons, check;
	LocalRoot local;
	LocalStack stack;
	bigtype m;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_alloc(local, &pos, SignPlus, 33);
	m = letdiv_half_bigdata(pos, 7);
	test(m == 5, "letdiv_half_bigdata1");
	test(equal_value_nosign_bignum(pos, 4), "letdiv_half_bigdata2");

	bigcons_char_local(local, &cons, 10, "123456789012345678901234567890");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	m = letdiv_half_bigdata(pos, 11);
	test(m == 7, "letdiv_half_bigdata3");

	setchar_bigcons(local, cons, 10, "11223344455667788991021324353");
	bignum_cons_alloc(local, &check, SignPlus, cons);
	test(equal_bb_real(pos, check), "letdiv_half_bigdata4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_bigdata(void)
{
	/* Calculate */
	TestBreak(test_multinumber1);
	TestBreak(test_multinumber2);
	TestBreak(test_plusnumber);
	TestBreak(test_plusnumber3);
	TestBreak(test_pluscarry);
	TestBreak(test_pluscarry4);
	TestBreak(test_multicarry);
	TestBreak(test_multicarry_bigdata);
	TestBreak(test_multicarry4);
	/* compare */
	TestBreak(test_equal_bigdata);
	TestBreak(test_compare_bigdata);
	/* plus / minus */
	TestBreak(test_minusnumber);
	TestBreak(test_minusnumber3);
	TestBreak(test_minuscarry);
	TestBreak(test_minuscarry4);
	TestBreak(test_TailCopy);
	TestBreak(test_setplusvalue_bigdata);
	TestBreak(test_plusvalue_bigdata_alloc);
	TestBreak(test_setminusvalue_bigdata);
	TestBreak(test_minusvalue_bigdata_alloc);
	TestBreak(test_plusloop);
	TestBreak(test_plus_bigdata_alloc);
	TestBreak(test_letplus_noexpand_bigdata);
	TestBreak(test_minus_bigdata_alloc);
	TestBreak(test_minuscheck_bigdata_alloc);
	TestBreak(test_setminus_noexpand);
	TestBreak(test_letminus_noexpand_bigdata);
	/* multiple */
	TestBreak(test_multicarry_fixnum);
	TestBreak(test_multicarry_bignum);
	TestBreak(test_setmultivalue_bigdata);
	TestBreak(test_setmulti_bigdata);
	TestBreak(test_multi_bigdata_alloc);
	/* division */
	TestBreak(test_divhalf);
	TestBreak(test_getshiftvalue);
	TestBreak(test_divfull);
	TestBreak(test_divdouble);
	TestBreak(test_divdouble_all);
	TestBreak(test_divcarry4_half);
	TestBreak(test_divcarry4_full);
	/* division */
	TestBreak(test_div_noexpand);
	TestBreak(test_letdiv_noexpand);
	TestBreak(test_letdivvalue_buffer);
	TestBreak(test_letdiv_noexpand_bigdata);
	/* setrem */
	TestBreak(test_setrem_noexpand);
	TestBreak(test_remvalue_buffer);
	TestBreak(test_setrem_noexpand_bigdata);
	TestBreak(test_divrem_bigdata_local);
	/* shift */
	TestBreak(test_power2_bigdata_alloc);
	TestBreak(test_shiftup_bigdata_alloc);
	TestBreak(test_division2_bigdata_alloc);
	/* output */
	TestBreak(test_letdiv_half_bigdata);

	return 0;
}

int test_bigdata(void)
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
		lisp_initialize = 1;
		result = testbreak_bigdata();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

