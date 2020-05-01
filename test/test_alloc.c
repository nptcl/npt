#include "alloc.h"
#include "build.h"
#include "degrade.h"

/*
 *  testbreak_alloc
 */
static int test_CopyJmpBuf(void)
{
	byte buffer[2 * sizeof(jmp_buf)];
	jmp_buf jmp;

	memset(buffer, 0, 2 * sizeof(jmp_buf));
	memset(&jmp, 0xAA, sizeof(jmp_buf));
	CopyJmpBuf(buffer, &jmp);
	test(buffer[0] == 0xAA, "CopyJmpBuf.1");
	test(buffer[1] == 0xAA, "CopyJmpBuf.2");
	test(buffer[sizeof(jmp_buf) - 1] == 0xAA, "CopyJmpBuf.3");
	test(buffer[sizeof(jmp_buf)] == 0x00, "CopyJmpBuf.4");

	RETURN;
}

static int test_WtType(void)
{
	byte buffer[2000];
	size_t value;

	memset(buffer, 0xAA, 2000);
	value = 333;
	WtType(buffer + 8, value);
	test(buffer[7] == 0xAA, "WtType.1");
	test(buffer[8 + sizeof(value)] == 0xAA, "WtType.2");
	test(buffer[8] != 0xAA, "WtType.3");
	value = 0;
	RdType(buffer + 8, value);
	test(value == 333, "RdType.1");

	RETURN;
}

static int test_WtTypePtr(void)
{
	byte buffer[2000];
	size_t value, *ptr;

	memset(buffer, 0xAA, 2000);
	value = 444;
	ptr = &value;
	WtTypePtr(buffer + 16, ptr);
	test(buffer[15] == 0xAA, "WtTypePtr.1");
	test(buffer[16 + sizeof(value)] == 0xAA, "WtTypePtr.2");
	test(buffer[16] != 0xAA, "WtTypePtr.3");
	value = 0;
	RdTypePtr(buffer + 16, ptr);
	test(value == 444, "RdTypePtr.1");

	RETURN;
}

static int test_WtByte(void)
{
	int i;
	byte buffer[2000];

	memset(buffer, 0xAA, 2000);
	for (i = 0; i < 10; i++) {
		WtByte(buffer + i, i);
	}
	test(buffer[3] == 3, "WtByte.1");
	test(buffer[9] == 9, "WtByte.2");
	test(buffer[10] == 0xAA, "WtByte.3");
	test(RdByte(buffer + 3) == 3, "ReadByte.1");
	test(RdByte(buffer + 9) == 9, "ReadByte.2");
	test(RdByte(buffer + 10) == 0xAA, "ReadByte.3");

	RETURN;
}


/*
 *  Align Pointer
 */
static int test_Align8Out(void)
{
	byte mem[1000];
	pbyte pos;

	pos = mem + 111;
	pos = (pbyte)(((uintptr_t)pos) & ((~((uintptr_t)0)) ^ 7));

	test(Align8Out(pos + 0) == 0, "Align8Out.1");
	test(Align8Out(pos + 1) == 1, "Align8Out.2");
	test(Align8Out(pos + 7) == 7, "Align8Out.3");
	test(Align8Out(pos + 8) == 0, "Align8Out.4");
	test(Align8Out(pos + 9) == 1, "Align8Out.5");
	test(Align8Space(pos + 0) == 0, "Align8Space.1");
	test(Align8Space(pos + 1) == 7, "Align8Space.2");
	test(Align8Space(pos + 7) == 1, "Align8Space.3");
	test(Align8Space(pos + 8) == 0, "Align8Space.4");
	test(Align8Space(pos + 9) == 7, "Align8Space.5");

	RETURN;
}

static int test_Align8Cut(void)
{
	uintptr_t value, value1, value2;

	test(PointerMask == (Align8Mask - 1UL + 8UL), "Align8Mask.1");
	value = Align8Cut(&value);
	test((value % 8UL) == 0, "Align8Cut.1");
	value = Align8Cut(8UL + (pbyte)&value);
	test((value % 8UL) == 0, "Align8Cut.2");

	value1 = Align8Cut(8UL * 3UL + 4UL);
	value2 = Align8Cut(8UL * 3UL + 2UL);
	test(value1 == value2, "Align8Cut.3");

	value1 = Align8Cut(8UL * 3UL + 4UL);
	value2 = Align8Cut(8UL * 2UL + 2UL);
	test(value1 == (value2 + 8UL), "Align8Cut.4");

	RETURN;
}

static int test_Align8Front(void)
{
	byte mem[0x010000];
	addr pos, a, cut;

	pos = (addr)(Align8Cut(mem) + 8);
	Align8Front(pos, &cut);
	test(pos == cut, "Align8Front.1");

	a = (addr)(((pbyte)pos) - 1);
	Align8Front(a, &cut);
	test(pos == cut, "Align8Front.2");

	a = (addr)(((pbyte)pos) + 3);
	Align8Front(a, &cut);
	test(pos < cut, "Align8Front.3");

	RETURN;
}

static int test_Align8Inplace(void)
{
	byte mem[0x010000];
	addr pos, a, cut;

	pos = (addr)(Align8Cut(mem) + 8);
	cut = (addr)Align8Inplace(pos);
	test(pos == cut, "Align8Inplace.1");

	a = (addr)(((pbyte)pos) - 1);
	cut = (addr)Align8Inplace(a);
	test(pos == cut, "Align8Inplace.2");

	a = (addr)(((pbyte)pos) + 3);
	cut = (addr)Align8Inplace(a);
	test(pos < cut, "Align8Inplace.3");

	RETURN;
}


/*
 *  Align Index
 */
static int test_AlignSize8Out(void)
{
	size_t pos;

	pos = 8000 + 111;
	pos = pos & ((~((size_t)0)) ^ 7);

	test(AlignSize8Out(pos + 0) == 0, "AlignSize8Out.1");
	test(AlignSize8Out(pos + 1) == 1, "AlignSize8Out.2");
	test(AlignSize8Out(pos + 7) == 7, "AlignSize8Out.3");
	test(AlignSize8Out(pos + 8) == 0, "AlignSize8Out.4");
	test(AlignSize8Out(pos + 9) == 1, "AlignSize8Out.5");
	test(AlignSize8Space(pos + 0) == 0, "AlignSize8Space.1");
	test(AlignSize8Space(pos + 1) == 7, "AlignSize8Space.2");
	test(AlignSize8Space(pos + 7) == 1, "AlignSize8Space.3");
	test(AlignSize8Space(pos + 8) == 0, "AlignSize8Space.4");
	test(AlignSize8Space(pos + 9) == 7, "AlignSize8Space.5");

	RETURN;
}

static int test_AlignSize8Cut(void)
{
	size_t value, value1, value2;

	test(IndexMask == (Index8Mask - 1UL + 8UL), "AlignSize8Mask.1");
	value = AlignSize8Cut(&value);
	test((value % 8UL) == 0, "AlignSize8Cut.1");
	value = AlignSize8Cut(8UL + 8UL);
	test((value % 8UL) == 0, "AlignSize8Cut.2");

	value1 = AlignSize8Cut(8UL * 3UL + 4UL);
	value2 = AlignSize8Cut(8UL * 3UL + 2UL);
	test(value1 == value2, "AlignSize8Cut.3");

	value1 = AlignSize8Cut(8UL * 3UL + 4UL);
	value2 = AlignSize8Cut(8UL * 2UL + 2UL);
	test(value1 == (value2 + 8UL), "AlignSize8Cut.4");

	RETURN;
}

static int test_AlignSize8Front(void)
{
	size_t pos, a, cut;

	pos = AlignSize8Cut(8000) + 8;
	AlignSize8Front(pos, &cut);
	test(pos == cut, "AlignSize8Front.1");

	a = pos - 1;
	AlignSize8Front(a, &cut);
	test(pos == cut, "AlignSize8Front.2");

	a = pos + 3;
	AlignSize8Front(a, &cut);
	test(pos < cut, "AlignSize8Front.3");

	RETURN;
}

static int test_AlignSize8Inplace(void)
{
	size_t pos, a, cut;

	pos = AlignSize8Cut(8000) + 8;
	cut = AlignSize8Inplace(pos);
	test(pos == cut, "AlignSize8Inplace.1");

	a = pos - 1;
	cut = AlignSize8Inplace(a);
	test(pos == cut, "AlignSize8Inplace.2");

	a = pos + 3;
	cut = AlignSize8Inplace(a);
	test(pos < cut, "AlignSize8Inplace.3");

	RETURN;
}


/*
 *  memory tools
 */
static int test_sizeoft(void)
{
	int value1;
	char value2[123];
	struct value3type {
		int a;
		char b;
		long c;
	} value3;

	test(sizeoft(value1) == (size_t)sizeof(int), "sizeoft.1");
	test(sizeoft(value2) == (size_t)sizeof(char[123]), "sizeoft.2");
	test(sizeoft(value3) == (size_t)sizeof(struct value3type), "sizeoft.3");

	RETURN;
}

static int test_sizeofm(void)
{
	int value1;
	char value2[123];
	struct value3type {
		int a;
		char b;
		long c;
	} value3;

	test(sizeofm(value1, 3) == (size_t)(3*sizeof(int)), "sizeofm.1");
	test(sizeofm(value2, 4) == (size_t)(4*sizeof(char[123])), "sizeofm.2");
	test(sizeofm(value3, 5) == (size_t)(5*sizeof(struct value3type)), "sizeofm.3");

	RETURN;
}

static int test_malloctype(void)
{
	int i, a;
	uint64_t *ptr, value, array[100];

	/* malloctype */
	ptr = malloctype(uint64_t);
	test(ptr, "malloctype.1");
	*ptr = 100;
	free(ptr);

	ptr = mallocsize(uint64_t, 1000);
	test(ptr, "malloctype.2");
	for (i = 0; i < 1000; i++)
		ptr[i] = i;

	/* mallocsize */
	ptr = reallocsize(ptr, uint64_t, 500);
	test(ptr, "reallocsize.1");
	for (i = 0; i < 500; i++)
		ptr[i] = i*2;

	ptr = reallocsize(ptr, uint64_t, 10000);
	test(ptr, "reallocsize.2");
	for (i = 0; i < 10000; i++)
		ptr[i] = i*3;

	/* cleartype */
	value = 1000;
	cleartype(value);
	test(value == 0, "cleartype.1");

	/* clearsize pointer */
	clearsize(ptr, 10000);
	a = 0;
	for (i = 0; i < 10000; i++) {
		if (ptr[i]) {
			a = 1;
			break;
		}
	}
	test(a == 0, "clearsize.1");

	/* clearsize array */
	for (i = 0; i < 100; i++)
		array[i] = i*5;
	clearsize(array, 100);
	a = 0;
	for (i = 0; i < 10000; i++) {
		if (ptr[i]) {
			a = 1;
			break;
		}
	}
	test(a == 0, "clearsize.2");

	free(ptr);

	RETURN;
}

static int test_mallocsize(void)
{
	void *ptr;

	ptr = mallocsize(int, 0x010000);
	memset(ptr, 0xAA, sizeofm(int, 0x010000));
	ptr = reallocsize(ptr, int, 0x020000);
	memset(ptr, 0xBB, sizeofm(int, 0x020000));
	free(ptr);

	return 0;
}

static int test_clearmemory(void)
{
	int value, *valuep, valuea[10];
	struct checkstr {
		int a;
		float b;
		long c;
	} strv, *strp, stra[10];

	valuep = &value;
	strp = &strv;

	/* clearmemor */
	memset(valuep, 0xAA, sizeof(int));
	clearmemory(valuep, sizeof(int));
	test(value == 0, "clearmemory.1");
	memset(strp, 0xAA, sizeof(struct checkstr));
	clearmemory(strp, sizeof(struct checkstr));
	test(strv.a == 0, "clearmemory.2");
	test(strv.b == 0, "clearmemory.3");
	test(strv.c == 0, "clearmemory.4");

	/* clearpoint */
	memset(valuep, 0xAA, sizeof(int));
	clearpoint(valuep);
	test(value == 0, "clearpoint.1");
	memset(strp, 0xAA, sizeof(struct checkstr));
	clearpoint(strp);
	test(strv.a == 0, "clearpoint.2");
	test(strv.b == 0, "clearpoint.3");
	test(strv.c == 0, "clearpoint.4");

	/* cleartype */
	memset(valuep, 0xAA, sizeof(int));
	cleartype(value);
	test(value == 0, "cleartype.1");
	memset(strp, 0xAA, sizeof(struct checkstr));
	cleartype(strv);
	test(strv.a == 0, "cleartype.2");
	test(strv.b == 0, "cleartype.3");
	test(strv.c == 0, "cleartype.4");

	/* cleartype */
	memset(valuea, 0xAA, sizeof(int) * 10);
	clearsize(valuea, 3);
	test(valuea[0] == 0, "clearsize.1");
	test(valuea[1] == 0, "clearsize.2");
	test(valuea[2] == 0, "clearsize.3");
	test(valuea[3] != 0, "clearsize.4");
	memset(stra, 0xAA, sizeof(struct checkstr) * 10);
	clearsize(stra, 3);
	test(stra[0].a == 0, "clearsize.5");
	test(stra[1].b == 0, "clearsize.6");
	test(stra[2].c == 0, "clearsize.7");
	test(stra[3].a != 0, "clearsize.8");

	RETURN;
}

static int test_aamemory(void)
{
	byte mem[100];

	cleartype(mem);
	aamemory(mem, 10);
	test(mem[0] == 0xAA, "aamemory.1");
	test(mem[9] == 0xAA, "aamemory.2");
	test(mem[10] == 0, "aamemory.3");

	cleartype(mem);
	aatype(mem);
	test(mem[0] == 0xAA, "aatype.1");
	test(mem[99] == 0xAA, "aatype.2");

	RETURN;
}

static int test_GetBitByte(void)
{
	byte v;

	v = 4;
	test(GetBitByte(v, 1) == 0, "GetBitByte.1");
	test(GetBitByte(v, 2) != 0, "GetBitByte.2");
	test(GetBitByte(v, 3) == 0, "GetBitByte.3");
	SetBitByte(v, 3, 1);
	test(GetBitByte(v, 1) == 0, "GetBitByte.4");
	test(GetBitByte(v, 2) != 0, "GetBitByte.5");
	test(GetBitByte(v, 3) != 0, "GetBitByte.6");
	test(GetBitByte(v, 4) == 0, "GetBitByte.7");
	SetBitByte(v, 2, 0);
	test(GetBitByte(v, 1) == 0, "GetBitByte.8");
	test(GetBitByte(v, 2) == 0, "GetBitByte.9");
	test(GetBitByte(v, 3) != 0, "GetBitByte.10");
	test(GetBitByte(v, 4) == 0, "GetBitByte.11");

	RETURN;
}


/*
 *  main
 */
int test_alloc(void)
{
	TITLE;

	TestBreak(test_CopyJmpBuf);
	TestBreak(test_WtType);
	TestBreak(test_WtTypePtr);
	TestBreak(test_WtByte);

	TestBreak(test_Align8Out);
	TestBreak(test_Align8Cut);
	TestBreak(test_Align8Front);
	TestBreak(test_Align8Inplace);

	TestBreak(test_AlignSize8Out);
	TestBreak(test_AlignSize8Cut);
	TestBreak(test_AlignSize8Front);
	TestBreak(test_AlignSize8Inplace);

	TestBreak(test_sizeoft);
	TestBreak(test_sizeofm);
	TestBreak(test_malloctype);
	TestBreak(test_mallocsize);
	TestBreak(test_clearmemory);
	TestBreak(test_aamemory);

	TestBreak(test_GetBitByte);

	return 0;
}

