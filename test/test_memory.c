#include "memory.c"
#include "degrade.h"
#include "heap.h"

#if 0

/*
 *  testbreak_memory
 */
static int test_sizecheck(void)
{
	/* 8byte alignment check (for 32bit) */
	test((ConsLength % 8UL) == 0, "sizecheck1");
	test((SymbolLength % 8UL) == 0, "sizecheck2");
	RETURN;
}

static int test_GetType(void)
{
	byte mem[10];
	addr pos;

	aatype(mem);
	pos = (addr)mem;
	pos[0] = 0xEE;
	test(GetType(pos) == 0xEE, "GetType");
	SetType(pos, 0xDD);
	test(pos[0] == 0xDD, "SetType");
	SetType(pos, 0xCC);
	test(GetType(pos) == 0xCC, "GetType");
	SetType(pos, 0xBB);
	test(GetType(pos) == 0xBB, "SetType");

	RETURN;
}

static int test_PtrStatus(void)
{
	byte mem[2000];
	addr pos;
	static const int index = 1;

	aatype(mem);
	pos = (addr)mem;
	test(PtrStatus(pos) == (pos + index), "PtrStatus");
	pos[index] = 0xEE;
	test(GetStatus(pos) == 0xEE, "GetStatus");
	SetStatus(pos, 0xDD);
	test(pos[index] == 0xDD, "SetStatus");
	SetStatus(pos, 0xCC);
	test(GetStatus(pos) == 0xCC, "GetStatus");
	SetStatus(pos, 0xBB);
	test(GetStatus(pos) == 0xBB, "SetStatus");
	SetStatusSize(pos, LISPSIZE_ARRAY4, LISPSTATUS_DYNAMIC);
	test(GetStatus(pos)
			== (LISPSIZE_ARRAY4 | (1<<LISPSTATUS_DYNAMIC)), "SetStatusSize");

	RETURN;
}

static int test_PtrUser(void)
{
	byte mem[2000];
	addr pos;
	static const int index = 3;

	aatype(mem);
	pos = (addr)mem;
	test(PtrUser(pos) == (pos + index), "PtrUser");
	pos[index] = 0xEE;
	test(GetUser(pos) == 0xEE, "GetUser");
	SetUser(pos, 0xDD);
	test(pos[index] == 0xDD, "SetUser");
	SetUser(pos, 0xCC);
	test(GetUser(pos) == 0xCC, "GetUser");
	SetUser(pos, 0xBB);
	test(GetUser(pos) == 0xBB, "SetUser");

	RETURN;
}

static int test_IsClass(void)
{
	test(IsClassSmall(LISPCLASS_Cons1), "IsClassSmall1");
	test(IsClassSmall(LISPCLASS_Symbol8), "IsClassSmall2");
	test(IsClassSmall(LISPCLASS_Size11), "IsClassSmall3");
	test(IsClassSmall(LISPCLASS_Size3D), "IsClassSmall3a");
	test(!IsClassSmall(LISPCLASS_SizeK), "IsClassSmall4");
	test(!IsClassSmall(LISPCLASS_SizeM), "IsClassSmall5");

	test(!IsClassLarge(LISPCLASS_Cons1), "IsClassLarge1");
	test(!IsClassLarge(LISPCLASS_Symbol8), "IsClassLarge2");
	test(!IsClassLarge(LISPCLASS_Size11), "IsClassLarge3");
	test(!IsClassLarge(LISPCLASS_Size3D), "IsClassLarge3a");
	test(IsClassLarge(LISPCLASS_SizeK), "IsClassLarge4");
	test(IsClassLarge(LISPCLASS_SizeM), "IsClassLarge5");

	RETURN;
}

static byte mbit(int a7, int a6, int a5, int a4, int a3, int a2, int a1, int a0)
{
	return
		((!!a7) << 7) | ((!!a6) << 6) | ((!!a5) << 5) |
		((!!a4) << 4) | ((!!a3) << 3) | ((!!a2) << 2) |
		((!!a1) << 1) |  (!!a0);
}
#define IfStatus(type, arg,a1,a2,a3,a4,a5,a6,a7,a8, check,v) { \
	pos[1] = mbit(a1,a2,a3,a4, a5,a6,a7,a8); \
	if (type(arg) check (v)) goto error; \
}
static int test_BitStatus(void)
{
	byte mem[2000];
	addr pos;

	aatype(mem);
	if (BitStatusSize    (mbit(0,0,0,0,0, 1,1,1)) != 7) goto error;
	if (BitStatusSize    (mbit(1,1,1,1,1, 0,0,0)) != 0) goto error;
	if (BitStatusSize    (mbit(1,0,1,0,1, 0,1,0)) != 2) goto error;
	if (BitStatusDynamic (mbit(0,0,0,0,1, 0,0,0)) == 0) goto error;
	if (BitStatusDynamic (mbit(1,1,1,1,0, 1,1,1)) != 0) goto error;
	if (BitStatusReadOnly(mbit(0,0,0,1,0, 0,0,0)) == 0) goto error;
	if (BitStatusReadOnly(mbit(1,1,1,0,1, 1,1,1)) != 0) goto error;
	if (BitStatusSystem  (mbit(0,0,1,0,0, 0,0,0)) == 0) goto error;
	if (BitStatusSystem  (mbit(1,1,0,1,1, 1,1,1)) != 0) goto error;
	if (BitStatusFixed   (mbit(0,1,0,0,0, 0,0,0)) == 0) goto error;
	if (BitStatusFixed   (mbit(1,0,1,1,1, 1,1,1)) != 0) goto error;
	if (BitStatusGc      (mbit(1,0,0,0,0, 0,0,0)) == 0) goto error;
	if (BitStatusGc      (mbit(0,1,1,1,1, 1,1,1)) != 0) goto error;

	pos = (addr)mem;
	IfStatus(GetStatusSize,     pos, 0,0,0,0,0, 1,1,1, !=, 7);
	IfStatus(GetStatusSize,     pos, 1,1,1,1,1, 0,0,0, !=, 0);
	IfStatus(GetStatusSize,     pos, 1,0,1,0,1, 0,1,0, !=, 2);
	IfStatus(GetStatusDynamic,  pos, 0,0,0,0,1, 0,0,0, ==, 0);
	IfStatus(GetStatusDynamic,  pos, 1,1,1,1,0, 1,1,1, !=, 0);
	IfStatus(GetStatusReadOnly, pos, 0,0,0,1,0, 0,0,0, ==, 0);
	IfStatus(GetStatusReadOnly, pos, 1,1,1,0,1, 1,1,1, !=, 0);
	IfStatus(GetStatusSystem,   pos, 0,0,1,0,0, 0,0,0, ==, 0);
	IfStatus(GetStatusSystem,   pos, 1,1,0,1,1, 1,1,1, !=, 0);
	IfStatus(GetStatusFixed,    pos, 0,1,0,0,0, 0,0,0, ==, 0);
	IfStatus(GetStatusFixed,    pos, 1,0,1,1,1, 1,1,1, !=, 0);
	IfStatus(GetStatusGc,       pos, 1,0,0,0,0, 0,0,0, ==, 0);
	IfStatus(GetStatusGc,       pos, 0,1,1,1,1, 1,1,1, !=, 0);

	test(1, "Status2");
	return 0;
error:
	fprintf(stderr, "Status2 error.\n");
	return 1;
}

static int test_GetStatus(void)
{
	byte mem[100];
	int i;

	aatype(mem);
	SetStatus(mem, 0);
	for (i = 0; i < 8; i++)
		test(GetStatusValue(mem, i) == 0, "GetStatusValue1");
	SetStatus(mem, 0xFF);
	for (i = 0; i < 8; i++)
		test(GetStatusValue(mem, i) != 0, "GetStatusValue2");

	SetStatus(mem, 1);
	test(GetStatusValue(mem, 0) != 0, "GetStatusValue3");
	test(GetStatusValue(mem, 1) == 0, "GetStatusValue3");
	test(GetStatusValue(mem, 7) == 0, "GetStatusValue3");
	SetStatus(mem, 2);
	test(GetStatusValue(mem, 0) == 0, "GetStatusValue4");
	test(GetStatusValue(mem, 1) != 0, "GetStatusValue4");
	test(GetStatusValue(mem, 2) == 0, "GetStatusValue4");
	test(GetStatusValue(mem, 7) == 0, "GetStatusValue4");
	SetStatus(mem, 2+16);
	test(GetStatusValue(mem, 0) == 0, "GetStatusValue5");
	test(GetStatusValue(mem, 1) != 0, "GetStatusValue5");
	test(GetStatusValue(mem, 2) == 0, "GetStatusValue5");
	test(GetStatusValue(mem, 3) == 0, "GetStatusValue5");
	test(GetStatusValue(mem, 4) != 0, "GetStatusValue5");
	test(GetStatusValue(mem, 5) == 0, "GetStatusValue5");
	test(GetStatusValue(mem, 6) == 0, "GetStatusValue5");
	test(GetStatusValue(mem, 7) == 0, "GetStatusValue5");
	SetStatusValue(mem, 2, 100);
	test(GetStatusValue(mem, 0) == 0, "GetStatusValue6");
	test(GetStatusValue(mem, 1) != 0, "GetStatusValue6");
	test(GetStatusValue(mem, 2) != 0, "GetStatusValue6");
	test(GetStatusValue(mem, 3) == 0, "GetStatusValue6");
	test(GetStatusValue(mem, 4) != 0, "GetStatusValue6");
	test(GetStatusValue(mem, 5) == 0, "GetStatusValue6");
	test(GetStatusValue(mem, 6) == 0, "GetStatusValue6");
	test(GetStatusValue(mem, 7) == 0, "GetStatusValue6");
	SetStatusValue(mem, 6, 1);
	test(GetStatusValue(mem, 0) == 0, "GetStatusValue7");
	test(GetStatusValue(mem, 1) != 0, "GetStatusValue7");
	test(GetStatusValue(mem, 2) != 0, "GetStatusValue7");
	test(GetStatusValue(mem, 3) == 0, "GetStatusValue7");
	test(GetStatusValue(mem, 4) != 0, "GetStatusValue7");
	test(GetStatusValue(mem, 5) == 0, "GetStatusValue7");
	test(GetStatusValue(mem, 6) != 0, "GetStatusValue7");
	test(GetStatusValue(mem, 7) == 0, "GetStatusValue7");
	SetStatusValue(mem, 7, 2);
	SetStatusValue(mem, 4, 0);
	test(GetStatusValue(mem, 0) == 0, "GetStatusValue8");
	test(GetStatusValue(mem, 1) != 0, "GetStatusValue8");
	test(GetStatusValue(mem, 2) != 0, "GetStatusValue8");
	test(GetStatusValue(mem, 3) == 0, "GetStatusValue8");
	test(GetStatusValue(mem, 4) == 0, "GetStatusValue8");
	test(GetStatusValue(mem, 5) == 0, "GetStatusValue8");
	test(GetStatusValue(mem, 6) != 0, "GetStatusValue8");
	test(GetStatusValue(mem, 7) != 0, "GetStatusValue8");

	RETURN;
}

static int test_PtrValue(void)
{
	byte mem[2000], u8;
	byte16 u16;
	byte32 u32;
	addr pos;
#ifdef LISP_ARCH_64BIT
	byte64 u64;
#endif

	pos = (addr)mem;

	/* byte */
	aatype(mem);
	u8 = 0x11;
	WtType(pos + 4, u8);
	test(*PtrByte1A(pos) == u8, "PtrValue1A");

	aatype(mem);
	u8 = 0x22;
	WtType(pos + 5, u8);
	test(*PtrByte1B(pos) == u8, "PtrValue1B");

	/* byte16 */
	aatype(mem);
	u16 = 0x1122;
	WtType(pos + 4, u16);
	test(*PtrValue2A(pos) == u16, "PtrValue2A");

	aatype(mem);
	u16 = 0x3344;
	WtType(pos + 6, u16);
	test(*PtrValue2B(pos) == u16, "PtrValue2B");

	aatype(mem);
	u16 = 0x5566;
	WtType(pos + 8, u16);
	test(*PtrValue2C(pos) == u16, "PtrValue2C");

	/* byte32 */
	aatype(mem);
	u32 = 0x11223344;
	WtType(pos + 4, u32);
	test(*PtrValue4A(pos) == u32, "PtrValue4A");

	aatype(mem);
	u32 = 0x33445566;
	WtType(pos + 8, u32);
	test(*PtrValue4B(pos) == u32, "PtrValue4B");

	aatype(mem);
	u32 = 0x55667788;
	WtType(pos + 12, u32);
	test(*PtrValue4C(pos) == u32, "PtrValue4C");

	aatype(mem);
	u32 = 0x778899BB;
	WtType(pos + 16, u32);
	test(*PtrValue4D(pos) == u32, "PtrValue4D");

#ifdef LISP_ARCH_64BIT
	/* byte64 */
	aatype(mem);
	u64 = 0x1122334455667788ULL;
	WtType(pos + 8, u64);
	test(*PtrValue8A(pos, byte64) == u64, "PtrValue8A");

	aatype(mem);
	u64 = 0x1122334455667788ULL;
	WtType(pos + 16, u64);
	test(*PtrValue8B(pos, byte64) == u64, "PtrValue8B");

	aatype(mem);
	u64 = 0x1122334455667788ULL;
	WtType(pos + 24, u64);
	test(*PtrValue8C(pos, byte64) == u64, "PtrValue8C");
#endif

	RETURN;
}

static int test_PtrByte2(void)
{
	byte mem[100], u8;
	byte16 u16;

	aatype(mem);
	u8 = 0x10;
	WtType(mem + 4, u8);
	test(*PtrValue2va(mem) == u8, "PtrValue2va");

	aatype(mem);
	u8 = 0x22;
	WtType(mem + 5, u8);
	test(*PtrValue2vb(mem) == u8, "PtrValue2vb");

	aatype(mem);
	u16 = 0x3344;
	WtType(mem + 4, u16);
	test(*PtrValue2V(mem) == u16, "PtrValue2V");

	aatype(mem);
	u16 = 0x5566;
	WtType(mem + 6, u16);
	test(*PtrValue2L(mem) == u16, "PtrValue2L");

	aatype(mem);
	test(PtrByte2P(mem) == mem + 8UL, "PtrValue2P");

	RETURN;
}

static int test_PtrByte4(void)
{
	byte mem[100];
	byte16 u16;
	byte32 u32;

	aatype(mem);
	u16 = 0x1122;
	WtType(mem + 4, u16);
	test(*PtrValue4va(mem) == u16, "PtrValue4va");

	aatype(mem);
	u16 = 0x3344;
	WtType(mem + 6, u16);
	test(*PtrValue4vb(mem) == u16, "PtrValue4vb");

	aatype(mem);
	u32 = 0x55667788;
	WtType(mem + 4, u32);
	test(*PtrValue4V(mem) == u32, "PtrValue4V");

	aatype(mem);
	test(PtrByte4P(mem) == mem + 16UL, "PtrValue4P");

	RETURN;
}

static int test_PtrValueL(void)
{
	byte mem[100];
	size_t size;

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	size = 0x8877665544332211ULL;
	WtType(mem + 16, size);
	test(*PtrValue8V(mem) == size, "PtrValue8V");
	test(PtrByte8P(mem) == mem + 24, "PtrValue8P");

	aatype(mem);
	size = 0x1122334455667788ULL;
	WtType(mem + 8, size);
	test(*PtrValueL(mem) == size, "PtrValueL");
#else
	aatype(mem);
	size = 0x11223344UL;
	WtType(mem + 12, size);
	test(*PtrValueL(mem) == size, "PtrValueL");
#endif

	RETURN;
}

static int test_PtrValueReserved(void)
{
	byte mem[1000];
	addr pos;
	size_t size;

	pos = (addr)mem;

	/* Reserved */
	memset(mem, 0xAA, 1000);
	size = 0x112233;
	SetSizeReserved(pos, size);
	size = 0;
	GetSizeReserved(pos, &size);
	memset(mem, 0xAA, 1000);
	test(size == 0x112233, "GetSizeReserved1");
	test(PtrValueReserved(pos) == (size_t *)(pos + 8UL), "PtrValueReserved1");
	SetValueReserved(pos, size);
	size = 0;
	GetValueReserved(pos, &size);
	test(size == 0x112233, "GetValueReserved1");

	/* Space */
	memset(mem, 0xAA, 1000);
	size = 0x112233;
	SetSizeSpace(pos, size);
	size = 0;
	GetSizeSpace(pos, &size);
	test(size == 0x112233, "GetSizeSpace1");
	memset(mem, 0xAA, 1000);
	test(PtrValueSpace(pos) == (size_t *)(pos + 8UL), "PtrValueSpace1");
	SetValueSpace(pos, size);
	size = 0;
	GetValueSpace(pos, &size);
	test(size == 0x112233, "GetValueSpace1");

	/* Space1 */
	memset(mem, 0xAA, 1000);
	SetValueSpace1(pos, 40);
	test(pos[1] == 40, "SetValueSpace1");
	size = 0;
	GetValueSpace1(pos, &size);
	test(size == 40, "GetValueSpace1");
	memset(mem, 0xAA, 1000);
	SetSizeSpace1(pos, 102);
	test(pos[1] == 100, "SetSizeSpace1");
	size = 0;
	GetSizeSpace1(pos, &size);
	test(size == 102, "GetSizeSpace1");

	RETURN;
}

#define ptrwrite(x, y) { \
	uintptr_t temp = (y); \
	WtType((x), temp); \
}
#define ptrtest(x, y, z)  test(((y) == (uintptr_t)(x)), (z))
static int test_PtrArray(void)
{
	byte mem[2000];
	addr pos;

	pos = (addr)mem;

	aatype(mem);
	ptrwrite(pos + 8 + PtrSize*0, 0x12345);
	ptrwrite(pos + 8 + PtrSize*1, 0x234567);
	ptrtest(*PtrArrayA2(pos), 0x12345,  "PtrArrayA21");

	aatype(mem);
	ptrwrite(pos + 8 + PtrSize*0, 0x12345);
	ptrwrite(pos + 8 + PtrSize*1, 0x234567);
	ptrtest(*PtrArraySS(pos), 0x12345,  "PtrArraySS1");

	aatype(mem);
	ptrwrite(pos + 16 + PtrSize*0, 0x12345);
	ptrwrite(pos + 16 + PtrSize*1, 0x234567);
	ptrtest(*PtrArrayA4(pos), 0x12345,  "PtrArrayA41");

	aatype(mem);
	ptrwrite(pos + 16 + PtrSize*0, 0x12345);
	ptrwrite(pos + 16 + PtrSize*1, 0x234567);
	ptrtest(*PtrArrayAB(pos), 0x12345,  "PtrArrayAB1");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	ptrwrite(pos + 24 + PtrSize*0, 0x12345);
	ptrwrite(pos + 24 + PtrSize*1, 0x234567);
	ptrtest(*PtrArrayA8(pos), 0x12345, "PtrArrayA81");
#endif

	RETURN;
}

#define writevalue(x, y, z) { \
	y temp = (z); \
	WtType((x), temp); \
}
static int test_PtrLenArray(void)
{
	byte mem[2000];
	addr pos;

	pos = (addr)mem;

	aatype(mem);
	*PtrValue2V(pos) = 0x1122;
	test(*PtrLenArrayA2(pos) == 0x1122, "PtrLenArrayA2");
	test(GetLenArrayA2(pos) == 0x1122, "GetLenArrayA2");

	aatype(mem);
	*PtrValue2va(pos) = 0xEE;
	test(*PtrLenArraySS(pos) == 0xEE, "PtrLenArraySS");
	test(GetLenArraySS(pos) == 0xEE, "GetLenArraySS");

	aatype(mem);
	*PtrValue4V(pos) = 0x11223344;
	test(*PtrLenArrayA4(pos) == 0x11223344, "PtrLenArrayA4");
	test(GetLenArrayA4(pos) == 0x11223344, "GetLenArrayA4");

	aatype(mem);
	*PtrValue4va(pos) = 0x9988;
	test(*PtrLenArrayAB(pos) == 0x9988, "PtrLenArrayAB");
	test(GetLenArrayAB(pos) == 0x9988, "GetLenArrayAB");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	*PtrValue8V(pos) = 0xAABBCCDD11223344ULL;
	test(*PtrLenArrayA8(pos) == 0xAABBCCDD11223344ULL, "PtrLenArrayA8");
	test(GetLenArrayA8(pos) == 0xAABBCCDD11223344ULL, "GetLenArrayA8");
#endif

	RETURN;
}

static int test_PtrLenBody(void)
{
	byte mem[2000];
	addr pos;

	pos = (addr)mem;

	aatype(mem);
	*PtrValue2V(pos) = 0x1122;
	test(*PtrLenBodyB2(pos) == 0x1122, "PtrLenBodyB2");
	test(GetLenBodyB2(pos) == 0x1122, "GetLenBodyB2");

	aatype(mem);
	*PtrValue2vb(pos) = 0xEE;
	test(*PtrLenBodySS(pos) == 0xEE, "PtrLenBodySS");
	test(GetLenBodySS(pos) == 0xEE, "GetLenBodySS");

	aatype(mem);
	*PtrValue4V(pos) = 0x11223344;
	test(*PtrLenBodyB4(pos) == 0x11223344, "PtrLenBodyB4");
	test(GetLenBodyB4(pos) == 0x11223344, "GetLenBodyB4");

	aatype(mem);
	*PtrValue4vb(pos) = 0x9988;
	test(*PtrLenBodyAB(pos) == 0x9988, "PtrLenBodyAB");
	test(GetLenBodyAB(pos) == 0x9988, "GetLenBodyAB");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	*PtrValue8V(pos) = 0xAABBCCDD11223344ULL;
	test(*PtrLenBodyB8(pos) == 0xAABBCCDD11223344ULL, "PtrLenBodyB8");
	test(GetLenBodyB8(pos) == 0xAABBCCDD11223344ULL, "GetLenBodyB8");
#endif

	RETURN;
}


static int test_PtrBody(void)
{
	byte mem[2000];
	addr pos, base;

	pos = (addr)mem;

	aatype(mem);
	*PtrLenArraySS(pos) = 10;
	base = PtrByte2P(pos);
	test(PtrBodyB2(pos) == base, "PtrBodyB2");
	test(PtrBodySS(pos) == base + 10*PtrSize, "PtrBodySS");
	test(PtrBodySSa(pos, 10) == base + 10*PtrSize, "PtrBodySSa");

	aatype(mem);
	*PtrLenArrayAB(pos) = 10;
	base = PtrByte4P(pos);
	test(PtrBodyB4(pos) == base, "PtrBodyB4");
	test(PtrBodyAB(pos) == base + 10*PtrSize, "PtrBodyAB");
	test(PtrBodyABa(pos, 10) == base + 10*PtrSize, "PtrBodyABa");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	base = PtrByte8P(pos);
	test(PtrBodyB8(pos) == base, "PtrBodyB8");
#endif

	RETURN;
}

static int test_RefBody(void)
{
	byte mem[2000];
	size_t *ptr;

	aatype(mem);
	ptr = (size_t *)PtrBodySSa(mem, 10);
	ptr[0] = 111;
	ptr[1] = 222;
	test(RefBodySSa(mem, 10, size_t) == 111, "RefBodySSa");
	test(RefBodySSai(mem, 10, size_t, 1) == 222, "RefBodySSai");

	aatype(mem);
	ptr = (size_t *)PtrBodyABa(mem, 10);
	ptr[0] = 111;
	ptr[1] = 222;
	test(RefBodyABa(mem, 10, size_t) == 111, "RefBodyABa");
	test(RefBodyABai(mem, 10, size_t, 1) == 222, "RefBodyABai");

	aatype(mem);
	ptr = (size_t *)PtrBodyB2(mem);
	ptr[0] = 111;
	ptr[1] = 222;
	test(RefBodyB2(mem, size_t) == 111, "RefBodyB2");
	test(RefBodyB2i(mem, size_t, 1) == 222, "RefBodyB2i");

	aatype(mem);
	*PtrLenArraySS(mem) = 10;
	ptr = (size_t *)PtrBodySS(mem);
	ptr[0] = 100;
	ptr[1] = 200;
	test(RefBodySS(mem, size_t) == 100, "RefBodySS");
	test(RefBodySSi(mem, size_t, 1) == 200, "RefBodySSi");

	aatype(mem);
	ptr = (size_t *)PtrBodyB4(mem);
	ptr[0] = 111;
	ptr[1] = 222;
	test(RefBodyB4(mem, size_t) == 111, "RefBodyB4");
	test(RefBodyB4i(mem, size_t, 1) == 222, "RefBodyB4i");

	aatype(mem);
	*PtrLenArrayAB(mem) = 10;
	ptr = (size_t *)PtrBodyAB(mem);
	ptr[0] = 100;
	ptr[1] = 200;
	test(RefBodyAB(mem, size_t) == 100, "RefBodyAB");
	test(RefBodyABi(mem, size_t, 1) == 200, "RefBodyABi");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	ptr = (size_t *)PtrBodyB8(mem);
	ptr[0] = 111;
	ptr[1] = 222;
	test(RefBodyB8(mem, size_t) == 111, "RefBodyB8");
	test(RefBodyB8i(mem, size_t, 1) == 222, "RefBodyB8i");
#endif

	RETURN;
}

static int test_GetvBodySSa(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;
	aatype(mem);
	*PtrValue2A(pos) = 10;
	*PtrValue2B(pos) = 20;
	SetvBodySSa(pos, 10, byte16, 0x1234);
	GetvBodySSa(pos, 10, byte16, &u16);
	test(u16 == 0x1234, "GetvBodySSa_byte16");
	SetvBodySSa(pos, 10, byte32, 0x987a);
	GetvBodySSa(pos, 10, byte32, &u32);
	test(u32 == 0x987a, "GetvBodySSa_byte32");
	SetvBodySSai(pos, 10, byte16, 3, 0x1234);
	SetvBodySSai(pos, 10, byte16, 4, 0x2345);
	SetvBodySSai(pos, 10, byte16, 5, 0x3456);
	GetvBodySSai(pos, 10, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodySSai_byte16");
	SetvBodySSai(pos, 10, byte32, 5, 0x7766);
	GetvBodySSai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodySSai_byte32");
	IncvBodySSai(pos, 10, byte32, 5, 2);
	GetvBodySSai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodySSai");
	DecvBodySSai(pos, 10, byte32, 5, 4);
	GetvBodySSai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodySSai");

	RETURN;
}

static int test_GetvBodyABa(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;
	aatype(mem);
	*PtrValue2A(pos) = 10;
	*PtrValue2B(pos) = 20;
	SetvBodyABa(pos, 10, byte16, 0x1234);
	GetvBodyABa(pos, 10, byte16, &u16);
	test(u16 == 0x1234, "GetvBodyABa_byte16");
	SetvBodyABa(pos, 10, byte32, 0x987a);
	GetvBodyABa(pos, 10, byte32, &u32);
	test(u32 == 0x987a, "GetvBodyABa_byte32");
	SetvBodyABai(pos, 10, byte16, 3, 0x1234);
	SetvBodyABai(pos, 10, byte16, 4, 0x2345);
	SetvBodyABai(pos, 10, byte16, 5, 0x3456);
	GetvBodyABai(pos, 10, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodyABai_byte16");
	SetvBodyABai(pos, 10, byte32, 5, 0x7766);
	GetvBodyABai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodyABai_byte32");
	IncvBodyABai(pos, 10, byte32, 5, 2);
	GetvBodyABai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodyABai");
	DecvBodyABai(pos, 10, byte32, 5, 4);
	GetvBodyABai(pos, 10, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodyABai");

	RETURN;
}

static int test_GetvBodySS(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;
	aatype(mem);
	*PtrValue2A(pos) = 10;
	*PtrValue2B(pos) = 20;
	SetvBodySS(pos, byte16, 0x1234);
	GetvBodySS(pos, byte16, &u16);
	test(u16 == 0x1234, "GetvBodySS_byte16");
	SetvBodySS(pos, byte32, 0x987a);
	GetvBodySS(pos, byte32, &u32);
	test(u32 == 0x987a, "GetvBodySS_byte32");
	SetvBodySSi(pos, byte16, 3, 0x1234);
	SetvBodySSi(pos, byte16, 4, 0x2345);
	SetvBodySSi(pos, byte16, 5, 0x3456);
	GetvBodySSi(pos, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodySSi_byte16");
	SetvBodySSi(pos, byte32, 5, 0x7766);
	GetvBodySSi(pos, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodySSi_byte32");
	IncvBodySSi(pos, byte32, 5, 2);
	GetvBodySSi(pos, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodySSi");
	DecvBodySSi(pos, byte32, 5, 4);
	GetvBodySSi(pos, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodySSi");

	RETURN;
}

static int test_GetvBodyB2(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem);
	SetvBodyB2(pos, byte16, 0x1234);
	GetvBodyB2(pos, byte16, &u16);
	test(u16 == 0x1234, "GetvBodyB2_byte16");
	SetvBodyB2(pos, byte32, 0x987a);
	GetvBodyB2(pos, byte32, &u32);
	test(u32 == 0x987a, "GetvBodyB2_byte32");
	SetvBodyB2i(pos, byte16, 3, 0x1234);
	SetvBodyB2i(pos, byte16, 4, 0x2345);
	SetvBodyB2i(pos, byte16, 5, 0x3456);
	GetvBodyB2i(pos, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodyB2i_byte16");
	SetvBodyB2i(pos, byte32, 5, 0x7766);
	GetvBodyB2i(pos, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodyB2i_byte32");
	IncvBodyB2i(pos, byte32, 5, 2);
	GetvBodyB2i(pos, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodyB2i");
	DecvBodyB2i(pos, byte32, 5, 4);
	GetvBodyB2i(pos, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodyB2i");

	RETURN;
}

static int test_GetvBodyAB(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;
	aatype(mem);
	*PtrValue2A(pos) = 10;
	*PtrValue2B(pos) = 20;
	SetvBodyAB(pos, byte16, 0x1234);
	GetvBodyAB(pos, byte16, &u16);
	test(u16 == 0x1234, "GetvBodyAB_byte16");
	SetvBodyAB(pos, byte32, 0x987a);
	GetvBodyAB(pos, byte32, &u32);
	test(u32 == 0x987a, "GetvBodyAB_byte32");
	SetvBodyABi(pos, byte16, 3, 0x1234);
	SetvBodyABi(pos, byte16, 4, 0x2345);
	SetvBodyABi(pos, byte16, 5, 0x3456);
	GetvBodyABi(pos, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodyABi_byte16");
	SetvBodyABi(pos, byte32, 5, 0x7766);
	GetvBodyABi(pos, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodyABi_byte32");
	IncvBodyABi(pos, byte32, 5, 2);
	GetvBodyABi(pos, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodyABi");
	DecvBodyABi(pos, byte32, 5, 4);
	GetvBodyABi(pos, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodyABi");

	RETURN;
}

static int test_GetvBodyB4(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem);
	SetvBodyB4(pos, byte16, 0x1234);
	GetvBodyB4(pos, byte16, &u16);
	test(u16 == 0x1234, "GetvBodyB4_byte16");
	SetvBodyB4(pos, byte32, 0x987a);
	GetvBodyB4(pos, byte32, &u32);
	test(u32 == 0x987a, "GetvBodyB4_byte32");
	SetvBodyB4i(pos, byte16, 3, 0x1234);
	SetvBodyB4i(pos, byte16, 4, 0x2345);
	SetvBodyB4i(pos, byte16, 5, 0x3456);
	GetvBodyB4i(pos, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodyB4i_byte16");
	SetvBodyB4i(pos, byte32, 5, 0x7766);
	GetvBodyB4i(pos, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodyB4i_byte32");
	IncvBodyB4i(pos, byte32, 5, 2);
	GetvBodyB4i(pos, byte32, 5, &u32);
	test(u32 == 0x7768, "IncvBodyB4i");
	DecvBodyB4i(pos, byte32, 5, 4);
	GetvBodyB4i(pos, byte32, 5, &u32);
	test(u32 == 0x7764, "DecvBodyB4i");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_GetvBodyB8(void)
{
	byte mem[1000];
	addr pos;
	byte16 u16;
	byte32 u32;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem);
	SetvBodyB8(pos, byte16, 0x1234);
	GetvBodyB8(pos, byte16, &u16);
	test(u16 == 0x1234, "GetvBodyB8");
	SetvBodyB8(pos, byte32, 0x987a);
	GetvBodyB8(pos, byte32, &u32);
	test(u32 == 0x987a, "GetvBodyB8");
	SetvBodyB8i(pos, byte16, 3, 0x1234);
	SetvBodyB8i(pos, byte16, 4, 0x2345);
	SetvBodyB8i(pos, byte16, 5, 0x3456);
	GetvBodyB8i(pos, byte16, 4, &u16);
	test(u16 == 0x2345, "GetvBodyB8i");
	SetvBodyB8i(pos, byte32, 5, 0x7766);
	GetvBodyB8i(pos, byte32, 5, &u32);
	test(u32 == 0x7766, "GetvBodyB8i");
	IncvBodyB8i(pos, byte32, 5, 2);
	GetvBodyB8i(pos, byte32, 5, &u32);
	test(u32 == 0x7768, "GetvBodyB8i");
	DecvBodyB8i(pos, byte32, 5, 4);
	GetvBodyB8i(pos, byte32, 5, &u32);
	test(u32 == 0x7764, "GetvBodyB8i");

	RETURN;
}
#endif

static int test_ischeck(void)
{
	byte mem[1000];
	addr pos;

	memset(mem, 0xAA, 1000);
	pos = (addr)mem;

	SetType(pos, LISPTYPE_STRING);
	test(! IsBoolean(pos), "IsBoolean1");
	test(IsBoolean(Nil), "IsBoolean2");
	test(IsBoolean(T), "IsBoolean3");
	SetType(pos, LISPTYPE_CONS);
	test(IsCons(pos), "IsCons1");
	test(IsList(pos), "IsCons2");

	SetStatus(pos, 0xF8 | LISPSIZE_ARRAYBODY);
	test(IsArray(pos), "IsArray1");
	test(IsBody(pos), "IsBody1");
	SetStatus(pos, 0xF8 | LISPSIZE_ARRAY4);
	test(IsArray(pos), "IsArray2");
	test(! IsBody(pos), "IsBody2");
	SetStatus(pos, 0xF8 | LISPSIZE_BODY8);
	test(! IsArray(pos), "IsArray3");
	test(IsBody(pos), "IsBody3");

	SetType(pos, LISPTYPE_STRING);
	test(! IsFloat(pos), "IsFloat1");
	SetType(pos, LISPTYPE_SINGLE_FLOAT);
	test(IsFloat(pos), "IsFloat2");
	SetType(pos, LISPTYPE_DOUBLE_FLOAT);
	test(IsFloat(pos), "IsFloat3");

	RETURN;
}

static int test_memorylength(void)
{
	size_t size;

	/* small */
	size = MemoryLengthSS(10, 20);
	test(size == (8+PtrSize*10+20), "MemoryLengthSS");
	size = MemoryLengthA2(33);
	test(size == (8+PtrSize*33), "MemoryLengthA2");
	size = MemoryLengthB2(44);
	test(size == (8+44), "MemoryLengthB2");

	size = MemoryLengthAB(10, 20);
	test(size == (16+PtrSize*10+20), "MemoryLengthAB");
	size = MemoryLengthA4(33);
	test(size == (16+PtrSize*33), "MemoryLengthA4");
	size = MemoryLengthB4(44);
	test(size == (16+44), "MemoryLengthB4");

#ifdef LISP_ARCH_64BIT
	size = MemoryLengthA8(33);
	test(size == (24+PtrSize*33), "MemoryLengthA8");
	size = MemoryLengthB8(44);
	test(size == (24+44), "MemoryLengthB8");
#endif

	/* check ConsLength, SymbolLength */
	test(MemoryLengthA2(2) == ConsLength, "ConsLength1");
	test(MemoryLengthA2(SYMBOL_INDEX_SIZE) == SymbolLength, "SymbolLength1");

	RETURN;
}

static int testbreak_memory(void)
{
	TestBreak(test_sizecheck);
	TestBreak(test_GetType);
	TestBreak(test_PtrStatus);
	TestBreak(test_PtrUser);
	TestBreak(test_IsClass);
	TestBreak(test_BitStatus);
	TestBreak(test_GetStatus);
	TestBreak(test_PtrValue);
	TestBreak(test_PtrByte2);
	TestBreak(test_PtrByte4);
	TestBreak(test_PtrValueL);
	TestBreak(test_PtrValueReserved);
	TestBreak(test_PtrArray);
	TestBreak(test_PtrLenArray);
	TestBreak(test_PtrLenBody);
	TestBreak(test_PtrBody);
	TestBreak(test_RefBody);
	TestBreak(test_GetvBodySSa);
	TestBreak(test_GetvBodyABa);
	TestBreak(test_GetvBodySS);
	TestBreak(test_GetvBodyB2);
	TestBreak(test_GetvBodyAB);
	TestBreak(test_GetvBodyB4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_GetvBodyB8);
#endif
	TestBreak(test_ischeck);
	TestBreak(test_memorylength);

	return 0;
}


/*
 *  function
 */
static int test_size_class(void)
{
	test(size_class(0) == LISPCLASS_Size11, "size_class1");
	test(size_class(1) == LISPCLASS_Size11, "size_class2");
	test(size_class(15) == LISPCLASS_Size11, "size_class3");
	test(size_class(16) == LISPCLASS_Size11, "size_class4");
	test(size_class(17) == LISPCLASS_Size12, "size_class5");
	test(size_class(32) == LISPCLASS_Size12, "size_class6");
	test(size_class(33) == LISPCLASS_Size13, "size_class7");
	test(size_class(128) == LISPCLASS_Size18, "size_class8");
	test(size_class(129) == LISPCLASS_Size21, "size_class9");
	test(size_class(192) == LISPCLASS_Size21, "size_class10");
	test(size_class(193) == LISPCLASS_Size22, "size_class11");
	test(size_class(256) == LISPCLASS_Size22, "size_class12");
	test(size_class(257) == LISPCLASS_Size23, "size_class13");
	test(size_class(512) == LISPCLASS_Size26, "size_class14");
	test(size_class(513) == LISPCLASS_Size31, "size_class15");
	test(size_class(768) == LISPCLASS_Size31, "size_class16");
	test(size_class(769) == LISPCLASS_Size32, "size_class17");
	test(size_class(1024) == LISPCLASS_Size32, "size_class18");
	test(size_class(1025) == LISPCLASS_Size33, "size_class19");
	test(size_class(3840) == LISPCLASS_Size3D, "size_class20");
	test(size_class(3841) == LISPCLASS_SizeK, "size_class21");
	test(size_class(4096) == LISPCLASS_SizeK, "size_class22");
	test(size_class(4097) == LISPCLASS_SizeK, "size_class23");
	test(size_class(4096*1024 - 4096) == LISPCLASS_SizeK, "size_class24");
	test(size_class(4096*1024 - 4096+1) == LISPCLASS_SizeM, "size_class25");
	test(size_class(4096*1024) == LISPCLASS_SizeM, "size_class26");

	RETURN;
}

static enum LISPCLASS getcls(size_t v)
{
	enum LISPCLASS index;
	size_t size;

	size_and_class(v, &index, &size);
	return index;
}

static size_t getsz(size_t v)
{
	enum LISPCLASS index;
	size_t size;

	size_and_class(v, &index, &size);
	return size;
}

static int test_size_split(void)
{
	test(size_split(0) == 16, "size_split1");
	test(size_split(1) == 16, "size_split2");
	test(size_split(15) == 16, "size_split3");
	test(size_split(16) == 16, "size_split4");
	test(size_split(17) == 32, "size_split5");
	test(size_split(32) == 32, "size_split6");
	test(size_split(33) == 48, "size_split7");
	test(size_split(128) == 128, "size_split8");
	test(size_split(129) == 192, "size_split9");
	test(size_split(192) == 192, "size_split10");
	test(size_split(193) == 256, "size_split11");
	test(size_split(256) == 256, "size_split12");
	test(size_split(257) == 320, "size_split13");
	test(size_split(512) == 512, "size_split14");
	test(size_split(513) == 768, "size_split15");
	test(size_split(768) == 768, "size_split16");
	test(size_split(769) == 1024, "size_split17");
	test(size_split(1024) == 1024, "size_split18");
	test(size_split(1025) == 1280, "size_split19");
	test(size_split(3840) == 3840, "size_split20");
	test(size_split(3841) == 4096, "size_split21");
	test(size_split(4096) == 4096, "size_split22");
	test(size_split(4097) == 8192, "size_split23");
	test(size_split(4096*1024 - 4096) == 4096*1024-4096, "size_split24");
	test(size_split(4096*1024 - 4096+1) == 4096*1024, "size_split25");
	test(size_split(4096*1024) == 4096*1024, "size_split26");
	test(size_split(4096*1024+1) == 4096*1024*2, "size_split27");

	RETURN;
}

static int test_size_and_class(void)
{
	test(getcls(0) == LISPCLASS_Size11, "getcls1");
	test(getcls(1) == LISPCLASS_Size11, "getcls2");
	test(getcls(15) == LISPCLASS_Size11, "getcls3");
	test(getcls(16) == LISPCLASS_Size11, "getcls4");
	test(getcls(17) == LISPCLASS_Size12, "getcls5");
	test(getcls(32) == LISPCLASS_Size12, "getcls6");
	test(getcls(33) == LISPCLASS_Size13, "getcls7");
	test(getcls(128) == LISPCLASS_Size18, "getcls8");
	test(getcls(129) == LISPCLASS_Size21, "getcls9");
	test(getcls(192) == LISPCLASS_Size21, "getcls10");
	test(getcls(193) == LISPCLASS_Size22, "getcls11");
	test(getcls(256) == LISPCLASS_Size22, "getcls12");
	test(getcls(257) == LISPCLASS_Size23, "getcls13");
	test(getcls(512) == LISPCLASS_Size26, "getcls14");
	test(getcls(513) == LISPCLASS_Size31, "getcls15");
	test(getcls(768) == LISPCLASS_Size31, "getcls16");
	test(getcls(769) == LISPCLASS_Size32, "getcls17");
	test(getcls(1024) == LISPCLASS_Size32, "getcls18");
	test(getcls(1025) == LISPCLASS_Size33, "getcls19");
	test(getcls(3840) == LISPCLASS_Size3D, "getcls20");
	test(getcls(3841) == LISPCLASS_SizeK, "getcls21");
	test(getcls(4096) == LISPCLASS_SizeK, "getcls22");
	test(getcls(4097) == LISPCLASS_SizeK, "getcls23");
	test(getcls(4096*1024 - 4096) == LISPCLASS_SizeK, "getcls24");
	test(getcls(4096*1024 - 4096+1) == LISPCLASS_SizeM, "getcls25");
	test(getcls(4096*1024) == LISPCLASS_SizeM, "getcls26");

	test(getsz(0) == 16, "getsz1");
	test(getsz(1) == 16, "getsz2");
	test(getsz(15) == 16, "getsz3");
	test(getsz(16) == 16, "getsz4");
	test(getsz(17) == 32, "getsz5");
	test(getsz(32) == 32, "getsz6");
	test(getsz(33) == 48, "getsz7");
	test(getsz(128) == 128, "getsz8");
	test(getsz(129) == 192, "getsz9");
	test(getsz(192) == 192, "getsz10");
	test(getsz(193) == 256, "getsz11");
	test(getsz(256) == 256, "getsz12");
	test(getsz(257) == 320, "getsz13");
	test(getsz(512) == 512, "getsz14");
	test(getsz(513) == 768, "getsz15");
	test(getsz(768) == 768, "getsz16");
	test(getsz(769) == 1024, "getsz17");
	test(getsz(1024) == 1024, "getsz18");
	test(getsz(1025) == 1280, "getsz19");
	test(getsz(3840) == 3840, "getsz20");
	test(getsz(3841) == 4096, "getsz21");
	test(getsz(4096) == 4096, "getsz22");
	test(getsz(4097) == 8192, "getsz23");
	test(getsz(4096*1024 - 4096) == 4096*1024-4096, "getsz24");
	test(getsz(4096*1024 - 4096+1) == 4096*1024, "getsz25");
	test(getsz(4096*1024) == 4096*1024, "getsz26");
	test(getsz(4096*1024+1) == 4096*1024*2, "getsz27");

	RETURN;
}

static int test_getobjectlength(void)
{
	byte mem[100];

	aatype(mem);
	SetStatus(mem, LISPSIZE_ARRAY2);
	*PtrValue2L(mem) = 100;
	test(getobjectlength(mem) == 100, "getobjectlength1");

	aatype(mem);
	SetStatus(mem, LISPSIZE_ARRAYBODY);
	*PtrValueL(mem) = 200;
	test(getobjectlength(mem) == 200, "getobjectlength2");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(mem, LISPSIZE_BODY8);
	*PtrValueL(mem) = 300;
	test(getobjectlength(mem) == 300, "getobjectlength3");
#endif

	RETURN;
}

static int test_getmemorylength(void)
{
	byte mem[100];

	/* getobjectlength */
	aatype(mem);
	SetStatus(mem, LISPSIZE_ARRAY2);
	*PtrValue2L(mem) = 100;
	test(getmemorylength(mem) == 100, "getmemorylength1");

	aatype(mem);
	SetStatus(mem, LISPSIZE_ARRAYBODY);
	*PtrValueL(mem) = 200;
	test(getmemorylength(mem) == 200, "getmemorylength2");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(mem, LISPSIZE_BODY8);
	*PtrValueL(mem) = 300;
	test(getmemorylength(mem) == 300, "getmemorylength3");
#endif

	/* space, reserve */
	aamemory(mem, 100);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 111);
	test(getmemorylength(mem) == 111, "getmemorylength6");

	aamemory(mem, 100);
	SetType(mem, LISPSYSTEM_SPACE);
	SetSizeSpace(mem, 0xABCDE);
	test(getmemorylength(mem) == 0xABCDE, "getmemorylength7");

	aamemory(mem, 100);
	SetType(mem, LISPSYSTEM_RESERVED);
	SetSizeReserved(mem, 100200);
	test(getmemorylength(mem) == 100200, "getmemorylength8");

	RETURN;
}

static int testbreak_function(void)
{
	TestBreak(test_size_class);
	TestBreak(test_size_split);
	TestBreak(test_size_and_class);
	TestBreak(test_getobjectlength);
	TestBreak(test_getmemorylength);

	return 0;
}


/*
 *  memory_function
 */
static int test_lenarray(void)
{
	byte mem[2000];
	addr pos;
	size_t size;

	pos = (addr)mem;

	aatype(mem);
	*PtrLenArraySS(mem) = 10;
	*PtrLenBodySS(mem) = 77;
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	size = 0;
	LenArraySS(pos, &size);
	test(size == 10, "LenArraySS-1");
	size = 0;
	lenarray(pos, &size);
	test(size == 10, "LenArraySS-2");

	aatype(mem);
	*PtrValue2V(pos) = 234;
	SetStatus(pos, LISPSIZE_ARRAY2);
	size = 0;
	LenArrayA2(pos, &size);
	test(size == 234, "LenArrayA2-1");
	size = 0;
	lenarray(pos, &size);
	test(size == 234, "LenArrayA2-2");

	aatype(mem);
	*PtrLenArrayAB(mem) = 10;
	*PtrLenBodyAB(mem) = 77;
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	size = 0;
	LenArrayAB(pos, &size);
	test(size == 10, "LenArrayAB-1");
	size = 0;
	lenarray(pos, &size);
	test(size == 10, "LenArrayAB-2");

	aatype(mem);
	*PtrValue4A(pos) = 234;
	SetStatus(pos, LISPSIZE_ARRAY4);
	size = 0;
	LenArrayA4(pos, &size);
	test(size == 234, "LenArrayA4-1");
	size = 0;
	lenarray(pos, &size);
	test(size == 234, "LenArrayA4-2");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY8);
	*PtrValue8V(pos) = 9876;
	size = 0;
	LenArrayA8(pos, &size);
	test(size == 9876, "LenArrayA8-1");
	size = 0;
	lenarray(pos, &size);
	test(size == 9876, "LenArrayA8-2");
#endif

	RETURN;
}

static int test_lenbody(void)
{
	byte mem[2000];
	addr pos;
	size_t size;

	pos = (addr)mem;

	aatype(mem);
	*PtrLenArraySS(mem) = 10;
	*PtrLenBodySS(mem) = 23;
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	size = 0;
	LenBodySS(pos, &size);
	test(size == 23, "LenBodySS-1");
	size = 0;
	lenbody(pos, &size);
	test(size == 23, "LenBodySS-2");

	aatype(mem);
	*PtrValue2V(pos) = 123;
	SetStatus(pos, LISPSIZE_BODY2);
	size = 0;
	LenBodyB2(pos, &size);
	test(size == 123, "LenBodyB2-1");
	size = 0;
	lenbody(pos, &size);
	test(size == 123, "LenBodyB2-2");

	aatype(mem);
	*PtrLenArrayAB(mem) = 10;
	*PtrLenBodyAB(mem) = 23;
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	size = 0;
	LenBodyAB(pos, &size);
	test(size == 23, "LenBodyAB-1");
	size = 0;
	lenbody(pos, &size);
	test(size == 23, "LenBodyAB-2");

	aatype(mem);
	*PtrValue4V(pos) = 123;
	SetStatus(pos, LISPSIZE_BODY4);
	size = 0;
	LenBodyB4(pos, &size);
	test(size == 123, "LenBodyB4-1");
	size = 0;
	lenbody(pos, &size);
	test(size == 123, "LenBodyB4-2");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	*PtrValue8V(pos) = 567;
	SetStatus(pos, LISPSIZE_BODY8);
	size = 0;
	LenBodyB8(pos, &size);
	test(size == 567, "LenBodyB8-1");
	size = 0;
	lenbody(pos, &size);
	test(size == 567, "LenBodyB8-2");
#endif

	RETURN;
}

static int test_posbody(void)
{
	byte mem[10000];
	addr pos, body;

	pos = (addr)mem;

	aatype(mem);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	*PtrLenArraySS(mem) = 11;
	*PtrLenBodySS(mem) = 22;
	body = 0;
	PosBodySS(pos, &body);
	test(body == (PtrByte2P(pos) + PtrSize*11), "posbodySS-1");
	body = 0;
	posbody(pos, &body);
	test(body == (PtrByte2P(pos) + PtrSize*11), "posbodySS-2");

	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY2);
	*PtrValue2V(pos) = 333;
	body = 0;
	PosBodyB2(pos, &body);
	test(body == PtrByte2P(pos), "posbodyB2-1");
	body = 0;
	posbody(pos, &body);
	test(body == PtrByte2P(pos), "posbodyB2-2");

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	*PtrLenArrayAB(mem) = 11;
	*PtrLenBodyAB(mem) = 22;
	body = 0;
	PosBodyAB(pos, &body);
	test(body == (PtrByte4P(pos) + PtrSize*11), "posbodyAB-1");
	body = 0;
	posbody(pos, &body);
	test(body == (PtrByte4P(pos) + PtrSize*11), "posbodyAB-2");

	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY4);
	*PtrValue4V(pos) = 333;
	body = 0;
	PosBodyB4(pos, &body);
	test(body == PtrByte4P(pos), "posbodyB4-1");
	body = 0;
	posbody(pos, &body);
	test(body == PtrByte4P(pos), "posbodyB4-2");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY8);
	*PtrValue8V(pos) = 444;
	body = 0;
	PosBodyB8(pos, &body);
	test(body == PtrByte8P(pos), "posbodyB8-1");
	body = 0;
	posbody(pos, &body);
	test(body == PtrByte8P(pos), "posbodyB8-2");
#endif

	RETURN;
}

static int test_posbodylen(void)
{
	byte mem[10000];
	addr pos, body;
	size_t len;

	pos = (addr)mem;

	/* smallsize */
	aatype(mem);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	*PtrLenArraySS(mem) = 11;
	*PtrLenBodySS(mem) = 22;

	body = 0;
	len = 0;
	PosBodyLenSS(pos, &body, &len);
	test(body == (PtrByte2P(pos) + PtrSize*11), "posbodylenSS-1");
	test(len == 22, "posbodylenSS-2");

	body = 0;
	len = 0;
	posbodylen(pos, &body, &len);
	test(body == (PtrByte2P(pos) + PtrSize*11), "posbodylenSS-3");
	test(len == 22, "posbodylenSS-4");
	body = 0;

	/* body4 */
	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY2);
	*PtrValue2V(pos) = 333;

	body = 0;
	len = 0;
	PosBodyLenB2(pos, &body, &len);
	test(body == PtrByte2P(pos), "posbodylenB2-1");
	test(len == 333, "posbodylenB2-2");

	body = 0;
	len = 0;
	posbodylen(pos, &body, &len);
	test(body == PtrByte2P(pos), "posbodylenB2-3");
	test(len == 333, "posbodylenB2-4");
	body = 0;

	/* arraybody */
	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	*PtrLenArrayAB(mem) = 11;
	*PtrLenBodyAB(mem) = 22;

	body = 0;
	len = 0;
	PosBodyLenAB(pos, &body, &len);
	test(body == (PtrByte4P(pos) + PtrSize*11), "posbodylenAB-1");
	test(len == 22, "posbodylenAB-2");

	body = 0;
	len = 0;
	posbodylen(pos, &body, &len);
	test(body == (PtrByte4P(pos) + PtrSize*11), "posbodylenAB-3");
	test(len == 22, "posbodylenAB-4");
	body = 0;

	/* body4 */
	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY4);
	*PtrValue4A(pos) = 333;

	body = 0;
	len = 0;
	PosBodyLenB4(pos, &body, &len);
	test(body == PtrByte4P(pos), "posbodylenB4-1");
	test(len == 333, "posbodylenB4-2");

	body = 0;
	len = 0;
	posbodylen(pos, &body, &len);
	test(body == PtrByte4P(pos), "posbodylenB4-3");
	test(len == 333, "posbodylenB4-4");
	body = 0;

#ifdef LISP_ARCH_64BIT
	/* body8 */
	aatype(mem);
	SetStatus(pos, LISPSIZE_BODY8);
	*PtrValue8V(pos) = 444;

	body = 0;
	len = 0;
	PosBodyLenB8(pos, &body, &len);
	test(body == PtrByte8P(pos), "posbodylenB8-1");
	test(len == 444, "posbodylenB8-2");

	body = 0;
	len = 0;
	posbodylen(pos, &body, &len);
	test(body == PtrByte8P(pos), "posbodylenB8-3");
	test(len == 444, "posbodylenB8-4");
	body = 0;
#endif

	RETURN;
}

static int test_GetArraySS(void)
{
	byte mem[1000], mem1[100], mem2[100], mem3[100];
	addr pos, check;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem1);
	aatype(mem2);
	aatype(mem3);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	SetStatusValue(mem1, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem2, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem3, LISPSTATUS_DYNAMIC, 0);
	*PtrLenArraySS(pos) = 3;
	PtrArraySS(pos)[0] = Unbound;
	PtrArraySS(pos)[1] = Unbound;
	PtrArraySS(pos)[2] = Unbound;
	SetArraySS(pos, 0, (addr)mem1);
	SetArraySS(pos, 1, (addr)mem2);
	setarray(pos, 2, (addr)mem3);
	check = 0;
	getarray(pos, 0, &check);
	test(check == (addr)mem1, "SetArraySS-1");
	GetArraySS(pos, 1, &check);
	test(check == (addr)mem2, "SetArraySS-2");
	GetArraySS(pos, 2, &check);
	test(check == (addr)mem3, "SetArraySS-3");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetArraySS_force(pos, 1, (addr)mem2);
	getarray(pos, 1, &check);
	test(check == (addr)mem2, "SetArraySS-4");

	RETURN;
}

static int test_GetArrayA2(void)
{
	byte mem[1000], mem1[100], mem2[100], mem3[100];
	addr pos, check;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem1);
	aatype(mem2);
	aatype(mem3);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetStatusValue(mem1, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem2, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem3, LISPSTATUS_DYNAMIC, 0);
	*PtrLenArrayA2(pos) = 3;
	PtrArrayA2(pos)[0] = Unbound;
	PtrArrayA2(pos)[1] = Unbound;
	PtrArrayA2(pos)[2] = Unbound;
	SetArrayA2(pos, 0, (addr)mem1);
	SetArrayA2(pos, 1, (addr)mem2);
	setarray(pos, 2, (addr)mem3);
	check = 0;
	getarray(pos, 0, &check);
	test(check == (addr)mem1, "SetArrayA2-1");
	GetArrayA2(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA2-2");
	GetArrayA2(pos, 2, &check);
	test(check == (addr)mem3, "SetArrayA2-3");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetArrayA2_force(pos, 1, (addr)mem2);
	getarray(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA2-4");

	RETURN;
}

static int test_GetArrayAB(void)
{
	byte mem[1000], mem1[100], mem2[100], mem3[100];
	addr pos, check;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem1);
	aatype(mem2);
	aatype(mem3);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	SetStatusValue(mem1, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem2, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem3, LISPSTATUS_DYNAMIC, 0);
	*PtrLenArrayAB(pos) = 3;
	PtrArrayAB(pos)[0] = Unbound;
	PtrArrayAB(pos)[1] = Unbound;
	PtrArrayAB(pos)[2] = Unbound;
	SetArrayAB(pos, 0, (addr)mem1);
	SetArrayAB(pos, 1, (addr)mem2);
	setarray(pos, 2, (addr)mem3);
	check = 0;
	getarray(pos, 0, &check);
	test(check == (addr)mem1, "SetArrayAB-1");
	GetArrayAB(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayAB-2");
	GetArrayAB(pos, 2, &check);
	test(check == (addr)mem3, "SetArrayAB-3");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetArrayAB_force(pos, 1, (addr)mem2);
	getarray(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayAB-4");

	RETURN;
}

static int test_GetArrayA4(void)
{
	byte mem[1000], mem1[100], mem2[100], mem3[100];
	addr pos, check;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem1);
	aatype(mem2);
	aatype(mem3);
	SetStatus(pos, LISPSIZE_ARRAY4);
	SetStatusValue(mem1, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem2, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem3, LISPSTATUS_DYNAMIC, 0);
	*PtrLenArrayA4(pos) = 3;
	PtrArrayA4(pos)[0] = Unbound;
	PtrArrayA4(pos)[1] = Unbound;
	PtrArrayA4(pos)[2] = Unbound;
	SetArrayA4(pos, 0, (addr)mem1);
	SetArrayA4(pos, 1, (addr)mem2);
	setarray(pos, 2, (addr)mem3);
	check = 0;
	getarray(pos, 0, &check);
	test(check == (addr)mem1, "SetArrayA4-1");
	GetArrayA4(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA4-2");
	GetArrayA4(pos, 2, &check);
	test(check == (addr)mem3, "SetArrayA4-3");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetArrayA4_force(pos, 1, (addr)mem2);
	getarray(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA4-4");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_GetArrayA8(void)
{
	byte mem[1000], mem1[100], mem2[100], mem3[100];
	addr pos, check;

	pos = (addr)mem;

	aatype(mem);
	aatype(mem1);
	aatype(mem2);
	aatype(mem3);
	SetStatus(pos, LISPSIZE_ARRAY8);
	SetStatusValue(mem1, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem2, LISPSTATUS_DYNAMIC, 0);
	SetStatusValue(mem3, LISPSTATUS_DYNAMIC, 0);
	*PtrLenArrayA8(pos) = 3;
	PtrArrayA8(pos)[0] = Unbound;
	PtrArrayA8(pos)[1] = Unbound;
	PtrArrayA8(pos)[2] = Unbound;
	SetArrayA8(pos, 0, (addr)mem1);
	SetArrayA8(pos, 1, (addr)mem2);
	setarray(pos, 2, (addr)mem3);
	check = 0;
	getarray(pos, 0, &check);
	test(check == (addr)mem1, "SetArrayA8-1");
	GetArrayA8(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA8-2");
	GetArrayA8(pos, 2, &check);
	test(check == (addr)mem3, "SetArrayA8-3");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetArrayA8_force(pos, 1, (addr)mem2);
	getarray(pos, 1, &check);
	test(check == (addr)mem2, "SetArrayA8-4");

	RETURN;
}
#endif

static int test_nilarray(void)
{
	byte mem[100000], nilobject[10];
	addr pos, check;

	Nil = (addr)nilobject;
	pos = (addr)mem;

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY2);
	nilarray2(pos, 6);
	GetArrayA2(pos, 0, &check);
	test(check == Nil, "nilarray2-1");
	GetArrayA2(pos, 5, &check);
	test(check == Nil, "nilarray2-2");
	GetArrayA2(pos, 6, &check);
	test(check != Nil, "nilarray2-3");

	aatype(mem);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	nilarray2(pos, 6);
	GetArraySS(pos, 0, &check);
	test(check == Nil, "nilarray2-4");
	GetArraySS(pos, 5, &check);
	test(check == Nil, "nilarray2-5");
	GetArraySS(pos, 6, &check);
	test(check != Nil, "nilarray2-6");

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY4);
	nilarray4(pos, 6);
	GetArrayA4(pos, 0, &check);
	test(check == Nil, "nilarray4-1");
	GetArrayA4(pos, 5, &check);
	test(check == Nil, "nilarray4-2");
	GetArrayA4(pos, 6, &check);
	test(check != Nil, "nilarray4-3");

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	nilarray4(pos, 6);
	GetArrayAB(pos, 0, &check);
	test(check == Nil, "nilarray-4");
	GetArrayAB(pos, 5, &check);
	test(check == Nil, "nilarray-5");
	GetArrayAB(pos, 6, &check);
	test(check != Nil, "nilarray-6");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY8);
	nilarray8(pos, 6);
	GetArrayA8(pos, 0, &check);
	test(check == Nil, "nilarray8-1");
	GetArrayA8(pos, 5, &check);
	test(check == Nil, "nilarray8-2");
	GetArrayA8(pos, 6, &check);
	test(check != Nil, "nilarray8-3");
#endif

	RETURN;
}

static int test_unboundarray(void)
{
	byte mem[100000], nilobject[10];
	addr pos, check;

	Nil = (addr)nilobject;
	pos = (addr)mem;

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY2);
	unboundarray2(pos, 6);
	GetArrayA2(pos, 0, &check);
	test(check == Unbound, "unboundarray2-1");
	GetArrayA2(pos, 5, &check);
	test(check == Unbound, "unboundarray2-2");
	GetArrayA2(pos, 6, &check);
	test(check != Unbound, "unboundarray2-3");

	aatype(mem);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	unboundarray2(pos, 6);
	GetArraySS(pos, 0, &check);
	test(check == Unbound, "unboundarray2-4");
	GetArraySS(pos, 5, &check);
	test(check == Unbound, "unboundarray2-5");
	GetArraySS(pos, 6, &check);
	test(check != Unbound, "unboundarray2-6");

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY4);
	unboundarray4(pos, 6);
	GetArrayA4(pos, 0, &check);
	test(check == Unbound, "unboundarray4-1");
	GetArrayA4(pos, 5, &check);
	test(check == Unbound, "unboundarray4-2");
	GetArrayA4(pos, 6, &check);
	test(check != Unbound, "unboundarray4-3");

	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	unboundarray4(pos, 6);
	GetArrayAB(pos, 0, &check);
	test(check == Unbound, "unboundarray-4");
	GetArrayAB(pos, 5, &check);
	test(check == Unbound, "unboundarray-5");
	GetArrayAB(pos, 6, &check);
	test(check != Unbound, "unboundarray-6");

#ifdef LISP_ARCH_64BIT
	aatype(mem);
	SetStatus(pos, LISPSIZE_ARRAY8);
	unboundarray8(pos, 6);
	GetArrayA8(pos, 0, &check);
	test(check == Unbound, "unboundarray8-1");
	GetArrayA8(pos, 5, &check);
	test(check == Unbound, "unboundarray8-2");
	GetArrayA8(pos, 6, &check);
	test(check != Unbound, "unboundarray8-3");
#endif

	RETURN;
}

static int testbreak_memory_function(void)
{
	TestBreak(test_lenarray);
	TestBreak(test_lenbody);
	TestBreak(test_posbody);
	TestBreak(test_posbodylen);
	TestBreak(test_GetArraySS);
	TestBreak(test_GetArrayA2);
	TestBreak(test_GetArrayAB);
	TestBreak(test_GetArrayA4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_GetArrayA8);
#endif
	TestBreak(test_nilarray);
	TestBreak(test_unboundarray);

	return 0;
}
#endif


/*
 *  main
 */
int test_memory(void)
{
	int result;

	TITLE;
	result = 0;
#if 0
	result |= testbreak_memory();
	result |= testbreak_function();
	result |= testbreak_memory_function();
#endif

	return result;
}


