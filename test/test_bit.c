#include "bit.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "cons.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

static int test_bitp(void)
{
	test(bitp(fixnumh(0)), "bitp1");
	test(bitp(fixnumh(1)), "bitp2");
	test(! bitp(fixnumh(2)), "bitp3");
	test(! bitp(fixnumh(-1)), "bitp4");
	test(! bitp(fixnumh(10)), "bitp5");
	test(! bitp(T), "bitp6");

	RETURN;
}

static int test_bit_getint(void)
{
	int value;

	value = 999;
	test(! bit_getint(fixnumh(0), &value), "bit_getint1");
	test(value == 0, "bit_getint2");
	test(! bit_getint(fixnumh(1), &value), "bit_getint3");
	test(value == 1, "bit_getint4");
	value = 999;
	test(bit_getint(fixnumh(2), &value), "bit_getint5");
	test(value == 999, "bit_getint6");
	test(bit_getint(fixnumh(-1), &value), "bit_getint7");
	test(bit_getint(fixnumh(10), &value), "bit_getint8");
	test(bit_getint(T, &value), "bit_getint9");

	RETURN;
}


/*
 *  bitcons
 */
static int test_bitbuffer_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct bitbuffer_struct *str;
	size_t size, check;

	local = Local_Thread;
	push_local(local, &stack);
	bitbuffer_local(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_BITBUFFER, "bitbuffer_local1");
	str = BitBufferStruct(pos);
	test(str->index == 0, "bitbuffer_local2");
	test(str->array == 0, "bitbuffer_local3");
	test(str->data[0] == 0, "bitbuffer_local4");
	test(str->data[9] == 0, "bitbuffer_local5");
	size = lenbodyr(pos);
	check = sizeoft(struct bitbuffer_struct) + sizeoft(fixed) * 10UL;
	test(size == check, "bitbuffer_local6");
	GetBitBuffer(pos, &pos);
	test(pos == Nil, "bitbuffer_local7");
	rollback_local(local, stack);

	RETURN;
}

static int test_getfixedsize(void)
{
	test(getfixedsize(0) == 0, "getfixedsize1");
	test(getfixedsize(1) == 1, "getfixedsize2");
	test(getfixedsize(2) == 1, "getfixedsize3");
	test(getfixedsize(8*sizeoft(fixed) - 1) == 1, "getfixedsize4");
	test(getfixedsize(8*sizeoft(fixed) + 0) == 1, "getfixedsize5");
	test(getfixedsize(8*sizeoft(fixed) + 1) == 2, "getfixedsize6");
	test(getfixedsize(8*sizeoft(fixed) + 2) == 2, "getfixedsize7");
	test(getfixedsize(2*8*sizeoft(fixed) - 1) == 2, "getfixedsize8");
	test(getfixedsize(2*8*sizeoft(fixed) + 0) == 2, "getfixedsize9");
	test(getfixedsize(2*8*sizeoft(fixed) + 1) == 3, "getfixedsize10");
	test(getfixedsize(2*8*sizeoft(fixed) + 2) == 3, "getfixedsize11");

	RETURN;
}

static int test_bitcons_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct bitcons_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &pos, 3);
	test(GetType(pos) == LISPSYSTEM_BITCONS, "bitcons_local1");
	str = BitConsStruct(pos);
	test(str->bitsize == sizeoft(fixed) * 8UL, "bitcons_local2");
	test(str->fixedsize == 1, "bitcons_local3");
	test(str->index == 0, "bitcons_local4");
	GetBitConsRoot(pos, &check);
	test(GetType(check) == LISPSYSTEM_BITBUFFER, "bitcons_local5");
	GetBitConsTail(pos, &pos);
	test(pos == check, "bitcons_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_pushnext_bitcons(void)
{
	addr pos, check, next;
	LocalRoot local;
	LocalStack stack;
	struct bitbuffer_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &pos, 5);
	pushnext_bitcons(local, pos, &next);
	GetBitConsRoot(pos, &check);
	test(check == next, "pushnext_bitcons1");

	str = BitBufferStruct(next);
	str->index = 8UL * sizeoft(fixed) - 1UL;
	pushnext_bitcons(local, pos, &next);
	GetBitConsRoot(pos, &check);
	test(check == next, "pushnext_bitcons2");

	str = BitBufferStruct(next);
	str->index = 8UL * sizeoft(fixed);
	pushnext_bitcons(local, pos, &next);
	GetBitConsRoot(pos, &check);
	test(check != next, "pushnext_bitcons3");
	GetBitConsTail(pos, &check);
	test(check == next, "pushnext_bitcons4");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_bitcons(void)
{
	addr pos, next;
	LocalRoot local;
	LocalStack stack;
	struct bitbuffer_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &pos, 1);
	push_bitcons(local, pos, 1);
	GetBitConsTail(pos, &next);
	str = BitBufferStruct(next);
	test(str->data[0], "push_bitcons1");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  bitmemory
 */
static int test_bitmemory_unsafe(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct bitmemory_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitmemory_unsafe(local, &pos, 9999);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_unsafe1");
	test(GetStatusDynamic(pos), "bitmemory_unsafe2");
	str = BitMemoryStruct(pos);
	test(str->bitsize == 9999, "bitmemory_unsafe3");
	test(str->fixedsize == getfixedsize(9999), "bitmemory_unsafe4");

	rollback_local(local, stack);

	RETURN;
}

static int test_bitmemory_alloc(void)
{
	addr pos;
	struct bitmemory_struct *str;

	bitmemory_alloc(NULL, &pos, 9999);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_alloc1");
	test(! GetStatusDynamic(pos), "bitmemory_alloc2");
	str = BitMemoryStruct(pos);
	test(str->bitsize == 9999, "bitmemory_alloc3");
	test(str->fixedsize == getfixedsize(9999), "bitmemory_alloc4");
	test(str->data[0] == 0, "bitmemory_alloc5");
	test(str->data[1] == 0, "bitmemory_alloc6");

	RETURN;
}

static int test_bitmemory_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bitmemory_local(local, &pos, 9999);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_local1");
	test(GetStatusDynamic(pos), "bitmemory_local2");

	rollback_local(local, stack);

	RETURN;
}

static int test_bitmemory_heap(void)
{
	addr pos;

	bitmemory_heap(&pos, 9999);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_heap1");
	test(! GetStatusDynamic(pos), "bitmemory_heap2");

	RETURN;
}

static int test_bitcheck(struct bitmemory_struct *str, unsigned index, unsigned bit)
{
	return (str->data[index] >> bit) & 1;
}

static int test_bitmemory_cons_alloc(void)
{
	int i;
	addr cons, pos;
	LocalRoot local;
	LocalStack stack;
	struct bitmemory_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (i = 0; i < 99; i++) {
		push_bitcons(local, cons, 1);
		push_bitcons(local, cons, 1);
		push_bitcons(local, cons, 1);
		push_bitcons(local, cons, 0);
		push_bitcons(local, cons, 0);
		push_bitcons(local, cons, 0);
		push_bitcons(local, cons, 1);
		push_bitcons(local, cons, 1);
	}
	bitmemory_cons_alloc(local, &pos, cons);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_cons_alloc1");
	test(GetStatusDynamic(pos), "bitmemory_cons_alloc2");
	str = BitMemoryStruct(pos);
	test(str->bitsize == 8*99, "bitmemory_cons_alloc3");
	test(str->fixedsize == getfixedsize(8*99), "bitmemory_cons_alloc4");
	test(  test_bitcheck(str, 0, 0), "bitmemory_cons_alloc5");
	test(  test_bitcheck(str, 0, 1), "bitmemory_cons_alloc6");
	test(  test_bitcheck(str, 0, 2), "bitmemory_cons_alloc7");
	test(! test_bitcheck(str, 0, 3), "bitmemory_cons_alloc8");
	test(! test_bitcheck(str, 0, 4), "bitmemory_cons_alloc9");
	test(! test_bitcheck(str, 0, 5), "bitmemory_cons_alloc10");
	test(  test_bitcheck(str, 0, 6), "bitmemory_cons_alloc11");
	test(  test_bitcheck(str, 1, 0), "bitmemory_cons_alloc12");
	test(  test_bitcheck(str, 1, 1), "bitmemory_cons_alloc13");
	test(  test_bitcheck(str, 1, 2), "bitmemory_cons_alloc14");
	test(! test_bitcheck(str, 1, 3), "bitmemory_cons_alloc15");
	test(! test_bitcheck(str, 1, 4), "bitmemory_cons_alloc16");
	test(! test_bitcheck(str, 1, 5), "bitmemory_cons_alloc17");
	test(  test_bitcheck(str, 1, 6), "bitmemory_cons_alloc18");

	rollback_local(local, stack);

	RETURN;
}

static int test_bitmemory_cons_local(void)
{
	addr cons, pos;
	LocalRoot local;
	LocalStack stack;
	struct bitmemory_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	push_bitcons(local, cons, 1);
	push_bitcons(local, cons, 1);
	push_bitcons(local, cons, 0);
	bitmemory_cons_local(local, &pos, cons);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_cons_local1");
	test(GetStatusDynamic(pos), "bitmemory_cons_local2");
	str = BitMemoryStruct(pos);
	test(  test_bitcheck(str, 0, 0), "bitmemory_cons_local3");
	test(  test_bitcheck(str, 0, 1), "bitmemory_cons_local4");
	test(! test_bitcheck(str, 0, 2), "bitmemory_cons_local5");

	rollback_local(local, stack);

	RETURN;
}

static int test_bitmemory_cons_heap(void)
{
	addr cons, pos;
	LocalRoot local;
	LocalStack stack;
	struct bitmemory_struct *str;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	push_bitcons(local, cons, 1);
	push_bitcons(local, cons, 1);
	push_bitcons(local, cons, 0);
	bitmemory_cons_heap(&pos, cons);
	test(GetType(pos) == LISPTYPE_BITVECTOR, "bitmemory_cons_heap1");
	test(! GetStatusDynamic(pos), "bitmemory_cons_heap2");
	str = BitMemoryStruct(pos);
	test(  test_bitcheck(str, 0, 0), "bitmemory_cons_heap3");
	test(  test_bitcheck(str, 0, 1), "bitmemory_cons_heap4");
	test(! test_bitcheck(str, 0, 2), "bitmemory_cons_heap5");

	rollback_local(local, stack);

	RETURN;
}

static int test_bitmemoryp(void)
{
	addr pos;

	bitmemory_heap(&pos, 10);
	test(bitmemoryp(pos), "bitmemoryp1");
	test(! bitmemoryp(Nil), "bitmemoryp2");

	RETURN;
}

static int test_bitmemory_memset_byte(void)
{
	addr pos;

	bitmemory_heap(&pos, 256);
	bitmemory_memset_byte(pos, 0xA0);
	test(! bitmemory_refint(pos, 0), "bitmemory_memset_byte1");
	test(! bitmemory_refint(pos, 1), "bitmemory_memset_byte2");
	test(! bitmemory_refint(pos, 2), "bitmemory_memset_byte3");
	test(! bitmemory_refint(pos, 3), "bitmemory_memset_byte4");
	test(! bitmemory_refint(pos, 4), "bitmemory_memset_byte5");
	test(  bitmemory_refint(pos, 5), "bitmemory_memset_byte6");
	test(! bitmemory_refint(pos, 6), "bitmemory_memset_byte7");
	test(  bitmemory_refint(pos, 7), "bitmemory_memset_byte8");
	test(! bitmemory_refint(pos, 64+0), "bitmemory_memset_byte9");
	test(! bitmemory_refint(pos, 64+1), "bitmemory_memset_byte10");
	test(! bitmemory_refint(pos, 64+2), "bitmemory_memset_byte11");
	test(! bitmemory_refint(pos, 64+3), "bitmemory_memset_byte12");
	test(! bitmemory_refint(pos, 64+4), "bitmemory_memset_byte13");
	test(  bitmemory_refint(pos, 64+5), "bitmemory_memset_byte14");
	test(! bitmemory_refint(pos, 64+6), "bitmemory_memset_byte15");
	test(  bitmemory_refint(pos, 64+7), "bitmemory_memset_byte16");

	RETURN;
}

static int test_bitmemory_memset(void)
{
	addr pos;

	bitmemory_heap(&pos, 256);
	bitmemory_memset(pos, 0);
	test(! bitmemory_refint(pos, 0), "bitmemory_memset1");
	test(! bitmemory_refint(pos, 66), "bitmemory_memset2");
	bitmemory_memset(pos, 0);
	test(! bitmemory_refint(pos, 1), "bitmemory_memset3");
	test(! bitmemory_refint(pos, 67), "bitmemory_memset4");

	RETURN;
}

static int test_bitmemory_copy_unsafe(void)
{
	int i, n;
	addr a, b;

	bitmemory_heap(&a, 100);
	for (i = n = 0; i < 10; i++) {
		bitmemory_setint(a, n++, 1);
		bitmemory_setint(a, n++, 1);
		bitmemory_setint(a, n++, 0);
		bitmemory_setint(a, n++, 0);
		bitmemory_setint(a, n++, 1);
	}
	bitmemory_heap(&b, 100);
	bitmemory_copy_unsafe(b, a, 100);
	test(bitmemory_equal(a, b), "bitmemory_copy_unsafe1");

	RETURN;
}

static int test_bitmemory_length(void)
{
	addr pos;
	size_t size;

	bitmemory_heap(&pos, 55);
	bitmemory_length(pos, &size);
	test(size == 55, "bitmemory_length1");

	RETURN;
}

static int test_bitmemory_equal(void)
{
	addr pos1, pos2;

	bitmemory_char_heap(&pos1, "1011");
	bitmemory_char_heap(&pos2, "1011");
	test(bitmemory_equal(pos1, pos2), "bitmemory_equal1");

	bitmemory_char_heap(&pos1, "1101");
	bitmemory_char_heap(&pos2, "1011");
	test(! bitmemory_equal(pos1, pos2), "bitmemory_equal2");

	bitmemory_char_heap(&pos1, "10110");
	bitmemory_char_heap(&pos2, "1011");
	test(! bitmemory_equal(pos1, pos2), "bitmemory_equal3");

	bitmemory_char_heap(&pos1, "1011");
	bitmemory_char_heap(&pos2, "10110");
	test(! bitmemory_equal(pos1, pos2), "bitmemory_equal4");

	bitmemory_char_heap(&pos1,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111");
	bitmemory_char_heap(&pos2,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111");
	test(bitmemory_equal(pos1, pos2), "bitmemory_equal5");

	bitmemory_char_heap(&pos1,
			"10010111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111");
	bitmemory_char_heap(&pos2,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111");
	test(! bitmemory_equal(pos1, pos2), "bitmemory_equal6");

	bitmemory_char_heap(&pos1,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111");
	bitmemory_char_heap(&pos2,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110101");
	test(! bitmemory_equal(pos1, pos2), "bitmemory_equal7");

	RETURN;
}

static int test_bitmemory_refint(void)
{
	addr pos;

	bitmemory_char_heap(&pos, "1011");
	test(  bitmemory_refint(pos, 0), "bitmemory_refint1");
	test(! bitmemory_refint(pos, 1), "bitmemory_refint2");
	test(  bitmemory_refint(pos, 2), "bitmemory_refint3");
	test(  bitmemory_refint(pos, 3), "bitmemory_refint4");

	RETURN;
}

static int test_bitmemory_getint(void)
{
	int check;
	addr pos;

	bitmemory_char_heap(&pos,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110101");
	bitmemory_getint(pos, 0, &check);
	test(check, "bitmemory_getint1");
	bitmemory_getint(pos, 1, &check);
	test(! check, "bitmemory_getint2");
	bitmemory_getint(pos, 2, &check);
	test(check, "bitmemory_getint3");
	bitmemory_getint(pos, 3, &check);
	test(check, "bitmemory_getint4");

	bitmemory_getint(pos, 65-1, &check);
	test(check, "bitmemory_getint5");
	bitmemory_getint(pos, 65+0, &check);
	test(check, "bitmemory_getint6");
	bitmemory_getint(pos, 65+1, &check);
	test(! check, "bitmemory_getint7");
	bitmemory_getint(pos, 65+2, &check);
	test(check, "bitmemory_getint8");
	bitmemory_getint(pos, 65+3, &check);
	test(check, "bitmemory_getint9");

	RETURN;
}

static int test_bitmemory_setint(void)
{
	addr pos1, pos2;

	bitmemory_char_heap(&pos1,
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110101");
	bitmemory_char_heap(&pos2,
			"10110111111000000111111000001101010101010111100100100111001110110"
			"01010111111000000111111000001101010101010111100100100111001110111"
			"10110111111000000111111000001101010101010111100100100111001110101");
	bitmemory_setint(pos1, 65-1, 0);
	bitmemory_setint(pos1, 65+0, 0);
	bitmemory_setint(pos1, 65+1, 1);
	bitmemory_setint(pos1, 65+2, 0);
	bitmemory_setint(pos1, 65+3, 1);
	test(bitmemory_equal(pos1, pos2), "bitmemory_setint1");

	RETURN;
}

static int test_bitmemory_get(void)
{
	addr pos, value;

	bitmemory_char_heap(&pos, "110010");
	bitmemory_get(NULL, pos, 0, &value);
	test(RefFixnum(value) == 1, "bitmemory_get1");
	bitmemory_get(NULL, pos, 1, &value);
	test(RefFixnum(value) == 1, "bitmemory_get2");
	bitmemory_get(NULL, pos, 2, &value);
	test(RefFixnum(value) == 0, "bitmemory_get3");
	bitmemory_get(NULL, pos, 4, &value);
	test(RefFixnum(value) == 1, "bitmemory_get4");
	bitmemory_get(NULL, pos, 5, &value);
	test(RefFixnum(value) == 0, "bitmemory_get5");

	RETURN;
}

static int test_bitmemory_aref(void)
{
	addr pos, value;

	bitmemory_char_heap(&pos, "110010");

	list_heap(&value, fixnumh(0), NULL);
	bitmemory_aref(NULL, pos, value, &value);
	test(RefFixnum(value) == 1, "bitmemory_aref1");

	list_heap(&value, fixnumh(2), NULL);
	bitmemory_aref(NULL, pos, value, &value);
	test(RefFixnum(value) == 0, "bitmemory_aref2");

	list_heap(&value, fixnumh(5), NULL);
	bitmemory_aref(NULL, pos, value, &value);
	test(RefFixnum(value) == 0, "bitmemory_aref3");

	RETURN;
}

static int test_bitmemory_set(void)
{
	addr pos, check;

	bitmemory_char_heap(&pos,   "110010");
	bitmemory_char_heap(&check, "100011");
	bitmemory_set(pos, 1, fixnumh(0));
	bitmemory_set(pos, 5, fixnumh(1));
	test(bitmemory_equal(pos, check), "bitmemory_set1");

	RETURN;
}

static int test_bitmemory_setf_aref(void)
{
	addr pos, check, args;

	bitmemory_char_heap(&pos,   "110010");
	bitmemory_char_heap(&check, "100011");
	list_heap(&args, fixnumh(1), NULL);
	bitmemory_setf_aref(pos, args, fixnumh(0));
	list_heap(&args, fixnumh(5), NULL);
	bitmemory_setf_aref(pos, args, fixnumh(1));
	test(bitmemory_equal(pos, check), "bitmemory_setf_aref1");

	RETURN;
}


/*
 *  bvarray
 */
static int test_array_bvarrayp(void)
{
	addr pos, list;

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_bvarrayp(pos), "array_bvarrayp1");

	GetTypeTable(&pos, Bit);
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	make_array_common(&pos, list, pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_bvarrayp(pos), "array_bvarrayp2");

	GetTypeTable(&pos, T);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_bvarrayp(pos), "array_bvarrayp3");

	RETURN;
}

static int test_bvarrayp(void)
{
	addr pos;

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, T, Nil, Nil, Nil);
	test(bvarrayp(pos), "bvarrayp1");
	test(! bvarrayp(T), "bvarrayp2");

	RETURN;
}

static int test_bitvectorp(void)
{
	addr pos;

	bitmemory_heap(&pos, 10);
	test(bitvectorp(pos), "bitvectorp1");

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(bitvectorp(pos), "bitvectorp2");
	test(! bitvectorp(T), "bitvectorp3");

	RETURN;
}

static int test_simple_array_bvarrayp(void)
{
	addr pos, list;

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(simple_array_bvarrayp(pos), "simple_array_bvarrayp1");

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, T, Nil, Nil, Nil);
	test(! simple_array_bvarrayp(pos), "simple_array_bvarrayp2");

	GetTypeTable(&pos, Bit);
	list_heap(&list, fixnumh(10), fixnumh(20), NULL);
	make_array_common(&pos, list, pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! simple_array_bvarrayp(pos), "simple_array_bvarrayp3");

	GetTypeTable(&pos, T);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! simple_array_bvarrayp(pos), "simple_array_bvarrayp4");

	RETURN;
}

static int test_simple_bvarrayp(void)
{
	addr pos;

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(simple_bvarrayp(pos), "simple_bvarrayp1");

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, T, Nil, Nil, Nil);
	test(! simple_bvarrayp(pos), "simple_bvarrayp2");

	test(! simple_bvarrayp(T), "simple_bvarrayp3");

	RETURN;
}

static int test_simple_bitvectorp(void)
{
	addr pos;

	bitmemory_heap(&pos, 10);
	test(simple_bitvectorp(pos), "simple_bitvectorp1");

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(simple_bitvectorp(pos), "simple_bitvectorp2");

	GetTypeTable(&pos, Bit);
	make_array_common(&pos, fixnumh(10), pos,
			Unbound, Unbound, T, Nil, Nil, Nil);
	test(! simple_bitvectorp(pos), "simple_bitvectorp3");

	test(! simple_bitvectorp(T), "simple_bitvectorp4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_bit(void)
{
	/* bit */
	TestBreak(test_bitp);
	TestBreak(test_bit_getint);
	/* bitcons */
	TestBreak(test_bitbuffer_local);
	TestBreak(test_getfixedsize);
	TestBreak(test_bitcons_local);
	TestBreak(test_pushnext_bitcons);
	TestBreak(test_push_bitcons);
	/* bitmemory */
	TestBreak(test_bitmemory_unsafe);
	TestBreak(test_bitmemory_alloc);
	TestBreak(test_bitmemory_local);
	TestBreak(test_bitmemory_heap);
	TestBreak(test_bitmemory_cons_alloc);
	TestBreak(test_bitmemory_cons_local);
	TestBreak(test_bitmemory_cons_heap);
	TestBreak(test_bitmemoryp);
	TestBreak(test_bitmemory_memset_byte);
	TestBreak(test_bitmemory_memset);
	TestBreak(test_bitmemory_copy_unsafe);
	TestBreak(test_bitmemory_length);
	TestBreak(test_bitmemory_equal);
	TestBreak(test_bitmemory_refint);
	TestBreak(test_bitmemory_getint);
	TestBreak(test_bitmemory_setint);
	TestBreak(test_bitmemory_get);
	TestBreak(test_bitmemory_aref);
	TestBreak(test_bitmemory_set);
	TestBreak(test_bitmemory_setf_aref);
	/* bvarray */
	TestBreak(test_array_bvarrayp);
	TestBreak(test_bvarrayp);
	TestBreak(test_bitvectorp);
	TestBreak(test_simple_array_bvarrayp);
	TestBreak(test_simple_bvarrayp);
	TestBreak(test_simple_bitvectorp);

	return 0;
}

int test_bit(void)
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
		build_readtable();
		lisp_initialize = 1;
		result = testbreak_bit();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

