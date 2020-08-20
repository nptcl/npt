#include "bignum.c"

#if 0
#include "c99.h"
#include "cons.h"
#include "constant.h"
#include "degrade.h"
#include "strtype.h"
#include "stream_string.h"

/*
 *  bignum
 */
static int test_alloc_bignum(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	alloc_bignum(local, &pos, 10);
	test(GetType(pos) == LISPTYPE_BIGNUM, "alloc_bignum1");
	test(RefAllocBignum(pos) == 10, "alloc_bignum2");
	GetSizeBignum(pos, &size);
	test(size == 0, "alloc_bignum3");
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "alloc_bignum4");
	GetRootBignum(pos, &pos);
	test(GetType(pos) == LISPSYSTEM_BIGDATA, "alloc_bignum5");

	rollback_local(local, stack);

	RETURN;
}

static int test_realloc_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root, check;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	realloc_bignum(local, pos, 10, 0);
	GetRootBignum(pos, &check);
	test(root == check, "realloc_bignum1");
	test(RefAllocBignum(pos) == 10, "realloc_bignum2");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	realloc_bignum(local, pos, 5, 0);
	GetRootBignum(pos, &check);
	test(root == check, "realloc_bignum3");
	test(RefAllocBignum(pos) == 10, "realloc_bignum4");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	realloc_bignum(local, pos, 10, 1);
	GetRootBignum(pos, &check);
	test(root != check, "realloc_bignum5");
	test(RefAllocBignum(pos) == 10, "realloc_bignum6");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	realloc_bignum(local, pos, 5, 1);
	GetRootBignum(pos, &check);
	test(root != check, "realloc_bignum7");
	test(RefAllocBignum(pos) == 5, "realloc_bignum8");

	alloc_bignum(local, &pos, 10);
	GetRootDataBignum(pos, &root, &data);
	data[0] = 10; data[1] = 11; data[2] = 12; data[3] = 13; data[4] = 14;
	SetSizeBignum(pos, 5);
	realloc_bignum(local, pos, 3, 0);
	GetRootBignum(pos, &check);
	test(root == check, "realloc_bignum9");

	realloc_bignum(local, pos, 3, 1);
	GetRootBignum(pos, &check);
	test(root != check, "realloc_bignum10");
	GetSizeBignum(pos, &size);
	test(size == 3, "realloc_bignum11");
	test(RefAllocBignum(pos) == 3, "realloc_bignum12");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 10, "realloc_bignum13");
	test(data[1] == 11, "realloc_bignum14");
	test(data[2] == 12, "realloc_bignum15");

	alloc_bignum(local, &pos, 10);
	GetRootDataBignum(pos, &root, &data);
	data[0] = 10; data[1] = 11; data[2] = 12; data[3] = 13; data[4] = 14;
	SetSizeBignum(pos, 5);
	realloc_bignum(local, pos, 8, 1);
	GetRootBignum(pos, &check);
	test(root != check, "realloc_bignum16");
	GetSizeBignum(pos, &size);
	test(size == 5, "realloc_bignum17");
	test(RefAllocBignum(pos) == 8, "realloc_bignum18");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 10, "realloc_bignum19");
	test(data[1] == 11, "realloc_bignum20");
	test(data[4] == 14, "realloc_bignum21");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignMinus, 10);
	GetSignBignum(pos, &sign);
	test(IsMinus(sign), "bignum_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 1, "bignum_alloc2");
	test(RefAllocBignum(pos) == 10, "bignum_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 0, "bignum_alloc4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_cons_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr cons, pos;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16,
			"46ce65ee5392ca7794d2d17124a80f0b" "69858583ad98db64896e295ddb32d7de"
			"67c88d5d0deaae1bcb9ddbbce0303abe" "dfbc0c1f703a94efdfb6c7bd790c316e"
			"9edb8d7bdb9baa007cf0a27d1b14fd74" "3731c85b617e2665f4ae2116a07ce706"
			"ce591bec85b2af178f8716ababe359a6" "24e955632e3d23ab869ffbe14be41785"
			"ded592bae8005ca61134b842f6cdfe0e" "539a1cabe381d381a368a74191b8727f"
			"a48049550993340d43fe8e694b2fa7cd" "4cf6e68b3f0fc4f9cd77be8b6bb0b726"
			"f0e4ca555ff882a72b55e48070c64d7c" "878ad6db56f715eaf866fc4708a8d2e6"
			"108bb710712362647c5b07b1cf3ff28e" "b5d624e6941edba355a932cd4a49034c"
			"292bb0c78f1fa835a33f709b50108368" "f4c311c3f03f57180e3eaf4fb671fe09"
			"495e24f44e1ae59b6c1b6d7edb1f6740" "41a0d3e85eca50ff006d9d37db74fbfa"
			"9f946ce900a03c6ec1addf5c123de54b" "af6dc4f1085cfeab354d58a7a8b48ab4"
			"391ca0f42f669d72bd1672a087008538" "6bb72ca9358375162c8c9ae4d49bc346");
	bignum_cons_alloc(local, &pos, SignMinus, cons);
	GetSignBignum(pos, &sign);
	test(IsMinus(sign), "bignum_cons_alloc1");
	GetSizeBignum(pos, &size);
	test(BIGCONS_SIZE < size, "bignum_cons_alloc2");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_copy_nosign_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos, right, root;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &right, SignMinus, 10);
	GetRootDataBignum(right, &root, &data);
	data[0] = 10;
	data[1] = 20;
	data[2] = 30;
	SetSizeBignum(right, 3);
	SetSignBignum(right, SignMinus);
	bignum_copy_nosign_alloc(local, &pos, right);
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "bignum_copy_nosign_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 3, "bignum_copy_nosign_alloc2");
	test(RefAllocBignum(pos) == 3, "bignum_copy_nosign_alloc3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 10, "bignum_copy_nosign_alloc4");
	test(data[1] == 20, "bignum_copy_nosign_alloc5");
	test(data[2] == 30, "bignum_copy_nosign_alloc6");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_copy_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos, right, root;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &right, SignMinus, 10);
	GetRootDataBignum(right, &root, &data);
	data[0] = 10;
	data[1] = 20;
	data[2] = 30;
	SetSizeBignum(right, 3);
	SetSignBignum(right, SignMinus);
	bignum_copy_alloc(local, &pos, right);
	GetSignBignum(pos, &sign);
	test(IsMinus(sign), "bignum_copy_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 3, "bignum_copy_alloc2");
	test(RefAllocBignum(pos) == 3, "bignum_copy_alloc3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 10, "bignum_copy_alloc4");
	test(data[1] == 20, "bignum_copy_alloc5");
	test(data[2] == 30, "bignum_copy_alloc6");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_value_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_alloc(local, &pos, SignPlus, 200);
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "bignum_value_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 1, "bignum_value_alloc2");
	test(RefAllocBignum(pos) == 1, "bignum_value_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 200, "bignum_value_alloc4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_value2_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value2_alloc(local, &pos, SignPlus, 10, 20);
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "bignum_value2_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 2, "bignum_value2_alloc2");
	test(RefAllocBignum(pos) == 2, "bignum_value2_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[1] == 10, "bignum_value2_alloc4");
	test(data[0] == 20, "bignum_value2_alloc5");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_zero_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_zero_alloc(local, &pos);
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "bignum_zero_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 1, "bignum_zero_alloc2");
	test(RefAllocBignum(pos) == 1, "bignum_zero_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 0, "bignum_zero_alloc4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_fixnum_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	fixnum_heap(&pos, -99);
	bignum_fixnum_alloc(local, &pos, pos);
	GetSignBignum(pos, &sign);
	test(IsMinus(sign), "bignum_fixnum_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 1, "bignum_fixnum_alloc2");
	test(RefAllocBignum(pos) == 1, "bignum_fixnum_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 99, "bignum_fixnum_alloc4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_fixnum_value_alloc(void)
{
	int sign;
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_fixnum_value_alloc(local, &pos, 99);
	GetSignBignum(pos, &sign);
	test(IsPlus(sign), "bignum_fixnum_value_alloc1");
	GetSizeBignum(pos, &size);
	test(size == 1, "bignum_fixnum_value_alloc2");
	test(RefAllocBignum(pos) == 1, "bignum_fixnum_value_alloc3");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 99, "bignum_fixnum_value_alloc4");
	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_counter_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 100);
	bignum_counter_local_(local, &right, left);
	test(bignump(right), "bignum_counter_alloc1");
	test(equal_fb_real(left, right), "bignum_counter_alloc2");

	bignum_value_local(local, &left, SignPlus, 200);
	bignum_counter_local_(local, &right, left);
	test(bignump(right), "bignum_counter_alloc3");
	test(equal_bb_real(left, right), "bignum_counter_alloc4");

	rollback_local(local, stack);

	RETURN;
}

static int test_getfixed_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 5);
	GetDataBignum(pos, &data);
	bigset(data, 0xAA, 10);
	data[0] = 10;
	data[1] = 11;
	data[2] = 12;
	data[4] = 14;
	getfixed_bignum(pos, 0, &check);
	test(check == 10, "getfixed_bignum1");
	getfixed_bignum(pos, 1, &check);
	test(check == 11, "getfixed_bignum2");
	getfixed_bignum(pos, 2, &check);
	test(check == 12, "getfixed_bignum3");
	getfixed_bignum(pos, 4, &check);
	test(check == 14, "getfixed_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_reffixed_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 5);
	GetDataBignum(pos, &data);
	bigset(data, 0xAA, 10);
	data[0] = 10;
	data[1] = 11;
	data[2] = 12;
	data[4] = 14;
	test(reffixed_bignum(pos, 0) == 10, "getfixed_bignum1");
	test(reffixed_bignum(pos, 1) == 11, "getfixed_bignum2");
	test(reffixed_bignum(pos, 2) == 12, "getfixed_bignum3");
	test(reffixed_bignum(pos, 4) == 14, "getfixed_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_setfixed_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 5);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0xAA, 10);

	setfixed_bignum(pos, 2, 200);
	GetSizeBignum(pos, &size);
	test(size == 5, "setfixed_bignum1");
	test(data[2] == 200, "setfixed_bignum2");

	setfixed_bignum(pos, 7, 300);
	GetSizeBignum(pos, &size);
	test(size == 8, "setfixed_bignum3");
	test(data[7] == 300, "setfixed_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_diet_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root, check;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 5);
	GetRootBignum(pos, &root);

	diet_bignum(local, pos);
	GetRootBignum(pos, &check);
	test(RefAllocBignum(pos) == 5, "diet_bignum1");
	test(root != check, "diet_bignum2");

	diet_bignum(local, pos);
	GetRootBignum(pos, &root);
	test(RefAllocBignum(pos) == 5, "diet_bignum3");
	test(root == check, "diet_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_sizepress_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0, 10);

	SetSizeBignum(pos, 1);
	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 1, "sizepress_bignum1");
	test(data[0] == 0, "sizepress_bignum2");

	SetSizeBignum(pos, 5);
	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 1, "sizepress_bignum3");
	test(data[0] == 0, "sizepress_bignum4");

	data[0] = 100;
	SetSizeBignum(pos, 1);
	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 1, "sizepress_bignum5");
	test(data[0] == 100, "sizepress_bignum6");

	data[0] = 100;
	SetSizeBignum(pos, 5);
	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 1, "sizepress_bignum7");
	test(data[0] == 100, "sizepress_bignum8");

	data[0] = 100;
	data[1] = 101;
	data[2] = 102;
	SetSizeBignum(pos, 5);
	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 3, "sizepress_bignum9");
	test(data[0] == 100, "sizepress_bignum10");
	test(data[1] == 101, "sizepress_bignum11");
	test(data[2] == 102, "sizepress_bignum12");

	sizepress_bignum(pos);
	test(RefSizeBignum(pos) == 3, "sizepress_bignum13");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  operation
 */
static int test_resize_nocopy_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root, check;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	resize_nocopy_bignum(local, pos, 10, 0);
	GetRootBignum(pos, &check);
	test(root == check, "resize_nocopy_bignum1");
	test(RefAllocBignum(pos) == 10, "resize_nocopy_bignum2");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	resize_nocopy_bignum(local, pos, 5, 0);
	GetRootBignum(pos, &check);
	test(root == check, "resize_nocopy_bignum3");
	test(RefAllocBignum(pos) == 10, "resize_nocopy_bignum4");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	resize_nocopy_bignum(local, pos, 10, 1);
	GetRootBignum(pos, &check);
	test(root != check, "resize_nocopy_bignum5");
	test(RefAllocBignum(pos) == 10, "resize_nocopy_bignum6");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	resize_nocopy_bignum(local, pos, 5, 1);
	GetRootBignum(pos, &check);
	test(root != check, "resize_nocopy_bignum7");
	test(RefAllocBignum(pos) == 5, "resize_nocopy_bignum8");

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	SetSizeBignum(pos, 5);
	resize_nocopy_bignum(local, pos, 3, 0);
	GetRootBignum(pos, &check);
	test(root == check, "resize_nocopy_bignum9");

	resize_nocopy_bignum(local, pos, 3, 1);
	GetRootBignum(pos, &check);
	test(root != check, "resize_nocopy_bignum10");
	GetSizeBignum(pos, &size);
	test(size == 3, "resize_nocopy_bignum11");
	test(RefAllocBignum(pos) == 3, "resize_nocopy_bignum12");
	GetRootBignum(pos, &root);

	alloc_bignum(local, &pos, 10);
	GetRootBignum(pos, &root);
	SetSizeBignum(pos, 5);
	resize_nocopy_bignum(local, pos, 8, 1);
	GetRootBignum(pos, &check);
	test(root != check, "resize_nocopy_bignum16");
	GetSizeBignum(pos, &size);
	test(size == 5, "resize_nocopy_bignum17");
	test(RefAllocBignum(pos) == 8, "resize_nocopy_bignum18");
	GetRootBignum(pos, &root);

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignMinus, 5);
	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(right, &root, &data);
	data[0] = 200;
	data[1] = 210;
	data[2] = 220;
	SetSizeBignum(right, 3);
	copy_bignum(local, left, right, 0);
	test(RefSizeBignum(left) == 3, "copy_bignum1");
	test(RefAllocBignum(left) == 5, "copy_bignum2");
	test(IsPlus(RefSignBignum(left)), "copy_bignum3");
	GetRootDataBignum(left, &root, &data);
	test(data[0] == 200, "copy_bignum4");
	test(data[1] == 210, "copy_bignum5");
	test(data[2] == 220, "copy_bignum6");

	SetSizeBignum(left, 1);
	copy_bignum(local, left, right, 1);
	test(RefSizeBignum(left) == 3, "copy_bignum7");
	test(RefAllocBignum(left) == 3, "copy_bignum8");

	rollback_local(local, stack);

	RETURN;
}

static int test_copy_noexpand_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &left, SignMinus, 5);
	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(right, &root, &data);
	data[0] = 200;
	data[1] = 210;
	data[2] = 220;
	SetSizeBignum(right, 3);
	copy_noexpand_bignum(left, right);
	test(RefSizeBignum(left) == 3, "copy_noexpand_bignum1");
	test(RefAllocBignum(left) == 5, "copy_noexpand_bignum2");
	test(IsPlus(RefSignBignum(left)), "copy_noexpand_bignum3");
	GetRootDataBignum(left, &root, &data);
	test(data[0] == 200, "copy_noexpand_bignum4");
	test(data[1] == 210, "copy_noexpand_bignum5");
	test(data[2] == 220, "copy_noexpand_bignum6");

	SetSizeBignum(left, 1);
	copy_noexpand_bignum(left, right);
	test(RefSizeBignum(left) == 3, "copy_noexpand_bignum7");
	test(RefAllocBignum(left) == 5, "copy_noexpand_bignum8");

	rollback_local(local, stack);

	RETURN;
}

static int test_setvalue_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignMinus, 10);
	SetSizeBignum(pos, 5);
	setvalue_bignum(pos, SignPlus, 99);
	test(RefSizeBignum(pos) == 1, "setvalue_bignum1");
	test(RefAllocBignum(pos) == 10, "setvalue_bignum2");
	test(IsPlus(RefSignBignum(pos)), "setvalue_bignum3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 99, "setvalue_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_setzero_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_alloc(local, &pos, SignMinus, 10);
	SetSizeBignum(pos, 5);
	setzero_bignum(pos);
	test(RefSizeBignum(pos) == 1, "setzero_bignum1");
	test(RefAllocBignum(pos) == 10, "setzero_bignum2");
	test(IsPlus(RefSignBignum(pos)), "setzero_bignum3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 0, "setzero_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_getbit_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignMinus, 10);
	GetDataBignum(pos, &data);
	bigset(data, 0, 10);
	data[0] = 1;
	data[1] = 1 << 2;
	SetSizeBignum(pos, 5);
	test(getbit_bignum(pos, 0), "getbit_bignum1");
	test(! getbit_bignum(pos, 1), "getbit_bignum2");
	test(! getbit_bignum(pos, BIGNUM_FULLBIT), "getbit_bignum3");
	test(getbit_bignum(pos, BIGNUM_FULLBIT + 2), "getbit_bignum4");

	bigset(data, 0xFF, 10);
	test(! getbit_bignum(pos, BIGNUM_FULLBIT * 5), "getbit_bignum5");
	test(getbit_bignum(pos, BIGNUM_FULLBIT * 5 - 1UL), "getbit_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_result_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 2);
	bignum_result_alloc(local, pos, &check);
	test(pos == check, "bignum_result_alloc1");

	bignum_value_alloc(local, &pos, SignPlus, 10);
	bignum_result_alloc(local, pos, &check);
	test(pos != check, "bignum_result_alloc2");
	test(GetType(check) == LISPTYPE_FIXNUM, "bignum_result_alloc3");
	test(RefFixnum(check) == 10, "bignum_result_alloc4");

	bignum_value_alloc(local, &pos, SignPlus, FIXNUM_MAX);
	bignum_result_alloc(local, pos, &check);
	test(pos != check, "bignum_result_alloc5");
	test(GetType(check) == LISPTYPE_FIXNUM, "bignum_result_alloc6");
	test(RefFixnum(check) == FIXNUM_MAX, "bignum_result_alloc7");

	bignum_value_alloc(local, &pos, SignPlus, FIXNUM_MAX + 1ULL);
	bignum_result_alloc(local, pos, &check);
	test(pos == check, "bignum_result_alloc8");

	bignum_value_alloc(local, &pos, SignMinus, FIXNUM_MIN);
	bignum_result_alloc(local, pos, &check);
	test(pos != check, "bignum_result_alloc9");
	test(GetType(check) == LISPTYPE_FIXNUM, "bignum_result_alloc10");
	test(RefFixnum(check) == FIXNUM_MIN, "bignum_result_alloc11");

	bignum_value_alloc(local, &pos, SignMinus, FIXNUM_UMIN + 1ULL);
	bignum_result_alloc(local, pos, &check);
	test(pos == check, "bignum_result_alloc12");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_throw_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(NULL, &pos, SignPlus, 10);
	test(! GetStatusDynamic(pos), "bignum_throw_heap1");
	bignum_throw_heap(pos, &check);
	test(pos == check, "bignum_throw_heap2");

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(GetStatusDynamic(pos), "bignum_throw_heap3");
	bignum_throw_heap(pos, &check);
	test(pos != check, "bignum_throw_heap4");
	test(! GetStatusDynamic(check), "bignum_throw_heap5");
	test(equal_bb_real(pos, check), "bignum_throw_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_fixnum_throw_heap(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&pos, 10);
	test(! GetStatusDynamic(pos), "fixnum_throw_heap1");
	fixnum_throw_heap(pos, &check);
	test(pos == check, "fixnum_throw_heap2");

	fixnum_local(local, &pos, 10);
	test(GetStatusDynamic(pos), "fixnum_throw_heap3");
	fixnum_throw_heap(pos, &check);
	test(pos != check, "fixnum_throw_heap4");
	test(! GetStatusDynamic(check), "fixnum_throw_heap5");
	test(RefFixnum(pos) == RefFixnum(check), "fixnum_throw_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_throw_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(GetStatusDynamic(pos), "bignum_throw_local1");
	bignum_throw_local(local, pos, &check);
	test(pos == check, "bignum_throw_local2");

	bignum_value_alloc(NULL, &pos, SignPlus, 10);
	test(! GetStatusDynamic(pos), "bignum_throw_local3");
	bignum_throw_local(local, pos, &check);
	test(pos != check, "bignum_throw_local4");
	test(GetStatusDynamic(check), "bignum_throw_local5");
	test(equal_bb_real(pos, check), "bignum_throw_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_fixnum_throw_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	test(GetStatusDynamic(pos), "fixnum_throw_local1");
	fixnum_throw_local(local, pos, &check);
	test(pos == check, "fixnum_throw_local2");

	fixnum_heap(&pos, 10);
	test(! GetStatusDynamic(pos), "fixnum_throw_local3");
	fixnum_throw_local(local, pos, &check);
	test(pos != check, "fixnum_throw_local4");
	test(GetStatusDynamic(check), "fixnum_throw_local5");
	test(RefFixnum(pos) == RefFixnum(check), "fixnum_throw_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_power2_bignum_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;
	size_t index;

	local = Local_Thread;
	push_local(local, &stack);

	power2_bignum_alloc(local, &pos, SignPlus, 0);
	test(GetType(pos) == LISPTYPE_FIXNUM, "power2_bignum_alloc1");
	test(RefFixnum(pos) == 1, "power2_bignum_alloc2");

	power2_bignum_alloc(local, &pos, SignMinus, 3);
	test(GetType(pos) == LISPTYPE_FIXNUM, "power2_bignum_alloc3");
	test(RefFixnum(pos) == -8, "power2_bignum_alloc4");

	power2_bignum_alloc(local, &pos, SignMinus, BIGNUM_FULLBIT);
	test(GetType(pos) == LISPTYPE_BIGNUM, "power2_bignum_alloc5");
	test(getbit_bignum(pos, BIGNUM_FULLBIT), "power2_bignum_alloc6");

	index = BIGNUM_FULLBIT * 10UL + 3UL;
	power2_bignum_alloc(local, &pos, SignPlus, index);
	test(GetType(pos) == LISPTYPE_BIGNUM, "power2_bignum_alloc7");
	test(getbit_bignum(pos, index), "power2_bignum_alloc8");

	rollback_local(local, stack);

	RETURN;
}

static int test_shiftup_bignum_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check, cons;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignMinus, 100);
	GetDataBignum(pos, &data);
	data[0] = 10;
	data[10] = 20;
	data[11] = 21;
	SetSizeBignum(pos, 12);
	shiftup_bignum_alloc(local, &check, pos, 0);
	test(pos == check, "shiftup_bignum_alloc1");
	shiftup_bignum_alloc(local, &pos, pos, BIGNUM_FULLBIT * 3);
	test(RefSizeBignum(pos) == 15, "shiftup_bignum_alloc1");
	test(IsMinus(RefSignBignum(pos)), "shiftup_bignum_alloc2");
	GetDataBignum(pos, &data);
	test(data[3] == 10, "shiftup_bignum_alloc3");
	test(data[13] == 20, "shiftup_bignum_alloc4");
	test(data[14] == 21, "shiftup_bignum_alloc5");

	bigcons_char_local(local, &cons, 16, "1234567890ABCDEF123456780ABCDEF");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	shiftup_bignum_alloc(local, &pos, pos, 4);
	bigcons_char_local(local, &cons, 16, "1234567890ABCDEF123456780ABCDEF0");
	bignum_cons_alloc(local, &check, SignPlus, cons);
	test(equal_bb_real(pos, check), "shiftup_bignum_alloc6");

	bigcons_char_local(local, &cons, 16, "1234567890ABCDEF");
	bignum_cons_alloc(local, &pos, SignMinus, cons);
	shiftup_bignum_alloc(local, &pos, pos, 256 + 4);
	bigcons_char_local(local, &cons, 16,
			"1234567890ABCDEF0000000000000000"
			"00000000000000000000000000000000"
			"0000000000000000" "0");
	bignum_cons_alloc(local, &check, SignMinus, cons);
	test(equal_bb_real(pos, check), "shiftup_bignum_alloc7");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  integer
 */
static int test_carryvalue_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	carryvalue_alloc(local, &pos, SignMinus, 10);
	test(RefAllocBignum(pos) == 2, "carryvalue_alloc1");
	test(RefSizeBignum(pos) == 2, "carryvalue_alloc2");
	test(IsMinus(RefSignBignum(pos)), "carryvalue_alloc3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] = 10, "carryvalue_alloc4");
	test(data[1] = 1, "carryvalue_alloc5");

	rollback_local(local, stack);

	RETURN;
}

static int test_castfixnum(void)
{
	fixnum value;
	fixed check;

	test(castfixnum(SignPlus, 10, &value), "castfixnum1");
	test(value == 10, "castfixnum2");
	test(castfixnum(SignMinus, 10, &value), "castfixnum3");
	test(value == -10, "castfixnum4");

	test(castfixnum(SignPlus, FIXNUM_MAX, &value), "castfixnum5");
	test(value == FIXNUM_MAX, "castfixnum6");
	test(castfixnum(SignMinus, FIXNUM_MAX, &value), "castfixnum7");
	test(value == -FIXNUM_MAX, "castfixnum8");

	test(castfixnum(SignPlus, FIXNUM_MAX + 1ULL, &value) == 0, "castfixnum9");
	check = FIXNUM_UMIN;
	test(castfixnum(SignMinus, check, &value), "castfixnum10");
	test(value == FIXNUM_MIN, "castfixnum11");
	check++;
	test(castfixnum(SignMinus, check, &value) == 0, "castfixnum12");

	RETURN;
}

static int test_fixnum_cons_alloc(void)
{
	int check;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 10, "12");
	check = fixnum_cons_alloc(local, &pos, SignMinus, cons);
	test(check == 0, "fixnum_cons_alloc1");
	test(fixnump(pos), "fixnum_cons_alloc2");
	test(RefFixnum(pos) == -12, "fixnum_cons_alloc3");

	setchar_bigcons(local, cons, 10, "123123123123123123123123123123");
	check = fixnum_cons_alloc(local, &pos, SignMinus, cons);
	test(check, "fixnum_cons_alloc4");

	rollback_local(local, stack);

	RETURN;
}

static int test_integer_cons_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr cons, pos;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 10, "22");
	integer_cons_alloc(local, &pos, SignMinus, cons);
	test(GetType(pos) == LISPTYPE_FIXNUM, "integer_cons_alloc");
	test(RefFixnum(pos) == -22, "integer_cons_alloc");

	setchar_bigcons(local, cons, 32,
			"46ce65ee5392ca7794d2d17124a80f0b" "69858583ad98db64896e295ddb32d7de"
			"67c88d5d0deaae1bcb9ddbbce0303abe" "dfbc0c1f703a94efdfb6c7bd790c316e");
	integer_cons_alloc(local, &pos, SignPlus, cons);
	test(GetType(pos) == LISPTYPE_BIGNUM, "integer_cons_alloc");

	rollback_local(local, stack);

	RETURN;
}

static int test_integer_fixed_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	integer_fixed_alloc(local, &pos, SignMinus, 10);
	test(GetType(pos) == LISPTYPE_FIXNUM, "integer_fixed_alloc1");
	test(RefFixnum(pos) == -10, "integer_fixed_alloc2");

	integer_fixed_alloc(local, &pos, SignPlus, 1ULL + (bigtype)FIXNUM_MAX);
	test(GetType(pos) == LISPTYPE_BIGNUM, "integer_fixed_alloc3");

	rollback_local(local, stack);

	RETURN;
}

static int test_integer_bignum_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignMinus, 10);
	integer_bignum_alloc(local, &pos, pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "integer_bignum_alloc1");
	test(RefFixnum(pos) == -10, "integer_bignum_alloc2");

	bignum_alloc(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 2);
	integer_bignum_alloc(local, &check, pos);
	test(GetType(check) == LISPTYPE_BIGNUM, "integer_bignum_alloc3");
	test(check == pos, "integer_bignum_alloc4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  integer-copy
 */
static int test_integer_copy_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	integer_copy_alloc(local, pos, &pos);
	test(fixnump(pos), "integer_copy_alloc1");
	test(RefFixnum(pos) == 10, "integer_copy_alloc2");

	bignum_value_local(local, &pos, SignMinus, 20);
	integer_copy_alloc(local, pos, &pos);
	test(bignump(pos), "integer_copy_alloc3");
	test(equal_value_bignum(pos, SignMinus, 20), "integer_copy_alloc4");

	bignum_value2_local(local, &pos, SignPlus, 20, 30);
	integer_copy_alloc(local, pos, &pos);
	test(bignump(pos), "integer_copy_alloc5");
	test(equal_value2_bignum(pos, SignPlus, 20, 30), "integer_copy_alloc6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  float
 */
static int test_HexToChar(void)
{
	test(HexToChar(0) == '0', "HexToChar1");
	test(HexToChar(9) == '9', "HexToChar2");
	test(HexToChar(10) == 'A', "HexToChar3");
	test(HexToChar(15) == 'F', "HexToChar4");
	test(HexToChar(16) == 'G', "HexToChar5");

	RETURN;
}

static int test_hexchar_bigtype(void)
{
	char data[100], check[100], *ptr;
	size_t size, i;

#if defined(BIGNUM_CODE_64BIT)
	size = 16;
#elif defined(BIGNUM_CODE_32BIT)
	size = 8;
#elif defined(BIGNUM_CODE_8BIT)
	size = 2;
#else
	size = 0;
#error Abort
#endif
	memset(data, 0xAA, 100);
	memset(check, 0xAA, 100);
	ptr = hexchar_bigtype(data, 0);
	test(ptr == data + size, "hexchar_bigtype1");
	for (i = 0; i < size; i++) check[i] = '0';
	test(memcmp(data, check, 100) == 0, "hexchar_bigtype2");

	memset(data, 0xAA, 100);
	memset(check, 0xAA, 100);
	ptr = hexchar_bigtype(data, 0xAB);
	for (i = 0; i < size; i++) check[i] = '0';
	check[size - 2] = 'A';
	check[size - 1] = 'B';
	test(memcmp(data, check, 100) == 0, "hexchar_bigtype3");

	RETURN;
}

static int test_hexfraction_string(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, cons;
	char data[100], *ptr;
	size_t exponent;

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF0123456789ABCDEFAAA");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	memset(data, 0xAA, 100);
	ptr = hexfraction_string(data, pos, LISP_FLOAT_SINGLE_FRACTION, &exponent);
	ptr[0] = 0;
	ptr = data;
	while (*ptr == '0') ptr++;
	test(memcmp(ptr, "1234567890", 4) == 0, "hexfraction_string1");

	rollback_local(local, stack);

	RETURN;
}

static int test_expchar_make_float(void)
{
	char data[100], *ptr;

	memset(data, 0xAA, 100);
	ptr = expchar_make_float(data, 0);
	test(ptr == data + 1, "expchar_make_float1");
	test(data[0] == '0', "expchar_make_float2");

	ptr = expchar_make_float(data, 5);
	test(ptr == data + 1, "expchar_make_float3");
	test(data[0] == '5', "expchar_make_float4");

	ptr = expchar_make_float(data, 9);
	test(ptr == data + 1, "expchar_make_float5");
	test(data[0] == '9', "expchar_make_float6");

	ptr = expchar_make_float(data, 10);
	test(ptr == data + 2, "expchar_make_float7");
	test(data[0] == '1', "expchar_make_float8");
	test(data[1] == '0', "expchar_make_float9");

	ptr = expchar_make_float(data, 654);
	test(ptr == data + 3, "expchar_make_float10");
	test(data[0] == '6', "expchar_make_float11");
	test(data[1] == '5', "expchar_make_float12");
	test(data[2] == '4', "expchar_make_float13");

	RETURN;
}

static int test_make_float_string(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, cons;
	char data[100];

	local = Local_Thread;
	push_local(local, &stack);

	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF0123456789ABCDEFAAA");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	memset(data, 0xAA, 100);
	make_float_string(data, pos, LISP_FLOAT_SINGLE_FRACTION);
	test(memcmp(data, "+0x", 3) == 0, "make_float_string1");

	bignum_cons_alloc(local, &pos, SignMinus, cons);
	memset(data, 0xAA, 100);
	make_float_string(data, pos, LISP_FLOAT_SINGLE_FRACTION);
	test(memcmp(data, "-0x", 3) == 0, "make_float_string2");

	rollback_local(local, stack);

	RETURN;
}

static int test_single_float_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, cons;
	char data[100];
	single_float value;

	local = Local_Thread;
	push_local(local, &stack);

	/* single */
	bignum_value_alloc(local, &pos, SignPlus, 100);
	value = single_float_bignum(pos);
	test(value == 100.0f, "single_float_bignum1");

	/* multiple */
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF1234567890ABCDEF");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	value = single_float_bignum(pos);
	snprintc(data, 100, "%+9.6e", value);
	test(strcmp(data, "+2.419786e+37") == 0, "single_float_bignum2");

	bignum_cons_alloc(local, &pos, SignMinus, cons);
	value = single_float_bignum(pos);
	snprintc(data, 100, "%+9.6e", value);
	test(strcmp(data, "-2.419786e+37") == 0, "single_float_bignum3");

	rollback_local(local, stack);

	RETURN;
}

static int test_double_float_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, cons;
	char data[100];
	double_float value;

	local = Local_Thread;
	push_local(local, &stack);

	/* single */
	bignum_value_alloc(local, &pos, SignPlus, 100);
	value = double_float_bignum(pos);
	test(value == 100.0f, "double_float_bignum1");

	/* multiple */
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF1234567890ABCDEF");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	value = double_float_bignum(pos);
	snprintc(data, 100, "%+18.15e", value);
	test(strcmp(data, "+2.419785720015125e+37") == 0, "double_float_bignum2");

	bignum_cons_alloc(local, &pos, SignMinus, cons);
	value = double_float_bignum(pos);
	snprintc(data, 100, "%+18.15e", value);
	test(strcmp(data, "-2.419785720015125e+37") == 0, "double_float_bignum3");

	rollback_local(local, stack);

	RETURN;
}

static int test_long_float_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, cons;
	char data[100];
	long_float value;

	local = Local_Thread;
	push_local(local, &stack);

	/* single */
	bignum_value_alloc(local, &pos, SignPlus, 100);
	value = long_float_bignum(pos);
	test(value == 100.0f, "long_float_bignum1");

	/* multiple */
#ifdef LISP_WINDOWS
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF1234567890ABCDEF");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	value = long_float_bignum(pos);
	snprintc(data, 100, "%+18.15e", value);
	test(strcmp(data, "+2.419785720015125e+37") == 0, "long_float_bignum2");

	bignum_cons_alloc(local, &pos, SignMinus, cons);
	value = long_float_bignum(pos);
	snprintc(data, 100, "%+18.15e", value);
	test(strcmp(data, "-2.419785720015125e+37") == 0, "long_float_bignum3");
#else
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "1234567890ABCDEF1234567890ABCDEF");
	bignum_cons_alloc(local, &pos, SignPlus, cons);
	value = long_float_bignum(pos);
	snprintc(data, 100, "%+21.18Le", value);
	test(strcmp(data, "+2.419785720015125273e+37") == 0, "long_float_bignum2");

	bignum_cons_alloc(local, &pos, SignMinus, cons);
	value = long_float_bignum(pos);
	snprintc(data, 100, "%+21.18Le", value);
	test(strcmp(data, "-2.419785720015125273e+37") == 0, "long_float_bignum3");
#endif

	rollback_local(local, stack);

	RETURN;
}

static int test_single_float_fixnum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 12);
	single_float_fixnum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "single_float_fixnum_alloc1");
	test(RefSingleFloat(right) == 12.0f, "single_float_fixnum_alloc2");

	fixnum_heap(&left, -12);
	single_float_fixnum_local(local, &right, left);
	test(GetStatusDynamic(right), "single_float_fixnum_alloc3");
	test(RefSingleFloat(right) == -12.0f, "single_float_fixnum_alloc4");

	fixnum_local(local, &left, 23);
	single_float_fixnum_heap(&right, left);
	test(! GetStatusDynamic(right), "single_float_fixnum_alloc5");
	test(RefSingleFloat(right) == 23.0f, "single_float_fixnum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_double_float_fixnum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 12);
	double_float_fixnum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "double_float_fixnum_alloc1");
	test(RefDoubleFloat(right) == 12.0, "double_float_fixnum_alloc2");

	fixnum_heap(&left, -12);
	double_float_fixnum_local(local, &right, left);
	test(GetStatusDynamic(right), "double_float_fixnum_alloc3");
	test(RefDoubleFloat(right) == -12.0, "double_float_fixnum_alloc4");

	fixnum_local(local, &left, 23);
	double_float_fixnum_heap(&right, left);
	test(! GetStatusDynamic(right), "double_float_fixnum_alloc5");
	test(RefDoubleFloat(right) == 23.0, "double_float_fixnum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_long_float_fixnum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 12);
	long_float_fixnum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "long_float_fixnum_alloc1");
	test(RefLongFloat(right) == 12.0L, "long_float_fixnum_alloc2");

	fixnum_heap(&left, -12);
	long_float_fixnum_local(local, &right, left);
	test(GetStatusDynamic(right), "long_float_fixnum_alloc3");
	test(RefLongFloat(right) == -12.0L, "long_float_fixnum_alloc4");

	fixnum_local(local, &left, 23);
	long_float_fixnum_heap(&right, left);
	test(! GetStatusDynamic(right), "long_float_fixnum_alloc5");
	test(RefLongFloat(right) == 23.0L, "long_float_fixnum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_single_float_bignum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 12);
	single_float_bignum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "single_float_bignum_alloc1");
	test(RefSingleFloat(right) == 12.0f, "single_float_bignum_alloc2");

	bignum_value_heap(&left, SignMinus, 12);
	single_float_bignum_local(local, &right, left);
	test(GetStatusDynamic(right), "single_float_bignum_alloc3");
	test(RefSingleFloat(right) == -12.0f, "single_float_bignum_alloc4");

	bignum_value_local(local, &left, SignMinus, 23);
	single_float_bignum_heap(&right, left);
	test(! GetStatusDynamic(right), "single_float_bignum_alloc5");
	test(RefSingleFloat(right) == -23.0f, "single_float_bignum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_double_float_bignum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 12);
	double_float_bignum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "double_float_bignum_alloc1");
	test(RefDoubleFloat(right) == 12.0, "double_float_bignum_alloc2");

	bignum_value_heap(&left, SignMinus, 12);
	double_float_bignum_local(local, &right, left);
	test(GetStatusDynamic(right), "double_float_bignum_alloc3");
	test(RefDoubleFloat(right) == -12.0, "double_float_bignum_alloc4");

	bignum_value_local(local, &left, SignMinus, 23);
	double_float_bignum_heap(&right, left);
	test(! GetStatusDynamic(right), "double_float_bignum_alloc5");
	test(RefDoubleFloat(right) == -23.0, "double_float_bignum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_long_float_bignum_alloc(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 12);
	long_float_bignum_alloc(local, &right, left);
	test(GetStatusDynamic(right), "long_float_bignum_alloc1");
	test(RefLongFloat(right) == 12.0L, "long_float_bignum_alloc2");

	bignum_value_heap(&left, SignMinus, 12);
	long_float_bignum_local(local, &right, left);
	test(GetStatusDynamic(right), "long_float_bignum_alloc3");
	test(RefLongFloat(right) == -12.0L, "long_float_bignum_alloc4");

	bignum_value_local(local, &left, SignMinus, 23);
	long_float_bignum_heap(&right, left);
	test(! GetStatusDynamic(right), "long_float_bignum_alloc5");
	test(RefLongFloat(right) == -23.0L, "long_float_bignum_alloc6");

	rollback_local(local, stack);

	RETURN;
}

static int test_printf_integer_float_size(void)
{
	size_t size;

	/* 0 */
	size = printf_integer_float_size(0);
	test(1+1+1 <= size, "printf_integer_float_size1");
	/* 8 */
	size = printf_integer_float_size(3);
	test(1+1+1 <= size, "printf_integer_float_size2");
	/* 16 */
	size = printf_integer_float_size(4);
	test(1+1+2 <= size, "printf_integer_float_size3");
	/* 65535 */
	size = printf_integer_float_size(16);
	test(1+1+5 <= size, "printf_integer_float_size4");
	/* 4294967296 */
	size = printf_integer_float_size(32);
	test(1+1+10 <= size, "printf_integer_float_size5");
	/* 4294967296 */
	size = printf_integer_float_size(32);
	test(1+1+10 <= size, "printf_integer_float_size6");
	/* 1606938044258990275541962092341162602522202993782792835301376 */
	size = printf_integer_float_size(200);
	test(1+1+61 <= size, "printf_integer_float_size7");

	RETURN;
}

static int test_bignum_single_float_alloc(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr pos, cons, temp;

	local = Local_Thread;
	push_local(local, &stack);

	check = bignum_single_float_alloc(local, &pos, 0.0f, 0);
	test(check == 0, "bignum_single_float_alloc1");
	test(zerop_bignum(pos), "bignum_single_float_alloc2");

	check = bignum_single_float_alloc(local, &pos, -0.0f, 0);
	test(check == 0, "bignum_single_float_alloc3");
	test(zerop_bignum(pos), "bignum_single_float_alloc4");

	check = bignum_single_float_alloc(local, &pos, 0.5f, 0);
	test(check, "bignum_single_float_alloc5");

	check = bignum_single_float_alloc(local, &pos, 1.0f, 0);
	test(check == 0, "bignum_single_float_alloc6");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_single_float_alloc7");
	test(GetStatusDynamic(pos), "bignum_single_float_alloc8");

	check = bignum_single_float_alloc(local, &pos, 1.0f, 1);
	test(check == 0, "bignum_single_float_alloc9");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_single_float_alloc10");
	test(! GetStatusDynamic(pos), "bignum_single_float_alloc11");

	check = bignum_single_float_alloc(local, &pos, -123.456e20f, 0);
	test(check == 0, "bignum_single_float_alloc12");
	bigcons_char_local(local, &cons, 10, "12345599439020522209280");
	bignum_cons_alloc(local, &temp, SignMinus, cons);
	test(equal_bb_real(pos, temp), "bignum_single_float_alloc13");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_double_float_alloc(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr pos, cons, temp;

	local = Local_Thread;
	push_local(local, &stack);

	check = bignum_double_float_alloc(local, &pos, 0.0, 0);
	test(check == 0, "bignum_double_float_alloc1");
	test(zerop_bignum(pos), "bignum_double_float_alloc2");

	check = bignum_double_float_alloc(local, &pos, -0.0, 0);
	test(check == 0, "bignum_double_float_alloc3");
	test(zerop_bignum(pos), "bignum_double_float_alloc4");

	check = bignum_double_float_alloc(local, &pos, 0.5, 0);
	test(check, "bignum_double_float_alloc5");

	check = bignum_double_float_alloc(local, &pos, 1.0, 0);
	test(check == 0, "bignum_double_float_alloc6");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_double_float_alloc7");
	test(GetStatusDynamic(pos), "bignum_double_float_alloc8");

	check = bignum_double_float_alloc(local, &pos, 1.0, 1);
	test(check == 0, "bignum_double_float_alloc9");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_double_float_alloc10");
	test(! GetStatusDynamic(pos), "bignum_double_float_alloc11");

	check = bignum_double_float_alloc(local, &pos, -123.456e20, 0);
	test(check == 0, "bignum_double_float_alloc12");
	bigcons_char_local(local, &cons, 10, "12345600000000000000000");
	bignum_cons_alloc(local, &temp, SignMinus, cons);
	test(equal_bb_real(pos, temp), "bignum_double_float_alloc13");

	rollback_local(local, stack);

	RETURN;
}

static int test_bignum_long_float_alloc(void)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr pos, cons, temp;

	local = Local_Thread;
	push_local(local, &stack);

	check = bignum_long_float_alloc(local, &pos, 0.0, 0);
	test(check == 0, "bignum_long_float_alloc1");
	test(zerop_bignum(pos), "bignum_long_float_alloc2");

	check = bignum_long_float_alloc(local, &pos, -0.0, 0);
	test(check == 0, "bignum_long_float_alloc3");
	test(zerop_bignum(pos), "bignum_long_float_alloc4");

	check = bignum_long_float_alloc(local, &pos, 0.5, 0);
	test(check, "bignum_long_float_alloc5");

	check = bignum_long_float_alloc(local, &pos, 1.0, 0);
	test(check == 0, "bignum_long_float_alloc6");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_long_float_alloc7");
	test(GetStatusDynamic(pos), "bignum_long_float_alloc8");

	check = bignum_long_float_alloc(local, &pos, 1.0, 1);
	test(check == 0, "bignum_long_float_alloc9");
	test(equal_value_bignum(pos, SignPlus, 1), "bignum_long_float_alloc10");
	test(! GetStatusDynamic(pos), "bignum_long_float_alloc11");

	check = bignum_long_float_alloc(local, &pos, -123.456e20, 0);
	test(check == 0, "bignum_long_float_alloc12");
	bigcons_char_local(local, &cons, 10, "12345600000000000000000");
	bignum_cons_alloc(local, &temp, SignMinus, cons);
	test(equal_bb_real(pos, temp), "bignum_long_float_alloc13");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  compare
 */
static int test_zerop_or_plusp_bignum(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 0);
	test(zerop_or_plusp_bignum(pos), "zerop_or_plusp_bignum1");
	bignum_value_local(local, &pos, SignMinus, 0);
	test(zerop_or_plusp_bignum(pos), "zerop_or_plusp_bignum2");
	bignum_value_local(local, &pos, SignPlus, 10);
	test(zerop_or_plusp_bignum(pos), "zerop_or_plusp_bignum3");
	bignum_value_local(local, &pos, SignMinus, 10);
	test(! zerop_or_plusp_bignum(pos), "zerop_or_plusp_bignum4");

	rollback_local(local, stack);

	RETURN;

}

static int test_plusp_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(plusp_bignum(pos), "plusp_bignum1");
	bignum_value_alloc(local, &pos, SignMinus, 20);
	test(! plusp_bignum(pos), "plusp_bignum2");
	bignum_zero_alloc(local, &pos);
	test(! plusp_bignum(pos), "plusp_bignum3");
	SetSignBignum(pos, SignMinus);
	test(! plusp_bignum(pos), "plusp_bignum4");
	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0xAA, 10);
	SetSizeBignum(pos, 5);
	test(plusp_bignum(pos), "plusp_bignum5");
	SetSignBignum(pos, SignMinus);
	test(! plusp_bignum(pos), "plusp_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_minusp_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(! minusp_bignum(pos), "minusp_bignum1");
	bignum_value_alloc(local, &pos, SignMinus, 20);
	test(minusp_bignum(pos), "minusp_bignum2");
	bignum_zero_alloc(local, &pos);
	test(! minusp_bignum(pos), "minusp_bignum3");
	SetSignBignum(pos, SignMinus);
	test(! minusp_bignum(pos), "minusp_bignum4");
	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0xAA, 10);
	SetSizeBignum(pos, 5);
	test(! minusp_bignum(pos), "minusp_bignum5");
	SetSignBignum(pos, SignMinus);
	test(minusp_bignum(pos), "minusp_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_zerop_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(! zerop_bignum(pos), "zerop_bignum1");
	bignum_value_alloc(local, &pos, SignMinus, 20);
	test(! zerop_bignum(pos), "zerop_bignum2");
	bignum_zero_alloc(local, &pos);
	test(zerop_bignum(pos), "zerop_bignum3");
	SetSignBignum(pos, SignMinus);
	test(zerop_bignum(pos), "zerop_bignum4");
	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0xAA, 10);
	SetSizeBignum(pos, 5);
	test(! zerop_bignum(pos), "zerop_bignum5");
	SetSignBignum(pos, SignMinus);
	test(! zerop_bignum(pos), "zerop_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_evenp_bignum(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_local(local, &pos, SignPlus, 10);
	test(evenp_bignum(pos), "evenp_bignum1");
	bignum_value_local(local, &pos, SignPlus, 11);
	test(! evenp_bignum(pos), "evenp_bignum2");
	bignum_value_local(local, &pos, SignMinus, 10);
	test(evenp_bignum(pos), "evenp_bignum3");
	bignum_value_local(local, &pos, SignMinus, 11);
	test(! evenp_bignum(pos), "evenp_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_oddp_bignum(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bignum_value_local(local, &pos, SignPlus, 10);
	test(! oddp_bignum(pos), "oddp_bignum1");
	bignum_value_local(local, &pos, SignPlus, 11);
	test(oddp_bignum(pos), "oddp_bignum2");
	bignum_value_local(local, &pos, SignMinus, 10);
	test(! oddp_bignum(pos), "oddp_bignum3");
	bignum_value_local(local, &pos, SignMinus, 11);
	test(oddp_bignum(pos), "oddp_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_castfixed(void)
{
	int sign;
	fixed value;

	castfixed(100, &sign, &value);
	test(IsPlus(sign), "castfixed1");
	test(value == 100, "castfixed2");

	castfixed(-100, &sign, &value);
	test(IsMinus(sign), "castfixed3");
	test(value == 100, "castfixed4");

	castfixed(FIXNUM_MAX, &sign, &value);
	test(IsPlus(sign), "castfixed5");
	test(value == (fixed)FIXNUM_MAX, "castfixed6");

	castfixed(FIXNUM_MIN, &sign, &value);
	test(IsMinus(sign), "castfixed7");
	test(value == FIXNUM_UMIN, "castfixed8");

	RETURN;
}

static int test_castfixed_fixnum(void)
{
	int sign;
	addr pos;
	fixed value;

	fixnum_heap(&pos, 100);
	castfixed_fixnum(pos, &sign, &value);
	test(IsPlus(sign), "castfixed_fixnum1");
	test(value == 100, "castfixed_fixnum2");

	fixnum_heap(&pos, -100);
	castfixed_fixnum(pos, &sign, &value);
	test(IsMinus(sign), "castfixed_fixnum3");
	test(value == 100, "castfixed_fixnum4");

	fixnum_heap(&pos, FIXNUM_MAX);
	castfixed_fixnum(pos, &sign, &value);
	test(IsPlus(sign), "castfixed_fixnum5");
	test(value == (fixed)FIXNUM_MAX, "castfixed_fixnum6");

	fixnum_heap(&pos, FIXNUM_MIN);
	castfixed_fixnum(pos, &sign, &value);
	test(IsMinus(sign), "castfixed_fixnum7");
	test(value == FIXNUM_UMIN, "castfixed_fixnum8");

	RETURN;
}

static int test_castfixed_integer(void)
{
	int check, sign;
	addr pos;
	fixed value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&pos, 10);
	value = 999;
	sign = 999;
	check = castfixed_integer(pos, &sign, &value);
	test(check == 0, "castfixed_integer1");
	test(IsPlus(sign), "castfixed_integer2");
	test(value == 10, "castfixed_integer3");

	bignum_value_local(local, &pos, SignMinus, 20);
	check = castfixed_integer(pos, &sign, &value);
	test(check == 0, "castfixed_integer4");
	test(IsMinus(sign), "castfixed_integer5");
	test(value == 20, "castfixed_integer6");

	bignum_value2_local(local, &pos, SignMinus, 20, 30);
	check = castfixed_integer(pos, &sign, &value);
	test(check, "castfixed_integer7");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_fb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &right, SignPlus, 100);
	fixnum_local(local, &left, 100);
	test(equal_fb_real(left, right), "equal_fb_real1");
	fixnum_local(local, &left, -100);
	test(! equal_fb_real(left, right), "equal_fb_real2");
	fixnum_local(local, &left, 88);
	test(! equal_fb_real(left, right), "equal_fb_real3");
	fixnum_local(local, &left, -88);
	test(! equal_fb_real(left, right), "equal_fb_real4");

	bignum_value_alloc(local, &right, SignMinus, 20);
	fixnum_local(local, &left, 20);
	test(! equal_fb_real(left, right), "equal_fb_real5");
	fixnum_local(local, &left, -20);
	test(equal_fb_real(left, right), "equal_fb_real6");
	fixnum_local(local, &left, 100);
	test(! equal_fb_real(left, right), "equal_fb_real7");
	fixnum_local(local, &left, -100);
	test(! equal_fb_real(left, right), "equal_fb_real8");

	bignum_zero_alloc(local, &right);
	fixnum_local(local, &left, 0);
	test(equal_fb_real(left, right), "equal_fb_real9");
	SetSignBignum(right, SignMinus);
	test(equal_fb_real(left, right), "equal_fb_real10");

	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(right, &root, &data);
	bigset(data, 0, 10);
	SetSizeBignum(right, 5);
	fixnum_local(local, &left, 0);
	test(! equal_fb_real(left, right), "equal_fb_real11");
	SetSignBignum(right, SignMinus);
	test(! equal_fb_real(left, right), "equal_fb_real12");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_bb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data1, *data2;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 0);
	bignum_value_alloc(local, &right, SignPlus, 0);
	test(equal_bb_real(left, right), "equal_bb_real1");
	SetSignBignum(left, SignMinus);
	test(equal_bb_real(left, right), "equal_bb_real2");

	bignum_value_alloc(local, &left, SignPlus, 10);
	bignum_value_alloc(local, &right, SignPlus, 0);
	test(! equal_bb_real(left, right), "equal_bb_real3");

	bignum_value_alloc(local, &left, SignPlus, 10);
	bignum_value_alloc(local, &right, SignMinus, 10);
	test(! equal_bb_real(left, right), "equal_bb_real4");

	bignum_value_alloc(local, &left, SignPlus, 33);
	bignum_value_alloc(local, &right, SignPlus, 33);
	test(equal_bb_real(left, right), "equal_bb_real5");

	bignum_alloc(local, &left, SignPlus, 10);
	bignum_alloc(local, &right, SignPlus, 10);
	GetRootDataBignum(left, &root, &data1);
	GetRootDataBignum(right, &root, &data2);
	bigset(data1, 0xAA, 10);
	bigset(data2, 0xAA, 10);
	data1[2] = data2[2] = 12;
	SetSizeBignum(left, 5);
	SetSizeBignum(right, 5);
	test(equal_bb_real(left, right), "equal_bb_real6");

	SetSignBignum(left, SignMinus);
	test(! equal_bb_real(left, right), "equal_bb_real7");
	SetSignBignum(left, SignPlus);

	SetSizeBignum(left, 4);
	test(! equal_bb_real(left, right), "equal_bb_real8");
	SetSizeBignum(left, 5);

	data1[3] = 100;
	test(! equal_bb_real(left, right), "equal_bb_real9");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_nosign_bignum(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &right, SignMinus, 10);
	test(equal_nosign_bignum(left, right), "equal_nosign_bignum1");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &right, SignPlus, 10);
	test(equal_nosign_bignum(left, right), "equal_nosign_bignum2");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignPlus, 20);
	test(! equal_nosign_bignum(left, right), "equal_nosign_bignum3");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value2_local(local, &right, SignPlus, 10, 10);
	test(! equal_nosign_bignum(left, right), "equal_nosign_bignum4");

	bignum_value2_local(local, &left, SignMinus, 10, 20);
	bignum_value2_local(local, &right, SignPlus, 10, 20);
	test(equal_nosign_bignum(left, right), "equal_nosign_bignum5");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bigtype(void)
{
	test(compare_bigtype(SignPlus, 0, SignMinus, 0) == 0, "compare_bigtype1");
	test(compare_bigtype(SignPlus, 0, SignPlus, 0) == 0, "compare_bigtype2");

	test(compare_bigtype(SignPlus, 1, SignPlus, 1) == 0, "compare_bigtype3");
	test(compare_bigtype(SignMinus, 1, SignMinus, 1) == 0, "compare_bigtype4");
	test(compare_bigtype(SignPlus, 1, SignMinus, 1) > 0, "compare_bigtype5");
	test(compare_bigtype(SignMinus, 1, SignPlus, 1) < 0, "compare_bigtype6");

	test(compare_bigtype(SignPlus, 10, SignPlus, 20) < 0, "compare_bigtype7");
	test(compare_bigtype(SignMinus, 10, SignPlus, 20) < 0, "compare_bigtype8");
	test(compare_bigtype(SignPlus, 10, SignMinus, 20) > 0, "compare_bigtype9");
	test(compare_bigtype(SignMinus, 10, SignMinus, 20) > 0, "compare_bigtype10");

	test(compare_bigtype(SignPlus, 30, SignPlus, 20) > 0, "compare_bigtype11");
	test(compare_bigtype(SignMinus, 30, SignPlus, 20) < 0, "compare_bigtype12");
	test(compare_bigtype(SignPlus, 30, SignMinus, 20) > 0, "compare_bigtype13");
	test(compare_bigtype(SignMinus, 30, SignMinus, 20) < 0, "compare_bigtype14");

	RETURN;
}

static int test_compare_value_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &right, SignPlus, 10);
	SetSizeBignum(right, 3);
	GetRootDataBignum(right, &root, &data);
	bigset(data, 0, 10);
	test(compare_value_bignum(10, right) < 0, "compare_value_bignum1");
	SetSignBignum(right, SignMinus);
	test(compare_value_bignum(10, right) > 0, "compare_value_bignum2");

	SetSizeBignum(right, 1);
	SetSignBignum(right, SignPlus);
	data[0] = 10;
	test(compare_value_bignum(-10, right) < 0, "compare_value_bignum3");
	SetSignBignum(right, SignMinus);
	data[0] = 10;
	test(compare_value_bignum(-10, right) == 0, "compare_value_bignum4");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bignum_value(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &left, SignPlus, 10);
	SetSizeBignum(left, 3);
	GetRootDataBignum(left, &root, &data);
	bigset(data, 0, 10);
	test(compare_bignum_value(left, 10) > 0, "compare_bignum_value1");
	SetSignBignum(left, SignMinus);
	test(compare_bignum_value(left, 10) < 0, "compare_bignum_value2");

	SetSizeBignum(left, 1);
	SetSignBignum(left, SignPlus);
	data[0] = 10;
	test(compare_bignum_value(left, -10) > 0, "compare_bignum_value3");
	SetSignBignum(left, SignMinus);
	data[0] = 10;
	test(compare_bignum_value(left, -10) == 0, "compare_bignum_value4");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_fb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_alloc(local, &left, 10);
	bignum_alloc(local, &right, SignPlus, 10);
	SetSizeBignum(right, 3);
	GetRootDataBignum(right, &root, &data);
	bigset(data, 0, 10);
	test(compare_fb_real(left, right) < 0, "compare_fb_real1");
	SetSignBignum(right, SignMinus);
	test(compare_fb_real(left, right) > 0, "compare_fb_real2");

	fixnum_alloc(local, &left, -10);
	SetSizeBignum(right, 1);
	SetSignBignum(right, SignPlus);
	data[0] = 10;
	test(compare_fb_real(left, right) < 0, "compare_fb_real3");
	SetSignBignum(right, SignMinus);
	data[0] = 10;
	test(compare_fb_real(left, right) == 0, "compare_fb_real4");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bf_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_alloc(local, &right, 10);
	bignum_alloc(local, &left, SignPlus, 10);
	SetSizeBignum(left, 3);
	GetRootDataBignum(left, &root, &data);
	bigset(data, 0, 10);
	test(compare_bf_real(left, right) > 0, "compare_bf_real1");
	SetSignBignum(left, SignMinus);
	test(compare_bf_real(left, right) < 0, "compare_bf_real2");

	fixnum_alloc(local, &right, -10);
	SetSizeBignum(left, 1);
	SetSignBignum(left, SignPlus);
	data[0] = 10;
	test(compare_bf_real(left, right) > 0, "compare_bf_real3");
	SetSignBignum(left, SignMinus);
	data[0] = 10;
	test(compare_bf_real(left, right) == 0, "compare_bf_real4");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_alloc(local, &left);
	bignum_zero_alloc(local, &right);
	test(compare_bb_real(left, right) == 0, "compare_bb_real1");
	SetSignBignum(left, SignMinus);
	test(compare_bb_real(left, right) == 0, "compare_bb_real2");

	bignum_zero_alloc(local, &left);
	bignum_value_alloc(local, &right, SignPlus, 10);
	test(compare_bb_real(left, right) < 0, "compare_bb_real3");
	SetSignBignum(right, SignMinus);
	test(compare_bb_real(left, right) > 0, "compare_bb_real4");

	bignum_zero_alloc(local, &right);
	bignum_value_alloc(local, &left, SignPlus, 10);
	test(compare_bb_real(left, right) > 0, "compare_bb_real5");
	SetSignBignum(left, SignMinus);
	test(compare_bb_real(left, right) < 0, "compare_bb_real6");

	bignum_value_alloc(local, &left, SignPlus, 10);
	bignum_value_alloc(local, &right, SignMinus, 20);
	test(compare_bb_real(left, right) > 0, "compare_bb_real7");
	SetSignBignum(left, SignMinus);
	SetSignBignum(right, SignPlus);
	test(compare_bb_real(left, right) < 0, "compare_bb_real8");

	bignum_value_alloc(local, &left, SignPlus, 10);
	bignum_value_alloc(local, &right, SignPlus, 10);
	test(compare_bb_real(left, right) == 0, "compare_bb_real9");
	bignum_value_alloc(local, &left, SignPlus, 10);
	bignum_value_alloc(local, &right, SignPlus, 20);
	test(compare_bb_real(left, right) < 0, "compare_bb_real10");
	bignum_value_alloc(local, &left, SignPlus, 20);
	bignum_value_alloc(local, &right, SignPlus, 10);
	test(compare_bb_real(left, right) > 0, "compare_bb_real11");

	bignum_value_alloc(local, &left, SignMinus, 10);
	bignum_value_alloc(local, &right, SignMinus, 10);
	test(compare_bb_real(left, right) == 0, "compare_bb_real12");
	bignum_value_alloc(local, &left, SignMinus, 10);
	bignum_value_alloc(local, &right, SignMinus, 20);
	test(compare_bb_real(left, right) > 0, "compare_bb_real13");
	bignum_value_alloc(local, &left, SignMinus, 20);
	bignum_value_alloc(local, &right, SignMinus, 10);
	test(compare_bb_real(left, right) < 0, "compare_bb_real14");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_value_nosign_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0, 10);
	SetSizeBignum(pos, 1);
	test(equal_value_nosign_bignum(pos, 0), "equal_value_nosign_bignum1");
	SetSizeBignum(pos, 3);
	test(! equal_value_nosign_bignum(pos, 0), "equal_value_nosign_bignum2");

	bignum_zero_alloc(local, &pos);
	test(equal_value_nosign_bignum(pos, 0), "equal_value_nosign_bignum3");
	SetSignBignum(pos, SignMinus);
	test(equal_value_nosign_bignum(pos, 0), "equal_value_nosign_bignum4");

	bignum_value_alloc(local, &pos, SignMinus, 10);
	test(equal_value_nosign_bignum(pos, 10), "equal_value_nosign_bignum5");
	test(! equal_value_nosign_bignum(pos, 20), "equal_value_nosign_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_value_bignum(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_alloc(local, &pos, SignPlus, 10);
	GetRootDataBignum(pos, &root, &data);
	bigset(data, 0, 10);
	SetSizeBignum(pos, 1);
	test(equal_value_bignum(pos, SignPlus, 0), "equal_value_bignum1");
	SetSizeBignum(pos, 3);
	test(! equal_value_bignum(pos, SignPlus, 0), "equal_value_bignum2");

	bignum_zero_alloc(local, &pos);
	test(equal_value_bignum(pos, SignPlus, 0), "equal_value_bignum3");
	test(equal_value_bignum(pos, SignMinus, 0), "equal_value_bignum4");
	SetSignBignum(pos, SignMinus);
	test(equal_value_bignum(pos, SignPlus, 0), "equal_value_bignum5");
	test(equal_value_bignum(pos, SignMinus, 0), "equal_value_bignum6");

	bignum_value_alloc(local, &pos, SignPlus, 10);
	test(equal_value_bignum(pos, SignPlus, 10), "equal_value_bignum7");
	test(! equal_value_bignum(pos, SignMinus, 10), "equal_value_bignum8");
	test(! equal_value_bignum(pos, SignPlus, 20), "equal_value_bignum9");
	test(! equal_value_bignum(pos, SignMinus, 20), "equal_value_bignum10");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_value2_nosign_bignum(void)
{
	int check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	check = equal_value2_nosign_bignum(pos, 0, 10);
	test(check, "equal_value2_nosign_bignum1");

	bignum_value_local(local, &pos, SignMinus, 10);
	check = equal_value2_nosign_bignum(pos, 0, 10);
	test(check, "equal_value2_nosign_bignum2");

	bignum_value_local(local, &pos, SignPlus, 10);
	check = equal_value2_nosign_bignum(pos, 10, 10);
	test(! check, "equal_value2_nosign_bignum3");

	bignum_value2_local(local, &pos, SignPlus, 10, 20);
	check = equal_value2_nosign_bignum(pos, 10, 20);
	test(check, "equal_value2_nosign_bignum4");

	bignum_value2_local(local, &pos, SignMinus, 10, 20);
	check = equal_value2_nosign_bignum(pos, 20, 10);
	test(! check, "equal_value2_nosign_bignum5");

	bignum_value2_local(local, &pos, SignMinus, 10, 20);
	check = equal_value2_nosign_bignum(pos, 10, 21);
	test(! check, "equal_value2_nosign_bignum6");

	rollback_local(local, stack);

	RETURN;
}

static int test_equal_value2_bignum(void)
{
	int check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	check = equal_value2_bignum(pos, SignPlus, 0, 10);
	test(check, "equal_value2_bignum1");

	bignum_value_local(local, &pos, SignPlus, 10);
	check = equal_value2_bignum(pos, SignMinus, 0, 10);
	test(! check, "equal_value2_bignum2");

	bignum_value_local(local, &pos, SignPlus, 10);
	check = equal_value2_bignum(pos, SignPlus, 10, 10);
	test(! check, "equal_value2_bignum3");

	bignum_value2_local(local, &pos, SignPlus, 10, 20);
	check = equal_value2_bignum(pos, SignPlus, 10, 20);
	test(check, "equal_value2_bignum4");

	bignum_value2_local(local, &pos, SignPlus, 10, 20);
	check = equal_value2_bignum(pos, SignMinus, 10, 20);
	test(! check, "equal_value2_bignum5");

	bignum_value2_local(local, &pos, SignMinus, 10, 20);
	check = equal_value2_bignum(pos, SignMinus, 20, 10);
	test(! check, "equal_value2_bignum6");

	bignum_value2_local(local, &pos, SignMinus, 10, 20);
	check = equal_value2_bignum(pos, SignMinus, 10, 21);
	test(! check, "equal_value2_bignum7");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bs_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 100);
	single_float_local(local, &right, +100.0f);
	test(compare_bs_real(left, right) == 0,
			"compare_bs_real1");
	single_float_local(local, &right, +100.1f);
	test(compare_bs_real(left, right) < 0,
			"compare_bs_real2");
	single_float_local(local, &right, +99.9f);
	test(compare_bs_real(left, right) > 0,
			"compare_bs_real3");
	single_float_local(local, &right, -100.0f);
	test(compare_bs_real(left, right) > 0,
			"compare_bs_real4");

	bignum_value_alloc(local, &left, SignMinus, 100);
	single_float_local(local, &right, -100.0f);
	test(compare_bs_real(left, right) == 0,
			"compare_bs_real5");
	single_float_local(local, &right, -100.1f);
	test(compare_bs_real(left, right) > 0,
			"compare_bs_real6");
	single_float_local(local, &right, -99.9f);
	test(compare_bs_real(left, right) < 0,
			"compare_bs_real7");
	single_float_local(local, &right, 100.0f);
	test(compare_bs_real(left, right) < 0,
			"compare_bs_real8");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bd_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 100);
	double_float_local(local, &right, +100.0);
	test(compare_bd_real(left, right) == 0,
			"compare_bd_real1");
	double_float_local(local, &right, +100.1);
	test(compare_bd_real(left, right) < 0,
			"compare_bd_real2");
	double_float_local(local, &right, +99.9);
	test(compare_bd_real(left, right) > 0,
			"compare_bd_real3");
	double_float_local(local, &right, -100.0);
	test(compare_bd_real(left, right) > 0,
			"compare_bd_real4");

	bignum_value_alloc(local, &left, SignMinus, 100);
	double_float_local(local, &right, -100.0);
	test(compare_bd_real(left, right) == 0,
			"compare_bd_real5");
	double_float_local(local, &right, -100.1);
	test(compare_bd_real(left, right) > 0,
			"compare_bd_real6");
	double_float_local(local, &right, -99.9);
	test(compare_bd_real(left, right) < 0,
			"compare_bd_real7");
	double_float_local(local, &right, 100.0);
	test(compare_bd_real(left, right) < 0,
			"compare_bd_real8");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_bl_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &left, SignPlus, 100);
	long_float_local(local, &right, +100.0L);
	test(compare_bl_real(left, right) == 0,
			"compare_bl_real1");
	long_float_local(local, &right, +100.1L);
	test(compare_bl_real(left, right) < 0,
			"compare_bl_real2");
	long_float_local(local, &right, +99.9L);
	test(compare_bl_real(left, right) > 0,
			"compare_bl_real3");
	long_float_local(local, &right, -100.0L);
	test(compare_bl_real(left, right) > 0,
			"compare_bl_real4");

	bignum_value_alloc(local, &left, SignMinus, 100);
	long_float_local(local, &right, -100.0L);
	test(compare_bl_real(left, right) == 0,
			"compare_bl_real5");
	long_float_local(local, &right, -100.1L);
	test(compare_bl_real(left, right) > 0,
			"compare_bl_real6");
	long_float_local(local, &right, -99.9L);
	test(compare_bl_real(left, right) < 0,
			"compare_bl_real7");
	long_float_local(local, &right, 100.0L);
	test(compare_bl_real(left, right) < 0,
			"compare_bl_real8");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_sb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	single_float_local(local, &left, +100.0f);
	bignum_value_alloc(local, &right, SignPlus, 100);
	test(compare_sb_real(left, right) == 0,
			"compare_bs_real1");
	single_float_local(local, &left, +100.1f);
	test(compare_sb_real(left, right) > 0,
			"compare_bs_real2");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_db_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	double_float_local(local, &left, +100.0);
	bignum_value_alloc(local, &right, SignPlus, 100);
	test(compare_db_real(left, right) == 0,
			"compare_bd_real1");
	double_float_local(local, &left, +100.1);
	test(compare_db_real(left, right) > 0,
			"compare_bd_real2");

	rollback_local(local, stack);

	RETURN;
}

static int test_compare_lb_real(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	long_float_local(local, &left, +100.0L);
	bignum_value_alloc(local, &right, SignPlus, 100);
	test(compare_lb_real(left, right) == 0, "compare_bl_real1");
	long_float_local(local, &left, +100.1L);
	test(compare_lb_real(left, right) > 0, "compare_bl_real2");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  byte
 */
static int test_fixnum_unsigned_byte_p(void)
{
	addr value;

	test(! fixnum_unsigned_byte_p(fixnumh(-8), 3), "fixnum_unsigned_byte_p1");
	test(! fixnum_unsigned_byte_p(fixnumh(-7), 3), "fixnum_unsigned_byte_p2");
	test(! fixnum_unsigned_byte_p(fixnumh(-1), 3), "fixnum_unsigned_byte_p3");
	test(fixnum_unsigned_byte_p(fixnumh(0), 3), "fixnum_unsigned_byte_p4");
	test(fixnum_unsigned_byte_p(fixnumh(6), 3), "fixnum_unsigned_byte_p5");
	test(fixnum_unsigned_byte_p(fixnumh(7), 3), "fixnum_unsigned_byte_p6");
	test(! fixnum_unsigned_byte_p(fixnumh(8), 3), "fixnum_unsigned_byte_p7");

	fixnum_heap(&value, FIXNUM_MAX);
	test(fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT), "fixnum_unsigned_byte_p8");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT), "fixnum_unsigned_byte_p9");

	fixnum_heap(&value, FIXNUM_MAX);
	test(fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT+1), "fixnum_unsigned_byte_p10");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT+1), "fixnum_unsigned_byte_p11");

	fixnum_heap(&value, FIXNUM_MAX);
	test(fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT-1), "fixnum_unsigned_byte_p12");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT-1), "fixnum_unsigned_byte_p13");

	fixnum_heap(&value, FIXNUM_MAX);
	test(! fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT-2), "fixnum_unsigned_byte_p14");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! fixnum_unsigned_byte_p(value, BIGNUM_FULLBIT-2), "fixnum_unsigned_byte_p15");

	RETURN;
}

static int test_bignum_unsigned_byte_p(void)
{
	int i, v, check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_alloc(local, &pos, SignPlus, 0);
	test(bignum_unsigned_byte_p(pos, 1), "bignum_unsigned_byte_p1");
	bignum_value_alloc(local, &pos, SignMinus, 0);
	test(bignum_unsigned_byte_p(pos, 1), "bignum_unsigned_byte_p2");

	bignum_value_alloc(local, &pos, SignMinus, 8);
	test(! bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p3");
	bignum_value_alloc(local, &pos, SignMinus, 7);
	test(! bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p4");
	bignum_value_alloc(local, &pos, SignMinus, 1);
	test(! bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p5");
	bignum_value_alloc(local, &pos, SignPlus, 1);
	test(bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p6");
	bignum_value_alloc(local, &pos, SignPlus, 6);
	test(bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p7");
	bignum_value_alloc(local, &pos, SignPlus, 7);
	test(bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p8");
	bignum_value_alloc(local, &pos, SignPlus, 8);
	test(! bignum_unsigned_byte_p(pos, 3), "bignum_unsigned_byte_p9");

	power2_bignum_alloc(local, &pos, SignPlus, 100);
	test(! bignum_unsigned_byte_p(pos, 100), "bignum_unsigned_byte_p10");
	minus_bf_real_local(local, pos, fixnuml(1), &pos);
	test(bignum_unsigned_byte_p(pos, 100), "bignum_unsigned_byte_p11");

	check = 1;
	for (i = 0; i < BIGNUM_FULLBIT * 3; i++) {
		v = 1000 + i;
		power2_bignum_alloc(local, &pos, SignPlus, v);
		if (bignum_unsigned_byte_p(pos, v)) {
			check = 1; break;
		}
		minus_bf_real_local(local, pos, fixnuml(1), &pos);
		if (! bignum_unsigned_byte_p(pos, v)) {
			check = 1; break;
		}
	}
	test(check, "bignum_unsigned_byte_p12");

	rollback_local(local, stack);

	RETURN;
}

static int test_fixnum_signed_byte_p(void)
{
	addr value;

	test(! fixnum_signed_byte_p(fixnumh(-5), 3), "fixnum_signed_byte_p1");
	test(fixnum_signed_byte_p(fixnumh(-4), 3), "fixnum_signed_byte_p2");
	test(fixnum_signed_byte_p(fixnumh(-3), 3), "fixnum_signed_byte_p3");
	test(fixnum_signed_byte_p(fixnumh(0), 3), "fixnum_signed_byte_p4");
	test(fixnum_signed_byte_p(fixnumh(3), 3), "fixnum_signed_byte_p5");
	test(! fixnum_signed_byte_p(fixnumh(4), 3), "fixnum_signed_byte_p6");
	test(! fixnum_signed_byte_p(fixnumh(5), 3), "fixnum_signed_byte_p7");

	fixnum_heap(&value, FIXNUM_MAX);
	test(fixnum_signed_byte_p(value, BIGNUM_FULLBIT), "fixnum_signed_byte_p8");
	fixnum_heap(&value, FIXNUM_MIN);
	test(fixnum_signed_byte_p(value, BIGNUM_FULLBIT), "fixnum_signed_byte_p9");

	fixnum_heap(&value, FIXNUM_MAX);
	test(fixnum_signed_byte_p(value, BIGNUM_FULLBIT+1), "fixnum_signed_byte_p10");
	fixnum_heap(&value, FIXNUM_MIN);
	test(fixnum_signed_byte_p(value, BIGNUM_FULLBIT+1), "fixnum_signed_byte_p11");

	fixnum_heap(&value, FIXNUM_MAX);
	test(! fixnum_signed_byte_p(value, BIGNUM_FULLBIT-1), "fixnum_signed_byte_p12");
	fixnum_heap(&value, FIXNUM_MIN);
	test(! fixnum_signed_byte_p(value, BIGNUM_FULLBIT-1), "fixnum_signed_byte_p13");

	RETURN;
}

static int test_bignum_signed_byte_p(void)
{
	int i, v, check;
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignMinus, 2);
	test(! bignum_signed_byte_p(pos, 1), "bignum_signed_byte_p1");
	bignum_value_local(local, &pos, SignMinus, 1);
	test(bignum_signed_byte_p(pos, 1), "bignum_signed_byte_p2");
	bignum_value_local(local, &pos, SignMinus, 0);
	test(bignum_signed_byte_p(pos, 1), "bignum_signed_byte_p3");
	bignum_value_local(local, &pos, SignPlus, 0);
	test(bignum_signed_byte_p(pos, 1), "bignum_signed_byte_p4");
	bignum_value_local(local, &pos, SignPlus, 1);
	test(! bignum_signed_byte_p(pos, 1), "bignum_signed_byte_p5");

	bignum_value_local(local, &pos, SignMinus, 5);
	test(! bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p6");
	bignum_value_local(local, &pos, SignMinus, 4);
	test(bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p7");
	bignum_value_local(local, &pos, SignMinus, 3);
	test(bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p8");
	bignum_value_local(local, &pos, SignPlus, 1);
	test(bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p9");
	bignum_value_local(local, &pos, SignPlus, 3);
	test(bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p10");
	bignum_value_local(local, &pos, SignPlus, 4);
	test(! bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p11");
	bignum_value_local(local, &pos, SignPlus, 5);
	test(! bignum_signed_byte_p(pos, 3), "bignum_signed_byte_p12");

	power2_bignum_alloc(local, &pos, SignMinus, 99);
	test(bignum_signed_byte_p(pos, 100), "bignum_signed_byte_p13");
	minus_bf_real_local(local, pos, fixnuml(1), &pos);
	test(! bignum_signed_byte_p(pos, 100), "bignum_signed_byte_p14");

	power2_bignum_alloc(local, &pos, SignPlus, 99);
	test(! bignum_signed_byte_p(pos, 100), "bignum_signed_byte_p15");
	minus_bf_real_local(local, pos, fixnuml(1), &pos);
	test(bignum_signed_byte_p(pos, 100), "bignum_signed_byte_p16");

	check = 1;
	for (i = 0; i < BIGNUM_FULLBIT * 3; i++) {
		v = 1000 + i;
		power2_bignum_alloc(local, &pos, SignMinus, v - 1);
		if (! bignum_signed_byte_p(pos, v)) {
			check = 1; break;
		}
		minus_bf_real_local(local, pos, fixnuml(1), &pos);
		if (bignum_signed_byte_p(pos, v)) {
			check = 1; break;
		}

		power2_bignum_alloc(local, &pos, SignPlus, v - 1);
		if (bignum_signed_byte_p(pos, v)) {
			check = 1; break;
		}
		minus_bf_real_local(local, pos, fixnuml(1), &pos);
		if (! bignum_signed_byte_p(pos, v)) {
			check = 1; break;
		}
	}
	test(check, "bignum_signed_byte_p17");

	rollback_local(local, stack);

	RETURN;
}

static int test_GetFixnum_bignum(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	test(GetFixnum_bignum(pos, &value) == 0, "GetFixnum_bignum1");
	test(value == 10, "GetFixnum_bignum2");

	bignum_value_local(local, &pos, SignMinus, 20);
	test(GetFixnum_bignum(pos, &value) == 0, "GetFixnum_bignum3");
	test(value == -20, "GetFixnum_bignum4");

	bignum_value2_local(local, &pos, SignPlus, 10, 20);
	test(GetFixnum_bignum(pos, &value), "GetFixnum_bignum5");

	bignum_value_local(local, &pos, SignPlus, FIXNUM_MAX);
	test(GetFixnum_bignum(pos, &value) == 0, "GetFixnum_bignum6");
	test(value == FIXNUM_MAX, "GetFixnum_bignum7");

	bignum_value_local(local, &pos, SignPlus, ((bigtype)FIXNUM_MAX) + 1UL);
	test(GetFixnum_bignum(pos, &value), "GetFixnum_bignum8");

	bignum_value_local(local, &pos, SignMinus, FIXNUM_UMIN);
	test(GetFixnum_bignum(pos, &value) == 0, "GetFixnum_bignum9");
	test(value == FIXNUM_MIN, "GetFixnum_bignum10");

	bignum_value_local(local, &pos, SignMinus, FIXNUM_UMIN + 1UL);
	test(GetFixnum_bignum(pos, &value), "GetFixnum_bignum11");

	rollback_local(local, stack);

	RETURN;
}

static int test_GetFixnum_signed(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&pos, 10);
	value = 999;
	test(GetFixnum_signed(pos, &value) == 0, "GetFixnum_signed1");
	test(value == 10, "GetFixnum_signed2");

	bignum_value_local(local, &pos, SignMinus, 20);
	test(GetFixnum_signed(pos, &value) == 0, "GetFixnum_signed3");
	test(value == -20, "GetFixnum_signed4");

	bignum_value2_local(local, &pos, SignPlus, 10, 20);
	test(GetFixnum_signed(pos, &value), "GetFixnum_signed5");

	character_local(local, &pos, 'A');
	test(GetFixnum_signed(pos, &value), "GetFixnum_signed6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  plus / minus
 */
static int test_plusvalue(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	plusvalue(local, pos, 20, &pos);
	test(RefAllocBignum(pos) == 2, "plusvalue1");
	test(RefSizeBignum(pos) == 1, "plusvalue2");
	test(IsPlus(RefSignBignum(pos)), "plusvalue3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 30, "plusvalue4");

	bignum_value_local(local, &pos, SignMinus, 30);
	plusvalue(local, pos, 10, &pos);
	test(RefAllocBignum(pos) == 1, "plusvalue5");
	test(RefSizeBignum(pos) == 1, "plusvalue6");
	test(IsMinus(RefSignBignum(pos)), "plusvalue7");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 20, "plusvalue8");

	bignum_value_local(local, &pos, SignMinus, 30);
	plusvalue(local, pos, 70, &pos);
	test(RefAllocBignum(pos) == 1, "plusvalue9");
	test(RefSizeBignum(pos) == 1, "plusvalue10");
	test(IsPlus(RefSignBignum(pos)), "plusvalue11");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 40, "plusvalue12");

	rollback_local(local, stack);

	RETURN;
}

static int test_minusvalue(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignMinus, 40);
	minusvalue(local, pos, 20, &pos);
	test(RefAllocBignum(pos) == 2, "minusvalue1");
	test(RefSizeBignum(pos) == 1, "minusvalue2");
	test(IsMinus(RefSignBignum(pos)), "minusvalue3");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 60, "minusvalue4");

	bignum_value_local(local, &pos, SignPlus, 40);
	minusvalue(local, pos, 10, &pos);
	test(RefAllocBignum(pos) == 1, "minusvalue5");
	test(RefSizeBignum(pos) == 1, "minusvalue6");
	test(IsPlus(RefSignBignum(pos)), "minusvalue7");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 30, "minusvalue8");

	bignum_value_local(local, &pos, SignPlus, 40);
	minusvalue(local, pos, 60, &pos);
	test(RefAllocBignum(pos) == 1, "minusvalue9");
	test(RefSizeBignum(pos) == 1, "minusvalue10");
	test(IsMinus(RefSignBignum(pos)), "minusvalue11");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 20, "minusvalue12");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  plus
 */
static int test_plus_vv_bignum_local(void)
{
	addr pos;
	bigtype *data;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	plus_vv_bignum_local(local, 10, 20, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local1");
	test(equal_value_bignum(pos, SignPlus, 30), "plus_vv_bignum_local2");

	plus_vv_bignum_local(local, 10, -50, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local3");
	test(equal_value_bignum(pos, SignMinus, 40), "plus_vv_bignum_local4");

	plus_vv_bignum_local(local, FIXNUM_MAX, 1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local5");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_bignum_local6");
	test(RefSizeBignum(pos) == 1, "plus_vv_bignum_local7");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_MAX + 1ULL, "plus_vv_bignum_local8");

	plus_vv_bignum_local(local, FIXNUM_MIN, -1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local9");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_bignum_local10");
	test(RefSizeBignum(pos) == 1, "plus_vv_bignum_local11");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_UMIN + 1ULL, "plus_vv_bignum_local12");

	plus_vv_bignum_local(local, FIXNUM_MIN, FIXNUM_MIN, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local13");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_bignum_local14");
	test(RefSizeBignum(pos) == 2, "plus_vv_bignum_local15");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 0, "plus_vv_bignum_local16");
	test(data[1] == 1, "plus_vv_bignum_local17");

	plus_vv_bignum_local(local, FIXNUM_MAX, FIXNUM_MAX, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_bignum_local18");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_bignum_local19");
	test(RefSizeBignum(pos) == 1, "plus_vv_bignum_local20");

	rollback_local(local, stack);

	RETURN;
}

#if 0
static int test_plus_vv_real_local(void)
{
	addr pos;
	bigtype *data;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	plus_vv_real_local(local, 10, 20, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_vv_real_local1");
	test(RefFixnum(pos) == 30, "plus_vv_real_local2");

	plus_vv_real_local(local, 10, -50, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_vv_real_local3");
	test(RefFixnum(pos) == -40, "plus_vv_real_local4");

	plus_vv_real_local(local, FIXNUM_MAX, 1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_local5");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_real_local6");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_local7");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_MAX + 1ULL, "plus_vv_real_local8");

	plus_vv_real_local(local, FIXNUM_MIN, -1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_local9");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_real_local10");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_local11");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_UMIN + 1ULL, "plus_vv_real_local12");

	plus_vv_real_local(local, FIXNUM_MIN, FIXNUM_MIN, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_local13");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_real_local14");
	test(RefSizeBignum(pos) == 2, "plus_vv_real_local15");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 0, "plus_vv_real_local16");
	test(data[1] == 1, "plus_vv_real_local17");

	plus_vv_real_local(local, FIXNUM_MAX, FIXNUM_MAX, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_local18");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_real_local19");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_local20");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_vv_real_common(void)
{
	addr pos;
	bigtype *data;

	plus_vv_real_common(10, 20, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_vv_real_common1");
	test(RefFixnum(pos) == 30, "plus_vv_real_common2");

	plus_vv_real_common(10, -50, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_vv_real_common3");
	test(RefFixnum(pos) == -40, "plus_vv_real_common4");

	plus_vv_real_common(FIXNUM_MAX, 1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_common5");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_real_common6");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_common7");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_MAX + 1ULL, "plus_vv_real_common8");

	plus_vv_real_common(FIXNUM_MIN, -1, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_common9");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_real_common10");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_common11");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == FIXNUM_UMIN + 1ULL, "plus_vv_real_common12");

	plus_vv_real_common(FIXNUM_MIN, FIXNUM_MIN, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_common13");
	test(IsMinus(RefSignBignum(pos)), "plus_vv_real_common14");
	test(RefSizeBignum(pos) == 2, "plus_vv_real_common15");
	GetRootDataBignum(pos, &pos, &data);
	test(data[0] == 0, "plus_vv_real_common16");
	test(data[1] == 1, "plus_vv_real_common17");

	plus_vv_real_common(FIXNUM_MAX, FIXNUM_MAX, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_vv_real_common18");
	test(IsPlus(RefSignBignum(pos)), "plus_vv_real_common19");
	test(RefSizeBignum(pos) == 1, "plus_vv_real_common20");

	RETURN;
}
#endif

static int test_plus_fv_bignum_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	plus_fv_bignum_local(local, pos, 0, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_bignum_local1");
	test(equal_value_bignum(pos, SignPlus, 10), "plus_fv_bignum_local2");

	fixnum_local(local, &pos, 0);
	plus_fv_bignum_local(local, pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_bignum_local3");
	test(equal_value_bignum(pos, SignMinus, 10), "plus_fv_bignum_local4");

	fixnum_local(local, &pos, 50);
	plus_fv_bignum_local(local, pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_bignum_local5");
	test(equal_value_bignum(pos, SignPlus, 40), "plus_fv_bignum_local6");

	fixnum_local(local, &pos, FIXNUM_MAX);
	plus_fv_bignum_local(local, pos, 10, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_bignum_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_fv_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	plus_fv_real_local(local, pos, 0, &check);
	test(pos == check, "plus_fv_real_local1");

	fixnum_local(local, &pos, 0);
	plus_fv_real_local(local, pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_fv_real_local2");
	test(RefFixnum(pos) == -10, "plus_fv_real_local3");

	fixnum_local(local, &pos, 50);
	plus_fv_real_local(local, pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_fv_real_local4");
	test(RefFixnum(pos) == 40, "plus_fv_real_local5");

	fixnum_local(local, &pos, FIXNUM_MAX);
	plus_fv_real_local(local, pos, 10, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_real_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_fv_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&pos, 10);
	plus_fv_real_common(pos, 0, &check);
	test(pos == check, "plus_fv_real_common1");

	fixnum_local(local, &pos, 0);
	plus_fv_real_common(pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_fv_real_common2");
	test(RefFixnum(pos) == -10, "plus_fv_real_common3");

	fixnum_local(local, &pos, 50);
	plus_fv_real_common(pos, -10, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_fv_real_common4");
	test(RefFixnum(pos) == 40, "plus_fv_real_common5");

	fixnum_local(local, &pos, FIXNUM_MAX);
	plus_fv_real_common(pos, 10, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_fv_real_common6");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_ff_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 10);
	fixnum_local(local, &right, 0);
	plus_ff_bignum_local(local, left, right, &left);
	test(equal_value_bignum(left, SignPlus, 10), "plus_ff_bignum_local1");

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, -10);
	plus_ff_bignum_local(local, left, right, &left);
	test(equal_value_bignum(left, SignMinus, 10), "plus_ff_bignum_local2");

	fixnum_local(local, &left, 50);
	fixnum_local(local, &right, -10);
	plus_ff_bignum_local(local, left, right, &left);
	test(equal_value_bignum(left, SignPlus, 40), "plus_ff_bignum_local3");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, 10);
	plus_ff_bignum_local(local, left, right, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "plus_ff_bignum_local4");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_ff_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 10);
	fixnum_local(local, &right, 0);
	plus_ff_real_local(local, left, right, &right);
	test(left == right, "plus_ff_real_local1");

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, -10);
	plus_ff_real_local(local, left, right, &left);
	test(left == right, "plus_ff_real_local2");

	fixnum_local(local, &left, 50);
	fixnum_local(local, &right, -10);
	plus_ff_real_local(local, left, right, &left);
	test(GetType(left) == LISPTYPE_FIXNUM, "plus_ff_real_local3");
	test(RefFixnum(left) == 40, "plus_ff_real_local4");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, 10);
	plus_ff_real_local(local, left, right, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "plus_ff_real_local5");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_ff_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 10);
	fixnum_local(local, &right, 0);
	plus_ff_real_common(left, right, &right);
	test(left == right, "plus_ff_real_common1");

	fixnum_local(local, &left, 0);
	fixnum_heap(&right, -10);
	plus_ff_real_common(left, right, &left);
	test(left == right, "plus_ff_real_common2");

	fixnum_local(local, &left, 50);
	fixnum_local(local, &right, -10);
	plus_ff_real_common(left, right, &left);
	test(GetType(left) == LISPTYPE_FIXNUM, "plus_ff_real_common3");
	test(RefFixnum(left) == 40, "plus_ff_real_common4");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, 10);
	plus_ff_real_common(left, right, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "plus_ff_real_common5");

	rollback_local(local, stack);

	RETURN;
}

static int test_plusfixnum_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_local(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 3);
	GetRootDataBignum(pos, &root, &data);
	data[0] = BIGNUM_FULL;
	data[1] = 10;
	data[2] = 20;
	plusfixnum_bignum_local(local, pos, 22, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plusfixnum_bignum_local1");
	test(IsPlus(RefSignBignum(pos)), "plusfixnum_bignum_local2");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 21, "plusfixnum_bignum_local3");
	test(data[1] == 11, "plusfixnum_bignum_local4");
	test(data[2] == 20, "plusfixnum_bignum_local5");

	bignum_value_local(local, &pos, SignMinus, 10);
	plusfixnum_bignum_local(local, pos, 40, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plusfixnum_bignum_local6");
	test(equal_value_bignum(pos, SignPlus, 30), "plusfixnum_bignum_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_plusfixnum_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_local(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 3);
	GetRootDataBignum(pos, &root, &data);
	data[0] = BIGNUM_FULL;
	data[1] = 10;
	data[2] = 20;
	plusfixnum_real_local(local, pos, 22, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plusfixnum_real_local1");
	test(IsPlus(RefSignBignum(pos)), "plusfixnum_real_local2");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 21, "plusfixnum_real_local3");
	test(data[1] == 11, "plusfixnum_real_local4");
	test(data[2] == 20, "plusfixnum_real_local5");

	bignum_value_local(local, &pos, SignMinus, 10);
	plusfixnum_real_local(local, pos, 40, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plusfixnum_real_local6");
	test(RefFixnum(pos) == 30, "plusfixnum_real_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_plusfixnum_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, root;
	bigtype *data;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_local(local, &pos, SignPlus, 10);
	SetSizeBignum(pos, 3);
	GetRootDataBignum(pos, &root, &data);
	data[0] = BIGNUM_FULL;
	data[1] = 10;
	data[2] = 20;
	plusfixnum_real_common(local, pos, 22, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plusfixnum_real_common1");
	test(IsPlus(RefSignBignum(pos)), "plusfixnum_real_common2");
	GetRootDataBignum(pos, &root, &data);
	test(data[0] == 21, "plusfixnum_real_common3");
	test(data[1] == 11, "plusfixnum_real_common4");
	test(data[2] == 20, "plusfixnum_real_common5");

	bignum_value_local(local, &pos, SignMinus, 10);
	plusfixnum_real_common(local, pos, 40, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plusfixnum_real_common6");
	test(RefFixnum(pos) == 30, "plusfixnum_real_common7");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bv_bignum_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	plus_bv_bignum_local(local, pos, 0, &pos);
	test(equal_value_bignum(pos, SignPlus, 10), "plus_bv_bignum_local1");

	bignum_zero_local(local, &pos);
	plus_bv_bignum_local(local, pos, -30, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_bv_bignum_local2");
	test(equal_value_bignum(pos, SignMinus, 30), "plus_bv_bignum_local3");

	bignum_value_local(local, &pos, SignPlus, 30);
	plus_bv_bignum_local(local, pos, -40, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "plus_bv_bignum_local4");
	test(equal_value_bignum(pos, SignMinus, 10), "plus_bv_bignum_local5");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bv_real_local(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	plus_bv_real_local(local, pos, 0, &check);
	test(pos == check, "plus_bv_real_local1");

	bignum_zero_local(local, &pos);
	plus_bv_real_local(local, pos, 30, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_bv_real_local2");
	test(RefFixnum(pos) == 30, "plus_bv_real_local3");

	bignum_value_local(local, &pos, SignPlus, 30);
	plus_bv_real_local(local, pos, -40, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_bv_real_local4");
	test(RefFixnum(pos) == -10, "plus_bv_real_local5");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bv_real_common(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&pos, SignPlus, 10);
	plus_bv_real_common(local, pos, 0, &check);
	test(pos == check, "plus_bv_real_common1");

	bignum_zero_local(local, &pos);
	plus_bv_real_common(local, pos, 30, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_bv_real_common2");
	test(RefFixnum(pos) == 30, "plus_bv_real_common3");

	bignum_value_local(local, &pos, SignPlus, 30);
	plus_bv_real_common(local, pos, -40, &pos);
	test(GetType(pos) == LISPTYPE_FIXNUM, "plus_bv_real_common4");
	test(RefFixnum(pos) == -10, "plus_bv_real_common5");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bf_bignum_local(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 0);
	plus_bf_bignum_local(local, left, right, &left);
	test(equal_value_bignum(left, SignPlus, 10), "plus_bf_bignum_local1");

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 10);
	plus_bf_bignum_local(local, left, right, &left);
	test(equal_value_bignum(left, SignPlus, 10), "plus_bf_bignum_local2");

	bignum_value_local(local, &left, SignPlus, 30);
	fixnum_local(local, &right, -40);
	plus_bf_bignum_local(local, left, right, &left);
	test(GetType(left) == LISPTYPE_BIGNUM, "plus_bf_bignum_local3");
	test(equal_value_bignum(left, SignMinus, 10), "plus_bf_bignum_local4");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bf_real_local(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 0);
	plus_bf_real_local(local, left, right, &right);
	test(left == right, "plus_bf_real_local1");

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 10);
	plus_bf_real_local(local, left, right, &left);
	test(left == right, "plus_bf_real_local2");

	bignum_value_local(local, &left, SignPlus, 30);
	fixnum_local(local, &right, -40);
	plus_bf_real_local(local, left, right, &left);
	test(GetType(left) == LISPTYPE_FIXNUM, "plus_bf_real_local3");
	test(RefFixnum(left) == -10, "plus_bf_real_local4");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bf_real_common(void)
{
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 10);
	fixnum_local(local, &right, 0);
	plus_bf_real_common(local, left, right, &right);
	test(left == right, "plus_bf_real_common1");

	bignum_zero_local(local, &left);
	fixnum_heap(&right, 10);
	plus_bf_real_common(local, left, right, &left);
	test(left == right, "plus_bf_real_common2");

	bignum_value_local(local, &left, SignPlus, 30);
	fixnum_local(local, &right, -40);
	plus_bf_real_common(local, left, right, &left);
	test(GetType(left) == LISPTYPE_FIXNUM, "plus_bf_real_common3");
	test(RefFixnum(left) == -10, "plus_bf_real_common4");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bb_bignum_local(void)
{
	addr left, right, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignPlus, 10);
	plus_bb_bignum_local(local, left, right, &check);
	test(equal_value_bignum(check, SignPlus, 10), "plus_bb_bignum_local1");
	plus_bb_bignum_local(local, right, left, &check);
	test(equal_value_bignum(check, SignPlus, 10), "plus_bb_bignum_local2");

	bignum_value_local(local, &left, SignMinus, 20);
	bignum_value_local(local, &right, SignMinus, 30);
	plus_bb_bignum_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_BIGNUM, "plus_bb_bignum_local3");
	test(equal_value_bignum(check, SignMinus, 50), "plus_bb_bignum_local4");

	bignum_value_local(local, &left, SignPlus, 50);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_bignum_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_BIGNUM, "plus_bb_bignum_local5");
	test(equal_value_bignum(check, SignPlus, 0), "plus_bb_bignum_local6");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_bignum_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_BIGNUM, "plus_bb_bignum_local7");
	test(equal_value_bignum(check, SignMinus, 40), "plus_bb_bignum_local8");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignPlus, 10);
	plus_bb_real_local(local, left, right, &check);
	test(right == check, "plus_bb_real_local1");
	plus_bb_real_local(local, right, left, &check);
	test(right == check, "plus_bb_real_local2");

	bignum_value_local(local, &left, SignMinus, 20);
	bignum_value_local(local, &right, SignMinus, 30);
	plus_bb_real_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_local3");
	test(RefFixnum(check) == -50, "plus_bb_real_local4");

	bignum_value_local(local, &left, SignPlus, 50);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_real_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_local5");
	test(RefFixnum(check) == 0, "plus_bb_real_local6");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_real_local(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_local7");
	test(RefFixnum(check) == -40, "plus_bb_real_local8");

	rollback_local(local, stack);

	RETURN;
}

static int test_plus_bb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_heap(&right, SignPlus, 10);
	plus_bb_real_common(local, left, right, &check);
	test(right == check, "plus_bb_real_common1");
	plus_bb_real_common(local, right, left, &check);
	test(right == check, "plus_bb_real_common2");

	bignum_value_local(local, &left, SignMinus, 20);
	bignum_value_local(local, &right, SignMinus, 30);
	plus_bb_real_common(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_common3");
	test(RefFixnum(check) == -50, "plus_bb_real_common4");

	bignum_value_local(local, &left, SignPlus, 50);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_real_common(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_common5");
	test(RefFixnum(check) == 0, "plus_bb_real_common6");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 50);
	plus_bb_real_common(local, left, right, &check);
	test(GetType(check) == LISPTYPE_FIXNUM, "plus_bb_real_common7");
	test(RefFixnum(check) == -40, "plus_bb_real_common8");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  minus
 */
static int test_sigrev_bignum_inplace(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	sigrev_bignum_inplace(pos);
	test(equal_value_bignum(pos, SignMinus, 10), "sigrev_bignum_inplace1");
	sigrev_bignum_inplace(pos);
	test(equal_value_bignum(pos, SignPlus, 10), "sigrev_bignum_inplace2");

	bignum_value_heap(&pos, SignMinus, 20);
	sigrev_bignum_inplace(pos);
	test(equal_value_bignum(pos, SignPlus, 20), "sigrev_bignum_inplace1");
	sigrev_bignum_inplace(pos);
	test(equal_value_bignum(pos, SignMinus, 20), "sigrev_bignum_inplace2");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_fixnum_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	sigrev_fixnum_bignum_local(local, pos, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "sigrev_fixnum_bignum_local1");
	test(equal_value_bignum(pos, SignMinus, 10), "sigrev_fixnum_bignum_local2");

	fixnum_local(local, &pos, FIXNUM_MAX);
	sigrev_fixnum_bignum_local(local, pos, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "sigrev_fixnum_bignum_local3");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_MAX), "sigrev_fixnum_bignum_local4");

	fixnum_local(local, &pos, FIXNUM_MIN);
	sigrev_fixnum_bignum_local(local, pos, &pos);
	test(GetType(pos) == LISPTYPE_BIGNUM, "sigrev_fixnum_bignum_local5");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN), "sigrev_fixnum_bignum_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_fixnum_integer_alloc(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&pos, 10);
	sigrev_fixnum_integer_local(local, pos, &check);
	test(fixnump(check), "sigrev_fixnum_integer_alloc1");
	test(RefFixnum(check) == -10, "sigrev_fixnum_integer_alloc2");
	sigrev_fixnum_integer_common(pos, &check);
	test(fixnump(check), "sigrev_fixnum_integer_alloc3");
	test(RefFixnum(check) == -10, "sigrev_fixnum_integer_alloc4");

	fixnum_heap(&pos, FIXNUM_MAX);
	sigrev_fixnum_integer_local(local, pos, &check);
	test(fixnump(check), "sigrev_fixnum_integer_alloc5");
	test(RefFixnum(check) == -FIXNUM_MAX, "sigrev_fixnum_integer_alloc6");
	sigrev_fixnum_integer_common(pos, &check);
	test(fixnump(check), "sigrev_fixnum_integer_alloc7");
	test(RefFixnum(check) == -FIXNUM_MAX, "sigrev_fixnum_integer_alloc8");

	fixnum_heap(&pos, FIXNUM_MIN);
	sigrev_fixnum_integer_local(local, pos, &check);
	test(bignump(check), "sigrev_fixnum_integer_alloc9");
	test(equal_value_bignum(check, SignPlus, FIXNUM_UMIN),
			"sigrev_fixnum_integer_alloc10");
	sigrev_fixnum_integer_common(pos, &check);
	test(bignump(check), "sigrev_fixnum_integer_alloc11");
	test(equal_value_bignum(check, SignPlus, FIXNUM_UMIN),
			"sigrev_fixnum_integer_alloc12");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_bignum_bignum_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	sigrev_bignum_bignum_local(local, pos, &pos);
	test(bignump(pos), "sigrev_bignum_bignum_local1");
	test(GetStatusDynamic(pos), "sigrev_bignum_bignum_local2");
	test(equal_value_bignum(pos, SignMinus, 10), "sigrev_bignum_bignum_local3");

	rollback_local(local, stack);

	RETURN;
}

static int test_sigrev_bignum_integer_alloc(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, FIXNUM_UMIN);
	sigrev_bignum_integer_local(local, pos, &check);
	test(fixnump(check), "sigrev_bignum_integer_alloc1");
	test(GetStatusDynamic(check), "sigrev_bignum_integer_alloc2");
	test(RefFixnum(check) == FIXNUM_MIN, "sigrev_bignum_integer_alloc3");
	sigrev_bignum_integer_common(pos, &check);
	test(fixnump(check), "sigrev_bignum_integer_alloc4");
	test(! GetStatusDynamic(check), "sigrev_bignum_integer_alloc5");
	test(RefFixnum(check) == FIXNUM_MIN, "sigrev_bignum_integer_alloc6");

	bignum_value_local(local, &pos, SignPlus, 100);
	sigrev_bignum_integer_local(local, pos, &check);
	test(fixnump(check), "sigrev_bignum_integer_alloc7");
	test(GetStatusDynamic(check), "sigrev_bignum_integer_alloc8");
	test(RefFixnum(check) == -100, "sigrev_bignum_integer_alloc9");
	sigrev_bignum_integer_common(pos, &check);
	test(fixnump(check), "sigrev_bignum_integer_alloc10");
	test(! GetStatusDynamic(check), "sigrev_bignum_integer_alloc11");
	test(RefFixnum(check) == -100, "sigrev_bignum_integer_alloc12");

	bignum_value2_local(local, &pos, SignPlus, 1, FIXNUM_UMIN);
	sigrev_bignum_integer_local(local, pos, &pos);
	test(bignump(pos), "sigrev_bignum_integer_alloc13");
	test(equal_value2_bignum(pos, SignMinus, 1, FIXNUM_UMIN),
			"sigrev_bignum_integer_alloc14");

	bignum_value_local(local, &pos, SignMinus, FIXNUM_UMIN);
	sigrev_bignum_integer_local(local, pos, &pos);
	test(bignump(pos), "sigrev_bignum_integer_alloc15");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN),
			"sigrev_bignum_integer_alloc16");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_vv_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = Local_Thread;
	push_local(local, &stack);

	minus_vv_bignum_local(local, 30, 10, &pos);
	test(bignump(pos), "minus_vv_bignum_local1");
	test(GetStatusDynamic(pos), "minus_vv_bignum_local2");
	test(equal_value_bignum(pos, SignPlus, 20), "minus_vv_bignum_local3");

	minus_vv_bignum_local(local, 30, -10, &pos);
	test(bignump(pos), "minus_vv_bignum_local4");
	test(equal_value_bignum(pos, SignPlus, 40), "minus_vv_bignum_local5");

	minus_vv_bignum_local(local, FIXNUM_MAX, -1, &pos);
	test(bignump(pos), "minus_vv_bignum_local6");
	test(equal_value_bignum(pos, SignPlus, ((bigtype)FIXNUM_MAX) + 1UL),
			"minus_vv_bignum_local7");

	minus_vv_bignum_local(local, FIXNUM_MIN, 1, &pos);
	test(bignump(pos), "minus_vv_bignum_local8");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + 1UL),
			"minus_vv_bignum_local9");

	minus_vv_bignum_local(local, FIXNUM_MIN, FIXNUM_MAX, &pos);
	test(bignump(pos), "minus_vv_bignum_local10");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_bignum_local11");

	minus_vv_bignum_local(local, FIXNUM_MAX, FIXNUM_MIN, &pos);
	test(bignump(pos), "minus_vv_bignum_local12");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_bignum_local13");

	rollback_local(local, stack);

	RETURN;
}

#if 0
static int test_minus_vv_real_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	minus_vv_real_local(local, 30, 10, &pos);
	test(fixnump(pos), "minus_vv_real_local1");
	test(GetStatusDynamic(pos), "minus_vv_real_local2");
	test(RefFixnum(pos) == 20, "minus_vv_real_local3");

	minus_vv_real_local(local, 30, -10, &pos);
	test(fixnump(pos), "minus_vv_real_local4");
	test(RefFixnum(pos) == 40, "minus_vv_real_local5");

	minus_vv_real_local(local, FIXNUM_MAX, -1, &pos);
	test(bignump(pos), "minus_vv_real_local6");
	test(equal_value_bignum(pos, SignPlus, ((bigtype)FIXNUM_MAX) + 1UL),
			"minus_vv_real_local7");

	minus_vv_real_local(local, FIXNUM_MIN, 1, &pos);
	test(bignump(pos), "minus_vv_real_local8");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + 1UL),
			"minus_vv_real_local9");

	minus_vv_real_local(local, FIXNUM_MIN, FIXNUM_MAX, &pos);
	test(bignump(pos), "minus_vv_real_local10");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_real_local11");

	minus_vv_real_local(local, FIXNUM_MAX, FIXNUM_MIN, &pos);
	test(bignump(pos), "minus_vv_real_local12");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_real_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_vv_real_common(void)
{
	addr pos;

	minus_vv_real_common(30, 10, &pos);
	test(fixnump(pos), "minus_vv_real_common1");
	test(! GetStatusDynamic(pos), "minus_vv_real_common2");
	test(RefFixnum(pos) == 20, "minus_vv_real_common3");

	minus_vv_real_common(30, -10, &pos);
	test(fixnump(pos), "minus_vv_real_common4");
	test(RefFixnum(pos) == 40, "minus_vv_real_common5");

	minus_vv_real_common(FIXNUM_MAX, -1, &pos);
	test(bignump(pos), "minus_vv_real_common6");
	test(equal_value_bignum(pos, SignPlus, ((bigtype)FIXNUM_MAX) + 1UL),
			"minus_vv_real_common7");

	minus_vv_real_common(FIXNUM_MIN, 1, &pos);
	test(bignump(pos), "minus_vv_real_common8");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + 1UL),
			"minus_vv_real_common9");

	minus_vv_real_common(FIXNUM_MIN, FIXNUM_MAX, &pos);
	test(bignump(pos), "minus_vv_real_common10");
	test(equal_value_bignum(pos, SignMinus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_real_common11");

	minus_vv_real_common(FIXNUM_MAX, FIXNUM_MIN, &pos);
	test(bignump(pos), "minus_vv_real_common12");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN + (bigtype)FIXNUM_MAX),
			"minus_vv_real_common13");

	RETURN;
}
#endif

static int test_minus_ff_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 10);
	minus_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_ff_bignum_alloc1");
	test(GetStatusDynamic(check), "minus_ff_bignum_alloc2");
	test(equal_value_bignum(check, SignMinus, 10), "minus_ff_bignum_alloc3");

	minus_ff_bignum_local(local, right, left, &check);
	test(right == check, "minus_ff_bignum_alloc4");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, FIXNUM_MIN);
	minus_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_ff_bignum_alloc5");
	test(GetStatusDynamic(check), "minus_ff_bignum_alloc6");
	test(equal_value_bignum(check, SignPlus, FIXNUM_MAX + FIXNUM_UMIN),
			"minus_ff_bignum_alloc7");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_ff_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 10);
	minus_ff_real_local(local, left, right, &check);
	test(fixnump(check), "minus_ff_real_local1");
	test(GetStatusDynamic(check), "minus_ff_real_local2");
	test(RefFixnum(check) == -10, "minus_ff_real_local3");

	minus_ff_real_local(local, right, left, &check);
	test(right == check, "minus_ff_real_local4");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, FIXNUM_MIN);
	minus_ff_real_local(local, left, right, &check);
	test(bignump(check), "minus_ff_real_local5");
	test(GetStatusDynamic(check), "minus_ff_real_local6");
	test(reffixed_bignum(check, 0) == FIXNUM_MAX + FIXNUM_UMIN,
			"minus_ff_real_local7");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_ff_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 0);
	fixnum_heap(&right, 10);
	minus_ff_real_common(left, right, &check);
	test(fixnump(check), "minus_ff_real_common1");
	test(! GetStatusDynamic(check), "minus_ff_real_common2");
	test(RefFixnum(check) == -10, "minus_ff_real_common3");

	minus_ff_real_common(right, left, &check);
	test(right == check, "minus_ff_real_common4");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, FIXNUM_MIN);
	minus_ff_real_common(left, right, &check);
	test(bignump(check), "minus_ff_real_common5");
	test(! GetStatusDynamic(check), "minus_ff_real_common6");
	test(reffixed_bignum(check, 0) == FIXNUM_MAX + FIXNUM_UMIN,
			"minus_ff_real_common7");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bf_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 10);
	minus_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bf_bignum_local1");
	test(GetStatusDynamic(check), "minus_bf_bignum_local2");
	test(equal_value_bignum(check, SignMinus, 10), "minus_bf_bignum_local3");

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bf_bignum_local(local, right, left, &check);
	test(right == check, "minus_bf_bignum_local4");

	bignum_value_local(local, &left, SignPlus, 15);
	fixnum_local(local, &right, 20);
	minus_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bf_bignum_local5");
	test(GetStatusDynamic(check), "minus_bf_bignum_local6");
	test(equal_value_bignum(check, SignMinus, 5), "minus_bf_bignum_local7");

	bignum_value_local(local, &left, SignMinus, 10);
	fixnum_local(local, &right, -40);
	minus_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bf_bignum_local8");
	test(GetStatusDynamic(check), "minus_bf_bignum_local9");
	test(equal_value_bignum(check, SignPlus, 30), "minus_bf_bignum_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 10);
	minus_bf_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_local1");
	test(GetStatusDynamic(check), "minus_bf_real_local2");
	test(RefFixnum(check) == -10, "minus_bf_real_local3");

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bf_real_local(local, right, left, &check);
	test(right == check, "minus_bf_real_local4");

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 20);
	minus_bf_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_local5");
	test(GetStatusDynamic(check), "minus_bf_real_local6");
	test(RefFixnum(check) == -10, "minus_bf_real_local7");

	bignum_value_local(local, &left, SignMinus, 10);
	fixnum_local(local, &right, -40);
	minus_bf_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_local8");
	test(GetStatusDynamic(check), "minus_bf_real_local9");
	test(RefFixnum(check) == 30, "minus_bf_real_local10");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 10);
	minus_bf_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_common1");
	test(! GetStatusDynamic(check), "minus_bf_real_common2");
	test(RefFixnum(check) == -10, "minus_bf_real_common3");

	fixnum_local(local, &left, 0);
	bignum_value_heap(&right, SignPlus, 20);
	minus_bf_real_common(local, right, left, &check);
	test(right == check, "minus_bf_real_common4");

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 20);
	minus_bf_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_common5");
	test(! GetStatusDynamic(check), "minus_bf_real_common6");
	test(RefFixnum(check) == -10, "minus_bf_real_common7");

	bignum_value_local(local, &left, SignMinus, 10);
	fixnum_local(local, &right, -40);
	minus_bf_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bf_real_common8");
	test(! GetStatusDynamic(check), "minus_bf_real_common9");
	test(RefFixnum(check) == 30, "minus_bf_real_common10");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fb_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &right, SignPlus, 10);
	minus_fb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_fb_bignum_local1");
	test(GetStatusDynamic(check), "minus_fb_bignum_local2");
	test(equal_value_bignum(check, SignMinus, 10), "minus_fb_bignum_local3");

	fixnum_local(local, &left, 10);
	bignum_zero_local(local, &right);
	minus_fb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_fb_bignum_local4");
	test(GetStatusDynamic(check), "minus_fb_bignum_local5");
	test(equal_value_bignum(check, SignPlus, 10), "minus_fb_bignum_local6");

	fixnum_local(local, &left, 10);
	bignum_value_local(local, &right, SignPlus, 30);
	minus_fb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_fb_bignum_local7");
	test(GetStatusDynamic(check), "minus_fb_bignum_local8");
	test(equal_value_bignum(check, SignMinus, 20), "minus_fb_bignum_local9");

	fixnum_local(local, &left, -40);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_fb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_fb_bignum_local10");
	test(GetStatusDynamic(check), "minus_fb_bignum_local11");
	test(equal_value_bignum(check, SignMinus, 30), "minus_fb_bignum_local12");

	fixnum_local(local, &left, 11);
	bignum_local(local, &right, SignPlus, 10);
	SetSizeBignum(right, 3);
	setfixed_bignum(right, 0, 30);
	setfixed_bignum(right, 1, 40);
	setfixed_bignum(right, 2, 50);
	minus_fb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_fb_bignum_local13");
	test(GetStatusDynamic(check), "minus_fb_bignum_local14");
	test(reffixed_bignum(check, 0) == 19, "minus_fb_bignum_local15");
	test(reffixed_bignum(check, 1) == 40, "minus_fb_bignum_local16");
	test(IsMinus(RefSignBignum(check)), "minus_fb_bignum_local17");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &right, SignPlus, 10);
	minus_fb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_local1");
	test(GetStatusDynamic(check), "minus_fb_real_local2");
	test(RefFixnum(check) == -10, "minus_fb_real_local3");

	fixnum_local(local, &left, 10);
	bignum_zero_local(local, &right);
	minus_fb_real_local(local, left, right, &check);
	test(left == check, "minus_fb_real_local4");

	fixnum_local(local, &left, 10);
	bignum_value_local(local, &right, SignPlus, 30);
	minus_fb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_local5");
	test(GetStatusDynamic(check), "minus_fb_real_local6");
	test(RefFixnum(check) == -20, "minus_fb_real_local7");

	fixnum_local(local, &left, -40);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_fb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_local8");
	test(GetStatusDynamic(check), "minus_fb_real_local9");
	test(RefFixnum(check) == -30, "minus_fb_real_local10");

	fixnum_local(local, &left, 11);
	bignum_local(local, &right, SignPlus, 10);
	SetSizeBignum(right, 3);
	setfixed_bignum(right, 0, 30);
	setfixed_bignum(right, 1, 40);
	setfixed_bignum(right, 2, 50);
	minus_fb_real_local(local, left, right, &check);
	test(bignump(check), "minus_fb_real_local11");
	test(GetStatusDynamic(check), "minus_fb_real_local12");
	test(reffixed_bignum(check, 0) == 19, "minus_fb_real_local13");
	test(reffixed_bignum(check, 1) == 40, "minus_fb_real_local14");
	test(IsMinus(RefSignBignum(check)), "minus_fb_real_local15");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_fb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	bignum_value_local(local, &right, SignPlus, 10);
	minus_fb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_common1");
	test(! GetStatusDynamic(check), "minus_fb_real_common2");
	test(RefFixnum(check) == -10, "minus_fb_real_common3");

	fixnum_heap(&left, 10);
	bignum_zero_local(local, &right);
	minus_fb_real_common(local, left, right, &check);
	test(left == check, "minus_fb_real_common4");

	fixnum_local(local, &left, 10);
	bignum_value_local(local, &right, SignPlus, 30);
	minus_fb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_common5");
	test(! GetStatusDynamic(check), "minus_fb_real_common6");
	test(RefFixnum(check) == -20, "minus_fb_real_common7");

	fixnum_local(local, &left, -40);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_fb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_fb_real_common8");
	test(! GetStatusDynamic(check), "minus_fb_real_common9");
	test(RefFixnum(check) == -30, "minus_fb_real_common10");

	fixnum_local(local, &left, 11);
	bignum_local(local, &right, SignPlus, 10);
	SetSizeBignum(right, 3);
	setfixed_bignum(right, 0, 30);
	setfixed_bignum(right, 1, 40);
	setfixed_bignum(right, 2, 50);
	minus_fb_real_common(local, left, right, &check);
	test(bignump(check), "minus_fb_real_common11");
	test(! GetStatusDynamic(check), "minus_fb_real_common12");
	test(reffixed_bignum(check, 0) == 19, "minus_fb_real_common13");
	test(reffixed_bignum(check, 1) == 40, "minus_fb_real_common14");
	test(IsMinus(RefSignBignum(check)), "minus_fb_real_common15");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bb_bignum_local(void)
{
	addr left, right, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local1");
	test(GetStatusDynamic(check), "minus_bb_bignum_local2");
	test(equal_value_bignum(check, SignPlus, 10), "minus_bb_bignum_local3");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_zero_local(local, &right);
	minus_bb_bignum_local(local, left, right, &check);
	test(check == left, "minus_bb_bignum_local4");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 20);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local5");
	test(GetStatusDynamic(check), "minus_bb_bignum_local6");
	test(equal_value_bignum(check, SignPlus, 30), "minus_bb_bignum_local7");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local8");
	test(GetStatusDynamic(check), "minus_bb_bignum_local9");
	test(equal_value_bignum(check, SignMinus, 30), "minus_bb_bignum_local10");

	bignum_value_local(local, &left, SignPlus, 20);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local11");
	test(GetStatusDynamic(check), "minus_bb_bignum_local12");
	test(zerop_bignum(check), "minus_bb_bignum_local13");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignPlus, 40);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local14");
	test(GetStatusDynamic(check), "minus_bb_bignum_local15");
	test(equal_value_bignum(check, SignMinus, 30), "minus_bb_bignum_local16");

	bignum_value_local(local, &left, SignMinus, 40);
	bignum_value_local(local, &right, SignMinus, 30);
	minus_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "minus_bb_bignum_local17");
	test(GetStatusDynamic(check), "minus_bb_bignum_local18");
	test(equal_value_bignum(check, SignMinus, 10), "minus_bb_bignum_local19");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local1");
	test(GetStatusDynamic(check), "minus_bb_real_local2");
	test(RefFixnum(check) == 10, "minus_bb_real_local3");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_zero_local(local, &right);
	minus_bb_real_local(local, left, right, &check);
	test(check == left, "minus_bb_real_local4");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 20);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local5");
	test(GetStatusDynamic(check), "minus_bb_real_local6");
	test(RefFixnum(check) == 30, "minus_bb_real_local7");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local8");
	test(GetStatusDynamic(check), "minus_bb_real_local9");
	test(RefFixnum(check) == -30, "minus_bb_real_local10");

	bignum_value_local(local, &left, SignPlus, 20);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local11");
	test(GetStatusDynamic(check), "minus_bb_real_local12");
	test(RefFixnum(check) == 0, "minus_bb_real_local13");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignPlus, 40);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local14");
	test(GetStatusDynamic(check), "minus_bb_real_local15");
	test(RefFixnum(check) == -30, "minus_bb_real_local16");

	bignum_value_local(local, &left, SignMinus, 40);
	bignum_value_local(local, &right, SignMinus, 30);
	minus_bb_real_local(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_local17");
	test(GetStatusDynamic(check), "minus_bb_real_local18");
	test(RefFixnum(check) == -10, "minus_bb_real_local19");

	rollback_local(local, stack);

	RETURN;
}

static int test_minus_bb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 10);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common1");
	test(! GetStatusDynamic(check), "minus_bb_real_common2");
	test(RefFixnum(check) == 10, "minus_bb_real_common3");

	bignum_value_heap(&left, SignMinus, 10);
	bignum_zero_local(local, &right);
	minus_bb_real_common(local, left, right, &check);
	test(check == left, "minus_bb_real_common4");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignMinus, 20);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common5");
	test(! GetStatusDynamic(check), "minus_bb_real_common6");
	test(RefFixnum(check) == 30, "minus_bb_real_common7");

	bignum_value_local(local, &left, SignMinus, 10);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common8");
	test(! GetStatusDynamic(check), "minus_bb_real_common9");
	test(RefFixnum(check) == -30, "minus_bb_real_common10");

	bignum_value_local(local, &left, SignPlus, 20);
	bignum_value_local(local, &right, SignPlus, 20);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common11");
	test(! GetStatusDynamic(check), "minus_bb_real_common12");
	test(RefFixnum(check) == 0, "minus_bb_real_common13");

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_value_local(local, &right, SignPlus, 40);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common14");
	test(! GetStatusDynamic(check), "minus_bb_real_common15");
	test(RefFixnum(check) == -30, "minus_bb_real_common16");

	bignum_value_local(local, &left, SignMinus, 40);
	bignum_value_local(local, &right, SignMinus, 30);
	minus_bb_real_common(local, left, right, &check);
	test(fixnump(check), "minus_bb_real_common17");
	test(! GetStatusDynamic(check), "minus_bb_real_common18");
	test(RefFixnum(check) == -10, "minus_bb_real_common19");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  multiple
 */
static int test_multisafe_fixnum(void)
{
	int check;
	fixnum value;

	check = multisafe_fixnum(10, 5, &value);
	test((! check) && value == 50, "multisafe_fixnum1");
	check = multisafe_fixnum(-10, 5, &value);
	test((! check) && value == -50, "multisafe_fixnum2");
	check = multisafe_fixnum(10, -5, &value);
	test((! check) && value == -50, "multisafe_fixnum3");
	check = multisafe_fixnum(-10, -5, &value);
	test((! check) && value == 50, "multisafe_fixnum4");

	check = multisafe_fixnum(FIXNUM_MAX, 10, &value);
	test(check, "multisafe_fixnum5");
	check = multisafe_fixnum(2, FIXNUM_MAX, &value);
	test(check, "multisafe_fixnum6");
	check = multisafe_fixnum(-2, FIXNUM_MIN, &value);
	test(check, "multisafe_fixnum7");
	check = multisafe_fixnum(2, FIXNUM_MIN, &value);
	test(check, "multisafe_fixnum8");

	RETURN;
}

static int test_multi_ff_bignum_local(void)
{
	addr left, right, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 10);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local1");
	test(GetStatusDynamic(check), "multi_ff_bignum_local2");
	test(zerop_bignum(check), "multi_ff_bignum_local3");

	fixnum_local(local, &left, 1);
	fixnum_local(local, &right, 10);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local4");
	test(GetStatusDynamic(check), "multi_ff_bignum_local5");
	test(equal_value_bignum(check, SignPlus, 10), "multi_ff_bignum_local6");

	fixnum_local(local, &left, -1);
	fixnum_local(local, &right, 10);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local7");
	test(GetStatusDynamic(check), "multi_ff_bignum_local8");
	test(equal_value_bignum(check, SignMinus, 10), "multi_ff_bignum_local9");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, 0);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local10");
	test(GetStatusDynamic(check), "multi_ff_bignum_local11");
	test(zerop_bignum(check), "multi_ff_bignum_local12");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, 1);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local13");
	test(GetStatusDynamic(check), "multi_ff_bignum_local14");
	test(equal_value_bignum(check, SignPlus, 20), "multi_ff_bignum_local15");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -1);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local16");
	test(GetStatusDynamic(check), "multi_ff_bignum_local17");
	test(equal_value_bignum(check, SignMinus, 20), "multi_ff_bignum_local18");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -3);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local19");
	test(GetStatusDynamic(check), "multi_ff_bignum_local20");
	test(equal_value_bignum(check, SignMinus, 60), "multi_ff_bignum_local21");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, -4);
	multi_ff_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_ff_bignum_local22");
	test(GetStatusDynamic(check), "multi_ff_bignum_local23");
	test(IsMinus(RefSignBignum(check)), "multi_ff_bignum_local24");
	test(RefSizeBignum(check) == 2, "multi_ff_bignum_local25");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_ff_bignum_local26");
	test(reffixed_bignum(check, 1) == 1, "multi_ff_bignum_local27");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_ff_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &left, 0);
	fixnum_local(local, &right, 10);
	multi_ff_real_local(local, left, right, &check);
	test(check == left, "multi_ff_real_local1");

	fixnum_local(local, &left, 1);
	fixnum_local(local, &right, 10);
	multi_ff_real_local(local, left, right, &check);
	test(check == right, "multi_ff_real_local2");

	fixnum_local(local, &left, -1);
	fixnum_local(local, &right, 10);
	multi_ff_real_local(local, left, right, &check);
	test(fixnump(check), "multi_ff_real_local3");
	test(GetStatusDynamic(check), "multi_ff_real_local4");
	test(RefFixnum(check) == -10, "multi_ff_real_local5");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, 0);
	multi_ff_real_local(local, left, right, &check);
	test(check == right, "multi_ff_real_local6");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, 1);
	multi_ff_real_local(local, left, right, &check);
	test(check == left, "multi_ff_real_local7");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -1);
	multi_ff_real_local(local, left, right, &check);
	test(fixnump(check), "multi_ff_real_local8");
	test(GetStatusDynamic(check), "multi_ff_real_local9");
	test(RefFixnum(check) == -20, "multi_ff_real_local10");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -3);
	multi_ff_real_local(local, left, right, &check);
	test(fixnump(check), "multi_ff_real_local11");
	test(GetStatusDynamic(check), "multi_ff_real_local12");
	test(RefFixnum(check) == -60, "multi_ff_real_local13");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, -4);
	multi_ff_real_local(local, left, right, &check);
	test(bignump(check), "multi_ff_real_local14");
	test(GetStatusDynamic(check), "multi_ff_real_local15");
	test(IsMinus(RefSignBignum(check)), "multi_ff_real_local16");
	test(RefSizeBignum(check) == 2, "multi_ff_real_local17");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_ff_real_local18");
	test(reffixed_bignum(check, 1) == 1, "multi_ff_real_local19");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_ff_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_heap(&left, 0);
	fixnum_heap(&right, 10);
	multi_ff_real_common(left, right, &check);
	test(check == left, "multi_ff_real_common1");

	fixnum_heap(&left, 1);
	fixnum_heap(&right, 10);
	multi_ff_real_common(left, right, &check);
	test(check == right, "multi_ff_real_common2");

	fixnum_local(local, &left, -1);
	fixnum_local(local, &right, 10);
	multi_ff_real_common(left, right, &check);
	test(fixnump(check), "multi_ff_real_common3");
	test(! GetStatusDynamic(check), "multi_ff_real_common4");
	test(RefFixnum(check) == -10, "multi_ff_real_common5");

	fixnum_heap(&left, 20);
	fixnum_heap(&right, 0);
	multi_ff_real_common(left, right, &check);
	test(check == right, "multi_ff_real_common6");

	fixnum_heap(&left, 20);
	fixnum_heap(&right, 1);
	multi_ff_real_common(left, right, &check);
	test(check == left, "multi_ff_real_common7");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -1);
	multi_ff_real_common(left, right, &check);
	test(fixnump(check), "multi_ff_real_common8");
	test(! GetStatusDynamic(check), "multi_ff_real_common9");
	test(RefFixnum(check) == -20, "multi_ff_real_common10");

	fixnum_local(local, &left, 20);
	fixnum_local(local, &right, -3);
	multi_ff_real_common(left, right, &check);
	test(fixnump(check), "multi_ff_real_common11");
	test(! GetStatusDynamic(check), "multi_ff_real_common12");
	test(RefFixnum(check) == -60, "multi_ff_real_common13");

	fixnum_local(local, &left, FIXNUM_MAX);
	fixnum_local(local, &right, -4);
	multi_ff_real_common(left, right, &check);
	test(bignump(check), "multi_ff_real_common14");
	test(! GetStatusDynamic(check), "multi_ff_real_common15");
	test(IsMinus(RefSignBignum(check)), "multi_ff_real_common16");
	test(RefSizeBignum(check) == 2, "multi_ff_real_common17");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_ff_real_common18");
	test(reffixed_bignum(check, 1) == 1, "multi_ff_real_common19");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bf_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 0);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local1");
	test(GetStatusDynamic(check), "multi_bf_bignum_local2");
	test(zerop_bignum(check), "multi_bf_bignum_local3");

	fixnum_local(local, &right, 1);
	multi_bf_bignum_local(local, left, right, &check);
	test(check == left, "multi_bf_bignum_local4");

	fixnum_local(local, &right, -1);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local5");
	test(GetStatusDynamic(check), "multi_bf_bignum_local6");
	test(equal_value_bignum(check, SignMinus, 10), "multi_bf_bignum_local7");

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 20);
	multi_bf_bignum_local(local, left, right, &check);
	test(check == left, "multi_bf_bignum_local8");

	bignum_value_local(local, &left, SignPlus, 1);
	fixnum_local(local, &right, 20);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local9");
	test(GetStatusDynamic(check), "multi_bf_bignum_local10");
	test(equal_value_bignum(check, SignPlus, 20), "multi_bf_bignum_local11");

	bignum_value_local(local, &left, SignMinus, 1);
	fixnum_local(local, &right, 20);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local12");
	test(GetStatusDynamic(check), "multi_bf_bignum_local13");
	test(equal_value_bignum(check, SignMinus, 20), "multi_bf_bignum_local14");

	bignum_value_local(local, &left, SignPlus, 20);
	fixnum_local(local, &right, -3);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local15");
	test(GetStatusDynamic(check), "multi_bf_bignum_local16");
	test(equal_value_bignum(check, SignMinus, 60), "multi_bf_bignum_local17");

	bignum_value_local(local, &left, SignMinus, FIXNUM_MAX);
	fixnum_local(local, &right, 4);
	multi_bf_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bf_bignum_local18");
	test(IsMinus(RefSignBignum(check)), "multi_bf_bignum_local19");
	test(RefSizeBignum(check) == 2, "multi_bf_bignum_local20");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_bf_bignum_local21");
	test(reffixed_bignum(check, 1) == 1, "multi_bf_bignum_local22");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bf_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	fixnum_local(local, &right, 0);
	multi_bf_real_local(local, left, right, &check);
	test(check == right, "multi_bf_real_local1");

	fixnum_local(local, &right, 1);
	multi_bf_real_local(local, left, right, &check);
	test(check == left, "multi_bf_real_local2");

	fixnum_local(local, &right, -1);
	multi_bf_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_local3");
	test(GetStatusDynamic(check), "multi_bf_real_local4");
	test(RefFixnum(check) == -10, "multi_bf_real_local5");

	bignum_zero_local(local, &left);
	fixnum_local(local, &right, 20);
	multi_bf_real_local(local, left, right, &check);
	test(check == left, "multi_bf_real_local6");

	bignum_value_local(local, &left, SignPlus, 1);
	fixnum_local(local, &right, 20);
	multi_bf_real_local(local, left, right, &check);
	test(check == right, "multi_bf_real_local7");

	bignum_value_local(local, &left, SignMinus, 1);
	fixnum_local(local, &right, 20);
	multi_bf_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_local8");
	test(GetStatusDynamic(check), "multi_bf_real_local9");
	test(RefFixnum(check) == -20, "multi_bf_real_local10");

	bignum_value_local(local, &left, SignPlus, 20);
	fixnum_local(local, &right, -3);
	multi_bf_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_local11");
	test(GetStatusDynamic(check), "multi_bf_real_local12");
	test(RefFixnum(check) == -60, "multi_bf_real_local13");

	bignum_value_local(local, &left, SignMinus, FIXNUM_MAX);
	fixnum_local(local, &right, 4);
	multi_bf_real_local(local, left, right, &check);
	test(bignump(check), "multi_bf_real_local14");
	test(GetStatusDynamic(check), "multi_bf_real_local15");
	test(IsMinus(RefSignBignum(check)), "multi_bf_real_local16");
	test(RefSizeBignum(check) == 2, "multi_bf_real_local17");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_bf_real_local18");
	test(reffixed_bignum(check, 1) == 1, "multi_bf_real_local19");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bf_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 10);
	fixnum_heap(&right, 0);
	multi_bf_real_common(local, left, right, &check);
	test(check == right, "multi_bf_real_common1");

	fixnum_heap(&right, 1);
	multi_bf_real_common(local, left, right, &check);
	test(check == left, "multi_bf_real_common2");

	fixnum_heap(&right, -1);
	multi_bf_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_common3");
	test(! GetStatusDynamic(check), "multi_bf_real_common4");
	test(RefFixnum(check) == -10, "multi_bf_real_common5");

	bignum_zero_heap(&left);
	fixnum_heap(&right, 20);
	multi_bf_real_common(local, left, right, &check);
	test(check == left, "multi_bf_real_common6");

	bignum_value_heap(&left, SignPlus, 1);
	fixnum_heap(&right, 20);
	multi_bf_real_common(local, left, right, &check);
	test(check == right, "multi_bf_real_common7");

	bignum_value_local(local, &left, SignMinus, 1);
	fixnum_local(local, &right, 20);
	multi_bf_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_common8");
	test(! GetStatusDynamic(check), "multi_bf_real_common9");
	test(RefFixnum(check) == -20, "multi_bf_real_common10");

	bignum_value_local(local, &left, SignPlus, 20);
	fixnum_local(local, &right, -3);
	multi_bf_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bf_real_common11");
	test(! GetStatusDynamic(check), "multi_bf_real_common12");
	test(RefFixnum(check) == -60, "multi_bf_real_common13");

	bignum_value_local(local, &left, SignMinus, FIXNUM_MAX);
	fixnum_local(local, &right, 4);
	multi_bf_real_common(local, left, right, &check);
	test(bignump(check), "multi_bf_real_common14");
	test(! GetStatusDynamic(check), "multi_bf_real_common15");
	test(IsMinus(RefSignBignum(check)), "multi_bf_real_common16");
	test(RefSizeBignum(check) == 2, "multi_bf_real_common17");
	test(reffixed_bignum(check, 0) == BIGNUM_FULL - 3, "multi_bf_real_common18");
	test(reffixed_bignum(check, 1) == 1, "multi_bf_real_common19");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_zero_local(local, &right);
	multi_bb_bignum_local(local, left, right, &check);
	test(check == right, "multi_bb_bignum_local1");

	bignum_value_local(local, &right, SignPlus, 1);
	multi_bb_bignum_local(local, left, right, &check);
	test(check == left, "multi_bb_bignum_local2");

	bignum_value_local(local, &right, SignMinus, 1);
	multi_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_bignum_local3");
	test(GetStatusDynamic(check), "multi_bb_bignum_local4");
	test(equal_value_bignum(check, SignMinus, 10), "multi_bb_bignum_local5");

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 20);
	multi_bb_bignum_local(local, left, right, &check);
	test(check == left, "multi_bb_bignum_local6");

	bignum_value_local(local, &left, SignPlus, 1);
	multi_bb_bignum_local(local, left, right, &check);
	test(check == right, "multi_bb_bignum_local7");

	bignum_value_local(local, &left, SignMinus, 1);
	multi_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_bignum_local8");
	test(GetStatusDynamic(check), "multi_bb_bignum_local9");
	test(equal_value_bignum(check, SignPlus, 20), "multi_bb_bignum_local10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_bignum_local11");
	test(GetStatusDynamic(check), "multi_bb_bignum_local12");
	test(equal_value_bignum(check, SignMinus, 30), "multi_bb_bignum_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_zero_local(local, &right);
	multi_bb_real_local(local, left, right, &check);
	test(check == right, "multi_bb_real_local1");

	bignum_value_local(local, &right, SignPlus, 1);
	multi_bb_real_local(local, left, right, &check);
	test(check == left, "multi_bb_real_local2");

	bignum_value_local(local, &right, SignMinus, 1);
	multi_bb_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_local3");
	test(GetStatusDynamic(check), "multi_bb_real_local4");
	test(RefFixnum(check) == -10, "multi_bb_real_local5");

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 20);
	multi_bb_real_local(local, left, right, &check);
	test(check == left, "multi_bb_real_local6");

	bignum_value_local(local, &left, SignPlus, 1);
	multi_bb_real_local(local, left, right, &check);
	test(check == right, "multi_bb_real_local7");

	bignum_value_local(local, &left, SignMinus, 1);
	multi_bb_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_local8");
	test(GetStatusDynamic(check), "multi_bb_real_local9");
	test(RefFixnum(check) == 20, "multi_bb_real_local10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_local11");
	test(GetStatusDynamic(check), "multi_bb_real_local12");
	test(RefFixnum(check) == -30, "multi_bb_real_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 10);
	bignum_zero_heap(&right);
	multi_bb_real_common(local, left, right, &check);
	test(check == right, "multi_bb_real_common1");

	bignum_value_heap(&right, SignPlus, 1);
	multi_bb_real_common(local, left, right, &check);
	test(check == left, "multi_bb_real_common2");

	bignum_value_heap(&right, SignMinus, 1);
	multi_bb_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_common3");
	test(! GetStatusDynamic(check), "multi_bb_real_common4");
	test(RefFixnum(check) == -10, "multi_bb_real_common5");

	bignum_zero_heap(&left);
	bignum_value_heap(&right, SignMinus, 20);
	multi_bb_real_common(local, left, right, &check);
	test(check == left, "multi_bb_real_common6");

	bignum_value_heap(&left, SignPlus, 1);
	multi_bb_real_common(local, left, right, &check);
	test(check == right, "multi_bb_real_common7");

	bignum_value_local(local, &left, SignMinus, 1);
	multi_bb_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_common8");
	test(! GetStatusDynamic(check), "multi_bb_real_common9");
	test(RefFixnum(check) == 20, "multi_bb_real_common10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bb_real_common11");
	test(! GetStatusDynamic(check), "multi_bb_real_common12");
	test(RefFixnum(check) == -30, "multi_bb_real_common13");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_nosign_bignum_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_zero_local(local, &right);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(check == right, "multi_bb_nosign_bignum_local1");

	bignum_value_local(local, &right, SignPlus, 1);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(check == left, "multi_bb_nosign_bignum_local2");

	bignum_value_local(local, &right, SignMinus, 1);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_bignum_local3");
	test(GetStatusDynamic(check), "multi_bb_nosign_bignum_local4");
	test(equal_value_nosign_bignum(check, 10), "multi_bb_nosign_bignum_local5");

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 20);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(check == left, "multi_bb_nosign_bignum_local6");

	bignum_value_local(local, &left, SignPlus, 1);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(check == right, "multi_bb_nosign_bignum_local7");

	bignum_value_local(local, &left, SignMinus, 1);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_bignum_local8");
	test(GetStatusDynamic(check), "multi_bb_nosign_bignum_local9");
	test(equal_value_nosign_bignum(check, 20), "multi_bb_nosign_bignum_local10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_nosign_bignum_local(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_bignum_local11");
	test(GetStatusDynamic(check), "multi_bb_nosign_bignum_local12");
	test(equal_value_nosign_bignum(check, 30), "multi_bb_nosign_bignum_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_nosign_real_local(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &left, SignPlus, 10);
	bignum_zero_local(local, &right);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(check == right, "multi_bb_nosign_real_local1");

	bignum_value_local(local, &right, SignPlus, 1);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(check == left, "multi_bb_nosign_real_local2");

	bignum_value_local(local, &right, SignMinus, 1);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_real_local3");
	test(GetStatusDynamic(check), "multi_bb_nosign_real_local4");
	test(equal_value_nosign_bignum(check, 10), "multi_bb_nosign_real_local5");

	bignum_zero_local(local, &left);
	bignum_value_local(local, &right, SignMinus, 20);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(check == left, "multi_bb_nosign_real_local6");

	bignum_value_local(local, &left, SignPlus, 1);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(check == right, "multi_bb_nosign_real_local7");

	bignum_value_local(local, &left, SignMinus, 1);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_real_local8");
	test(GetStatusDynamic(check), "multi_bb_nosign_real_local9");
	test(equal_value_nosign_bignum(check, 20), "multi_bb_nosign_real_local10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_nosign_real_local(local, left, right, &check);
	test(fixnump(check), "multi_bb_nosign_real_local11");
	test(GetStatusDynamic(check), "multi_bb_nosign_real_local12");
	test(RefFixnum(check) ==  30, "multi_bb_nosign_real_local13");

	rollback_local(local, stack);

	RETURN;
}

static int test_multi_bb_nosign_real_common(void)
{
	LocalRoot local;
	LocalStack stack;
	addr left, right, check;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_heap(&left, SignPlus, 10);
	bignum_zero_heap(&right);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(check == right, "multi_bb_nosign_real_common1");

	bignum_value_heap(&right, SignPlus, 1);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(check == left, "multi_bb_nosign_real_common2");

	bignum_value_heap(&right, SignMinus, 1);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_real_common3");
	test(! GetStatusDynamic(check), "multi_bb_nosign_real_common4");
	test(equal_value_nosign_bignum(check, 10), "multi_bb_nosign_real_common5");

	bignum_zero_heap(&left);
	bignum_value_heap(&right, SignMinus, 20);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(check == left, "multi_bb_nosign_real_common6");

	bignum_value_heap(&left, SignPlus, 1);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(check == right, "multi_bb_nosign_real_common7");

	bignum_value_heap(&left, SignMinus, 1);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(bignump(check), "multi_bb_nosign_real_common8");
	test(! GetStatusDynamic(check), "multi_bb_nosign_real_common9");
	test(equal_value_nosign_bignum(check, 20), "multi_bb_nosign_real_common10");

	bignum_value_local(local, &left, SignPlus, 15);
	bignum_value_local(local, &right, SignMinus, 2);
	multi_bb_nosign_real_common(local, left, right, &check);
	test(fixnump(check), "multi_bb_nosign_real_common11");
	test(! GetStatusDynamic(check), "multi_bb_nosign_real_common12");
	test(RefFixnum(check) ==  30, "multi_bb_nosign_real_common13");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  math
 */
static int test_abs_fixnum_integer_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	abs_fixnum_integer_local(local, pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_local1");
	test(GetStatusDynamic(pos), "abs_fixnum_integer_local2");
	test(RefFixnum(pos) == 10, "abs_fixnum_integer_local3");

	fixnum_local(local, &pos, -20);
	abs_fixnum_integer_local(local, pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_local4");
	test(GetStatusDynamic(pos), "abs_fixnum_integer_local5");
	test(RefFixnum(pos) == 20, "abs_fixnum_integer_local6");

	fixnum_local(local, &pos, FIXNUM_MAX);
	abs_fixnum_integer_local(local, pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_local7");
	test(GetStatusDynamic(pos), "abs_fixnum_integer_local8");
	test(RefFixnum(pos) == FIXNUM_MAX, "abs_fixnum_integer_local9");

	fixnum_local(local, &pos, FIXNUM_MIN);
	abs_fixnum_integer_local(local, pos, &pos);
	test(bignump(pos), "abs_fixnum_integer_local10");
	test(GetStatusDynamic(pos), "abs_fixnum_integer_local11");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN), "abs_fixnum_integer_local12");

	rollback_local(local, stack);

	RETURN;
}

static int test_abs_fixnum_integer_common(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	fixnum_local(local, &pos, 10);
	abs_fixnum_integer_common(pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_common1");
	test(! GetStatusDynamic(pos), "abs_fixnum_integer_common2");
	test(RefFixnum(pos) == 10, "abs_fixnum_integer_common3");

	fixnum_local(local, &pos, -20);
	abs_fixnum_integer_common(pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_common4");
	test(! GetStatusDynamic(pos), "abs_fixnum_integer_common5");
	test(RefFixnum(pos) == 20, "abs_fixnum_integer_common6");

	fixnum_local(local, &pos, FIXNUM_MAX);
	abs_fixnum_integer_common(pos, &pos);
	test(fixnump(pos), "abs_fixnum_integer_common7");
	test(! GetStatusDynamic(pos), "abs_fixnum_integer_common8");
	test(RefFixnum(pos) == FIXNUM_MAX, "abs_fixnum_integer_common9");

	fixnum_local(local, &pos, FIXNUM_MIN);
	abs_fixnum_integer_common(pos, &pos);
	test(bignump(pos), "abs_fixnum_integer_common10");
	test(! GetStatusDynamic(pos), "abs_fixnum_integer_common11");
	test(equal_value_bignum(pos, SignPlus, FIXNUM_UMIN), "abs_fixnum_integer_common12");

	rollback_local(local, stack);

	RETURN;
}

static int test_abs_bignum_integer_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	abs_bignum_integer_local(local, pos, &pos);
	test(bignump(pos), "abs_bignum_integer_local1");
	test(GetStatusDynamic(pos), "abs_bignum_integer_local2");
	test(equal_value_bignum(pos, SignPlus, 10), "abs_bignum_integer_local3");

	bignum_value_local(local, &pos, SignMinus, 20);
	abs_bignum_integer_local(local, pos, &pos);
	test(bignump(pos), "abs_bignum_integer_local4");
	test(GetStatusDynamic(pos), "abs_bignum_integer_local5");
	test(equal_value_bignum(pos, SignPlus, 20), "abs_bignum_integer_local6");

	rollback_local(local, stack);

	RETURN;
}

static int test_abs_bignum_integer_common(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	bignum_value_local(local, &pos, SignPlus, 10);
	abs_bignum_integer_common(pos, &pos);
	test(bignump(pos), "abs_bignum_integer_common1");
	test(! GetStatusDynamic(pos), "abs_bignum_integer_common2");
	test(equal_value_bignum(pos, SignPlus, 10), "abs_bignum_integer_common3");

	bignum_value_local(local, &pos, SignMinus, 20);
	abs_bignum_integer_common(pos, &pos);
	test(bignump(pos), "abs_bignum_integer_common4");
	test(! GetStatusDynamic(pos), "abs_bignum_integer_common5");
	test(equal_value_bignum(pos, SignPlus, 20), "abs_bignum_integer_common6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  output
 */
static int equalstream(addr stream, const char *right)
{
	int result;
	addr left;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	string_stream_local(local, stream, &left);
	result = string_equal_char(left, right);
	rollback_local(local, stack);
	clear_output_string_stream(stream);

	return result;
}

static int test_output_nosign_fixnum(void)
{
	addr stream;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	output_nosign_fixnum(stream, 0, 10, 0);
	test(equalstream(stream, "0"), "output_nosign_fixnum1");
	output_nosign_fixnum(stream, 12, 10, 0);
	test(equalstream(stream, "12"), "output_nosign_fixnum2");
	output_nosign_fixnum(stream, -56, 10, 0);
	test(equalstream(stream, "56"), "output_nosign_fixnum3");
	output_nosign_fixnum(stream, 0xAB, 16, 0);
	test(equalstream(stream, "ab"), "output_nosign_fixnum4");
	output_nosign_fixnum(stream, -0xFE, 16, 1);
	test(equalstream(stream, "FE"), "output_nosign_fixnum5");
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_charqueue_nreverse(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	unicode *ptr;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_local(local, &pos, 2);
	pushchar_charqueue_local(local, pos, "abcdefghij");
	GetCharQueueRoot(pos, &pos);
	charqueue_nreverse(pos, &pos);
	test(pos != Nil, "charqueue_nreverse1");

	GetCharBitSize(pos, &size);
	test(size == 2, "charqueue_nreverse2");
	ptr = PtrCharBitChar(pos);
	test(ptr[0] == 'i', "charqueue_nreverse3");
	test(ptr[1] == 'j', "charqueue_nreverse4");

	GetCharBitNext(pos, &pos);
	GetCharBitSize(pos, &size);
	test(size == 2, "charqueue_nreverse5");
	ptr = PtrCharBitChar(pos);
	test(ptr[0] == 'g', "charqueue_nreverse6");
	test(ptr[1] == 'h', "charqueue_nreverse7");

	GetCharBitNext(pos, &pos);
	GetCharBitNext(pos, &pos);
	GetCharBitNext(pos, &pos);
	ptr = PtrCharBitChar(pos);
	test(ptr[0] == 'a', "charqueue_nreverse8");
	test(ptr[1] == 'b', "charqueue_nreverse9");
	GetCharBitNext(pos, &pos);
	test(pos == Nil, "charqueue_nreverse10");

	rollback_local(local, stack);

	RETURN;
}

static int test_charqueue_nreverse_output(void)
{
	addr pos, stream;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	charqueue_local(local, &pos, 2);
	pushchar_charqueue_local(local, pos, "abcdefghi");

	open_output_string_stream(&stream, 0);
	charqueue_nreverse_output(pos, stream);
	test(equalstream(stream, "ihgfedcba"), "charqueue_nreverse_output1");
	close_stream(stream);

	rollback_local(local, stack);

	RETURN;
}

static int test_output_nosign_bignum(void)
{
	addr stream, pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	bignum_zero_alloc(local, &pos);
	output_nosign_bignum(local, stream, pos, 10, 0);
	test(equalstream(stream, "0"), "output_nosign_bignum1");
	bignum_value_alloc(local, &pos, signplus_bignum, 12);
	output_nosign_bignum(local, stream, pos, 10, 0);
	test(equalstream(stream, "12"), "output_nosign_bignum2");
	bignum_value_alloc(local, &pos, signminus_bignum, 56);
	output_nosign_bignum(local, stream, pos, 10, 0);
	test(equalstream(stream, "56"), "output_nosign_bignum3");
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "abcdef01234567890abcdef0123456");
	bignum_cons_alloc(local, &pos, signplus_bignum, cons);
	output_nosign_bignum(local, stream, pos, 16, 0);
	test(equalstream(stream, "abcdef01234567890abcdef0123456"),
			"output_nosign_bignum4");
	setchar_bigcons(local, cons, 16, "aabbccddeeff00112233445566778899aabbcc");
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	output_nosign_bignum(local, stream, pos, 16, 1);
	test(equalstream(stream, "AABBCCDDEEFF00112233445566778899AABBCC"),
			"output_nosign_bignum5");
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_output_nosign_comma_fixnum(void)
{
	addr stream;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);
	output_nosign_comma_fixnum(local, stream, 0, 10, 0, 2, ',');
	test(equalstream(stream, "0"), "output_nosign_comma_fixnum1");
	output_nosign_comma_fixnum(local, stream, 12, 10, 0, 2, ',');
	test(equalstream(stream, "12"), "output_nosign_comma_fixnum2");
	output_nosign_comma_fixnum(local, stream, -567, 10, 0, 2, ',');
	test(equalstream(stream, "5,67"), "output_nosign_comma_fixnum3");
	output_nosign_comma_fixnum(local, stream, 0xAB, 16, 0, 2, ',');
	test(equalstream(stream, "ab"), "output_nosign_comma_fixnum4");
	output_nosign_comma_fixnum(local, stream, -0xABCDEF, 16, 1, 2, ',');
	test(equalstream(stream, "AB,CD,EF"), "output_nosign_comma_fixnum5");
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}

static int test_output_nosign_comma_bignum(void)
{
	addr stream, pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	open_output_string_stream(&stream, 0);

	bignum_zero_alloc(local, &pos);
	output_nosign_comma_bignum(local, stream, pos, 10, 0, 2, '-');
	test(equalstream(stream, "0"), "output_nosign_comma_bignum1");
	bignum_value_alloc(local, &pos, signplus_bignum, 12);
	output_nosign_comma_bignum(local, stream, pos, 10, 0, 2, '-');
	test(equalstream(stream, "12"), "output_nosign_comma_bignum2");
	bignum_value_alloc(local, &pos, signminus_bignum, 567);
	output_nosign_comma_bignum(local, stream, pos, 10, 0, 2, '-');
	test(equalstream(stream, "5-67"), "output_nosign_comma_bignum3");
	bigcons_local(local, &cons);
	setchar_bigcons(local, cons, 16, "abcdef01234567890abcdef0123456");
	bignum_cons_alloc(local, &pos, signplus_bignum, cons);
	output_nosign_comma_bignum(local, stream, pos, 16, 0, 3, ',');
	test(equalstream(stream, "abc,def,012,345,678,90a,bcd,ef0,123,456"),
			"output_nosign_comma_bignum4");
	setchar_bigcons(local, cons, 16, "aabbccddeeff00112233445566778899aabbcc");
	bignum_cons_alloc(local, &pos, signminus_bignum, cons);
	output_nosign_comma_bignum(local, stream, pos, 16, 1, 5, '=');
	test(equalstream(stream, "AAB=BCCDD=EEFF0=01122=33445=56677=8899A=ABBCC"),
			"output_nosign_comma_bignum5");
	close_stream(stream);
	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_bignum(void)
{
	/* bignum */
	TestBreak(test_alloc_bignum);
	TestBreak(test_realloc_bignum);
	TestBreak(test_bignum_alloc);
	TestBreak(test_bignum_cons_alloc);
	TestBreak(test_bignum_copy_nosign_alloc);
	TestBreak(test_bignum_copy_alloc);
	TestBreak(test_bignum_value_alloc);
	TestBreak(test_bignum_value2_alloc);
	TestBreak(test_bignum_zero_alloc);
	TestBreak(test_bignum_fixnum_alloc);
	TestBreak(test_bignum_fixnum_value_alloc);
	TestBreak(test_bignum_counter_alloc);
	TestBreak(test_getfixed_bignum);
	TestBreak(test_reffixed_bignum);
	TestBreak(test_setfixed_bignum);
	TestBreak(test_diet_bignum);
	TestBreak(test_sizepress_bignum);
	/* operation */
	TestBreak(test_resize_nocopy_bignum);
	TestBreak(test_copy_bignum);
	TestBreak(test_copy_noexpand_bignum);
	TestBreak(test_setvalue_bignum);
	TestBreak(test_setzero_bignum);
	TestBreak(test_getbit_bignum);
	TestBreak(test_bignum_result_alloc);
	TestBreak(test_bignum_throw_heap);
	TestBreak(test_fixnum_throw_heap);
	TestBreak(test_bignum_throw_local);
	TestBreak(test_fixnum_throw_local);
	TestBreak(test_power2_bignum_alloc);
	TestBreak(test_shiftup_bignum_alloc);
	/* integer */
	TestBreak(test_carryvalue_alloc);
	TestBreak(test_castfixnum);
	TestBreak(test_fixnum_cons_alloc);
	TestBreak(test_integer_cons_alloc);
	TestBreak(test_integer_fixed_alloc);
	TestBreak(test_integer_bignum_alloc);
	/* integer-copy */
	TestBreak(test_integer_copy_alloc);
	/* float */
	TestBreak(test_HexToChar);
	TestBreak(test_hexchar_bigtype);
	TestBreak(test_hexfraction_string);
	TestBreak(test_expchar_make_float);
	TestBreak(test_make_float_string);
	TestBreak(test_single_float_bignum);
	TestBreak(test_double_float_bignum);
	TestBreak(test_long_float_bignum);
	TestBreak(test_single_float_fixnum_alloc);
	TestBreak(test_double_float_fixnum_alloc);
	TestBreak(test_long_float_fixnum_alloc);
	TestBreak(test_single_float_bignum_alloc);
	TestBreak(test_double_float_bignum_alloc);
	TestBreak(test_long_float_bignum_alloc);
	TestBreak(test_printf_integer_float_size);
	TestBreak(test_bignum_single_float_alloc);
	TestBreak(test_bignum_double_float_alloc);
	TestBreak(test_bignum_long_float_alloc);
	/* compare */
	TestBreak(test_zerop_or_plusp_bignum);
	TestBreak(test_plusp_bignum);
	TestBreak(test_minusp_bignum);
	TestBreak(test_zerop_bignum);
	TestBreak(test_evenp_bignum);
	TestBreak(test_oddp_bignum);
	TestBreak(test_castfixed);
	TestBreak(test_castfixed_fixnum);
	TestBreak(test_castfixed_integer);
	TestBreak(test_equal_fb_real);
	TestBreak(test_equal_bb_real);
	TestBreak(test_equal_nosign_bignum);
	TestBreak(test_compare_bigtype);
	TestBreak(test_compare_value_bignum);
	TestBreak(test_compare_bignum_value);
	TestBreak(test_compare_fb_real);
	TestBreak(test_compare_bf_real);
	TestBreak(test_compare_bb_real);
	TestBreak(test_equal_value_nosign_bignum);
	TestBreak(test_equal_value_bignum);
	TestBreak(test_equal_value2_nosign_bignum);
	TestBreak(test_equal_value2_bignum);
	TestBreak(test_compare_bs_real);
	TestBreak(test_compare_bd_real);
	TestBreak(test_compare_bl_real);
	TestBreak(test_compare_sb_real);
	TestBreak(test_compare_db_real);
	TestBreak(test_compare_lb_real);
	/* byte */
	TestBreak(test_fixnum_unsigned_byte_p);
	TestBreak(test_bignum_unsigned_byte_p);
	TestBreak(test_fixnum_signed_byte_p);
	TestBreak(test_bignum_signed_byte_p);
	TestBreak(test_GetFixnum_bignum);
	TestBreak(test_GetFixnum_signed);
	/* plus / minus */
	TestBreak(test_plusvalue);
	TestBreak(test_minusvalue);
	/* plus */
	TestBreak(test_plus_vv_bignum_local);
	TestBreak(test_plus_fv_bignum_local);
	TestBreak(test_plus_fv_real_local);
	TestBreak(test_plus_fv_real_common);
	TestBreak(test_plus_ff_bignum_local);
	TestBreak(test_plus_ff_real_local);
	TestBreak(test_plus_ff_real_common);
	TestBreak(test_plusfixnum_bignum_local);
	TestBreak(test_plusfixnum_real_local);
	TestBreak(test_plusfixnum_real_common);
	TestBreak(test_plus_bv_bignum_local);
	TestBreak(test_plus_bv_real_local);
	TestBreak(test_plus_bv_real_common);
	TestBreak(test_plus_bf_bignum_local);
	TestBreak(test_plus_bf_real_local);
	TestBreak(test_plus_bf_real_common);
	TestBreak(test_plus_bb_bignum_local);
	TestBreak(test_plus_bb_real_local);
	TestBreak(test_plus_bb_real_common);
	/* minus */
	TestBreak(test_sigrev_bignum_inplace);
	TestBreak(test_sigrev_fixnum_bignum_local);
	TestBreak(test_sigrev_fixnum_integer_alloc);
	TestBreak(test_sigrev_bignum_bignum_local);
	TestBreak(test_sigrev_bignum_integer_alloc);
	TestBreak(test_minus_vv_bignum_local);
	TestBreak(test_minus_ff_bignum_local);
	TestBreak(test_minus_ff_real_local);
	TestBreak(test_minus_ff_real_common);
	TestBreak(test_minus_bf_bignum_local);
	TestBreak(test_minus_bf_real_local);
	TestBreak(test_minus_bf_real_common);
	TestBreak(test_minus_fb_bignum_local);
	TestBreak(test_minus_fb_real_local);
	TestBreak(test_minus_fb_real_common);
	TestBreak(test_minus_bb_bignum_local);
	TestBreak(test_minus_bb_real_local);
	TestBreak(test_minus_bb_real_common);
	/* multiple */
	TestBreak(test_multisafe_fixnum);
	TestBreak(test_multi_ff_bignum_local);
	TestBreak(test_multi_ff_real_local);
	TestBreak(test_multi_ff_real_common);
	TestBreak(test_multi_bf_bignum_local);
	TestBreak(test_multi_bf_real_local);
	TestBreak(test_multi_bf_real_common);
	TestBreak(test_multi_bb_bignum_local);
	TestBreak(test_multi_bb_real_local);
	TestBreak(test_multi_bb_real_common);
	TestBreak(test_multi_bb_nosign_bignum_local);
	TestBreak(test_multi_bb_nosign_real_local);
	TestBreak(test_multi_bb_nosign_real_common);
	/* math */
	TestBreak(test_abs_fixnum_integer_local);
	TestBreak(test_abs_fixnum_integer_common);
	TestBreak(test_abs_bignum_integer_local);
	TestBreak(test_abs_bignum_integer_common);
	/* output */
	TestBreak(test_output_nosign_fixnum);
	TestBreak(test_charqueue_nreverse);
	TestBreak(test_charqueue_nreverse_output);
	TestBreak(test_output_nosign_bignum);
	TestBreak(test_output_nosign_comma_fixnum);
	TestBreak(test_output_nosign_comma_bignum);

	return 0;
}

static void testinit_bignum(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

#endif

int test_bignum(void)
{
#if 0
	DegradeTitle;
	return DegradeCode(bignum);
#endif
	return 0;
}

