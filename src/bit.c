#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bit.h"
#include "build.h"
#include "condition.h"
#include "format.h"
#include "integer.h"
#include "local.h"
#include "heap.h"
#include "number.h"
#include "sequence.h"

#define FixedBit		(sizeoft(fixed) * 8UL)

/*
 *  bit
 */
int bitp(addr pos)
{
	fixnum value;

	if (GetType(pos) != LISPTYPE_FIXNUM)
		return 0;
	GetFixnum(pos, &value);

	return value == 0 || value == 1;
}

int bit_getint(addr pos, int *ret)
{
	fixnum value;

	if (GetType(pos) != LISPTYPE_FIXNUM)
		return 1;
	GetFixnum(pos, &value);
	if (value != 0 && value != 1)
		return 1;
	*ret = (int)value;

	return 0;
}

int bit_getint_error_(addr pos, int *ret)
{
	if (bit_getint(pos, ret))
		return fmte_("Bit value ~S must be a 0 or 1.", pos, NULL);
	return 0;
}


/*
 *  bitcons
 */
struct bitbuffer_struct {
	size_t index, array;
#ifdef __cplusplus
	fixed data[1];
#else
	fixed data[];
#endif
};

#define BitBufferStruct(x) ((struct bitbuffer_struct *)PtrBodySSa(x, 1))
#define RefBitBuffer(x)		RefArraySS(x,0)
#define GetBitBuffer(x,y)	GetArraySS(x,0,y)
#define SetBitBuffer(x,y)	SetArraySS(x,0,y)

static void bitbuffer_local(LocalRoot local, addr *ret, size_t fixedsize)
{
	addr pos;
	struct bitbuffer_struct *str;
	size_t allsize, bodysize;

	bodysize = fixedsize * sizeoft(fixed);
	allsize = sizeoft(struct bitbuffer_struct) + bodysize;
	Check(0xFF < allsize, "size error");
	local_smallsize(local, &pos, LISPSYSTEM_BITBUFFER, 1, allsize);
	str = BitBufferStruct(pos);
	str->index = 0;
	str->array = 0;
	memset(str->data, 0, bodysize);
	*ret = pos;
}

struct bitcons_struct {
	size_t bitsize, fixedsize, index;
};

#define BitConsStruct(x) ((struct bitcons_struct *)PtrBodySSa(x, 2))
#define GetBitConsRoot(x,y) GetArraySS(x,0,y)
#define GetBitConsTail(x,y) GetArraySS(x,1,y)
#define SetBitConsRoot(x,y) SetArraySS(x,0,y)
#define SetBitConsTail(x,y) SetArraySS(x,1,y)

#ifdef LISP_DEBUG
#define BITMEMORY_SIZE	1UL
#else
#define BITMEMORY_SIZE	(16UL * 8UL * 8UL)
#endif

static size_t getfixedsize(size_t bitsize)
{
	size_t size, fixedsize;

	/* bit -> byte */
	size = bitsize / 8;
	if (bitsize % 8)
		size++;

	/* byte -> word */
	fixedsize = size / sizeoft(fixed);
	if (size % sizeoft(fixed))
		fixedsize++;

	return fixedsize;
}

void bitcons_local(LocalRoot local, addr *ret, size_t bitsize)
{
	addr pos, child;
	struct bitcons_struct *str;
	size_t fixedsize;

	local_smallsize(local, &pos, LISPSYSTEM_BITCONS,
			2, sizeoft(struct bitcons_struct));
	if (bitsize == 0)
		bitsize = BITMEMORY_SIZE;
	fixedsize = getfixedsize(bitsize);
	bitbuffer_local(local, &child, fixedsize);
	SetBitConsRoot(pos, child);
	SetBitConsTail(pos, child);
	str = BitConsStruct(pos);
	str->fixedsize = fixedsize;
	str->bitsize = 8UL * fixedsize * sizeoft(fixed);
	str->index = 0;
	*ret = pos;
}

static void pushnext_bitcons(LocalRoot local, addr pos, addr *ret)
{
	addr child, one;
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;

	GetBitConsTail(pos, &child);
	str1 = BitConsStruct(pos);
	str2 = BitBufferStruct(child);
	if (str2->index < str1->bitsize) {
		*ret = child;
	}
	else {
		bitbuffer_local(local, &one, str1->fixedsize);
		SetBitBuffer(child, one);
		SetBitConsTail(pos, one);
		*ret = one;
	}
}

void push_bitcons(LocalRoot local, addr pos, int value)
{
	addr child;
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;
	size_t q, r;

	CheckType(pos, LISPSYSTEM_BITCONS);
	pushnext_bitcons(local, pos, &child);
	str2 = BitBufferStruct(child);
	q = str2->index / FixedBit;
	r = str2->index % FixedBit;
	if (value)
		str2->data[q] |= ((fixed)1UL) << r;
	str2->array = q;
	str2->index++;
	str1 = BitConsStruct(pos);
	str1->index++;
}


/*
 *  bitmemory
 */
void bitmemory_unsafe(LocalRoot local, addr *ret, size_t bitsize)
{
	addr pos;
	size_t fixedsize, allsize;
	struct bitmemory_struct *str;

	fixedsize = getfixedsize(bitsize);
	allsize = fixedsize * sizeoft(fixed) + sizeoft(struct bitmemory_struct);
	alloc_body(local, &pos, LISPTYPE_BITVECTOR, allsize);
	str = BitMemoryStruct(pos);
	str->bitsize = bitsize;
	str->fixedsize = fixedsize;
#ifdef LISP_DEBUG
	bitmemory_memset_byte(pos, 0xAA);
#endif
	*ret = pos;
}

void bitmemory_alloc(LocalRoot local, addr *ret, size_t bitsize)
{
	bitmemory_unsafe(local, ret, bitsize);
	bitmemory_memset(*ret, 0);
}

void bitmemory_local(LocalRoot local, addr *ret, size_t bitsize)
{
	Check(local == NULL, "local error");
	bitmemory_alloc(local, ret, bitsize);
}

void bitmemory_heap(addr *ret, size_t bitsize)
{
	bitmemory_alloc(NULL, ret, bitsize);
}

void bitmemory_cons_alloc(LocalRoot local, addr *ret, addr cons)
{
	struct bitcons_struct *str1;
	struct bitbuffer_struct *str2;
	fixed *data;
	addr pos;
	size_t bitsize, array, i;
#ifdef LISP_DEBUG
	size_t fixedsize;
#endif

	str1 = BitConsStruct(cons);
	bitsize = str1->index;
	bitmemory_unsafe(local, &pos, bitsize);
	if (bitsize) {
#ifdef LISP_DEBUG
		fixedsize = getfixedsize(bitsize);
#endif
		data = BitMemoryStruct(pos)->data;
		GetBitConsRoot(cons, &cons);
		for (i = 0; cons != Nil; i += array) {
			Check(fixedsize <= i, "size error");
			str2 = BitBufferStruct(cons);
			array = str2->array + 1UL;
			memcpy(data + i, str2->data, array * sizeoft(fixed));
			GetBitBuffer(cons, &cons);
		}
	}
	*ret = pos;
}

void bitmemory_cons_local(LocalRoot local, addr *ret, addr cons)
{
	Check(local == NULL, "local error");
	bitmemory_cons_alloc(local, ret, cons);
}

void bitmemory_cons_heap(addr *ret, addr cons)
{
	bitmemory_cons_alloc(NULL, ret, cons);
}

void bitmemory_char_heap(addr *ret, const char *str)
{
	char c;
	addr cons;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		c = *str;
		if (c == '\0')
			break;
		Check(c != '0' && c != '1', "string error");
		push_bitcons(local, cons, (int)(c - '0'));
		str++;
	}
	bitmemory_cons_heap(ret, cons);
	rollback_local(local, stack);
}

void bitmemory_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr vector;
	size_t size;

	bitmemory_length(pos, &size);
	bitmemory_alloc(local, &vector, size);
	bitmemory_copy_unsafe(vector, pos, size);
	*ret = vector;
}

void bitmemory_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	bitmemory_copy_alloc(local, ret, pos);
}

void bitmemory_copy_heap(addr *ret, addr pos)
{
	bitmemory_copy_alloc(NULL, ret, pos);
}

int bitmemoryp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR;
}

void bitmemory_memset_byte(addr pos, byte value)
{
	struct bitmemory_struct *str;
	str = BitMemoryStruct(pos);
	memset(str->data, value, str->fixedsize * sizeoft(fixed));
}

void bitmemory_memset(addr pos, int value)
{
	Check(value != 0 && value != 1, "value error");
	bitmemory_memset_byte(pos, value? 0xFF: 0x00);
}

void bitmemory_copy_unsafe(addr pos, addr refer, size_t bitsize)
{
	struct bitmemory_struct *str1, *str2;
	size_t fixedsize;
	fixed *data1;
	const fixed *data2;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str1 = BitMemoryStruct(pos);
	str2 = BitMemoryStruct(refer);
	Check(str1->bitsize < bitsize, "size1 error");
	Check(str2->bitsize < bitsize, "size2 error");
	fixedsize = getfixedsize(bitsize);
	data1 = str1->data;
	data2 = str2->data;
	memcpy(data1, data2, fixedsize * sizeof(fixed));
}

void bitmemory_length(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_BITVECTOR);
	*ret = BitMemoryStruct(pos)->bitsize;
}

int bitmemory_equal_length(addr pos1, addr pos2)
{
	size_t size1, size2;

	CheckType(pos1, LISPTYPE_BITVECTOR);
	CheckType(pos2, LISPTYPE_BITVECTOR);
	bitmemory_length(pos1, &size1);
	bitmemory_length(pos2, &size2);

	return size1 == size2;
}

int bitmemory_equal(addr pos1, addr pos2)
{
	struct bitmemory_struct *str1, *str2;
	int check1, check2;
	size_t size, i, quot;

	CheckType(pos1, LISPTYPE_BITVECTOR);
	CheckType(pos2, LISPTYPE_BITVECTOR);
	bitmemory_length(pos1, &size);
	bitmemory_length(pos2, &i);
	if (size != i)
		return 0;
	quot = size / FixedBit;
	if (quot) {
		str1 = BitMemoryStruct(pos1);
		str2 = BitMemoryStruct(pos2);
		if (memcmp(str1->data, str2->data, quot * sizeof(fixed)))
			return 0;
	}
	for (i = quot * FixedBit; i < size; i++) {
		bitmemory_getint_unsafe(pos1, i, &check1);
		bitmemory_getint_unsafe(pos2, i, &check2);
		if (check1 != check2)
			return 0;
	}

	return 1;
}

int bitmemory_refint_debug(addr pos, size_t index)
{
	int check;
	check = 0;
	Error(bitmemory_getint_(pos, index, &check));
	return check;
}

void bitmemory_getint_unsafe(addr pos, size_t index, int *ret)
{
	struct bitmemory_struct *str;
	size_t q, r;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str = BitMemoryStruct(pos);
	q = index / FixedBit;
	r = index % FixedBit;
	*ret = (int)((str->data[q] >> r) & 0x01);
}

void bitmemory_setint_unsafe(addr pos, size_t index, int value)
{
	struct bitmemory_struct *str;
	size_t q, r;

	CheckType(pos, LISPTYPE_BITVECTOR);
	str = BitMemoryStruct(pos);
	q = index / FixedBit;
	r = index % FixedBit;
	if (value)
		str->data[q] |= ((fixed)1UL) << r;
	else
		str->data[q] &= ~(((fixed)1UL) << r);
}

int bitmemory_getint_(addr pos, size_t index, int *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_BITVECTOR);
	bitmemory_length(pos, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	bitmemory_getint_unsafe(pos, index, ret);

	return 0;
}

int bitmemory_setint_(addr pos, size_t index, int value)
{
	size_t size;

	bitmemory_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	bitmemory_setint_unsafe(pos, index, value);

	return 0;
}

int bitmemory_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	int check;
	size_t size;

	CheckType(pos, LISPTYPE_BITVECTOR);
	bitmemory_length(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	}
	bitmemory_getint_unsafe(pos, index, &check);
	fixnum_alloc(local, ret, check? 1: 0);

	return 0;
}

int bitmemory_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_BITVECTOR);
	if (! consp(args)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return bitmemory_get_(local, pos, index, ret);
}

int bitmemory_set_(addr pos, size_t index, addr value)
{
	int check;
	size_t size;

	if (bit_getint(value, &check))
		return fmte_("The argument ~S must be bit type.", value, NULL);
	bitmemory_length(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(index), NULL);
	bitmemory_setint_unsafe(pos, index, check);

	return 0;
}

int bitmemory_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_BITVECTOR);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return bitmemory_set_(pos, index, value);
}

void bitmemory_bitcalc(addr pos, addr pos1, addr pos2, bitcalc_call call)
{
	struct bitmemory_struct *str, *str1, *str2;
	size_t size, i;
	fixed *data, *data1, *data2;

	str = BitMemoryStruct(pos);
	str1 = BitMemoryStruct(pos1);
	str2 = BitMemoryStruct(pos2);
	Check(str->bitsize != str1->bitsize, "size1 error");
	Check(str->bitsize != str2->bitsize, "size2 error");
	size = str->fixedsize;
	data = str->data;
	data1 = str1->data;
	data2 = str2->data;
	for (i = 0; i < size; i++)
		data[i] = (*call)(data1[i], data2[i]);
}

void bitmemory_bitnot(addr pos, addr pos1)
{
	struct bitmemory_struct *str, *str1;
	size_t size, i;
	fixed *data, *data1;

	str = BitMemoryStruct(pos);
	str1 = BitMemoryStruct(pos1);
	Check(str->bitsize != str1->bitsize, "size1 error");
	size = str->fixedsize;
	data = str->data;
	data1 = str1->data;
	for (i = 0; i < size; i++)
		data[i] = ~(data1[i]);
}

static int bitmemory_fillbit_(addr pos, int value, size_t index1, size_t index2)
{
	for (; index1 < index2; index1++) {
		Return(bitmemory_setint_(pos, index1, value));
	}

	return 0;
}

static int bitmemory_fillset_(addr pos, int value, size_t index1, size_t index2)
{
	size_t byte1, byte2, check;
	byte *data;

	byte1 = index1 / 8UL;
	byte2 = index2 / 8UL;
	if (byte1 == byte2)
		return bitmemory_fillbit_(pos, value, index1, index2);

	/* front */
	check = byte1 * 8UL;
	if (check != index1) {
		Return(bitmemory_fillbit_(pos, value, index1, check + 8UL));
	}

	/* tail */
	check = byte2 * 8UL;
	if (check != index2) {
		Return(bitmemory_fillbit_(pos, value, check, index2));
	}

	/* byte */
	data = (byte *)BitMemoryStruct(pos)->data;
	memset(data + byte1, value? 0xFF: 0x00, byte2 - byte1 - 1);

	return 0;
}

int bitmemory_fill_(addr pos, addr item, addr start, addr end)
{
	int value;
	size_t index1, index2;

	if (bit_getint(item, &value))
		return fmte_("FILL item ~S must be a bit (0 or 1 integer).", item, NULL);

	bitmemory_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return bitmemory_fillset_(pos, value, index1, index2);
}

int bitmemory_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2)
{
	int value;
	addr root;
	size_t i;

	bitmemory_unsafe(NULL, &root, index2 - index1);
	/* too slow */
	for (i = 0; index1 < index2; index1++, i++) {
		Return(bitmemory_getint_(pos, index1, &value));
		Return(bitmemory_setint_(root, i, value));
	}

	return Result(ret, root);
}

int bitmemory_subseq_(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	bitmemory_length(pos, &index1);
	Return(size_start_end_sequence_(start, end, index1, &index1, &index2, NULL));
	return bitmemory_subseq_index_(ret, pos, index1, index2);
}

int bitmemory_setget_(addr pos1, size_t index1, addr pos2, size_t index2)
{
	int value;

	Return(bitmemory_getint_(pos2, index2, &value));
	Return(bitmemory_setint_(pos1, index1, value));

	return 0;
}

int bitmemory_reverse_(LocalRoot local, addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	bitmemory_length(pos, &size);
	bitmemory_unsafe(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		Return(bitmemory_getint_(pos, x, &temp));
		Return(bitmemory_setint_(one, y, temp));
	}

	return Result(ret, one);
}

int bitmemory_nreverse_(addr *ret, addr pos)
{
	int a, b;
	size_t size, x, y;

	bitmemory_length(pos, &size);
	if (size <= 1)
		return 0;
	x = 0;
	y = size - 1;
	while (x < y) {
		Return(bitmemory_getint_(pos, x, &a));
		Return(bitmemory_getint_(pos, y, &b));
		Return(bitmemory_setint_(pos, x, b));
		Return(bitmemory_setint_(pos, y, a));
		x++;
		y--;
	}

	return Result(ret, pos);
}


/*
 *  bvarray
 */
int array_bvarrayp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->dimension == 1 && str->type == ARRAY_TYPE_BIT;
}

int bvarrayp(addr pos)
{
	if (GetType(pos) != LISPTYPE_ARRAY)
		return 0;
	return array_bvarrayp(pos);
}

int bitvectorp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR || bvarrayp(pos);
}

int simple_array_bvarrayp(addr pos)
{
	struct array_struct *str;
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	return str->simple && str->dimension == 1 && str->type == ARRAY_TYPE_BIT;
}

int simple_bvarrayp(addr pos)
{
	if (GetType(pos) != LISPTYPE_ARRAY)
		return 0;
	return simple_array_bvarrayp(pos);
}

int simple_bitvectorp(addr pos)
{
	return GetType(pos) == LISPTYPE_BITVECTOR || simple_bvarrayp(pos);
}


void bvarray_length(addr pos, size_t *ret)
{
	Check(! bvarrayp(pos), "type error");
	*ret = ArrayInfoStruct(pos)->front;
}

int bvarray_getint_(addr pos, size_t index, int *ret)
{
	int check;

	if (ArrayInfoStruct(pos)->front <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}
	Return(array_get_bit_(pos, index, &check));
	return Result(ret, check);
}

int bvarray_setint_(addr pos, size_t index, int value)
{
	if (ArrayInfoStruct(pos)->front <= index)
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	return array_set_bit_(pos, index, value);
}


/*
 *  bitvector
 */
int bitvector_length_(addr pos, size_t *ret)
{
	if (bitmemoryp(pos)) {
		bitmemory_length(pos, ret);
		return 0;
	}
	if (bvarrayp(pos)) {
		bvarray_length(pos, ret);
		return 0;
	}
	return fmte_("type error", NULL);
}

int bitvector_getint_(addr pos, size_t index, int *ret)
{
	if (bitmemoryp(pos))
		return bitmemory_getint_(pos, index, ret);

	if (bvarrayp(pos))
		return bvarray_getint_(pos, index, ret);

	*ret = 0;
	return fmte_("type error", NULL);
}

int bitvector_setint_(addr pos, size_t index, int value)
{
	if (bitmemoryp(pos))
		return bitmemory_setint_(pos, index, value);

	if (bvarrayp(pos))
		return bvarray_setint_(pos, index, value);

	return fmte_("type error", NULL);
}

static int bitmemory_array_equal(addr left, addr right, int *ret)
{
	int check1, check2;
	size_t size, i;

	Check(! bitmemoryp(left), "type left error");
	Check(! bvarrayp(right), "type right error");
	bitmemory_length(left, &size);
	bvarray_length(right, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(left, i, &check1));
		Return(bvarray_getint_(right, i, &check2));
		if (check1 != check2)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int bvarray_array_equal(addr left, addr right, int *ret)
{
	int check1, check2;
	size_t size, i;

	Check(! bvarrayp(left), "type left error");
	Check(! bvarrayp(right), "type right error");
	bvarray_length(left, &size);
	bvarray_length(right, &i);
	if (size != i)
		return Result(ret, 0);
	for (i = 0; i < size; i++) {
		Return(bvarray_getint_(left, i, &check1));
		Return(bvarray_getint_(right, i, &check2));
		if (check1 != check2)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int bitvector_equal_(addr left, addr right, int *ret)
{
	Check(! bitvectorp(left), "type left error");
	Check(! bitvectorp(right), "type right error");
	if (bitmemoryp(left)) {
		if (bitmemoryp(right))
			return Result(ret, bitmemory_equal(left, right));
		if (bvarrayp(right))
			return bitmemory_array_equal(left, right, ret);
	}
	if (bvarrayp(left)) {
		if (bitmemoryp(right))
			return bitmemory_array_equal(right, left, ret);
		if (bvarrayp(right))
			return bvarray_array_equal(left, right, ret);
	}

	*ret = 0;
	return fmte_("type error", NULL);
}

