#include "bigdata.h"
#include "bignum.h"
#include "boole.h"
#include "bytespec.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
#include "integer.h"
#include "setf.h"
#include "symbol.h"

typedef void (*BooleCall)(LocalRoot, addr, addr, addr *);
static BooleCall BooleTable[Boole_Size];

union boole_fixnumfixed {
	fixnum sign;
	fixed unsign;
};

/*
 *  boole-struct
 */
struct boole_struct {
	unsigned sign : 1;
	unsigned carry : 1;
	bigtype *data, v, temp;
	size_t size, index;
	addr pos;
};

static void boole_struct_fixnum(struct boole_struct *ptr, addr pos)
{
	int check;
	unsigned sign;

	CheckType(pos, LISPTYPE_FIXNUM);
	castfixed_fixnum(pos, &check, &(ptr->temp));
	sign = IsPlus(check)? 0: 1;
	ptr->sign = sign;
	ptr->carry = sign;
	ptr->data = &(ptr->temp);
	ptr->v = sign? ~((bigtype)0): 0;
	ptr->size = 1;
	ptr->index = 0;
	ptr->pos = pos;
}

static void boole_struct_bignum(struct boole_struct *ptr, addr pos)
{
	int check;
	unsigned sign;
	bigtype *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	GetSignBignum(pos, &check);
	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	sign = IsPlus(check)? 0: 1;
	ptr->sign = sign;
	ptr->carry = sign;
	ptr->data = data;
	ptr->temp = 0;
	ptr->v = sign? ~((bigtype)0): 0;
	ptr->size = size;
	ptr->index = 0;
	ptr->pos = pos;
}

static void boole_struct_integer(struct boole_struct *ptr, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			boole_struct_fixnum(ptr, pos);
			break;

		case LISPTYPE_BIGNUM:
			boole_struct_bignum(ptr, pos);
			break;

		default:
			TypeError(pos, INTEGER);
			return;
	}
}

static int boole_struct_sign(
		struct boole_struct *a,
		struct boole_struct *b,
		bigtype (*call)(bigtype, bigtype))
{
	bigtype v1 = a->sign? a->v: 0;
	bigtype v2 = b->sign? b->v: 0;
	return (*call)(v1, v2)? SignMinus: SignPlus;
}

static void boole_struct_alloc(LocalRoot local,
		struct boole_struct *a,
		struct boole_struct *b,
		struct boole_struct *c,
		bigtype (*call)(bigtype, bigtype))
{
	int sign;
	size_t size;
	addr pos;

	sign = boole_struct_sign(a, b, call);
	size = (a->size < b->size)? b->size: a->size;
	size++;
	bignum_local(local, &pos, sign, size);
	SetSizeBignum(pos, size);
	boole_struct_bignum(c, pos);
}

static bigtype boole_struct_get(struct boole_struct *ptr)
{
	bigtype v;

	/* out of range */
	if (ptr->size <= ptr->index)
		return ptr->v;
	/* plus */
	v = ptr->data[ptr->index++];
	if (ptr->sign == 0)
		return v;
	/* not carry */
	if (ptr->carry == 0)
		return ~v;
	/* carry over */
	if (v == 0)
		return 0;
	/* carry end */
	ptr->carry = 0;
	return (~v) + 1ULL;
}

static void boole_struct_set(struct boole_struct *ptr, bigtype v)
{
	Check(ptr->size <= ptr->index, "size error");
	ptr->data[ptr->index++] = v;
}

static void boole_struct_result(struct boole_struct *ptr)
{
	addr pos;
	bigtype v;
	size_t size, i;
	struct boole_struct boole1, boole2;

	pos = ptr->pos;
	if (ptr->sign) {
		boole_struct_bignum(&boole1, pos);
		boole_struct_bignum(&boole2, pos);
		size = ptr->size;
		for (i = 0; i < size; i++) {
			v = boole_struct_get(&boole1);
			boole_struct_set(&boole2, v);
		}
	}
	sizepress_bignum(pos);
}


/*
 *  bit operator
 */
static void boole_call2_common(
		struct boole_struct *boole1,
		struct boole_struct *boole2,
		struct boole_struct *boole3,
		bigtype (*call)(bigtype, bigtype))
{
	bigtype x, y;
	size_t i, size;

	size = boole3->size;
	for (i = 0; i < size; i++) {
		x = boole_struct_get(boole1);
		y = boole_struct_get(boole2);
		boole_struct_set(boole3, (*call)(x, y));
	}
}

static void boole_call_bignum(LocalRoot local, addr a, addr b, addr *ret,
		bigtype (*call)(bigtype, bigtype))
{
	struct boole_struct boole1, boole2, boole3;
	LocalStack stack;

	push_local(local, &stack);
	boole_struct_integer(&boole1, a);
	boole_struct_integer(&boole2, b);
	boole_struct_alloc(local, &boole1, &boole2, &boole3, call);
	boole_call2_common(&boole1, &boole2, &boole3, call);
	boole_struct_result(&boole3);
	bignum_result_heap(boole3.pos, ret);
	rollback_local(local, stack);
}

static void boole_call_fixnum(LocalRoot local, addr a, addr b, addr *ret,
		bigtype (*call)(bigtype, bigtype))
{
	union boole_fixnumfixed x, y;

	GetFixnum(a, &(x.sign));
	GetFixnum(b, &(y.sign));
	x.unsign = (*call)(x.unsign, y.unsign);
	fixnum_heap(ret, x.sign);
}

static int boole_fixnum_p(addr a, addr b)
{
	fixnum x, y;

	if ((! fixnump(a)) || (! fixnump(b))) return 0;
	GetFixnum(a, &x);
	GetFixnum(b, &y);

	return (x != FIXNUM_MIN) && (y != FIXNUM_MIN);
}

static void boole_call_common(LocalRoot local, addr a, addr b, addr *ret,
		bigtype (*call)(bigtype, bigtype))
{
	if (boole_fixnum_p(a, b))
		boole_call_fixnum(local, a, b, ret, call);
	else
		boole_call_bignum(local, a, b, ret, call);
}

static void logcall_common(LocalRoot local, addr args, addr *ret,
		fixnum ident, void (*call)(LocalRoot, addr, addr, addr *))
{
	addr left, right;

	/* no args */
	if (args == Nil) {
		fixnum_heap(ret, ident);
		return;
	}

	/* only one */
	getcons(args, &left, &args);
	if (args == Nil) {
		*ret = left;
		return;
	}

	/* list */
	while (args != Nil) {
		getcons(args, &right, &args);
		(*call)(local, left, right, &left);
	}
	*ret = left;
}


/*
 *  logand
 */
static bigtype boole_call_and(bigtype a, bigtype b)
{
	return a & b;
}

static void boole_and_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_and);
}

void logand_common(LocalRoot local, addr args, addr *ret)
{
	logcall_common(local, args, ret, -1, boole_and_common);
}


/*
 *  logandc1
 */
static bigtype boole_call_andc1(bigtype a, bigtype b)
{
	return (~a) & b;
}

void logandc1_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_andc1);
}

#define boole_andc1_common logandc1_common


/*
 *  logandc2
 */
static bigtype boole_call_andc2(bigtype a, bigtype b)
{
	return a & (~b);
}

void logandc2_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_andc2);
}

#define boole_andc2_common logandc2_common


/*
 *  logeqv
 */
static bigtype boole_call_eqv(bigtype a, bigtype b)
{
	return ~(a ^ b);
}

static void boole_eqv_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_eqv);
}

void logeqv_common(LocalRoot local, addr args, addr *ret)
{
	logcall_common(local, args, ret, -1, boole_eqv_common);
}


/*
 *  logior
 */
static bigtype boole_call_ior(bigtype a, bigtype b)
{
	return a | b;
}

static void boole_ior_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_ior);
}

void logior_common(LocalRoot local, addr args, addr *ret)
{
	logcall_common(local, args, ret, 0, boole_ior_common);
}


/*
 *  lognand
 */
static bigtype boole_call_nand(bigtype a, bigtype b)
{
	return ~(a & b);
}

void lognand_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_nand);
}

#define boole_nand_common lognand_common


/*
 *  lognor
 */
static bigtype boole_call_nor(bigtype a, bigtype b)
{
	return ~(a | b);
}

void lognor_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_nor);
}

#define boole_nor_common lognor_common


/*
 *  lognot
 */
void lognot_common(LocalRoot local, addr a, addr *ret)
{
	addr b;
	fixnum_heap(&b, 0);
	logorc1_common(local, a, b, ret);
}

static void boole_c1_common(LocalRoot local, addr a, addr b, addr *ret)
{
	lognot_common(local, a, ret);
}

static void boole_c2_common(LocalRoot local, addr a, addr b, addr *ret)
{
	lognot_common(local, b, ret);
}


/*
 *  logorc1
 */
static bigtype boole_call_orc1(bigtype a, bigtype b)
{
	return (~a) | b;
}

void logorc1_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_orc1);
}

#define boole_orc1_common logorc1_common


/*
 *  logorc2
 */
static bigtype boole_call_orc2(bigtype a, bigtype b)
{
	return a | (~b);
}

void logorc2_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_orc2);
}

#define boole_orc2_common logorc2_common


/*
 *  logxor
 */
static bigtype boole_call_xor(bigtype a, bigtype b)
{
	return a ^ b;
}

static void boole_xor_common(LocalRoot local, addr a, addr b, addr *ret)
{
	boole_call_common(local, a, b, ret, boole_call_xor);
}

void logxor_common(LocalRoot local, addr args, addr *ret)
{
	logcall_common(local, args, ret, 0, boole_xor_common);
}


/*
 *  boole
 */
static void boole_1_common(LocalRoot local, addr a, addr b, addr *ret)
{
	integer_throw_heap(a, ret);
}

static void boole_2_common(LocalRoot local, addr a, addr b, addr *ret)
{
	integer_throw_heap(b, ret);
}

static void boole_clr_common(LocalRoot local, addr a, addr b, addr *ret)
{
	fixnum_heap(ret, 0);
}

static void boole_set_common(LocalRoot local, addr a, addr b, addr *ret)
{
	fixnum_heap(ret, -1);
}

void boole_common(LocalRoot local, addr op, addr a, addr b, addr *ret)
{
	fixnum index;
	BooleCall call;

	CheckLocalType(local, op, LISPTYPE_FIXNUM);
	GetFixnum(op, &index);
	Check(index < 0 || ((fixnum)Boole_Size) <= index, "index error");
	call = BooleTable[(int)index];
	Check(call == NULL, "boole call error");
	(*call)(local, a, b, ret);
}


/*
 *  logbitp
 */
static int logbitp_fixnum(addr index, addr pos)
{
	union boole_fixnumfixed u;
	size_t size;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &(u.sign));
	if (getindex_integer(index, &size) || LISP_INTEGER_BIT <= size) {
		/* out of range */
		return u.sign < 0;
	}
	else {
		/* fixnum */
		return (int)((u.unsign >> size) & 1);
	}
}

static int logbitp_plus(addr pos, size_t size)
{
	size_t m, n;
	bigtype *data;

	n = size / BIGNUM_FULLBIT;
	m = size % BIGNUM_FULLBIT;
	GetDataBignum(pos, &data);
	return (int)((data[n] >> m) & 1);
}

static int logbitp_minus(addr pos, size_t size)
{
	size_t m, n, i;
	bigtype *data, v, carry;

	n = size / BIGNUM_FULLBIT;
	m = size % BIGNUM_FULLBIT;
	GetDataBignum(pos, &data);

	carry = 1;
	for (i = 0; i < n; i++) {
		if (data[i] != 0) {
			carry = 0;
			break;
		}
	}
	v = ~(data[n]) + carry;

	return (int)((v >> m) & 1);
}

static int logbitp_bignum(addr index, addr pos)
{
	int sign;
	size_t size, check;

	CheckType(pos, LISPTYPE_BIGNUM);
	/* range check */
	GetSignBignum(pos, &sign);
	GetSizeBignum(pos, &size);
	check = size * BIGNUM_FULLBIT;
	if (getindex_integer(index, &size) || check <= size)
		return IsPlus(sign)? 0: 1;
	/* sign */
	if (IsPlus(sign))
		return logbitp_plus(pos, size);
	else
		return logbitp_minus(pos, size);
}

int logbitp_common(addr index, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return logbitp_fixnum(index, pos);

		case LISPTYPE_BIGNUM:
			return logbitp_bignum(index, pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}


/*
 *  logcount
 */
static size_t logcount_bigtype(bigtype v, unsigned sign)
{
	size_t count;

	if (sign)
		v = ~v;
	for (count = 0; v; v >>= 1) {
		if (v & 1)
			count++;
	}

	return count;
}

static size_t logcount_fixnum(addr pos)
{
	union boole_fixnumfixed u;

	CheckType(pos, LISPTYPE_FIXNUM);
	GetFixnum(pos, &(u.sign));
	return logcount_bigtype(u.unsign, u.sign < 0);
}

static size_t logcount_bignum(addr pos)
{
	unsigned sign;
	bigtype v;
	size_t i, size, count;
	struct boole_struct boole;

	boole_struct_bignum(&boole, pos);
	sign = boole.sign;
	size = boole.size;
	count = 0;
	for (i = 0; i < size; i++) {
		v = boole_struct_get(&boole);
		count += logcount_bigtype(v, sign);
	}

	return count;
}

size_t logcount_common(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return logcount_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return logcount_bignum(pos);

		default:
			TypeError(pos, INTEGER);
			return 0;
	}
}


/*
 *  logtest
 */
int logtest_common(LocalRoot local, addr a, addr b)
{
	struct boole_struct boole1, boole2;
	bigtype x, y;
	size_t size, i;

	boole_struct_integer(&boole1, a);
	boole_struct_integer(&boole2, b);
	size = (boole1.size < boole2.size)? boole2.size: boole1.size;
	size++; /* sign check */
	for (i = 0; i < size; i++) {
		x = boole_struct_get(&boole1);
		y = boole_struct_get(&boole2);
		if (x & y)
			return 1;
	}

	return 0;
}


/*
 *  initialize
 */
void init_boole(void)
{
	BooleTable[Boole_1] = boole_1_common;
	BooleTable[Boole_2] = boole_2_common;
	BooleTable[Boole_And] = boole_and_common;
	BooleTable[Boole_AndC1] = boole_andc1_common;
	BooleTable[Boole_AndC2] = boole_andc2_common;
	BooleTable[Boole_C1] = boole_c1_common;
	BooleTable[Boole_C2] = boole_c2_common;
	BooleTable[Boole_Clr] = boole_clr_common;
	BooleTable[Boole_Eqv] = boole_eqv_common;
	BooleTable[Boole_Ior] = boole_ior_common;
	BooleTable[Boole_Nand] = boole_nand_common;
	BooleTable[Boole_Nor] = boole_nor_common;
	BooleTable[Boole_Orc1] = boole_orc1_common;
	BooleTable[Boole_Orc2] = boole_orc2_common;
	BooleTable[Boole_Set] = boole_set_common;
	BooleTable[Boole_Xor] = boole_xor_common;
}


/*
 *  deposit-field
 */
static size_t deposit_field_maxsize(
		struct boole_struct *ptr1,
		struct boole_struct *ptr2,
		struct bytespec_mask *mask)
{
	size_t size1, size2, size3;

	size1 = bytespec_mask_getsize(mask);
	size2 = ptr1->size;
	size3 = ptr2->size;
	size1 = (size1 < size2)? size2: size1;
	size1 = (size1 < size3)? size3: size1;

	return size1;
}

static void deposit_field_alloc(LocalRoot local,
		struct boole_struct *init,
		struct boole_struct *right,
		size_t size)
{
	int sign;
	addr pos;

	sign = right->sign? SignMinus: SignPlus;
	bignum_local(local, &pos, sign, size);
	SetSizeBignum(pos, size);
	boole_struct_bignum(init, pos);
}

static void deposit_field_copy(
		struct boole_struct *ptr1,
		struct boole_struct *ptr2,
		struct boole_struct *write,
		struct bytespec_mask *mask,
		size_t size)
{
	bigtype v1, v2, m1, m2;
	size_t i;

	for (i = 0; i < size; i++) {
		v1 = boole_struct_get(ptr1);
		v2 = boole_struct_get(ptr2);
		m1 = bytespec_mask_get(mask);
		m2 = ~m1;
		boole_struct_set(write, (v1 & m1) | (v2 & m2));
	}
}

void deposit_field_common(LocalRoot local, addr *ret, addr a, addr spec, addr b)
{
	struct boole_struct str1, str2, write;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0) {
		integer_throw_heap(b, ret);
		return;
	}
	boole_struct_integer(&str1, a);
	boole_struct_integer(&str2, b);
	size = deposit_field_maxsize(&str1, &str2, &mask);

	push_local(local, &stack);
	deposit_field_alloc(local, &write, &str2, size);
	deposit_field_copy(&str1, &str2, &write, &mask, size);
	boole_struct_result(&write);
	bignum_result_heap(write.pos, ret);
	rollback_local(local, stack);
}


/*
 *  dpb
 */
struct dpb_struct {
	struct boole_struct boole;
	size_t size, start, index, bit;
	bigtype carry;
};

static void dbp_struct_integer(struct dpb_struct *ptr, addr pos, size_t shift)
{
	size_t m, n, size;

	boole_struct_integer(&(ptr->boole), pos);
	n = shift / BIGNUM_FULLBIT;
	m = shift % BIGNUM_FULLBIT;
	size = m? n+1: n;
	size += ptr->boole.size;

	ptr->start = n;
	ptr->bit = m;
	ptr->size = size;
	ptr->carry = 0;
	ptr->index = 0;
}

static size_t dpb_maxsize(
		struct dpb_struct *ptr1,
		struct boole_struct *ptr2,
		struct bytespec_mask *mask)
{
	size_t size1, size2, size3;

	size1 = bytespec_mask_getsize(mask);
	size2 = ptr1->size;
	size3 = ptr2->size;
	size1 = (size1 < size2)? size2: size1;
	size1 = (size1 < size3)? size3: size1;

	return size1;
}

static bigtype dpb_struct_get(struct dpb_struct *ptr)
{
	bigtype v, carry;
	size_t index, bit;

	index = ptr->index++;
	if (index < ptr->start)
		return 0;
	bit = ptr->bit;
	v = boole_struct_get(&(ptr->boole));
	if (bit) {
		carry = ptr->carry;
		ptr->carry = v >> (BIGNUM_FULLBIT - bit);
		v = (v << bit) | carry;
	}

	return v;
}

static void dpb_alloc(LocalRoot local,
		struct boole_struct *init,
		struct boole_struct *right,
		size_t size)
{
	deposit_field_alloc(local, init, right, size);
}

static void dpb_copy(
		struct dpb_struct *ptr1,
		struct boole_struct *ptr2,
		struct boole_struct *write,
		struct bytespec_mask *mask,
		size_t size)
{
	bigtype v1, v2, m1, m2;
	size_t i;

	for (i = 0; i < size; i++) {
		v1 = dpb_struct_get(ptr1);
		v2 = boole_struct_get(ptr2);
		m1 = bytespec_mask_get(mask);
		m2 = ~m1;
		boole_struct_set(write, (v1 & m1) | (v2 & m2));
	}
}

void dpb_common(LocalRoot local, addr *ret, addr a, addr spec, addr b)
{
	struct dpb_struct str1;
	struct boole_struct str2, write;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0) {
		integer_throw_heap(b, ret);
		return;
	}
	dbp_struct_integer(&str1, a, mask.position);
	boole_struct_integer(&str2, b);
	size = dpb_maxsize(&str1, &str2, &mask);

	push_local(local, &stack);
	dpb_alloc(local, &write, &str2, size);
	dpb_copy(&str1, &str2, &write, &mask, size);
	boole_struct_result(&write);
	bignum_result_heap(write.pos, ret);
	rollback_local(local, stack);
}


/*
 *  ldb
 */
struct ldb_struct {
	struct boole_struct boole;
	size_t size, count, shift, start;
	bigtype carry;
};

static void boole_struct_front(struct boole_struct *ptr, size_t front)
{
	bigtype *data;

	/* out of range */
	if (ptr->size <= ptr->index) {
		return;
	}
	if (ptr->size <= ptr->index + front) {
		ptr->index += front;
		return;
	}
	/* plus */
	if (ptr->sign == 0) {
		ptr->index += front;
		return;
	}
	/* not carry */
	if (ptr->carry == 0) {
		ptr->index += front;
		return;
	}
	/* carry */
	data = ptr->data;
	while (front) {
		front--;
		if (data[ptr->index++]) {
			ptr->carry = 0;
			break;
		}
		if (ptr->size <= ptr->index) {
			return;
		}
	}
	/* carry end */
	ptr->index += front;
}

static void ldb_struct_integer(struct ldb_struct *ptr, addr var, addr spec)
{
	size_t size, pos, shift, div;
	struct bytespec_struct *str;
	struct boole_struct *boole;

	/* bytespec */
	str = ByteSpecStruct(spec);
	size = str->size;
	pos = str->position;

	/* size == 0 */
	if (size == 0) {
		ptr->count = 0;
		return;
	}

	/* size */
	div = size / BIGNUM_FULLBIT;
	shift = size % BIGNUM_FULLBIT;
	ptr->count = shift? (div + 1): div;

	/* start */
	div = pos / BIGNUM_FULLBIT;
	shift = pos % BIGNUM_FULLBIT;

	/* boole */
	boole = &(ptr->boole);
	boole_struct_integer(boole, var);
	boole_struct_front(boole, div);

	/* struct */
	ptr->carry = shift? boole_struct_get(boole): 0;
	ptr->shift = shift;
	ptr->size = size;
}

static void ldb_struct_alloc(LocalRoot local, size_t count, addr *ret)
{
	addr pos;

	bignum_local(local, &pos, SignPlus, count);
	SetSizeBignum(pos, count);
	*ret = pos;
}

static bigtype ldb_struct_get(struct ldb_struct *ptr)
{
	bigtype v1, v2, v;
	size_t shift;

	v1 = ptr->carry;
	v2 = boole_struct_get(&(ptr->boole));
	shift = ptr->shift;
	if (shift == 0)
		return v2;
	v = (v2 << (BIGNUM_FULLBIT - shift)) | (v1 >> shift);
	ptr->carry = v2;

	return v;
}

void ldb_common(LocalRoot local, addr *ret, addr spec, addr pos)
{
	LocalStack stack;
	struct ldb_struct str;
	size_t size, count, i;
	bigtype *data, v, m;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	ldb_struct_integer(&str, pos, spec);
	count = str.count;
	if (count == 0) {
		fixnum_heap(ret, 0);
		return;
	}

	push_local(local, &stack);
	ldb_struct_alloc(local, count, &pos);
	GetDataBignum(pos, &data);
	size = str.size;
	for (i = 0; i < count; i++) {
		if (size == 0) {
			data[i] = 0;
		}
		else if (BIGNUM_FULLBIT <= size) {
			data[i] = ldb_struct_get(&str);
			size -= BIGNUM_FULLBIT;
		}
		else {
			v = ldb_struct_get(&str);
			m = (1ULL << size) - 1ULL;
			data[i] = v & m;
			size = 0;
		}
	}
	sizepress_bignum(pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}


/*
 *  (define-setf-expander ldb (spec place &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion place env)
 *      (let ((v (gensym))
 *            (g (car g)))
 *        (values `(,g ,@a)
 *                `(nil ,@b)
 *                `(,v)
 *                `(prog1 ,v
 *                        (setq ,g (dpb ,v ,spec ,r))
 *                        ,w)
 *                r))))
 */
void setf_ldb(Execute ptr, addr form, addr env)
{
	addr args, spec, place, ra, rb, rg, rw, rr, a, b, g, w, r, v;
	addr prog1, setq, dpb;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &spec, &args);
	if (! consp(form)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	GetConst(COMMON_PROG1, &prog1);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_DPB, &dpb);
	make_gensym(ptr, &v);
	getcar(g, &g);
	cons_heap(&ra, g, a);
	cons_heap(&rb, Nil, b);
	conscar_heap(&rg, v);
	list_heap(&dpb, dpb, v, spec, r, NULL);
	list_heap(&setq, setq, g, dpb, NULL);
	list_heap(&rw, prog1, v, setq, w, NULL);
	rr = r;
	setvalues_control(ptr, ra, rb, rg, rw, rr, NULL);
	return;

error:
	fmte("SETF-LDB form ~S must be a (byte-space place) form.", form, NULL);
}


/*
 *  ldb-test
 */
int ldb_test_common(addr spec, addr pos)
{
	struct ldb_struct str;
	size_t size, count, i;
	bigtype v, m;

	CheckType(spec, LISPTYPE_BYTESPEC);
	ldb_struct_integer(&str, pos, spec);
	count = str.count;
	if (count == 0)
		return 0;

	size = str.size;
	for (i = 0; i < count; i++) {
		if (size == 0) {
			break;
		}
		else if (BIGNUM_FULLBIT <= size) {
			if (ldb_struct_get(&str))
				return 1;
			size -= BIGNUM_FULLBIT;
		}
		else {
			v = ldb_struct_get(&str);
			m = (1ULL << size) - 1ULL;
			if (v & m)
				return 1;
			size = 0;
		}
	}

	return 0;
}


/*
 *  mask-field
 */
static size_t mask_field_maxsize(
		struct boole_struct *ptr,
		struct bytespec_mask *mask)
{
	size_t size1 = bytespec_mask_getsize(mask);
	size_t size2 = ptr->size;
	return (size1 < size2)? size2: size1;
}

static void mask_field_struct_alloc(LocalRoot local, size_t count, addr *ret)
{
	ldb_struct_alloc(local, count, ret);
}

static void mask_field_copy(
		struct boole_struct *ptr,
		struct bytespec_mask *mask,
		addr pos,
		size_t size)
{
	bigtype v, m;
	bigtype *data;
	size_t i;

	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		v = boole_struct_get(ptr);
		m = bytespec_mask_get(mask);
		data[i] = v & m;
	}
}

void mask_field_common(LocalRoot local, addr *ret, addr spec, addr pos)
{
	struct boole_struct str;
	struct bytespec_mask mask;
	size_t size;
	LocalStack stack;

	CheckLocalType(local, spec, LISPTYPE_BYTESPEC);
	bytespec_mask_init(&mask, spec);
	if (mask.size == 0) {
		integer_throw_heap(pos, ret);
		return;
	}
	boole_struct_integer(&str, pos);
	size = mask_field_maxsize(&str, &mask);

	push_local(local, &stack);
	mask_field_struct_alloc(local, size, &pos);
	mask_field_copy(&str, &mask, pos, size);
	sizepress_bignum(pos);
	bignum_result_heap(pos, ret);
	rollback_local(local, stack);
}


/*
 *  (define-setf-expander mask-field (spec place &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion place env)
 *      (let ((v (gensym))
 *            (g (car g)))
 *        (values `(,g ,@a)
 *                `(nil ,@b)
 *                `(,v)
 *                `(prog1 ,v
 *                        (setq ,g (deposit-field ,v ,spec ,r))
 *                        ,w)
 *                r))))
 */
void setf_mask_field(Execute ptr, addr form, addr env)
{
	addr args, spec, place, ra, rb, rg, rw, rr, a, b, g, w, r, v;
	addr prog1, setq, deposit;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &spec, &args);
	if (! consp(form)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	GetConst(COMMON_PROG1, &prog1);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_DEPOSIT_FIELD, &deposit);
	make_gensym(ptr, &v);
	getcar(g, &g);
	cons_heap(&ra, g, a);
	cons_heap(&rb, Nil, b);
	conscar_heap(&rg, v);
	list_heap(&deposit, deposit, v, spec, r, NULL);
	list_heap(&setq, setq, g, deposit, NULL);
	list_heap(&rw, prog1, v, setq, w, NULL);
	rr = r;
	setvalues_control(ptr, ra, rb, rg, rw, rr, NULL);
	return;

error:
	fmte("SETF-DEPOSIT-FIELD form ~S must be a (byte-space place) form.", form, NULL);
}

