#ifndef __BIGNUM_HEADER__
#define __BIGNUM_HEADER__

#include "build.h"
#include "typedef.h"
#include "typedef_integer.h"

struct bignuminfo {
	size_t alloc, size;
};

#define signplus_bignum     0
#define signminus_bignum    1

#define IsIntegerFloat(x) (((x) - (truncf(x))) == 0.0f)
#define IsIntegerDouble(x) (((x) - (trunc(x))) == 0.0)
#define IsIntegerLongFloat(x) (((x) - (truncl(x))) == 0.0L)

#ifdef BIGNUM_TYPE_64BIT
#define alloc_bigdata(m,p,s)    \
	alloc_body8((m), (p), LISPSYSTEM_BIGDATA, sizeofm(bigtype, (s)))
#define PtrDataBignum(pos)      ((bigtype *)PtrBodyB8(pos))
#endif

#ifdef BIGNUM_TYPE_32BIT
#define alloc_bigdata(m,p,s)    \
	alloc_body4((m), (p), LISPSYSTEM_BIGDATA, sizeofm(bigtype, (s)))
#define PtrDataBignum(pos)  ((bigtype *)PtrBodyB4(pos))
#endif

#define GetDataBignum(pos, data) { \
	addr __root; \
	GetRootBignum(pos, &__root); \
	*(data) = PtrDataBignum(__root); \
}
#define GetRootDataBignum(pos, root, data) { \
	GetRootBignum(pos, root); \
	*(data) = PtrDataBignum(*root); \
}

#define StructBignum_Low(p) ((struct bignuminfo *)PtrBodySSa((p), 1))
#define RefAllocBignum_Low(p) (StructBignum(p)->alloc)
#define SetSizeBignum_Low(p,v) (StructBignum(p)->size = (v))
#define GetSizeBignum_Low(p,v) (*(v) = StructBignum(p)->size)
#define RefSizeBignum_Low(p)   (StructBignum(p)->size)
#define SetRootBignum_Low(p,v) SetArraySS((p),0,(v))
#define GetRootBignum_Low(p,v) GetArraySS((p),0,(v))
#define SetSignBignum_Low(p,v) SetUser((p), (byte)(v))
#define GetSignBignum_Low(p,v) (*(v) = (int)GetUser(p))
#define RefSignBignum_Low(p) ((int)GetUser(p))

#ifdef LISP_DEBUG
#define StructBignum struct_bignum
#define RefAllocBignum refalloc_bignum
#define SetSizeBignum setsize_bignum
#define GetSizeBignum getsize_bignum
#define RefSizeBignum refsize_bignum
#define SetRootBignum setroot_bignum
#define GetRootBignum getroot_bignum
#define SetSignBignum setsign_bignum
#define GetSignBignum getsign_bignum
#define RefSignBignum refsign_bignum
#else
#define StructBignum StructBignum_Low
#define RefAllocBignum RefAllocBignum_Low
#define SetSizeBignum SetSizeBignum_Low
#define GetSizeBignum GetSizeBignum_Low
#define RefSizeBignum RefSizeBignum_Low
#define SetRootBignum SetRootBignum_Low
#define GetRootBignum GetRootBignum_Low
#define SetSignBignum SetSignBignum_Low
#define GetSignBignum GetSignBignum_Low
#define RefSignBignum RefSignBignum_Low
#endif

int fixnump(addr pos);
int bignump(addr pos);
struct bignuminfo *struct_bignum(addr pos);
size_t refalloc_bignum(addr pos);
void setsize_bignum(addr pos, size_t value);
void getsize_bignum(addr pos, size_t *ret);
size_t refsize_bignum(addr pos);
void setroot_bignum(addr pos, addr value);
void getroot_bignum(addr pos, addr *ret);
void setsign_bignum(addr pos, int sign);
void getsign_bignum(addr pos, int *ret);
int refsign_bignum(addr pos);

void alloc_bignum(LocalRoot local, addr *ret, size_t alloc);
void alloc_plus_bignum(LocalRoot local, addr *ret, size_t a, size_t b);
void realloc_bignum(LocalRoot local, addr pos, size_t alloc, int force);

void bignum_alloc(LocalRoot local, addr *ret, int sign, size_t size);
void bignum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
void bignum_copy_nosign_alloc(LocalRoot local, addr *ret, addr right);
void bignum_copy_alloc(LocalRoot local, addr *ret, addr right);
void bignum_value_alloc(LocalRoot local, addr *ret, int sign, fixed value);
void bignum_value2_alloc(LocalRoot local, addr *ret, int sign, fixed high, fixed low);
void bignum_zero_alloc(LocalRoot local, addr *ret);
void bignum_fixnum_alloc(LocalRoot local, addr *ret, addr value);
void bignum_fixnum_value_alloc(LocalRoot local, addr *ret, fixnum value);
void bignum_counter_alloc(LocalRoot local, addr *ret, addr index);
void bignum_result_alloc(LocalRoot local, addr pos, addr *ret);
void bignum_integer_alloc(LocalRoot local, addr *ret, addr pos);
#define bignum_heap(r,a,b) bignum_alloc(NULL,(r),(a),(b))
#define bignum_cons_heap(r,a,b) bignum_cons_alloc(NULL,(r),(a),(b))
#define bignum_copy_nosign_heap(r,a) bignum_copy_nosign_alloc(NULL,(r),(a))
#define bignum_copy_heap(r,a) bignum_copy_alloc(NULL,(r),(a))
#define bignum_value_heap(r,s,v) bignum_value_alloc(NULL,(r),(s),(v))
#define bignum_value2_heap(r,a,b,c) bignum_value2_alloc(NULL,(r),(a),(b),(c))
#define bignum_zero_heap(r) bignum_zero_alloc(NULL,(r))
#define bignum_fixnum_heap(r,v) bignum_fixnum_alloc(NULL,(r),(v))
#define bignum_fixnum_value_heap(r,v) bignum_fixnum_value_alloc(NULL,(r),(v))
#define bignum_counter_heap(r,a) bignum_counter_alloc(NULL,(r),(a))
#define bignum_result_heap(p,r) bignum_result_alloc(NULL,(p),(r))
#define bignum_integer_heap(p,r) bignum_integer_alloc(NULL,(p),(r))
#ifdef LISP_DEBUG
void bignum_debug(LocalRoot local, addr *ret, int sign, size_t size);
void bignum_cons_debug(LocalRoot local, addr *ret, int sign, addr cons);
void bignum_copy_nosign_debug(LocalRoot local, addr *ret, addr right);
void bignum_copy_debug(LocalRoot local, addr *ret, addr right);
void bignum_value_debug(LocalRoot local, addr *ret, int sign, fixed value);
void bignum_value2_debug(LocalRoot local, addr *ret, int sign, fixed high, fixed low);
void bignum_zero_debug(LocalRoot local, addr *ret);
void bignum_fixnum_debug(LocalRoot local, addr *ret, addr value);
void bignum_fixnum_value_debug(LocalRoot local, addr *ret, fixnum value);
void bignum_counter_debug(LocalRoot local, addr *ret, addr index);
void bignum_result_debug(LocalRoot local, addr pos, addr *ret);
void bignum_integer_debug(LocalRoot local, addr *ret, addr pos);
#define bignum_local bignum_debug
#define bignum_cons_local bignum_cons_debug
#define bignum_copy_nosign_local bignum_copy_nosign_debug
#define bignum_copy_local bignum_copy_debug
#define bignum_value_local bignum_value_debug
#define bignum_value2_local bignum_value2_debug
#define bignum_zero_local bignum_zero_debug
#define bignum_fixnum_local bignum_fixnum_debug
#define bignum_fixnum_value_local bignum_fixnum_value_debug
#define bignum_counter_local bignum_counter_debug
#define bignum_result_local bignum_result_debug
#define bignum_integer_local bignum_integer_debug
#else
#define bignum_local bignum_alloc
#define bignum_cons_local bignum_cons_alloc
#define bignum_copy_nosign_local bignum_copy_nosign_alloc
#define bignum_copy_local bignum_copy_alloc
#define bignum_value_local bignum_value_alloc
#define bignum_value2_local bignum_value2_alloc
#define bignum_zero_local bignum_zero_alloc
#define bignum_fixnum_local bignum_fixnum_alloc
#define bignum_fixnum_value_local bignum_fixnum_value_alloc
#define bignum_counter_local bignum_counter_alloc
#define bignum_result_local bignum_result_alloc
#define bignum_integer_local bignum_integer_alloc
#endif

#define fixnum_result_alloc fixnum_throw_alloc
#define fixnum_result_local fixnum_throw_local
#define fixnum_result_heap fixnum_throw_heap

void getfixed_bignum(addr pos, size_t index, fixed *value);
fixed reffixed_bignum(addr pos, size_t index);
void setfixed_bignum(addr pos, size_t index, fixed value);
void sizepress_bignum(addr left);
void copy_bignum(LocalRoot local, addr left, addr right, int force);
void copy_noexpand_bignum(addr left, addr right);
void setvalue_bignum(addr left, int sign, bigtype value);
void setzero_bignum(addr left);
int getbit_bignum(addr pos, size_t index);
void incf_bignum(addr pos, bigtype value);
void decf_bignum(addr pos, bigtype value);

void bignum_throw_heap(addr pos, addr *ret);
void bignum_throw_local(LocalRoot local, addr pos, addr *ret);
void bignum_throw_alloc(LocalRoot local, addr pos, addr *ret);
void fixnum_throw_heap(addr pos, addr *ret);
void fixnum_throw_local(LocalRoot local, addr pos, addr *ret);
void fixnum_throw_alloc(LocalRoot local, addr pos, addr *ret);

void power2_bignum_alloc(LocalRoot local, addr *ret, int sign, size_t value);
void power2_bignum_local(LocalRoot local, addr *ret, int sign, size_t value);
void power2_bignum_heap(addr *ret, int sign, size_t value);
void shiftup_bignum_alloc(LocalRoot local, addr *ret, addr left, size_t value);
void shiftup_bignum_local(LocalRoot local, addr *ret, addr left, size_t value);
void shiftup_bignum_heap(addr *ret, addr left, size_t value);

int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
int fixnum_cons_local(LocalRoot local, addr *ret, int sign, addr cons);
int fixnum_cons_heap(addr *ret, int sign, addr cons);
void integer_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
void integer_cons_local(LocalRoot local, addr *ret, int sign, addr cons);
void integer_cons_heap(addr *ret, int sign, addr cons);
void integer_fixed_alloc(LocalRoot local, addr *ret, int sign, fixed value);
void integer_fixed_local(LocalRoot local, addr *ret, int sign, fixed value);
void integer_fixed_heap(addr *ret, int sign, fixed value);
void integer_bignum_alloc(LocalRoot local, addr *ret, addr pos);
void integer_bignum_local(LocalRoot local, addr *ret, addr pos);
void integer_bignum_heap(addr *ret, addr pos);

void integer_copy_alloc(LocalRoot local, addr pos, addr *ret);
void integer_copy_local(LocalRoot local, addr pos, addr *ret);
void integer_copy_heap(addr pos, addr *ret);
void integer_copysign_alloc(LocalRoot local, int sign, addr pos, addr *ret);
void integer_copysign_local(LocalRoot local, int sign, addr pos, addr *ret);
void integer_copysign_heap(int sign, addr pos, addr *ret);

single_float single_float_fixnum(addr pos);
double_float double_float_fixnum(addr pos);
long_float long_float_fixnum(addr pos);
single_float single_float_bignum(addr pos);
double_float double_float_bignum(addr pos);
long_float long_float_bignum(addr pos);

void single_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
void single_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
void single_float_fixnum_heap(addr *ret, addr pos);
void double_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
void double_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
void double_float_fixnum_heap(addr *ret, addr pos);
void long_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
void long_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
void long_float_fixnum_heap(addr *ret, addr pos);
void single_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
void single_float_bignum_local(LocalRoot local, addr *ret, addr pos);
void single_float_bignum_heap(addr *ret, addr pos);
void double_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
void double_float_bignum_local(LocalRoot local, addr *ret, addr pos);
void double_float_bignum_heap(addr *ret, addr pos);
void long_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
void long_float_bignum_local(LocalRoot local, addr *ret, addr pos);
void long_float_bignum_heap(addr *ret, addr pos);


/*
 *  compare
 */
int zerop_or_plusp_bignum(addr pos);
int plusp_bignum(addr pos);
int minusp_bignum(addr pos);
int zerop_bignum(addr pos);
int evenp_bignum(addr pos);
int oddp_bignum(addr pos);
void castfixed(fixnum value, int *sign, fixed *result);
void castfixed_fixnum(addr pos, int *sign, fixed *result);
int castfixed_integer(addr value, int *sign, fixed *result);
#define castbigtype castfixed
#define castbigtype_integer castfixed_integer
int bignum_single_float_alloc(LocalRoot local,
		addr *ret, single_float value, int isheap);
#define bignum_single_float_heap(m,r,v) bignum_single_float_alloc((m),(r),(v),1)
#define bignum_single_float_local(m,r,v) bignum_single_float_alloc((m),(r),(v),0)
int bignum_double_float_alloc(LocalRoot local,
		addr *ret, double_float value, int isheap);
#define bignum_double_float_heap(m,r,v) bignum_double_float_alloc((m),(r),(v),1)
#define bignum_double_float_local(m,r,v) bignum_double_float_alloc((m),(r),(v),0)
int bignum_long_float_alloc(LocalRoot local,
		addr *ret, long_float value, int isheap);
#define bignum_long_float_heap(m,r,v) bignum_long_float_alloc((m),(r),(v),1)
#define bignum_long_float_local(m,r,v) bignum_long_float_alloc((m),(r),(v),0)

int equal_bb_real(addr left, addr right);
int equal_nosign_bignum(addr left, addr right);
int equal_fb_real(addr left, addr right);
#define equal_bf_real(a,b) equal_fb_real((b), (a))
int equal_value_nosign_bignum(addr left, bigtype value);
int equal_value_bignum(addr left, int sign1, bigtype value);
int equal_value2_nosign_bignum(addr left, bigtype high, bigtype low);
int equal_value2_bignum(addr left, int sign1, bigtype high, bigtype low);

int compare_value_bignum(fixnum left, addr right);
int compare_bignum_value(addr value, fixnum right);
int compare_fb_real(addr left, addr right);
int compare_bf_real(addr left, addr right);
int compare_bb_real(addr left, addr right);
int compare_bs_real(addr left, addr right);
int compare_bd_real(addr left, addr right);
int compare_bl_real(addr left, addr right);
int compare_sb_real(addr left, addr right);
int compare_db_real(addr left, addr right);
int compare_lb_real(addr left, addr right);

int fixnum_unsigned_byte_p(addr value, size_t size);
int bignum_unsigned_byte_p(addr value, size_t size);
int fixnum_signed_byte_p(addr value, size_t size);
int bignum_signed_byte_p(addr value, size_t size);

int getfixnum_bignum(addr pos, fixnum *ret);
int getfixnumtype(addr pos, fixnum *ret);
int getfixed1_bignum(addr pos, int *sign, fixed *ret);

/*
 *  bignum calculation
 */
void plus_fv_bignum_local(LocalRoot local, addr left, fixnum value2, addr *ret);
void plus_fv_real_local(LocalRoot local, addr left, fixnum value2, addr *ret);
void plus_fv_real_common(addr left, fixnum value2, addr *ret);
void plus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_ff_real_common(addr left, addr right, addr *ret);
void plus_bv_bignum_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bv_real_local(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bv_real_common(LocalRoot local, addr left, fixnum right, addr *ret);
void plus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define plus_fb_bignum_local(m,a,b,r) plus_bf_bignum_local((m),(b),(a),(r))
#define plus_fb_real_local(m,a,b,r) plus_bf_real_local((m),(b),(a),(r))
#define plus_fb_real_common(m,a,b,r) plus_bf_real_common((m),(b),(a),(r))
void plus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void plus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

void sigrev_bignum_inplace(addr pos);
void sigrev_fixnum_bignum_local(LocalRoot local, addr left, addr *ret);
void sigrev_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
void sigrev_fixnum_integer_common(addr left, addr *ret);
void sigrev_bignum_bignum_local(LocalRoot local, addr left, addr *ret);
void sigrev_bignum_integer_local(LocalRoot local, addr left, addr *ret);
void sigrev_bignum_integer_common(addr left, addr *ret);

void minus_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_ff_real_common(addr left, addr right, addr *ret);
void minus_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_fb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void minus_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);

void multi_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_ff_real_common(addr left, addr right, addr *ret);
void multi_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_fb_bignum_local(a,b,c,d) multi_bf_bignum_local((a),(c),(b),(d))
#define multi_fb_real_local(a,b,c,d) multi_bf_real_local((a),(c),(b),(d))
#define multi_fb_real_common(a,b,c,d) multi_bf_real_common((a),(c),(b),(d))
void multi_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);
void multi_bb_nosign_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bb_nosign_real_local(LocalRoot local, addr left, addr right, addr *ret);
void multi_bb_nosign_real_common(LocalRoot local, addr left, addr right, addr *ret);

/* math */
void abs_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
void abs_fixnum_integer_common(addr left, addr *ret);
void abs_bignum_integer_local(LocalRoot local, addr left, addr *ret);
void abs_bignum_integer_common(addr left, addr *ret);

/* output */
void decimal_charqueue_fixnum_local(LocalRoot local, addr pos, addr queue);
void decimal_charqueue_bignum_local(LocalRoot local, addr pos, addr queue);
void decimal_charqueue_integer_local(LocalRoot local, addr pos, addr queue);

void output_nosign_fixnum(LocalRoot local, addr stream,
		fixnum value, unsigned base, int upperp);
void output_nosign_bignum(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
void output_nosign_comma_fixnum(LocalRoot local, addr stream,
		fixnum value, unsigned base, int upperp, size_t range, unicode comma);
void output_nosign_comma_bignum(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);

#endif

