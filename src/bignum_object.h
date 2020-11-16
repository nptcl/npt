#ifndef __BIGNUM_OBJECT_HEADER__
#define __BIGNUM_OBJECT_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

#define bignump _n(bignump)
#define struct_bignum _n(struct_bignum)
#define refalloc_bignum _n(refalloc_bignum)
#define setsize_bignum _n(setsize_bignum)
#define getsize_bignum _n(getsize_bignum)
#define refsize_bignum _n(refsize_bignum)
#define setroot_bignum _n(setroot_bignum)
#define getroot_bignum _n(getroot_bignum)
#define setsign_bignum _n(setsign_bignum)
#define getsign_bignum _n(getsign_bignum)
#define refsign_bignum _n(refsign_bignum)
#define alloc_bignum _n(alloc_bignum)
#define alloc_plus_bignum _n(alloc_plus_bignum)
#define realloc_bignum _n(realloc_bignum)
#define bignum_alloc _n(bignum_alloc)
#define bignum_cons_alloc _n(bignum_cons_alloc)
#define bignum_copy_nosign_alloc _n(bignum_copy_nosign_alloc)
#define bignum_copy_alloc _n(bignum_copy_alloc)
#define bignum_value_alloc _n(bignum_value_alloc)
#define bignum_value2_alloc _n(bignum_value2_alloc)
#define bignum_zero_alloc _n(bignum_zero_alloc)
#define bignum_fixnum_alloc _n(bignum_fixnum_alloc)
#define bignum_fixnum_value_alloc _n(bignum_fixnum_value_alloc)
#define bignum_counter_alloc_ _n(bignum_counter_alloc_)
#define bignum_result_alloc _n(bignum_result_alloc)
#define bignum_integer_alloc_ _n(bignum_integer_alloc_)
#define bignum_debug _n(bignum_debug)
#define bignum_cons_debug _n(bignum_cons_debug)
#define bignum_copy_nosign_debug _n(bignum_copy_nosign_debug)
#define bignum_copy_debug _n(bignum_copy_debug)
#define bignum_value_debug _n(bignum_value_debug)
#define bignum_value2_debug _n(bignum_value2_debug)
#define bignum_zero_debug _n(bignum_zero_debug)
#define bignum_fixnum_debug _n(bignum_fixnum_debug)
#define bignum_fixnum_value_debug _n(bignum_fixnum_value_debug)
#define bignum_counter_debug_ _n(bignum_counter_debug_)
#define bignum_result_debug _n(bignum_result_debug)
#define bignum_integer_debug_ _n(bignum_integer_debug_)
#define getfixed_bignum _n(getfixed_bignum)
#define reffixed_bignum _n(reffixed_bignum)
#define setfixed_bignum _n(setfixed_bignum)
#define diet_bignum _n(diet_bignum)
#define sizepress_bignum _n(sizepress_bignum)
#define copy_bignum _n(copy_bignum)
#define copy_noexpand_bignum _n(copy_noexpand_bignum)
#define setvalue_bignum _n(setvalue_bignum)
#define setzero_bignum _n(setzero_bignum)
#define getbit_bignum _n(getbit_bignum)
#define incf_bignum _n(incf_bignum)
#define decf_bignum _n(decf_bignum)
#define bignum_throw_heap _n(bignum_throw_heap)
#define bignum_throw_local _n(bignum_throw_local)
#define bignum_throw_alloc _n(bignum_throw_alloc)
#define fixnum_throw_heap _n(fixnum_throw_heap)
#define fixnum_throw_local _n(fixnum_throw_local)
#define fixnum_throw_alloc _n(fixnum_throw_alloc)
#define castfixed _n(castfixed)
#define castfixed_fixnum _n(castfixed_fixnum)
#define castfixed_integer _n(castfixed_integer)

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

#define bignum_local bignum_debug
#define bignum_cons_local bignum_cons_debug
#define bignum_copy_nosign_local bignum_copy_nosign_debug
#define bignum_copy_local bignum_copy_debug
#define bignum_value_local bignum_value_debug
#define bignum_value2_local bignum_value2_debug
#define bignum_zero_local bignum_zero_debug
#define bignum_fixnum_local bignum_fixnum_debug
#define bignum_fixnum_value_local bignum_fixnum_value_debug
#define bignum_counter_local_ bignum_counter_debug_
#define bignum_result_local bignum_result_debug
#define bignum_integer_local_ bignum_integer_debug_
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

#define bignum_local bignum_alloc
#define bignum_cons_local bignum_cons_alloc
#define bignum_copy_nosign_local bignum_copy_nosign_alloc
#define bignum_copy_local bignum_copy_alloc
#define bignum_value_local bignum_value_alloc
#define bignum_value2_local bignum_value2_alloc
#define bignum_zero_local bignum_zero_alloc
#define bignum_fixnum_local bignum_fixnum_alloc
#define bignum_fixnum_value_local bignum_fixnum_value_alloc
#define bignum_counter_local_ bignum_counter_alloc_
#define bignum_result_local bignum_result_alloc
#define bignum_integer_local_ bignum_integer_alloc_
#endif

#define fixnum_result_alloc fixnum_throw_alloc
#define fixnum_result_local fixnum_throw_local
#define fixnum_result_heap fixnum_throw_heap

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
int bignum_counter_alloc_(LocalRoot local, addr *ret, addr index);
void bignum_result_alloc(LocalRoot local, addr pos, addr *ret);
int bignum_integer_alloc_(LocalRoot local, addr *ret, addr pos);
#define bignum_heap(r,a,b) bignum_alloc(NULL,(r),(a),(b))
#define bignum_cons_heap(r,a,b) bignum_cons_alloc(NULL,(r),(a),(b))
#define bignum_copy_nosign_heap(r,a) bignum_copy_nosign_alloc(NULL,(r),(a))
#define bignum_copy_heap(r,a) bignum_copy_alloc(NULL,(r),(a))
#define bignum_value_heap(r,s,v) bignum_value_alloc(NULL,(r),(s),(v))
#define bignum_value2_heap(r,a,b,c) bignum_value2_alloc(NULL,(r),(a),(b),(c))
#define bignum_zero_heap(r) bignum_zero_alloc(NULL,(r))
#define bignum_fixnum_heap(r,v) bignum_fixnum_alloc(NULL,(r),(v))
#define bignum_fixnum_value_heap(r,v) bignum_fixnum_value_alloc(NULL,(r),(v))
#define bignum_counter_heap_(r,a) bignum_counter_alloc_(NULL,(r),(a))
#define bignum_result_heap(p,r) bignum_result_alloc(NULL,(p),(r))
#define bignum_integer_heap_(p,r) bignum_integer_alloc_(NULL,(p),(r))

void bignum_debug(LocalRoot local, addr *ret, int sign, size_t size);
void bignum_cons_debug(LocalRoot local, addr *ret, int sign, addr cons);
void bignum_copy_nosign_debug(LocalRoot local, addr *ret, addr right);
void bignum_copy_debug(LocalRoot local, addr *ret, addr right);
void bignum_value_debug(LocalRoot local, addr *ret, int sign, fixed value);
void bignum_value2_debug(LocalRoot local, addr *ret, int sign, fixed high, fixed low);
void bignum_zero_debug(LocalRoot local, addr *ret);
void bignum_fixnum_debug(LocalRoot local, addr *ret, addr value);
void bignum_fixnum_value_debug(LocalRoot local, addr *ret, fixnum value);
int bignum_counter_debug_(LocalRoot local, addr *ret, addr index);
void bignum_result_debug(LocalRoot local, addr pos, addr *ret);
int bignum_integer_debug_(LocalRoot local, addr *ret, addr pos);

void getfixed_bignum(addr pos, size_t index, fixed *value);
fixed reffixed_bignum(addr pos, size_t index);
void setfixed_bignum(addr pos, size_t index, fixed value);
void diet_bignum(LocalRoot local, addr pos);
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

void castfixed(fixnum value, int *sign, fixed *result);
void castfixed_fixnum(addr pos, int *sign, fixed *result);
int castfixed_integer(addr value, int *sign, fixed *result);
#define castbigtype castfixed
#define castbigtype_integer castfixed_integer

#endif

