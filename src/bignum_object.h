#ifndef __BIGNUM_OBJECT_HEADER__
#define __BIGNUM_OBJECT_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

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
#define bignum_integer_local bignum_integer_debug
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
#define bignum_integer_local bignum_integer_alloc
#endif

#define fixnum_result_alloc fixnum_throw_alloc
#define fixnum_result_local fixnum_throw_local
#define fixnum_result_heap fixnum_throw_heap

_g int fixnump(addr pos);
_g int bignump(addr pos);
_g struct bignuminfo *struct_bignum(addr pos);
_g size_t refalloc_bignum(addr pos);
_g void setsize_bignum(addr pos, size_t value);
_g void getsize_bignum(addr pos, size_t *ret);
_g size_t refsize_bignum(addr pos);
_g void setroot_bignum(addr pos, addr value);
_g void getroot_bignum(addr pos, addr *ret);
_g void setsign_bignum(addr pos, int sign);
_g void getsign_bignum(addr pos, int *ret);
_g int refsign_bignum(addr pos);

_g void alloc_bignum(LocalRoot local, addr *ret, size_t alloc);
_g void alloc_plus_bignum(LocalRoot local, addr *ret, size_t a, size_t b);
_g void realloc_bignum(LocalRoot local, addr pos, size_t alloc, int force);

_g void bignum_alloc(LocalRoot local, addr *ret, int sign, size_t size);
_g void bignum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
_g void bignum_copy_nosign_alloc(LocalRoot local, addr *ret, addr right);
_g void bignum_copy_alloc(LocalRoot local, addr *ret, addr right);
_g void bignum_value_alloc(LocalRoot local, addr *ret, int sign, fixed value);
_g void bignum_value2_alloc(LocalRoot local, addr *ret, int sign, fixed high, fixed low);
_g void bignum_zero_alloc(LocalRoot local, addr *ret);
_g void bignum_fixnum_alloc(LocalRoot local, addr *ret, addr value);
_g void bignum_fixnum_value_alloc(LocalRoot local, addr *ret, fixnum value);
_g int bignum_counter_alloc_(LocalRoot local, addr *ret, addr index);
_g void bignum_result_alloc(LocalRoot local, addr pos, addr *ret);
_g void bignum_integer_alloc(LocalRoot local, addr *ret, addr pos);
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
#define bignum_integer_heap(p,r) bignum_integer_alloc(NULL,(p),(r))

_g void bignum_debug(LocalRoot local, addr *ret, int sign, size_t size);
_g void bignum_cons_debug(LocalRoot local, addr *ret, int sign, addr cons);
_g void bignum_copy_nosign_debug(LocalRoot local, addr *ret, addr right);
_g void bignum_copy_debug(LocalRoot local, addr *ret, addr right);
_g void bignum_value_debug(LocalRoot local, addr *ret, int sign, fixed value);
_g void bignum_value2_debug(LocalRoot local, addr *ret, int sign, fixed high, fixed low);
_g void bignum_zero_debug(LocalRoot local, addr *ret);
_g void bignum_fixnum_debug(LocalRoot local, addr *ret, addr value);
_g void bignum_fixnum_value_debug(LocalRoot local, addr *ret, fixnum value);
_g int bignum_counter_debug_(LocalRoot local, addr *ret, addr index);
_g void bignum_result_debug(LocalRoot local, addr pos, addr *ret);
_g void bignum_integer_debug(LocalRoot local, addr *ret, addr pos);

_g void getfixed_bignum(addr pos, size_t index, fixed *value);
_g fixed reffixed_bignum(addr pos, size_t index);
_g void setfixed_bignum(addr pos, size_t index, fixed value);
_g void sizepress_bignum(addr left);
_g void copy_bignum(LocalRoot local, addr left, addr right, int force);
_g void copy_noexpand_bignum(addr left, addr right);
_g void setvalue_bignum(addr left, int sign, bigtype value);
_g void setzero_bignum(addr left);
_g int getbit_bignum(addr pos, size_t index);
_g void incf_bignum(addr pos, bigtype value);
_g void decf_bignum(addr pos, bigtype value);

_g void bignum_throw_heap(addr pos, addr *ret);
_g void bignum_throw_local(LocalRoot local, addr pos, addr *ret);
_g void bignum_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void fixnum_throw_heap(addr pos, addr *ret);
_g void fixnum_throw_local(LocalRoot local, addr pos, addr *ret);
_g void fixnum_throw_alloc(LocalRoot local, addr pos, addr *ret);

_g void castfixed(fixnum value, int *sign, fixed *result);
_g void castfixed_fixnum(addr pos, int *sign, fixed *result);
_g int castfixed_integer(addr value, int *sign, fixed *result);
#define castbigtype castfixed
#define castbigtype_integer castfixed_integer

#endif

