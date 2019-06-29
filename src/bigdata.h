#ifndef __BIGDATA_HEADER__
#define __BIGDATA_HEADER__

#include "build.h"
#include "typedef.h"
#include "typedef_integer.h"

/*****************************************************************************
  Macro
 *****************************************************************************/
/*
 *  macro
 *    LISP_BIGNUM_DEBUG -> 8bit
 *    LISP_64BIT        -> 64bit
 *    LISP_32BIT        -> 32bit
 */
#undef BIGNUM_FULLCODE
#undef BIGNUM_TYPE_8BIT
#undef BIGNUM_TYPE_32BIT
#undef BIGNUM_TYPE_64BIT
#undef BIGNUM_CODE_8BIT
#undef BIGNUM_CODE_32BIT
#undef BIGNUM_CODE_64BIT

/* 8bit debug */
#ifdef LISP_BIGNUM_DEBUG
#define BIGNUM_TYPE_32BIT
#define BIGNUM_CODE_8BIT
#define BIGNUM_INFO_TYPE    "32bit"

/* 64bit */
#elif defined LISP_64BIT
#define BIGNUM_FULLCODE
#define BIGNUM_TYPE_64BIT
#define BIGNUM_CODE_64BIT
#define BIGNUM_INFO_TYPE    "64bit"

/* 32bit */
#elif defined LISP_32BIT
#define BIGNUM_FULLCODE
#define BIGNUM_TYPE_32BIT
#define BIGNUM_CODE_32BIT
#define BIGNUM_INFO_TYPE    "32bit"
#endif

/* code */
#ifdef BIGNUM_CODE_64BIT
#define BIGNUM_INFO_CODE    "64bit-full"
#define BIGNUM_FULL         0xFFFFFFFFFFFFFFFFULL
#define BIGNUM_HALF         0xFFFFFFFFULL
#define BIGNUM_FULLBIT      64
#define BIGNUM_HALFBIT      32
#define BIGCONS_SIZE        32
#endif

#ifdef BIGNUM_CODE_32BIT
#define BIGNUM_INFO_CODE    "32bit-full"
#define BIGNUM_FULL         0xFFFFFFFFUL
#define BIGNUM_HALF         0xFFFFUL
#define BIGNUM_FULLBIT      32
#define BIGNUM_HALFBIT      16
#define BIGCONS_SIZE        32
#endif

#ifdef BIGNUM_CODE_8BIT
#define BIGNUM_INFO_CODE    "8bit-debug"
#define BIGNUM_FULL         0xFFUL
#define BIGNUM_HALF         0xFUL
#define BIGNUM_FULLBIT      8
#define BIGNUM_HALFBIT      4
#define BIGCONS_SIZE        3
#endif

/* bigtype */
#define PRIxB				PRIxF
#define plussign_bignum(x)	((x)->sign == 0)
#define minussign_bignum(x)	((x)->sign != 0)

/* copy operator */
#define bigcpy(x,y,z) memcpy((x), (y), sizeoft(bigtype) * (z))
#define bigmove(x,y,z) memmove((x), (y), sizeoft(bigtype) * (z))
#define bigset(x,y,z) memset((x), (y), sizeoft(bigtype) * (z))
#define bigcmp(x,y,z) memcmp((x), (y), sizeoft(bigtype) * (z))

#ifdef BIGNUM_CODE_8BIT
#define CUTB(x)				(BIGNUM_FULL & (x))
#define SETB(x)				((x) = (BIGNUM_FULL & (x)))
#else
#define CUTB(x)				(x)
#define SETB(x)
#endif

#define HIGHVALUE(x)		CUTB(CUTB(x) >> BIGNUM_HALFBIT)
#define LOWVALUE(x)			CUTB(BIGNUM_HALF & (x))
#define HIGHLOW(high,low)	CUTB((CUTB(high) << BIGNUM_HALFBIT) | CUTB(low))

#define SignPlus			(signplus_bignum)
#define SignMinus			(signminus_bignum)
#define SignNot(x)			(!(x))
#define IsPlus(x)			((x) == SignPlus)
#define IsMinus(x)			((x) != SignPlus)
#define SignMulti(x,y)		(((x) == (y))? SignPlus: SignMinus)


/*****************************************************************************
  declaration
 *****************************************************************************/
_g void plusnumber_bigdata(bigtype *result, bigtype *carry);
_g void multicarry_bigdata(bigtype *result, bigtype value, bigtype *carry);
_g int equal_bigdata(addr left, addr right);
_g int compare_bigdata(addr left, addr right);

_g void setplusvalue_bigdata(addr set, addr left, int sign, fixed right);
_g void setminusvalue_bigdata(addr set, addr left, int sign, fixed right);
_g void plusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret);
_g void minusvalue_bigdata_alloc(LocalRoot local,
		addr left, int sign, fixed right, addr *ret);
_g void plus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g void letplus_noexpand_bigdata(addr left, addr right);
_g void minus_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g int minuscheck_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret);
_g int letminus_noexpand_bigdata(addr left, addr right);
_g void multicarry_fixnum(LocalRoot local, fixnum left, fixnum right, addr *ret);
_g void multicarry_bignum(LocalRoot local, fixnum left, fixnum right, addr *ret);
_g void setmultivalue_bigdata(addr pos, addr left, bigtype right);
_g void setmulti_bigdata(addr pos, addr left, addr right);
_g void multi_bigdata_alloc(LocalRoot local, addr left, addr right, addr *ret);

_g void letdiv_noexpand_bigdata(LocalRoot local, addr left, addr right);
_g void setrem_noexpand_bigdata(LocalRoot local, addr set, addr left, addr right);
_g void divrem_bigdata_local(LocalRoot local,
		addr *quot, addr *rem, addr left, addr right);

_g void power2_bigdata_alloc(LocalRoot local, addr *ret, size_t value);
_g void division2_bigdata_alloc(LocalRoot local, addr *ret, addr left);
_g void shiftup_bigdata_alloc(LocalRoot local, addr *ret, addr left, size_t value);
_g void shiftdown_bigdata_alloc(LocalRoot local, addr *ret, addr left, size_t value);

_g bigtype letdiv_half_bigdata(addr left, bigtype right);

#endif

