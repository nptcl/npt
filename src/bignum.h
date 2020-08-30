#ifndef __BIGNUM_HEADER__
#define __BIGNUM_HEADER__

#include "local.h"
#include "typedef.h"

#define power2_bignum_alloc _n(power2_bignum_alloc)
#define power2_bignum_local _n(power2_bignum_local)
#define power2_bignum_heap _n(power2_bignum_heap)
#define shiftup_bignum_alloc _n(shiftup_bignum_alloc)
#define shiftup_bignum_local _n(shiftup_bignum_local)
#define shiftup_bignum_heap _n(shiftup_bignum_heap)
#define fixnum_cons_alloc _n(fixnum_cons_alloc)
#define fixnum_cons_local _n(fixnum_cons_local)
#define fixnum_cons_heap _n(fixnum_cons_heap)
#define integer_cons_alloc _n(integer_cons_alloc)
#define integer_cons_local _n(integer_cons_local)
#define integer_cons_heap _n(integer_cons_heap)
#define integer_fixed_alloc _n(integer_fixed_alloc)
#define integer_fixed_local _n(integer_fixed_local)
#define integer_fixed_heap _n(integer_fixed_heap)
#define integer_bignum_alloc _n(integer_bignum_alloc)
#define integer_bignum_local _n(integer_bignum_local)
#define integer_bignum_heap _n(integer_bignum_heap)
#define integer_copysign_alloc_ _n(integer_copysign_alloc_)
#define integer_copysign_local_ _n(integer_copysign_local_)
#define integer_copysign_heap_ _n(integer_copysign_heap_)
#define single_float_fixnum _n(single_float_fixnum)
#define double_float_fixnum _n(double_float_fixnum)
#define long_float_fixnum _n(long_float_fixnum)
#define single_float_bignum_ _n(single_float_bignum_)
#define double_float_bignum_ _n(double_float_bignum_)
#define long_float_bignum_ _n(long_float_bignum_)
#define single_float_fixnum_alloc _n(single_float_fixnum_alloc)
#define single_float_fixnum_local _n(single_float_fixnum_local)
#define single_float_fixnum_heap _n(single_float_fixnum_heap)
#define double_float_fixnum_alloc _n(double_float_fixnum_alloc)
#define double_float_fixnum_local _n(double_float_fixnum_local)
#define double_float_fixnum_heap _n(double_float_fixnum_heap)
#define long_float_fixnum_alloc _n(long_float_fixnum_alloc)
#define long_float_fixnum_local _n(long_float_fixnum_local)
#define long_float_fixnum_heap _n(long_float_fixnum_heap)
#define single_float_bignum_alloc_ _n(single_float_bignum_alloc_)
#define single_float_bignum_local_ _n(single_float_bignum_local_)
#define single_float_bignum_heap_ _n(single_float_bignum_heap_)
#define double_float_bignum_alloc_ _n(double_float_bignum_alloc_)
#define double_float_bignum_local_ _n(double_float_bignum_local_)
#define double_float_bignum_heap_ _n(double_float_bignum_heap_)
#define long_float_bignum_alloc_ _n(long_float_bignum_alloc_)
#define long_float_bignum_local_ _n(long_float_bignum_local_)
#define long_float_bignum_heap_ _n(long_float_bignum_heap_)
#define bignum_single_float_unsafe _n(bignum_single_float_unsafe)
#define bignum_double_float_unsafe _n(bignum_double_float_unsafe)
#define bignum_long_float_unsafe _n(bignum_long_float_unsafe)
#define bignum_single_float_local_ _n(bignum_single_float_local_)
#define bignum_single_float_heap_ _n(bignum_single_float_heap_)
#define bignum_double_float_local_ _n(bignum_double_float_local_)
#define bignum_double_float_heap_ _n(bignum_double_float_heap_)
#define bignum_long_float_local_ _n(bignum_long_float_local_)
#define bignum_long_float_heap_ _n(bignum_long_float_heap_)
#define GetFixnum_bignum _n(GetFixnum_bignum)
#define GetFixnum_signed _n(GetFixnum_signed)
#define getfixnum_signed_ _n(getfixnum_signed_)
#define GetFixnum_unsigned _n(GetFixnum_unsigned)
#define getfixnum_unsigned_ _n(getfixnum_unsigned_)
#define getfixed1_bignum _n(getfixed1_bignum)
#define abs_fixnum_integer_local _n(abs_fixnum_integer_local)
#define abs_fixnum_integer_common _n(abs_fixnum_integer_common)
#define abs_bignum_integer_local _n(abs_bignum_integer_local)
#define abs_bignum_integer_common _n(abs_bignum_integer_common)

/* power */
_g void power2_bignum_alloc(LocalRoot local, addr *ret, int sign, size_t value);
_g void power2_bignum_local(LocalRoot local, addr *ret, int sign, size_t value);
_g void power2_bignum_heap(addr *ret, int sign, size_t value);
_g void shiftup_bignum_alloc(LocalRoot local, addr *ret, addr left, size_t value);
_g void shiftup_bignum_local(LocalRoot local, addr *ret, addr left, size_t value);
_g void shiftup_bignum_heap(addr *ret, addr left, size_t value);

/* integer */
_g int fixnum_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
_g int fixnum_cons_local(LocalRoot local, addr *ret, int sign, addr cons);
_g int fixnum_cons_heap(addr *ret, int sign, addr cons);
_g void integer_cons_alloc(LocalRoot local, addr *ret, int sign, addr cons);
_g void integer_cons_local(LocalRoot local, addr *ret, int sign, addr cons);
_g void integer_cons_heap(addr *ret, int sign, addr cons);
_g void integer_fixed_alloc(LocalRoot local, addr *ret, int sign, fixed value);
_g void integer_fixed_local(LocalRoot local, addr *ret, int sign, fixed value);
_g void integer_fixed_heap(addr *ret, int sign, fixed value);
_g void integer_bignum_alloc(LocalRoot local, addr *ret, addr pos);
_g void integer_bignum_local(LocalRoot local, addr *ret, addr pos);
_g void integer_bignum_heap(addr *ret, addr pos);

/* integer copy */
_g int integer_copysign_alloc_(LocalRoot local, int sign, addr pos, addr *ret);
_g int integer_copysign_local_(LocalRoot local, int sign, addr pos, addr *ret);
_g int integer_copysign_heap_(int sign, addr pos, addr *ret);

/* float */
_g single_float single_float_fixnum(addr pos);
_g double_float double_float_fixnum(addr pos);
_g long_float long_float_fixnum(addr pos);
_g int single_float_bignum_(addr pos, single_float *ret);
_g int double_float_bignum_(addr pos, double_float *ret);
_g int long_float_bignum_(addr pos, long_float *ret);

_g void single_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void single_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void single_float_fixnum_heap(addr *ret, addr pos);
_g void double_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void double_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void double_float_fixnum_heap(addr *ret, addr pos);
_g void long_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void long_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void long_float_fixnum_heap(addr *ret, addr pos);
_g int single_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos);
_g int single_float_bignum_local_(LocalRoot local, addr *ret, addr pos);
_g int single_float_bignum_heap_(addr *ret, addr pos);
_g int double_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos);
_g int double_float_bignum_local_(LocalRoot local, addr *ret, addr pos);
_g int double_float_bignum_heap_(addr *ret, addr pos);
_g int long_float_bignum_alloc_(LocalRoot local, addr *ret, addr pos);
_g int long_float_bignum_local_(LocalRoot local, addr *ret, addr pos);
_g int long_float_bignum_heap_(addr *ret, addr pos);

_g void bignum_single_float_unsafe(
		LocalRoot local, single_float v, int is_heap, addr *ret);
_g void bignum_double_float_unsafe(
		LocalRoot local, double_float v, int is_heap, addr *ret);
_g void bignum_long_float_unsafe(
		LocalRoot local, long_float v, int is_heap, addr *ret);

_g int bignum_single_float_local_(LocalRoot local, single_float v, addr *rv, int *ret);
_g int bignum_single_float_heap_(LocalRoot local, single_float v, addr *rv, int *ret);
_g int bignum_double_float_local_(LocalRoot local, double_float v, addr *rv, int *ret);
_g int bignum_double_float_heap_(LocalRoot local, double_float v, addr *rv, int *ret);
_g int bignum_long_float_local_(LocalRoot local, long_float v, addr *rv, int *ret);
_g int bignum_long_float_heap_(LocalRoot local, long_float v, addr *rv, int *ret);

_g int GetFixnum_bignum(addr pos, fixnum *ret);
_g int GetFixnum_signed(addr pos, fixnum *ret);
_g int getfixnum_signed_(addr pos, fixnum *ret);
_g int GetFixnum_unsigned(addr pos, fixnum *ret);
_g int getfixnum_unsigned_(addr pos, fixnum *ret);
_g int getfixed1_bignum(addr pos, int *sign, fixed *ret);

_g void abs_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
_g void abs_fixnum_integer_common(addr left, addr *ret);
_g void abs_bignum_integer_local(LocalRoot local, addr left, addr *ret);
_g void abs_bignum_integer_common(addr left, addr *ret);

#endif

