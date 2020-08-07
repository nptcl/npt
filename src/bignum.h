#ifndef __BIGNUM_HEADER__
#define __BIGNUM_HEADER__

#include "local.h"
#include "typedef.h"

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
_g void integer_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void integer_copy_local(LocalRoot local, addr pos, addr *ret);
_g void integer_copy_heap(addr pos, addr *ret);
_g void integer_copysign_alloc(LocalRoot local, int sign, addr pos, addr *ret);
_g void integer_copysign_local(LocalRoot local, int sign, addr pos, addr *ret);
_g void integer_copysign_heap(int sign, addr pos, addr *ret);

/* float */
_g single_float single_float_fixnum(addr pos);
_g double_float double_float_fixnum(addr pos);
_g long_float long_float_fixnum(addr pos);
_g single_float single_float_bignum(addr pos);
_g double_float double_float_bignum(addr pos);
_g long_float long_float_bignum(addr pos);

_g void single_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void single_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void single_float_fixnum_heap(addr *ret, addr pos);
_g void double_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void double_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void double_float_fixnum_heap(addr *ret, addr pos);
_g void long_float_fixnum_alloc(LocalRoot local, addr *ret, addr pos);
_g void long_float_fixnum_local(LocalRoot local, addr *ret, addr pos);
_g void long_float_fixnum_heap(addr *ret, addr pos);
_g void single_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
_g void single_float_bignum_local(LocalRoot local, addr *ret, addr pos);
_g void single_float_bignum_heap(addr *ret, addr pos);
_g void double_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
_g void double_float_bignum_local(LocalRoot local, addr *ret, addr pos);
_g void double_float_bignum_heap(addr *ret, addr pos);
_g void long_float_bignum_alloc(LocalRoot local, addr *ret, addr pos);
_g void long_float_bignum_local(LocalRoot local, addr *ret, addr pos);
_g void long_float_bignum_heap(addr *ret, addr pos);

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

_g int fixnum_unsigned_byte_p(addr value, size_t size);
_g int bignum_unsigned_byte_p(addr value, size_t size);
_g int fixnum_signed_byte_p(addr value, size_t size);
_g int bignum_signed_byte_p(addr value, size_t size);

_g int GetFixnum_bignum(addr pos, fixnum *ret);
_g int GetFixnum_signed(addr pos, fixnum *ret);
_g void getfixnum_signed(addr pos, fixnum *ret);
_g int GetFixnum_unsigned(addr pos, fixnum *ret);
_g void getfixnum_unsigned(addr pos, fixnum *ret);
_g int getfixed1_bignum(addr pos, int *sign, fixed *ret);

_g void abs_fixnum_integer_local(LocalRoot local, addr left, addr *ret);
_g void abs_fixnum_integer_common(addr left, addr *ret);
_g void abs_bignum_integer_local(LocalRoot local, addr left, addr *ret);
_g void abs_bignum_integer_common(addr left, addr *ret);

#endif

