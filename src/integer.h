#ifndef __INTEGER_HEADER__
#define __INTEGER_HEADER__

#include <stdint.h>
#include "local.h"
#include "typedef.h"

_g int fixnump(addr pos);
_g int bignump(addr pos);
_g int integerp(addr pos);

_g void integer_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void integer_throw_local(LocalRoot local, addr pos, addr *ret);
_g void integer_throw_heap(addr pos, addr *ret);
_g void integer_result_alloc(LocalRoot local, addr pos, addr *ret);
_g void integer_result_local(LocalRoot local, addr pos, addr *ret);
_g void integer_result_heap(addr pos, addr *ret);

_g void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void fixnum_copy_local(LocalRoot local, addr pos, addr *ret);
_g void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret);
_g void integer_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void integer_copy_local(LocalRoot local, addr pos, addr *ret);
_g void integer_copy_heap(addr pos, addr *ret);

/* fixnum */
#define zerop_or_plusp_fixnum(a) (0 <= RefFixnum(a))
#define plusp_fixnum(a) (0 < RefFixnum(a))
#define minusp_fixnum(a) (RefFixnum(a) < 0)
#define zerop_fixnum(a) (RefFixnum(a) == 0)
#define equal_ff_real(a,b) (RefFixnum(a) == RefFixnum(b))
#define compare_ff_real fixnumcompare

/* integer */
_g void getsign_integer(addr pos, int *ret);
_g int zerop_or_plusp_integer(addr pos);
_g int plusp_integer(addr pos);
_g int minusp_integer(addr pos);
_g int zerop_integer(addr pos);
_g int equal_integer(addr left, addr right);
#define not_equal_integer(a,b) (! equal_integer((a), (b)))
_g int compare_integer(addr left, addr right);
#define less_integer(a,b) (compare_integer((a), (b)) < 0)
#define less_equal_integer(a,b) (compare_integer((a), (b)) <= 0)
#define greater_integer(a,b) (compare_integer((a), (b)) > 0)
#define greater_equal_integer(a,b) (compare_integer((a), (b)) >= 0)

_g int less_integer_clang(addr left, addr right);
_g int less_equal_integer_clang(addr left, addr right);
_g void sign_reverse_integer_common(addr pos, addr *ret);
_g int evenp_integer(addr left);

_g void output_nosign_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
_g void output_nosign_comma_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);
_g void string_nosign_comma_integer(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma);

/* size */
_g void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value);
_g void make_index_integer_local(LocalRoot local, addr *ret, size_t value);
_g void make_index_integer_heap(addr *ret, size_t value);
_g void make_indexmax_alloc(LocalRoot local, addr *ret);
_g int getindex_integer(addr pos, size_t *ret);
_g void getindex_error(addr pos, size_t *ret);

_g addr reference_index_integer_alloc(LocalRoot local, size_t value);
_g addr reference_index_integer_local(LocalRoot local, size_t value);
_g addr reference_index_integer_heap(size_t value);
#define intsizea reference_index_integer_alloc
#define intsizeh reference_index_integer_heap
#define intsizel(v) reference_index_integer_local(Local_Thread, (v))
_g int getindex_sign_integer(addr pos, int *sign, size_t *ret);

_g int getunsigned_integer(addr pos, size_t *ret);
_g void getunsigned_error(addr pos, size_t *ret);
_g void fixnum_unsigned_error(addr *ret, size_t value);

/* standard type */
_g void int8_integer_alloc(LocalRoot local, addr *ret, int8_t value);
_g void int16_integer_alloc(LocalRoot local, addr *ret, int16_t value);
_g void int32_integer_alloc(LocalRoot local, addr *ret, int32_t value);
_g void uint8_integer_alloc(LocalRoot local, addr *ret, uint8_t value);
_g void uint16_integer_alloc(LocalRoot local, addr *ret, uint16_t value);
_g void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value);
#ifdef LISP_64BIT
_g void int64_integer_alloc(LocalRoot local, addr *ret, int64_t value);
_g void uint64_integer_alloc(LocalRoot local, addr *ret, uint64_t value);
#endif

/* oneplus */
_g void oneplus_integer_common(LocalRoot local, addr value, addr *ret);
_g void oneminus_integer_common(LocalRoot local, addr value, addr *ret);

_g void plus_fi_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_fi_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_fi_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bi_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bi_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_bi_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ii_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ii_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void plus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void minus_ii_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ii_real_common(LocalRoot local, addr left, addr right, addr *ret);

_g void ash_bignum_common(LocalRoot local, addr pos, int sign2, size_t size, addr *ret);
_g void ash_integer_common(LocalRoot local, addr pos, addr count, addr *ret);
_g void integer_length_common(addr pos, addr *ret);
_g void parse_integer_common(LocalRoot local,
		addr string, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *position);

#endif

