#ifndef __INTEGER_HEADER__
#define __INTEGER_HEADER__

#include <stdint.h>
#include "local.h"
#include "typedef.h"

#define integerp _n(integerp)
#define minusp_integerp _n(minusp_integerp)
#define integer_throw_alloc_ _n(integer_throw_alloc_)
#define integer_throw_local_ _n(integer_throw_local_)
#define integer_throw_heap_ _n(integer_throw_heap_)
#define integer_result_alloc_ _n(integer_result_alloc_)
#define integer_result_local_ _n(integer_result_local_)
#define integer_result_heap_ _n(integer_result_heap_)
#define fixnum_copy_alloc _n(fixnum_copy_alloc)
#define fixnum_copy_local _n(fixnum_copy_local)
#define fixnum_copy_heap _n(fixnum_copy_heap)
#define integer_copy_alloc_ _n(integer_copy_alloc_)
#define integer_copy_local_ _n(integer_copy_local_)
#define integer_copy_heap_ _n(integer_copy_heap_)
#define getsign_integer_ _n(getsign_integer_)
#define zerop_or_plusp_integer_ _n(zerop_or_plusp_integer_)
#define plusp_integer_ _n(plusp_integer_)
#define minusp_integer_ _n(minusp_integer_)
#define zerop_integer_ _n(zerop_integer_)
#define equal_integer_ _n(equal_integer_)
#define not_equal_integer_ _n(not_equal_integer_)
#define compare_integer_ _n(compare_integer_)
#define less_integer_ _n(less_integer_)
#define less_equal_integer_ _n(less_equal_integer_)
#define greater_integer_ _n(greater_integer_)
#define greater_equal_integer_ _n(greater_equal_integer_)
#define zerop_integer_debug _n(zerop_integer_debug)
#define plusp_integer_debug _n(plusp_integer_debug)
#define minusp_integer_debug _n(minusp_integer_debug)
#define less_integer_debug _n(less_integer_debug)
#define less_equal_integer_debug _n(less_equal_integer_debug)
#define sign_reverse_integer_common_ _n(sign_reverse_integer_common_)
#define evenp_integer_ _n(evenp_integer_)
#define output_nosign_integer_ _n(output_nosign_integer_)
#define output_nosign_comma_integer_ _n(output_nosign_comma_integer_)
#define string_nosign_comma_integer_ _n(string_nosign_comma_integer_)
#define make_index_integer_alloc _n(make_index_integer_alloc)
#define make_index_integer_local _n(make_index_integer_local)
#define make_index_integer_heap _n(make_index_integer_heap)
#define make_indexmax_alloc _n(make_indexmax_alloc)
#define reference_index_integer_alloc _n(reference_index_integer_alloc)
#define reference_index_integer_local _n(reference_index_integer_local)
#define reference_index_integer_heap _n(reference_index_integer_heap)
#define GetIndex_integer _n(GetIndex_integer)
#define getindex_integer_ _n(getindex_integer_)
#define getindex_sign_integer_ _n(getindex_sign_integer_)
#define GetIndex_fixnum _n(GetIndex_fixnum)
#define getindex_fixnum_ _n(getindex_fixnum_)
#define fixnum_index_heap_ _n(fixnum_index_heap_)
#define GetByte_integer _n(GetByte_integer)
#define int8_integer_alloc _n(int8_integer_alloc)
#define int16_integer_alloc _n(int16_integer_alloc)
#define int32_integer_alloc _n(int32_integer_alloc)
#define uint8_integer_alloc _n(uint8_integer_alloc)
#define uint16_integer_alloc _n(uint16_integer_alloc)
#define uint32_integer_alloc _n(uint32_integer_alloc)
#define int64_integer_alloc _n(int64_integer_alloc)
#define uint64_integer_alloc _n(uint64_integer_alloc)
#define oneplus_integer_common_ _n(oneplus_integer_common_)
#define oneminus_integer_common_ _n(oneminus_integer_common_)
#define plus_fi_bignum_local_ _n(plus_fi_bignum_local_)
#define plus_fi_real_local_ _n(plus_fi_real_local_)
#define plus_fi_real_common_ _n(plus_fi_real_common_)
#define plus_bi_bignum_local_ _n(plus_bi_bignum_local_)
#define plus_bi_real_local_ _n(plus_bi_real_local_)
#define plus_bi_real_common_ _n(plus_bi_real_common_)
#define plus_ii_bignum_local_ _n(plus_ii_bignum_local_)
#define plus_ii_real_local_ _n(plus_ii_real_local_)
#define plus_ii_real_common_ _n(plus_ii_real_common_)
#define minus_ii_real_common_ _n(minus_ii_real_common_)
#define multi_ii_real_common_ _n(multi_ii_real_common_)
#define ash_bignum_common _n(ash_bignum_common)
#define ash_integer_common_ _n(ash_integer_common_)
#define integer_length_value_ _n(integer_length_value_)
#define integer_length_common_ _n(integer_length_common_)
#define parse_integer_clang _n(parse_integer_clang)

#define IsByteSign(x)  (0 <= (x) && (x) <= 0xFF)
#define IsByteUnsign(x)  ((x) <= 0xFF)

_g int integerp(addr pos);
_g int minusp_integerp(addr pos);

_g int integer_throw_alloc_(LocalRoot local, addr pos, addr *ret);
_g int integer_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int integer_throw_heap_(addr pos, addr *ret);
_g int integer_result_alloc_(LocalRoot local, addr pos, addr *ret);
_g int integer_result_local_(LocalRoot local, addr pos, addr *ret);
_g int integer_result_heap_(addr pos, addr *ret);

_g void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void fixnum_copy_local(LocalRoot local, addr pos, addr *ret);
_g void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret);
_g int integer_copy_alloc_(LocalRoot local, addr pos, addr *ret);
_g int integer_copy_local_(LocalRoot local, addr pos, addr *ret);
_g int integer_copy_heap_(addr pos, addr *ret);

/* fixnum */
#define zerop_or_plusp_fixnum(a) (0 <= RefFixnum(a))
#define plusp_fixnum(a) (0 < RefFixnum(a))
#define minusp_fixnum(a) (RefFixnum(a) < 0)
#define zerop_fixnum(a) (RefFixnum(a) == 0)
#define equal_ff_real(a,b) (RefFixnum(a) == RefFixnum(b))
#define compare_ff_real fixnumcompare

/* integer */
_g int getsign_integer_(addr pos, int *ret);
_g int zerop_or_plusp_integer_(addr pos, int *ret);
_g int plusp_integer_(addr pos, int *ret);
_g int minusp_integer_(addr pos, int *ret);
_g int zerop_integer_(addr pos, int *ret);
_g int equal_integer_(addr left, addr right, int *ret);
_g int not_equal_integer_(addr left, addr right, int *ret);
_g int compare_integer_(addr left, addr right, int *ret);
_g int less_integer_(addr left, addr right, int *ret);
_g int less_equal_integer_(addr left, addr right, int *ret);
_g int greater_integer_(addr left, addr right, int *ret);
_g int greater_equal_integer_(addr left, addr right, int *ret);

_g int zerop_integer_debug(addr pos);
_g int plusp_integer_debug(addr pos);
_g int minusp_integer_debug(addr pos);
_g int less_integer_debug(addr left, addr right);
_g int less_equal_integer_debug(addr left, addr right);

_g int sign_reverse_integer_common_(addr pos, addr *ret);
_g int evenp_integer_(addr left, int *ret);

_g int output_nosign_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
_g int output_nosign_comma_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);
_g int string_nosign_comma_integer_(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma);

/* size */
_g void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value);
_g void make_index_integer_local(LocalRoot local, addr *ret, size_t value);
_g void make_index_integer_heap(addr *ret, size_t value);
_g void make_indexmax_alloc(LocalRoot local, addr *ret);
_g addr reference_index_integer_alloc(LocalRoot local, size_t value);
_g addr reference_index_integer_local(LocalRoot local, size_t value);
_g addr reference_index_integer_heap(size_t value);
#define intsizea reference_index_integer_alloc
#define intsizeh reference_index_integer_heap
#define intsizel(v) reference_index_integer_local(Local_Thread, (v))

_g int GetIndex_integer(addr pos, size_t *ret);
_g int getindex_integer_(addr pos, size_t *ret);
_g int getindex_sign_integer_(addr pos, int *sign, size_t *value, int *ret);
_g int GetIndex_fixnum(addr pos, size_t *ret);
_g int getindex_fixnum_(addr pos, size_t *ret);
_g int fixnum_index_heap_(addr *ret, size_t value);
_g int GetByte_integer(addr pos, byte *ret);

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
_g int oneplus_integer_common_(LocalRoot local, addr value, addr *ret);
_g int oneminus_integer_common_(LocalRoot local, addr value, addr *ret);

_g int plus_fi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_fi_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_fi_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_bi_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_bi_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_bi_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_ii_bignum_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_ii_real_local_(LocalRoot local, addr left, addr right, addr *ret);
_g int plus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int minus_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);
_g int multi_ii_real_common_(LocalRoot local, addr left, addr right, addr *ret);

_g void ash_bignum_common(LocalRoot local, addr pos, int sign2, size_t size, addr *ret);
_g int ash_integer_common_(LocalRoot local, addr pos, addr count, addr *ret);
_g int integer_length_value_(addr pos, size_t *ret);
_g int integer_length_common_(addr pos, addr *ret);
_g int parse_integer_clang(LocalRoot local,
		addr string, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *position);

#endif

