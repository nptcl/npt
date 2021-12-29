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
#define getunicode_integer_ _n(getunicode_integer_)
#define int8_integer_alloc _n(int8_integer_alloc)
#define int16_integer_alloc _n(int16_integer_alloc)
#define int32_integer_alloc _n(int32_integer_alloc)
#define uint8_integer_alloc _n(uint8_integer_alloc)
#define uint16_integer_alloc _n(uint16_integer_alloc)
#define uint32_integer_alloc _n(uint32_integer_alloc)
#define int64_integer_alloc _n(int64_integer_alloc)
#define uint64_integer_alloc _n(uint64_integer_alloc)
#define IsByteSign(x)  (0 <= (x) && (x) <= 0xFF)
#define IsByteUnsign(x)  ((x) <= 0xFF)

int integerp(addr pos);
int minusp_integerp(addr pos);

int integer_throw_alloc_(LocalRoot local, addr pos, addr *ret);
int integer_throw_local_(LocalRoot local, addr pos, addr *ret);
int integer_throw_heap_(addr pos, addr *ret);
int integer_result_alloc_(LocalRoot local, addr pos, addr *ret);
int integer_result_local_(LocalRoot local, addr pos, addr *ret);
int integer_result_heap_(addr pos, addr *ret);

void fixnum_copy_alloc(LocalRoot local, addr pos, addr *ret);
void fixnum_copy_local(LocalRoot local, addr pos, addr *ret);
void fixnum_copy_heap(LocalRoot local, addr pos, addr *ret);
int integer_copy_alloc_(LocalRoot local, addr pos, addr *ret);
int integer_copy_local_(LocalRoot local, addr pos, addr *ret);
int integer_copy_heap_(addr pos, addr *ret);

/* fixnum */
#define zerop_or_plusp_fixnum(a) (0 <= RefFixnum(a))
#define plusp_fixnum(a) (0 < RefFixnum(a))
#define minusp_fixnum(a) (RefFixnum(a) < 0)
#define zerop_fixnum(a) (RefFixnum(a) == 0)
#define equal_ff_real(a,b) (RefFixnum(a) == RefFixnum(b))
#define compare_ff_real fixnumcompare

/* integer */
int getsign_integer_(addr pos, int *ret);
int zerop_or_plusp_integer_(addr pos, int *ret);
int plusp_integer_(addr pos, int *ret);
int minusp_integer_(addr pos, int *ret);
int zerop_integer_(addr pos, int *ret);
int equal_integer_(addr left, addr right, int *ret);
int not_equal_integer_(addr left, addr right, int *ret);
int compare_integer_(addr left, addr right, int *ret);
int less_integer_(addr left, addr right, int *ret);
int less_equal_integer_(addr left, addr right, int *ret);
int greater_integer_(addr left, addr right, int *ret);
int greater_equal_integer_(addr left, addr right, int *ret);

int zerop_integer_debug(addr pos);
int plusp_integer_debug(addr pos);
int minusp_integer_debug(addr pos);
int less_integer_debug(addr left, addr right);
int less_equal_integer_debug(addr left, addr right);

int sign_reverse_integer_common_(addr pos, addr *ret);
int evenp_integer_(addr left, int *ret);

/* size */
void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value);
void make_index_integer_local(LocalRoot local, addr *ret, size_t value);
void make_index_integer_heap(addr *ret, size_t value);
void make_indexmax_alloc(LocalRoot local, addr *ret);
addr reference_index_integer_alloc(LocalRoot local, size_t value);
addr reference_index_integer_local(LocalRoot local, size_t value);
addr reference_index_integer_heap(size_t value);
#define intsizea reference_index_integer_alloc
#define intsizeh reference_index_integer_heap
#define intsizel(v) reference_index_integer_local(Local_Thread, (v))

int GetIndex_integer(addr pos, size_t *ret);
int getindex_integer_(addr pos, size_t *ret);
int getindex_sign_integer_(addr pos, int *sign, size_t *value, int *ret);
int GetIndex_fixnum(addr pos, size_t *ret);
int getindex_fixnum_(addr pos, size_t *ret);
int fixnum_index_heap_(addr *ret, size_t value);
int GetByte_integer(addr pos, byte *ret);
int getunicode_integer_(addr pos, unicode *ret);

/* standard type */
void int8_integer_alloc(LocalRoot local, addr *ret, int8_t value);
void int16_integer_alloc(LocalRoot local, addr *ret, int16_t value);
void int32_integer_alloc(LocalRoot local, addr *ret, int32_t value);
void uint8_integer_alloc(LocalRoot local, addr *ret, uint8_t value);
void uint16_integer_alloc(LocalRoot local, addr *ret, uint16_t value);
void uint32_integer_alloc(LocalRoot local, addr *ret, uint32_t value);
#ifdef LISP_64BIT
void int64_integer_alloc(LocalRoot local, addr *ret, int64_t value);
void uint64_integer_alloc(LocalRoot local, addr *ret, uint64_t value);
#endif

#endif

