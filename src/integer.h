#ifndef __INTEGER_HEADER__
#define __INTEGER_HEADER__

#include <stdint.h>
#include "local.h"
#include "typedef.h"

int fixnump(addr pos);
int bignump(addr pos);
int integerp(addr pos);

int integer_result_alloc(LocalRoot local, addr pos, addr *ret);
void integer_throw_alloc(LocalRoot local, addr pos, addr *ret);
void integer_throw_local(LocalRoot local, addr pos, addr *ret);
void integer_throw_heap(addr pos, addr *ret);

/* fixnum */
#define zerop_or_plusp_fixnum(a) (0 <= RefFixnum(a))
#define plusp_fixnum(a) (0 < RefFixnum(a))
#define minusp_fixnum(a) (RefFixnum(a) < 0)
#define zerop_fixnum(a) (RefFixnum(a) == 0)
#define equal_ff_real(a,b) (RefFixnum(a) == RefFixnum(b))
#define compare_ff_real fixnumcompare

/* integer */
int zerop_or_plusp_integer(addr pos);
int plusp_integer(addr pos);
int minusp_integer(addr pos);
int zerop_integer(addr pos);
int equal_integer(addr left, addr right);
#define not_equal_integer(a,b) (! equal_integer((a), (b)))
int compare_integer(addr left, addr right);
#define less_integer(a,b) (compare_integer((a), (b)) < 0)
#define less_equal_integer(a,b) (compare_integer((a), (b)) <= 0)
#define greater_integer(a,b) (compare_integer((a), (b)) > 0)
#define greater_equal_integer(a,b) (compare_integer((a), (b)) >= 0)

int less_integer_clang(addr left, addr right);
int less_equal_integer_clang(addr left, addr right);

void oneplus_integer_common(LocalRoot local, addr value, addr *ret);
void oneminus_integer_common(LocalRoot local, addr value, addr *ret);
void plus_integer_common(LocalRoot local, addr left, addr right, addr *ret);
void minus_integer(LocalRoot local, addr left, addr right, addr *ret);
void multi_integer(LocalRoot local, addr left, addr right, addr *ret);

int evenp_integer(addr left);

void output_nosign_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp);
void output_nosign_comma_integer(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma);
void string_nosign_comma_integer(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma);

/* size */
void make_index_integer_alloc(LocalRoot local, addr *ret, size_t value);
void make_indexmax_alloc(LocalRoot local, addr *ret);
int getindex_integer(addr pos, size_t *ret);
void getindex_error(addr pos, size_t *ret);

addr reference_index_integer_alloc(LocalRoot local, size_t value);
addr reference_index_integer_local(LocalRoot local, size_t value);
addr reference_index_integer_heap(size_t value);
#define intsizea reference_index_integer_alloc
#define intsizeh reference_index_integer_heap
#define intsizel(v) reference_index_integer_local(Local_Thread, (v))

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

