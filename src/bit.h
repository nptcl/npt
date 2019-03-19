#ifndef __BIT_HEADER__
#define __BIT_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

/* bit */
int bitp(addr pos);
int bit_getint(addr pos, int *ret);

/* bitmemory */
void bitcons_local(LocalRoot local, addr *ret, size_t bitsize);
void push_bitcons(LocalRoot local, addr pos, int value);

void bitmemory_unsafe(LocalRoot local, addr *ret, size_t bitsize);
void bitmemory_alloc(LocalRoot local, addr *ret, size_t bitsize);
void bitmemory_local(LocalRoot local, addr *ret, size_t bitsize);
void bitmemory_heap(addr *ret, size_t bitsize);
void bitmemory_cons_alloc(LocalRoot local, addr *ret, addr cons);
void bitmemory_cons_local(LocalRoot local, addr *ret, addr cons);
void bitmemory_cons_heap(addr *ret, addr cons);
void bitmemory_char_heap(addr *ret, const char *str);
void bitmemory_copy_alloc(LocalRoot local, addr *ret, addr pos);
void bitmemory_copy_local(LocalRoot local, addr *ret, addr pos);
void bitmemory_copy_heap(addr *ret, addr pos);
int bitmemoryp(addr pos);

void bitmemory_memset_byte(addr pos, byte value);
void bitmemory_memset(addr pos, int value);
void bitmemory_copy_unsafe(addr pos, addr refer, size_t bitsize);
void bitmemory_length(addr pos, size_t *ret);
int bitmemory_equal_length(addr pos1, addr pos2);
int bitmemory_equal(addr left, addr right);
int bitmemory_refint(addr pos, size_t index);
void bitmemory_getint(addr pos, size_t index, int *ret);
void bitmemory_setint(addr pos, size_t index, int value);
void bitmemory_get(LocalRoot local, addr pos, size_t index, addr *ret);
void bitmemory_set(addr pos, size_t index, addr value);
void bitmemory_aref(LocalRoot local, addr pos, addr args, addr *ret);
void bitmemory_setf_aref(addr pos, addr args, addr value);
void bitmemory_bitcalc(addr pos, addr pos1, addr pos2, bitcalc_call call);
void bitmemory_bitnot(addr pos, addr pos1);
void bitmemory_fill(addr pos, addr item, addr start, addr end);
void bitmemory_subseq_index(addr *ret, addr pos, size_t index1, size_t index2);
void bitmemory_subseq(addr *ret, addr pos, addr start, addr end);
void bitmemory_setget(addr pos1, size_t index1, addr pos2, size_t index2);
void bitmemory_adjust(addr *ret, addr array, size_t size, addr value, addr check);
void bitmemory_reverse(LocalRoot local, addr *ret, addr pos);
void bitmemory_nreverse(addr *ret, addr pos);

/* bvarray */
int array_bvarrayp(addr pos);
int bvarrayp(addr pos);
int bitvectorp(addr pos);
int simple_array_bvarrayp(addr pos);
int simple_bvarrayp(addr pos);
int simple_bitvectorp(addr pos);

void bvarray_length(addr pos, size_t *ret);
int bvarray_refint(addr pos, size_t index);
void bvarray_getint(addr pos, size_t index, int *ret);

/* bitvector */
void bitvector_length(addr pos, size_t *ret);
int bitvector_refint(addr pos, size_t index);
void bitvector_getint(addr pos, size_t index, int *ret);
int bitvector_equal(addr left, addr right);

#endif

