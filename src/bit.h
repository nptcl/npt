#ifndef __BIT_HEADER__
#define __BIT_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

struct bitmemory_struct {
	size_t bitsize, fixedsize;
#ifdef __cplusplus
	fixed data[1];
#else
	fixed data[];
#endif
};

#define BitMemoryStruct(x) ((struct bitmemory_struct *)posbodyr(x))

/* bit */
_g int bitp(addr pos);
_g int bit_getint(addr pos, int *ret);
_g void bit_getint_error(addr pos, int *ret);

/* bitmemory */
_g void bitcons_local(LocalRoot local, addr *ret, size_t bitsize);
_g void push_bitcons(LocalRoot local, addr pos, int value);

_g void bitmemory_unsafe(LocalRoot local, addr *ret, size_t bitsize);
_g void bitmemory_alloc(LocalRoot local, addr *ret, size_t bitsize);
_g void bitmemory_local(LocalRoot local, addr *ret, size_t bitsize);
_g void bitmemory_heap(addr *ret, size_t bitsize);
_g void bitmemory_cons_alloc(LocalRoot local, addr *ret, addr cons);
_g void bitmemory_cons_local(LocalRoot local, addr *ret, addr cons);
_g void bitmemory_cons_heap(addr *ret, addr cons);
_g void bitmemory_char_heap(addr *ret, const char *str);
_g void bitmemory_copy_alloc(LocalRoot local, addr *ret, addr pos);
_g void bitmemory_copy_local(LocalRoot local, addr *ret, addr pos);
_g void bitmemory_copy_heap(addr *ret, addr pos);
_g int bitmemoryp(addr pos);

_g void bitmemory_memset_byte(addr pos, byte value);
_g void bitmemory_memset(addr pos, int value);
_g void bitmemory_copy_unsafe(addr pos, addr refer, size_t bitsize);
_g void bitmemory_length(addr pos, size_t *ret);
_g int bitmemory_equal_length(addr pos1, addr pos2);
_g int bitmemory_equal(addr left, addr right);
_g int bitmemory_refint(addr pos, size_t index);
_g void bitmemory_getint(addr pos, size_t index, int *ret);
_g void bitmemory_setint(addr pos, size_t index, int value);
_g void bitmemory_get(LocalRoot local, addr pos, size_t index, addr *ret);
_g void bitmemory_set(addr pos, size_t index, addr value);
_g void bitmemory_aref(LocalRoot local, addr pos, addr args, addr *ret);
_g void bitmemory_setf_aref(addr pos, addr args, addr value);
_g void bitmemory_bitcalc(addr pos, addr pos1, addr pos2, bitcalc_call call);
_g void bitmemory_bitnot(addr pos, addr pos1);
_g void bitmemory_fill(addr pos, addr item, addr start, addr end);
_g void bitmemory_subseq_index(addr *ret, addr pos, size_t index1, size_t index2);
_g void bitmemory_subseq(addr *ret, addr pos, addr start, addr end);
_g void bitmemory_setget(addr pos1, size_t index1, addr pos2, size_t index2);
_g void bitmemory_reverse(LocalRoot local, addr *ret, addr pos);
_g void bitmemory_nreverse(addr *ret, addr pos);

/* bvarray */
_g int array_bvarrayp(addr pos);
_g int bvarrayp(addr pos);
_g int bitvectorp(addr pos);
_g int simple_array_bvarrayp(addr pos);
_g int simple_bvarrayp(addr pos);
_g int simple_bitvectorp(addr pos);

_g void bvarray_length(addr pos, size_t *ret);
_g int bvarray_refint(addr pos, size_t index);
_g void bvarray_getint(addr pos, size_t index, int *ret);
_g void bvarray_setint(addr pos, size_t index, int value);

/* bitvector */
_g void bitvector_length(addr pos, size_t *ret);
_g int bitvector_refint(addr pos, size_t index);
_g void bitvector_getint(addr pos, size_t index, int *ret);
_g void bitvector_setint(addr pos, size_t index, int value);
_g int bitvector_equal(addr left, addr right);

#endif

