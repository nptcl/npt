#ifndef __BIT_HEADER__
#define __BIT_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

#define bitp _n(bitp)
#define bit_getint _n(bit_getint)
#define bit_getint_error_ _n(bit_getint_error_)
#define bitcons_local _n(bitcons_local)
#define push_bitcons _n(push_bitcons)
#define bitmemory_unsafe _n(bitmemory_unsafe)
#define bitmemory_alloc _n(bitmemory_alloc)
#define bitmemory_local _n(bitmemory_local)
#define bitmemory_heap _n(bitmemory_heap)
#define bitmemory_cons_alloc _n(bitmemory_cons_alloc)
#define bitmemory_cons_local _n(bitmemory_cons_local)
#define bitmemory_cons_heap _n(bitmemory_cons_heap)
#define bitmemory_char_heap _n(bitmemory_char_heap)
#define bitmemory_copy_alloc _n(bitmemory_copy_alloc)
#define bitmemory_copy_local _n(bitmemory_copy_local)
#define bitmemory_copy_heap _n(bitmemory_copy_heap)
#define bitmemoryp _n(bitmemoryp)
#define bitmemory_memset_byte _n(bitmemory_memset_byte)
#define bitmemory_memset _n(bitmemory_memset)
#define bitmemory_copy_unsafe _n(bitmemory_copy_unsafe)
#define bitmemory_length _n(bitmemory_length)
#define bitmemory_equal_length _n(bitmemory_equal_length)
#define bitmemory_equal _n(bitmemory_equal)
#define bitmemory_refint_debug _n(bitmemory_refint_debug)
#define bitmemory_getint_unsafe _n(bitmemory_getint_unsafe)
#define bitmemory_setint_unsafe _n(bitmemory_setint_unsafe)
#define bitmemory_getint_ _n(bitmemory_getint_)
#define bitmemory_setint_ _n(bitmemory_setint_)
#define bitmemory_get_ _n(bitmemory_get_)
#define bitmemory_set_ _n(bitmemory_set_)
#define bitmemory_aref_ _n(bitmemory_aref_)
#define bitmemory_setf_aref_ _n(bitmemory_setf_aref_)
#define bitmemory_bitcalc _n(bitmemory_bitcalc)
#define bitmemory_bitnot _n(bitmemory_bitnot)
#define bitmemory_fill_ _n(bitmemory_fill_)
#define bitmemory_subseq_index_ _n(bitmemory_subseq_index_)
#define bitmemory_subseq_ _n(bitmemory_subseq_)
#define bitmemory_setget_ _n(bitmemory_setget_)
#define bitmemory_reverse_ _n(bitmemory_reverse_)
#define bitmemory_nreverse_ _n(bitmemory_nreverse_)
#define array_bvarrayp _n(array_bvarrayp)
#define bvarrayp _n(bvarrayp)
#define bitvectorp _n(bitvectorp)
#define simple_array_bvarrayp _n(simple_array_bvarrayp)
#define simple_bvarrayp _n(simple_bvarrayp)
#define simple_bitvectorp _n(simple_bitvectorp)
#define bvarray_length _n(bvarray_length)
#define bvarray_getint_ _n(bvarray_getint_)
#define bvarray_setint_ _n(bvarray_setint_)
#define bitvector_length_ _n(bitvector_length_)
#define bitvector_getint_ _n(bitvector_getint_)
#define bitvector_setint_ _n(bitvector_setint_)
#define bitvector_equal_ _n(bitvector_equal_)

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
int bitp(addr pos);
int bit_getint(addr pos, int *ret);
int bit_getint_error_(addr pos, int *ret);

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
int bitmemory_refint_debug(addr pos, size_t index);
void bitmemory_getint_unsafe(addr pos, size_t index, int *ret);
void bitmemory_setint_unsafe(addr pos, size_t index, int value);
int bitmemory_getint_(addr pos, size_t index, int *ret);
int bitmemory_setint_(addr pos, size_t index, int value);
int bitmemory_get_(LocalRoot local, addr pos, size_t index, addr *ret);
int bitmemory_set_(addr pos, size_t index, addr value);
int bitmemory_aref_(LocalRoot local, addr pos, addr args, addr *ret);
int bitmemory_setf_aref_(addr pos, addr args, addr value);
void bitmemory_bitcalc(addr pos, addr pos1, addr pos2, bitcalc_call call);
void bitmemory_bitnot(addr pos, addr pos1);
int bitmemory_fill_(addr pos, addr item, addr start, addr end);
int bitmemory_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2);
int bitmemory_subseq_(addr *ret, addr pos, addr start, addr end);
int bitmemory_setget_(addr pos1, size_t index1, addr pos2, size_t index2);
int bitmemory_reverse_(LocalRoot local, addr *ret, addr pos);
int bitmemory_nreverse_(addr *ret, addr pos);

/* bvarray */
int array_bvarrayp(addr pos);
int bvarrayp(addr pos);
int bitvectorp(addr pos);
int simple_array_bvarrayp(addr pos);
int simple_bvarrayp(addr pos);
int simple_bitvectorp(addr pos);

void bvarray_length(addr pos, size_t *ret);
int bvarray_getint_(addr pos, size_t index, int *ret);
int bvarray_setint_(addr pos, size_t index, int value);

/* bitvector */
int bitvector_length_(addr pos, size_t *ret);
int bitvector_getint_(addr pos, size_t index, int *ret);
int bitvector_setint_(addr pos, size_t index, int value);
int bitvector_equal_(addr left, addr right, int *ret);

#endif

