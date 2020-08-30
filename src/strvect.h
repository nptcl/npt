#ifndef __STRVECT_HEADER__
#define __STRVECT_HEADER__

#include <stddef.h>
#include "local.h"
#include "character.h"
#include "typedef.h"

#define strvect_alloc _n(strvect_alloc)
#define strvect_local _n(strvect_local)
#define strvect_heap _n(strvect_heap)
#define strvect_copy_alloc _n(strvect_copy_alloc)
#define strvect_copy_local _n(strvect_copy_local)
#define strvect_copy_heap _n(strvect_copy_heap)
#define strvect_character_alloc_ _n(strvect_character_alloc_)
#define strvect_character_local_ _n(strvect_character_local_)
#define strvect_character_heap_ _n(strvect_character_heap_)
#define strvect_length _n(strvect_length)
#define strvect_posbodylen _n(strvect_posbodylen)
#define unicode_character_type _n(unicode_character_type)
#define strvect_update_character_type_ _n(strvect_update_character_type_)
#define strvectp _n(strvectp)
#define strvect_base_p_ _n(strvect_base_p_)
#define strvect_simple_p _n(strvect_simple_p)
#define strvect_char_alloc _n(strvect_char_alloc)
#define strvect_char_local _n(strvect_char_local)
#define strvect_char_heap _n(strvect_char_heap)
#define stringh _n(stringh)
#define strvect_sizeu_alloc_ _n(strvect_sizeu_alloc_)
#define strvect_sizeu_local_ _n(strvect_sizeu_local_)
#define strvect_sizeu_heap_ _n(strvect_sizeu_heap_)
#define strvect_equal_binary _n(strvect_equal_binary)
#define strvect_equalp_binary _n(strvect_equalp_binary)
#define strvect_equal_char _n(strvect_equal_char)
#define strvect_equalp_char _n(strvect_equalp_char)
#define strvect_equal _n(strvect_equal)
#define strvect_equalp _n(strvect_equalp)
#define strvect_character_equal _n(strvect_character_equal)
#define strvect_character_equalp _n(strvect_character_equalp)
#define strvect_compare_binary _n(strvect_compare_binary)
#define strvect_comparep_binary _n(strvect_comparep_binary)
#define strvect_compare_char _n(strvect_compare_char)
#define strvect_comparep_char _n(strvect_comparep_char)
#define strvect_compare _n(strvect_compare)
#define strvect_comparep _n(strvect_comparep)
#define strvect_designer_equal_char _n(strvect_designer_equal_char)
#define strvect_designer_equalp_char _n(strvect_designer_equalp_char)
#define strvect_getc _n(strvect_getc)
#define strvect_setc_unsafe _n(strvect_setc_unsafe)
#define strvect_setc_ _n(strvect_setc_)
#define strvect_setall_ _n(strvect_setall_)
#define strvect_get_ _n(strvect_get_)
#define strvect_set_ _n(strvect_set_)
#define strvect_aref_ _n(strvect_aref_)
#define strvect_setf_aref_ _n(strvect_setf_aref_)
#define strvect_fill_ _n(strvect_fill_)
#define strvect_subseq_alloc_ _n(strvect_subseq_alloc_)
#define strvect_subseq_index_ _n(strvect_subseq_index_)
#define strvect_subseq_ _n(strvect_subseq_)
#define strvect_setget_ _n(strvect_setget_)
#define strvect_reverse_ _n(strvect_reverse_)
#define strvect_nreverse_ _n(strvect_nreverse_)
#define strvect_char1_heap_ _n(strvect_char1_heap_)
#define strvect_size1_heap_ _n(strvect_size1_heap_)

#define StringBodyLength(x)			(IdxSize + sizeoft(unicode)*(x))
#define PtrStringBase(x)			posbodyr(x)
#define PtrStringSize(x)			((size_t *)PtrStringBase(x))
#define RefStringSize(x)			(*PtrStringSize(x))
#define GetStringSize(x,v)			(*(v) = RefStringSize(x))
#define SetStringSize(x,v)			(RefStringSize(x) = (v))
#define PtrStringUnicode(x)			((unicode *)(PtrStringBase(x) + IdxSize))
#define GetStringUnicode(x,v)		(*(v) = PtrStringUnicode(x))

_g void strvect_alloc(LocalRoot root, addr *ret, size_t len);
_g void strvect_local(LocalRoot root, addr *ret, size_t len);
_g void strvect_heap(addr *ret, size_t len);

_g void strvect_copy_alloc(LocalRoot root, addr *ret, addr value);
_g void strvect_copy_local(LocalRoot root, addr *ret, addr value);
_g void strvect_copy_heap(addr *ret, addr value);

_g int strvect_character_alloc_(LocalRoot local, addr *ret, addr pos);
_g int strvect_character_local_(LocalRoot local, addr *ret, addr pos);
_g int strvect_character_heap_(addr *ret, addr pos);

_g void strvect_length(addr pos, size_t *ret);
_g void strvect_posbodylen(addr pos, const unicode **body, size_t *len);
_g enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u);
_g int strvect_update_character_type_(addr pos);

_g int strvectp(addr pos);
_g int strvect_base_p_(addr pos, int *ret);
_g int strvect_simple_p(addr pos);

_g void strvect_char_alloc(LocalRoot local, addr *ret, const char *arg);
_g void strvect_char_local(LocalRoot root, addr *ret, const char *arg);
_g void strvect_char_heap(addr *ret, const char *arg);
_g addr stringh(const char *arg); /* for debug */

_g int strvect_sizeu_alloc_(LocalRoot, addr *, const unicode *, size_t);
_g int strvect_sizeu_local_(LocalRoot, addr *, const unicode *, size_t);
_g int strvect_sizeu_heap_(addr *, const unicode *, size_t);

_g int strvect_equal_binary(addr left, const unicode *right, size_t len);
_g int strvect_equalp_binary(addr left, const unicode *right, size_t len);
_g int strvect_equal_char(addr left, const char *right);
_g int strvect_equalp_char(addr left, const char *right);
_g int strvect_equal(addr left, addr right);
_g int strvect_equalp(addr left, addr right);
_g int strvect_character_equal(addr left, addr right);
_g int strvect_character_equalp(addr left, addr right);

_g int strvect_compare_binary(addr left, const unicode *right, size_t len);
_g int strvect_comparep_binary(addr left, const unicode *right, size_t len);
_g int strvect_compare_char(addr left, const char *right);
_g int strvect_comparep_char(addr left, const char *right);
_g int strvect_compare(addr left, addr right);
_g int strvect_comparep(addr left, addr right);

_g int strvect_designer_equal_char(addr left, const char *right);
_g int strvect_designer_equalp_char(addr left, const char *right);

_g void strvect_getc(addr pos, size_t len, unicode *c);
_g void strvect_setc_unsafe(addr pos, size_t index, unicode c);
_g int strvect_setc_(addr pos, size_t index, unicode c);
_g int strvect_setall_(addr pos, unicode c);
_g int strvect_get_(LocalRoot local, addr pos, size_t index, addr *ret);
_g int strvect_set_(addr pos, size_t index, addr value);
_g int strvect_aref_(LocalRoot local, addr pos, addr args, addr *ret);
_g int strvect_setf_aref_(addr pos, addr args, addr value);
_g int strvect_fill_(addr pos, addr item, addr start, addr end);
_g int strvect_subseq_alloc_(LocalRoot local, addr *ret, addr pos, size_t x, size_t y);
_g int strvect_subseq_index_(addr *ret, addr pos, size_t index1, size_t index2);
_g int strvect_subseq_(addr *ret, addr pos, addr start, addr end);
_g int strvect_setget_(addr pos1, size_t index1, addr pos2, size_t index2);
_g int strvect_reverse_(LocalRoot local, addr *ret, addr pos);
_g int strvect_nreverse_(addr *ret, addr pos);

#define string_getdirect(body, index, value) \
	(*(value) = ((const unicode *)(body))[index])

_g int strvect_char1_heap_(addr *ret, const char *arg, unicode c);
_g int strvect_size1_heap_(addr *, const char *, size_t);

#endif

