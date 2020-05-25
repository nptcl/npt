#ifndef __STRVECT_HEADER__
#define __STRVECT_HEADER__

#include <stddef.h>
#include "local.h"
#include "typedef.h"

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

_g void strvect_character_alloc(LocalRoot local, addr *ret, addr pos);
_g void strvect_character_local(LocalRoot local, addr *ret, addr pos);
_g void strvect_character_heap(addr *ret, addr pos);

_g void strvect_length(addr pos, size_t *ret);
_g void strvect_posbodylen(addr pos, const unicode **body, size_t *len);
_g enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u);
_g void strvect_update_character_type(addr pos);

_g int strvectp(addr pos);
_g int strvect_base_p(addr pos);
_g int strvect_simple_p(addr pos);

_g void strvect_char_alloc(LocalRoot root, addr *ret, const char *arg);
_g void strvect_char_local(LocalRoot root, addr *ret, const char *arg);
_g void strvect_char_heap(addr *ret, const char *arg);
_g addr stringh(const char *arg); /* for debug */

_g void strvect_char1_alloc(LocalRoot local, addr *ret, const char *arg, unicode c);
_g void strvect_char1_local(LocalRoot local, addr *ret, const char *arg, unicode c);
_g void strvect_char1_heap(addr *ret, const char *arg, unicode c);

_g void strvect_size1_heap(addr *, const char *, size_t);
_g void strvect_size1_alloc(LocalRoot, addr *, const char *, size_t);
_g void strvect_size1_local(LocalRoot, addr *, const char *, size_t);

_g void strvect_sizeu_heap(addr *, const unicode *, size_t);
_g void strvect_sizeu_alloc(LocalRoot, addr *, const unicode *, size_t);
_g void strvect_sizeu_local(LocalRoot, addr *, const unicode *, size_t);

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

_g unicode strvect_refc(addr pos, size_t len);
_g void strvect_getc(addr pos, size_t len, unicode *c);
_g void strvect_setc(addr pos, size_t len, unicode c);
_g void strvect_setall(addr pos, unicode c);
_g void strvect_get(LocalRoot local, addr pos, size_t len, addr *ret);
_g void strvect_set(addr pos, size_t len, addr value);
_g void strvect_aref(LocalRoot local, addr pos, addr args, addr *ret);
_g void strvect_setf_aref(addr pos, addr args, addr value);
_g void strvect_fill(addr pos, addr item, addr start, addr end);
_g void strvect_subseq_index(addr *ret, addr pos, size_t index1, size_t index2);
_g void strvect_subseq(addr *ret, addr pos, addr start, addr end);
_g void strvect_setget(addr pos1, size_t index1, addr pos2, size_t index2);
_g void strvect_reverse(LocalRoot local, addr *ret, addr pos);
_g void strvect_nreverse(addr *ret, addr pos);

#define string_getdirect(body, index, value) \
	(*(value) = ((const unicode *)(body))[index])

#endif

