#ifndef __STRVECT_HEADER__
#define __STRVECT_HEADER__

#include <stddef.h>
#include "local.h"
#include "character.h"
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

