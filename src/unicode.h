#ifndef __UNICODE_HEADER__
#define __UNICODE_HEADER__

#include "local.h"
#include "typedef.h"

#define CHARACTER_CACHE		0x80

enum CHARACTER_TYPE {
	CHARACTER_TYPE_EMPTY,
	CHARACTER_TYPE_STANDARD,
	CHARACTER_TYPE_BASE,
	CHARACTER_TYPE_EXTENDED,
	CHARACTER_TYPE_INVALID,
	CHARACTER_TYPE_SIZE
};

/* macro */
#define PtrCharacter_Low(x)			((const unicode *)PtrBodyB2(x))
#define RefCharacter_Low(x)			(*PtrCharacter_Low(x))
#define GetCharacter_Low(x,v)		GetvBodyB2((x),unicode,(v))
#define SetCharacter_Low(x,v)		SetvBodyB2((x),unicode,(v))
/* debug */
#ifdef LISP_DEBUG
#define RefCharacter(x)				(*ptrcharacter(x))
#define GetCharacter(x,v)			getcharacter((x),(v))
#define SetCharacter_unsafe(x,v)	setcharacter_unsafe((x),(v))
/* release */
#else
#define RefCharacter(x)				RefCharacter_Low(x)
#define GetCharacter(x,v)			GetCharacter_Low(x,v)
#define SetCharacter_unsafe(x,v)	SetCharacter_Low(x,v)
#endif

#define StringBodyLength(x)			(IdxSize + sizeoft(unicode)*(x))
#define PtrStringBase(x)			posbodyr(x)
#define PtrStringSize(x)			((size_t *)PtrStringBase(x))
#define RefStringSize(x)			(*PtrStringSize(x))
#define GetStringSize(x,v)			(*(v) = RefStringSize(x))
#define SetStringSize(x,v)			(RefStringSize(x) = (v))
#define PtrStringUnicode(x)			((unicode *)(PtrStringBase(x) + IdxSize))
#define GetStringUnicode(x,v)		(*(v) = PtrStringUnicode(x))

#define RefCharacterType(x)			((enum CHARACTER_TYPE)GetUser(x))
#define GetCharacterType(x,v)		(*(v) = RefCharacterType(x))
#define SetCharacterType(x,v)		SetUser((x),(byte)(v))

/* character */
_g addr make_character_allocr(LocalRoot local, unicode value);
_g addr character_allocr(LocalRoot root, unicode value);
_g addr character_localr(LocalRoot root, unicode value);
_g addr character_heapr(unicode value);
_g void character_alloc(LocalRoot root, addr *ret, unicode value);
_g void character_local(LocalRoot root, addr *ret, unicode value);
_g void character_heap(addr *ret, unicode value);

_g enum CHARACTER_TYPE character_type(unicode u);
_g const unicode *ptrcharacter(addr pos);
_g unicode refcharacter(addr pos);
_g void getcharacter(addr pos, unicode *value);
_g void setcharacter_unsafe(addr pos, unicode value);
_g int standard_char_p(addr pos);
_g int base_char_p(addr pos);
_g int extended_char_p(addr pos);
_g int characterp(addr pos);

_g int unicode_equalp(unicode left, unicode right);
_g int unicode_comparep(unicode left, unicode right);
_g int character_equal(addr left, addr right);
_g int character_equalp(addr left, addr right);
_g int character_compare(addr left, addr right);
_g int character_comparep(addr left, addr right);
_g int character_unicode_equal(addr left, unicode right);
_g int character_unicode_equalp(addr left, unicode right);
_g int character_unicode_compare(addr left, unicode right);
_g int character_unicode_comparep(addr left, unicode right);

/* character2 */
_g void character2_heap(addr *ret, unicode a, unicode b);
_g unicode refcharacter2a(addr pos);
_g unicode refcharacter2b(addr pos);
_g void getcharacter2a(addr pos, unicode *ret);
_g void getcharacter2b(addr pos, unicode *ret);
_g void setcharacter2a(addr pos, unicode value);
_g void setcharacter2b(addr pos, unicode value);

/* string */
_g addr strvect_allocr(LocalRoot root, size_t len);
_g addr strvect_localr(LocalRoot root, size_t len);
_g addr strvect_heapr(size_t len);
_g void strvect_alloc(LocalRoot root, addr *ret, size_t len);
_g void strvect_local(LocalRoot root, addr *ret, size_t len);
_g void strvect_heap(addr *ret, size_t len);

_g addr strvect_character_allocr(LocalRoot local, addr pos);
_g addr strvect_character_localr(LocalRoot local, addr pos);
_g addr strvect_character_heapr(addr pos);
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

_g addr strvect_char_allocr(LocalRoot root, const char *arg);
_g addr strvect_char_localr(LocalRoot root, const char *arg);
_g addr strvect_char_heapr(const char *arg);
_g void strvect_char_alloc(LocalRoot root, addr *ret, const char *arg);
_g void strvect_char_local(LocalRoot root, addr *ret, const char *arg);
_g void strvect_char_heap(addr *ret, const char *arg);
_g void strvect_char1_alloc(LocalRoot local, addr *ret, const char *arg, unicode c);
_g void strvect_char1_local(LocalRoot local, addr *ret, const char *arg, unicode c);
_g void strvect_char1_heap(addr *ret, const char *arg, unicode c);

_g addr strvect_size1_heapr(const char *, size_t);
_g addr strvect_size1_allocr(LocalRoot, const char *, size_t);
_g addr strvect_size1_localr(LocalRoot, const char *, size_t);
_g void strvect_size1_heap(addr *, const char *, size_t);
_g void strvect_size1_alloc(LocalRoot, addr *, const char *, size_t);
_g void strvect_size1_local(LocalRoot, addr *, const char *, size_t);

_g addr strvect_sizeu_heapr(const unicode *, size_t);
_g addr strvect_sizeu_allocr(LocalRoot, const unicode *, size_t);
_g addr strvect_sizeu_localr(LocalRoot, const unicode *, size_t);
_g void strvect_sizeu_heap(addr *, const unicode *, size_t);
_g void strvect_sizeu_alloc(LocalRoot, addr *, const unicode *, size_t);
_g void strvect_sizeu_local(LocalRoot, addr *, const unicode *, size_t);

_g int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
_g int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
_g int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
_g int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
_g int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2);
_g int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2);
_g int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2);
_g int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2);

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

/* string */
#define string_getdirect(body, index, value) \
	(*(value) = ((const unicode *)(body))[index])

/* string utf8,16 object */
_g addr string8_size_heapr(const char *name, size_t len);
_g addr string8_size_localr(LocalRoot local, const char *name, size_t len);
_g addr string8_size_allocr(LocalRoot local, const char *name, size_t len);
_g void string8_size_heap(addr *ret, const char *name, size_t len);
_g void string8_size_local(LocalRoot local, addr *ret, const char *name, size_t len);
_g void string8_size_alloc(LocalRoot local, addr *ret, const char *name, size_t len);

_g addr string8_null_heapr(const char *name);
_g addr string8_null_localr(LocalRoot local, const char *name);
_g addr string8_null_allocr(LocalRoot local, const char *name);
_g void string8_null_heap(addr *ret, const char *name);
_g void string8_null_local(LocalRoot local, addr *ret, const char *name);
_g void string8_null_alloc(LocalRoot local, addr *ret, const char *name);

_g addr string16_size_heapr(const byte16 *name, size_t len);
_g addr string16_size_localr(LocalRoot local, const byte16 *name, size_t len);
_g addr string16_size_allocr(LocalRoot local, const byte16 *name, size_t len);
_g void string16_size_heap(addr *ret, const byte16 *name, size_t len);
_g void string16_size_local(LocalRoot local, addr *ret, const byte16 *name, size_t len);
_g void string16_size_alloc(LocalRoot local, addr *ret, const byte16 *name, size_t len);

_g addr string16_null_heapr(const byte16 *name);
_g addr string16_null_localr(LocalRoot local, const byte16 *name);
_g addr string16_null_allocr(LocalRoot local, const byte16 *name);
_g void string16_null_heap(addr *ret, const byte16 *name);
_g void string16_null_local(LocalRoot local, addr *ret, const byte16 *name);
_g void string16_null_alloc(LocalRoot local, addr *ret, const byte16 *name);

#endif

