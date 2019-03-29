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
addr make_character_allocr(LocalRoot local, unicode value);
addr character_allocr(LocalRoot root, unicode value);
addr character_localr(LocalRoot root, unicode value);
addr character_heapr(unicode value);
void character_alloc(LocalRoot root, addr *ret, unicode value);
void character_local(LocalRoot root, addr *ret, unicode value);
void character_heap(addr *ret, unicode value);

enum CHARACTER_TYPE character_type(unicode u);
const unicode *ptrcharacter(addr pos);
unicode refcharacter(addr pos);
void getcharacter(addr pos, unicode *value);
void setcharacter_unsafe(addr pos, unicode value);
int standard_char_p(addr pos);
int base_char_p(addr pos);
int extended_char_p(addr pos);
int characterp(addr pos);

int unicode_equalp(unicode left, unicode right);
int unicode_comparep(unicode left, unicode right);
int character_equal(addr left, addr right);
int character_equalp(addr left, addr right);
int character_compare(addr left, addr right);
int character_comparep(addr left, addr right);
int character_unicode_equal(addr left, unicode right);
int character_unicode_equalp(addr left, unicode right);
int character_unicode_compare(addr left, unicode right);
int character_unicode_comparep(addr left, unicode right);

/* character2 */
void character2_heap(addr *ret, unicode a, unicode b);
unicode refcharacter2a(addr pos);
unicode refcharacter2b(addr pos);
void getcharacter2a(addr pos, unicode *ret);
void getcharacter2b(addr pos, unicode *ret);
void setcharacter2a(addr pos, unicode value);
void setcharacter2b(addr pos, unicode value);

/* string */
addr strvect_allocr(LocalRoot root, size_t len);
addr strvect_localr(LocalRoot root, size_t len);
addr strvect_heapr(size_t len);
void strvect_alloc(LocalRoot root, addr *ret, size_t len);
void strvect_local(LocalRoot root, addr *ret, size_t len);
void strvect_heap(addr *ret, size_t len);

addr strvect_character_allocr(LocalRoot local, addr pos);
addr strvect_character_localr(LocalRoot local, addr pos);
addr strvect_character_heapr(addr pos);
void strvect_character_alloc(LocalRoot local, addr *ret, addr pos);
void strvect_character_local(LocalRoot local, addr *ret, addr pos);
void strvect_character_heap(addr *ret, addr pos);

void strvect_length(addr pos, size_t *ret);
void strvect_posbodylen(addr pos, const unicode **body, size_t *len);
enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u);
void strvect_update_character_type(addr pos);

int strvectp(addr pos);
int strvect_base_p(addr pos);
int strvect_simple_p(addr pos);

addr strvect_char_allocr(LocalRoot root, const char *arg);
addr strvect_char_localr(LocalRoot root, const char *arg);
addr strvect_char_heapr(const char *arg);
void strvect_char_alloc(LocalRoot root, addr *ret, const char *arg);
void strvect_char_local(LocalRoot root, addr *ret, const char *arg);
void strvect_char_heap(addr *ret, const char *arg);
void strvect_char1_alloc(LocalRoot local, addr *ret, const char *arg, unicode c);
void strvect_char1_local(LocalRoot local, addr *ret, const char *arg, unicode c);
void strvect_char1_heap(addr *ret, const char *arg, unicode c);

addr strvect_size1_heapr(const char *, size_t);
addr strvect_size1_allocr(LocalRoot, const char *, size_t);
addr strvect_size1_localr(LocalRoot, const char *, size_t);
void strvect_size1_heap(addr *, const char *, size_t);
void strvect_size1_alloc(LocalRoot, addr *, const char *, size_t);
void strvect_size1_local(LocalRoot, addr *, const char *, size_t);

addr strvect_sizeu_heapr(const unicode *, size_t);
addr strvect_sizeu_allocr(LocalRoot, const unicode *, size_t);
addr strvect_sizeu_localr(LocalRoot, const unicode *, size_t);
void strvect_sizeu_heap(addr *, const unicode *, size_t);
void strvect_sizeu_alloc(LocalRoot, addr *, const unicode *, size_t);
void strvect_sizeu_local(LocalRoot, addr *, const unicode *, size_t);

int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2);
int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2);
int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2);
int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2);
int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2);

int strvect_equal_binary(addr left, const unicode *right, size_t len);
int strvect_equalp_binary(addr left, const unicode *right, size_t len);
int strvect_equal_char(addr left, const char *right);
int strvect_equalp_char(addr left, const char *right);
int strvect_equal(addr left, addr right);
int strvect_equalp(addr left, addr right);
int strvect_character_equal(addr left, addr right);
int strvect_character_equalp(addr left, addr right);

int strvect_compare_binary(addr left, const unicode *right, size_t len);
int strvect_comparep_binary(addr left, const unicode *right, size_t len);
int strvect_compare_char(addr left, const char *right);
int strvect_comparep_char(addr left, const char *right);
int strvect_compare(addr left, addr right);
int strvect_comparep(addr left, addr right);

unicode strvect_refc(addr pos, size_t len);
void strvect_getc(addr pos, size_t len, unicode *c);
void strvect_setc(addr pos, size_t len, unicode c);
void strvect_setall(addr pos, unicode c);
void strvect_get(LocalRoot local, addr pos, size_t len, addr *ret);
void strvect_set(addr pos, size_t len, addr value);
void strvect_aref(LocalRoot local, addr pos, addr args, addr *ret);
void strvect_setf_aref(addr pos, addr args, addr value);
void strvect_fill(addr pos, addr item, addr start, addr end);
void strvect_subseq_index(addr *ret, addr pos, size_t index1, size_t index2);
void strvect_subseq(addr *ret, addr pos, addr start, addr end);
void strvect_setget(addr pos1, size_t index1, addr pos2, size_t index2);
void strvect_adjust(addr *ret, addr array, size_t size, addr value, addr check);
void strvect_reverse(LocalRoot local, addr *ret, addr pos);
void strvect_nreverse(addr *ret, addr pos);

/* string */
#define string_getdirect(body, index, value) \
	(*(value) = ((const unicode *)(body))[index])

/* string utf8,16 object */
addr string8_size_heapr(const char *name, size_t len);
addr string8_size_localr(LocalRoot local, const char *name, size_t len);
addr string8_size_allocr(LocalRoot local, const char *name, size_t len);
void string8_size_heap(addr *ret, const char *name, size_t len);
void string8_size_local(LocalRoot local, addr *ret, const char *name, size_t len);
void string8_size_alloc(LocalRoot local, addr *ret, const char *name, size_t len);

addr string8_null_heapr(const char *name);
addr string8_null_localr(LocalRoot local, const char *name);
addr string8_null_allocr(LocalRoot local, const char *name);
void string8_null_heap(addr *ret, const char *name);
void string8_null_local(LocalRoot local, addr *ret, const char *name);
void string8_null_alloc(LocalRoot local, addr *ret, const char *name);

addr string16_size_heapr(const byte16 *name, size_t len);
addr string16_size_localr(LocalRoot local, const byte16 *name, size_t len);
addr string16_size_allocr(LocalRoot local, const byte16 *name, size_t len);
void string16_size_heap(addr *ret, const byte16 *name, size_t len);
void string16_size_local(LocalRoot local, addr *ret, const byte16 *name, size_t len);
void string16_size_alloc(LocalRoot local, addr *ret, const byte16 *name, size_t len);

addr string16_null_heapr(const byte16 *name);
addr string16_null_localr(LocalRoot local, const byte16 *name);
addr string16_null_allocr(LocalRoot local, const byte16 *name);
void string16_null_heap(addr *ret, const byte16 *name);
void string16_null_local(LocalRoot local, addr *ret, const byte16 *name);
void string16_null_alloc(LocalRoot local, addr *ret, const byte16 *name);

#endif

