#include <wchar.h>
#include "array.h"
#include "c99.h"
#include "character.h"
#include "condition.h"
#include "encode.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "number.h"
#include "object.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "unicode.h"
#include "type_parse.h"

/*
 *  character
 */
_g addr make_character_allocr(LocalRoot local, unicode value)
{
	addr pos;
	alloc_body2(local, &pos, LISPTYPE_CHARACTER, sizeoft(unicode));
	setcharacter_unsafe(pos, value);
	return pos;
}

_g addr character_localr(LocalRoot local, unicode value)
{
	Check(local == NULL, "local error");
	return make_character_allocr(local, value);
}
_g addr character_heapr(unicode value)
{
	addr cache, pos;

	if (value < CHARACTER_CACHE) {
		GetConst(CHARACTER_CACHE, &cache);
		Check(cache == Unbound, "Unbound error, (build_character).");
		GetArrayA4(cache, (size_t)value, &pos);
		if (pos != Nil) return pos;
		pos = make_character_allocr(NULL, value);
		SetArrayA4(cache, (size_t)value, pos);
		return pos;
	}
	return make_character_allocr(NULL, value);
}
_g addr character_allocr(LocalRoot local, unicode value)
{
	if (local)
		return character_localr(local, value);
	else
		return character_heapr(value);
}

_g void character_alloc(LocalRoot local, addr *ret, unicode value)
{
	*ret = character_allocr(local, value);
}
_g void character_local(LocalRoot local, addr *ret, unicode value)
{
	*ret = character_localr(local, value);
}
_g void character_heap(addr *ret, unicode value)
{
	*ret = character_heapr(value);
}

_g const unicode *ptrcharacter(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	return PtrCharacter_Low(pos);
}

_g unicode refcharacter(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	return RefCharacter_Low(pos);
}

_g void getcharacter(addr pos, unicode *value)
{
	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(pos, value);
}

_g enum CHARACTER_TYPE character_type(unicode u)
{
	if (isStandardType(u))
		return CHARACTER_TYPE_STANDARD;
	if (isBaseType(u))
		return CHARACTER_TYPE_BASE;
	if (isExtendedType(u))
		return CHARACTER_TYPE_EXTENDED;

	return CHARACTER_TYPE_INVALID;
}

_g void setcharacter_unsafe(addr pos, unicode value)
{
	enum CHARACTER_TYPE type;

	Check(GetType(pos) != LISPTYPE_CHARACTER, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	type = character_type(value);
	if (type == CHARACTER_TYPE_INVALID)
		fmte("Invaild character code.", NULL);
	SetCharacterType(pos, type);
	SetCharacter_Low(pos, value);
}

_g int standard_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isstandardtype(RefCharacter(pos));
}

_g int base_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isbasetype(RefCharacter(pos));
}

_g int extended_char_p(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER && isextendedtype(RefCharacter(pos));
}

_g int characterp(addr pos)
{
	return GetType(pos) == LISPTYPE_CHARACTER;
}

_g int unicode_equalp(unicode left, unicode right)
{
	return toUpperUnicode(left) == toUpperUnicode(right);
}

#define ReturnCompare(a, b) { \
	if ((a) < (b)) return -1; \
	if ((a) > (b)) return 1; \
	return 0; \
}

_g int unicode_comparep(unicode left, unicode right)
{
	left = toUpperUnicode(left);
	right = toUpperUnicode(right);
	ReturnCompare(left, right);
}

_g int character_equal(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);

	return a == b;
}

_g int character_equalp(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}

_g int character_compare(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);
	ReturnCompare(a, b);
}

_g int character_comparep(addr left, addr right)
{
	unicode a, b;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &a);
	GetCharacter_Low(right, &b);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	ReturnCompare(a, b);
}

_g int character_unicode_equal(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &u);

	return u == right;
}

_g int character_unicode_equalp(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &u);

	return toUpperUnicode(u) == toUpperUnicode(right);
}

_g int character_unicode_compare(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &u);
	ReturnCompare(u, right);
}

_g int character_unicode_comparep(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter_Low(left, &u);
	u = toUpperUnicode(u);
	right = toUpperUnicode(right);
	ReturnCompare(u, right);
}


/* character2 */
#define PtrCharacter2(x) ((unicode *)PtrBodyB2(x))
_g void character2_heap(addr *ret, unicode a, unicode b)
{
	addr pos;
	unicode *ptr;

	heap_body2(&pos, LISPSYSTEM_CHARACTER2, sizeoft(unicode) * 2);
	ptr = PtrCharacter2(pos);
	ptr[0] = a;
	ptr[1] = b;
	*ret = pos;
}

_g unicode refcharacter2a(addr pos)
{
	return PtrCharacter2(pos)[0];
}

_g unicode refcharacter2b(addr pos)
{
	return PtrCharacter2(pos)[1];
}

_g void getcharacter2a(addr pos, unicode *ret)
{
	*ret = PtrCharacter2(pos)[0];
}

_g void getcharacter2b(addr pos, unicode *ret)
{
	*ret = PtrCharacter2(pos)[1];
}

_g void setcharacter2a(addr pos, unicode value)
{
	PtrCharacter2(pos)[0] = value;
}

_g void setcharacter2b(addr pos, unicode value)
{
	PtrCharacter2(pos)[1] = value;
}


/*
 *  buffer compare
 */
#define equal_code(p1, p2, s1, s2) { \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		if ((unicode)p1[i] != (unicode)p2[i]) { \
			return 0; \
		} \
	} \
	return 1; \
}

#define equalp_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 != s2) return 0; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a != b) return 0; \
	} \
	return 1; \
}

#define compare_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

#define comparep_code(p1, p2, s1, s2) { \
	unicode a, b; \
	size_t i; \
	if (s1 < s2) return -1; \
	if (s1 > s2) return 1; \
	for (i = 0; i < s1; i++) { \
		a = p1[i]; \
		b = p2[i]; \
		a = toUpperUnicode(a); \
		b = toUpperUnicode(b); \
		if (a < b) return -1; \
		if (a > b) return 1; \
	} \
	return 0; \
}

_g int memu_equal(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 != s2) return 0;
	return memcmp(p1, p2, s1 * sizeoft(unicode)) == 0;
}

_g int memu_compare(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	if (s1 < s2) return -1;
	if (s1 > s2) return 1;
	return memcmp(p1, p2, s1 * sizeoft(unicode));
}

_g int memu_equalp(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

_g int memu_comparep(const unicode *p1, const unicode *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}

_g int memu1_equal(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equal_code(p1, p2, s1, s2);
}

_g int memu1_equalp(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	equalp_code(p1, p2, s1, s2);
}

_g int memu1_compare(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	compare_code(p1, p2, s1, s2);
}

_g int memu1_comparep(const unicode *p1, const byte *p2, size_t s1, size_t s2)
{
	comparep_code(p1, p2, s1, s2);
}


/*
 *  strvect
 */
_g addr strvect_allocr(LocalRoot local, size_t len)
{
	addr pos;

	pos = allocr_body(local, LISPTYPE_STRING, StringBodyLength(len));
	SetCharacterType(pos, CHARACTER_TYPE_EMPTY);
	SetStringSize(pos, len);

	return pos;
}
_g addr strvect_localr(LocalRoot local, size_t len)
{
	Check(local == NULL, "local error");
	return strvect_allocr(local, len);
}
_g addr strvect_heapr(size_t len)
{
	return strvect_allocr(NULL, len);
}
_g void strvect_alloc(LocalRoot local, addr *ret, size_t len)
{
	*ret = strvect_allocr(local, len);
}
_g void strvect_local(LocalRoot local, addr *ret, size_t len)
{
	Check(local == NULL, "local error");
	*ret = strvect_allocr(local, len);
}
_g void strvect_heap(addr *ret, size_t len)
{
	*ret = strvect_allocr(NULL, len);
}

_g addr strvect_character_allocr(LocalRoot local, addr pos)
{
	unicode u;
	GetCharacter(pos, &u);
	return strvect_sizeu_allocr(local, &u, 1);
}
_g addr strvect_character_localr(LocalRoot local, addr pos)
{
	Check(local == NULL, "local error");
	return strvect_character_allocr(local, pos);
}
_g addr strvect_character_heapr(addr pos)
{
	return strvect_character_allocr(NULL, pos);
}
_g void strvect_character_alloc(LocalRoot local, addr *ret, addr pos)
{
	*ret = strvect_character_allocr(local, pos);
}
_g void strvect_character_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	*ret = strvect_character_allocr(local, pos);
}
_g void strvect_character_heap(addr *ret, addr pos)
{
	*ret = strvect_character_allocr(NULL, pos);
}

_g void strvect_length(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type left error");
	GetStringSize(pos, ret);
}

_g void strvect_posbodylen(addr pos, const unicode **body, size_t *len)
{
	Check(GetType(pos) != LISPTYPE_STRING, "type error");
	GetStringSize(pos, len);
	GetStringUnicode(pos, body);
}

_g enum CHARACTER_TYPE unicode_character_type(enum CHARACTER_TYPE type, unicode u)
{
	if (type == CHARACTER_TYPE_EMPTY) {
		return character_type(u);
	}
	if (isStandardType(u)) {
		return type;
	}
	if (isBaseType(u)) {
		if (type == CHARACTER_TYPE_STANDARD)
			return CHARACTER_TYPE_BASE;
		return type;
	}
	if (isExtendedType(u)) {
		if (type != CHARACTER_TYPE_INVALID && type != CHARACTER_TYPE_EXTENDED)
			return CHARACTER_TYPE_EXTENDED;
		return type;
	}
	return CHARACTER_TYPE_INVALID;
}

_g void strvect_update_character_type(addr pos)
{
	enum CHARACTER_TYPE type;
	const unicode *body;
	size_t i, size;

	strvect_posbodylen(pos, &body, &size);
	type = CHARACTER_TYPE_EMPTY;
	for (i = 0; i < size; i++) {
		type = unicode_character_type(type, body[i]);
		if (type == CHARACTER_TYPE_INVALID)
			fmte("Invalid character code.", NULL);
	}
	SetCharacterType(pos, type);
}

_g int strvectp(addr pos)
{
	return GetType(pos) == LISPTYPE_STRING;
}

_g int strvect_base_p(addr pos)
{
	enum CHARACTER_TYPE type;

	if (! strvectp(pos))
		return 0;
	strvect_update_character_type(pos);
	GetCharacterType(pos, &type);
	switch (type) {
		case CHARACTER_TYPE_EMPTY:
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			return 1;

		default:
			return 0;
	}
}

_g int strvect_simple_p(addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	return 0;
}

_g addr strvect_char_allocr(LocalRoot local, const char *arg)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	strvect_update_character_type(pos);

	return pos;
}
_g addr strvect_char_localr(LocalRoot local, const char *arg)
{
	Check(local == NULL, "local error");
	return strvect_char_allocr(local, arg);
}
_g addr strvect_char_heapr(const char *arg)
{
	return strvect_char_allocr(NULL, arg);
}
_g void strvect_char_alloc(LocalRoot local, addr *ret, const char *arg)
{
	*ret = strvect_char_allocr(local, arg);
}
_g void strvect_char_local(LocalRoot local, addr *ret, const char *arg)
{
	Check(local == NULL, "local error");
	*ret = strvect_char_allocr(local, arg);
}
_g void strvect_char_heap(addr *ret, const char *arg)
{
	*ret = strvect_char_allocr(NULL, arg);
}

_g void strvect_char1_alloc(LocalRoot local, addr *ret, const char *arg, unicode c)
{
	addr pos;
	unicode *destroy;
	size_t size, i;

	size = strlen(arg);
	strvect_alloc(local, &pos, size + 1ULL);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	destroy[i] = c;
	strvect_update_character_type(pos);
	*ret = pos;
}
_g void strvect_char1_local(LocalRoot local, addr *ret, const char *arg, unicode c)
{
	Check(local == NULL, "local error");
	strvect_char1_alloc(local, ret, arg, c);
}
_g void strvect_char1_heap(addr *ret, const char *arg, unicode c)
{
	strvect_char1_alloc(NULL, ret, arg, c);
}

_g addr strvect_size1_allocr(LocalRoot local, const char *arg, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t i;

	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = (unicode)arg[i];
	strvect_update_character_type(pos);

	return pos;
}
_g addr strvect_size1_localr(LocalRoot local, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strvect_size1_allocr(local, arg, size);
}
_g addr strvect_size1_heapr(const char *arg, size_t size)
{
	return strvect_size1_allocr(NULL, arg, size);
}
_g void strvect_size1_alloc(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	*ret = strvect_size1_allocr(local, arg, size);
}
_g void strvect_size1_local(LocalRoot local, addr *ret, const char *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strvect_size1_allocr(local, arg, size);
}
_g void strvect_size1_heap(addr *ret, const char *arg, size_t size)
{
	*ret = strvect_size1_allocr(NULL, arg, size);
}

_g addr strvect_sizeu_allocr(LocalRoot local, const unicode *arg, size_t size)
{
	addr pos;
	unicode *destroy;

	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	memcpy(destroy, arg, sizeoft(unicode) * size);
	strvect_update_character_type(pos);

	return pos;
}
_g addr strvect_sizeu_localr(LocalRoot local, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	return strvect_sizeu_allocr(local, arg, size);
}
_g addr strvect_sizeu_heapr(const unicode *arg, size_t size)
{
	return strvect_sizeu_allocr(NULL, arg, size);
}
_g void strvect_sizeu_alloc(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	*ret = strvect_sizeu_allocr(local, arg, size);
}
_g void strvect_sizeu_local(LocalRoot local, addr *ret, const unicode *arg, size_t size)
{
	Check(local == NULL, "local error");
	*ret = strvect_sizeu_allocr(local, arg, size);
}
_g void strvect_sizeu_heap(addr *ret, const unicode *arg, size_t size)
{
	*ret = strvect_sizeu_allocr(NULL, arg, size);
}


/*
 *  strvect_equal
 */
_g int strvect_equal_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equal(body, right, size1, size2);
}

_g int strvect_equalp_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_equalp(body, right, size1, size2);
}

_g int strvect_equal_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equal(body1, (const byte *)body2, size1, size2);
}

_g int strvect_equalp_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_equalp(body1, (const byte *)body2, size1, size2);
}

_g int strvect_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equal_binary(left, body, size);
}

_g int strvect_equalp(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_equalp_binary(left, body, size);
}

_g int strvect_character_equal(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);

	return size == 1 && body[0] == RefCharacter(right);
}

_g int strvect_character_equalp(addr left, addr right)
{
	const unicode *body;
	unicode a, b;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_CHARACTER, "type right error");
	strvect_posbodylen(left, &body, &size);
	if (size != 1) return 0;
	a = body[0];
	GetCharacter(right, &b);

	return toUpperUnicode(a) == toUpperUnicode(b);
}


/*
 *  strvect_compare
 */
_g int strvect_compare_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_compare(body, right, size1, size2);
}

_g int strvect_comparep_binary(addr left, const unicode *right, size_t size2)
{
	const unicode *body;
	size_t size1;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body, &size1);

	return memu_comparep(body, right, size1, size2);
}

_g int strvect_compare_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_compare(body1, (const byte *)body2, size1, size2);
}

_g int strvect_comparep_char(addr left, const char *body2)
{
	const unicode *body1;
	size_t size1, size2;

	Check(GetType(left) != LISPTYPE_STRING, "type error");
	strvect_posbodylen(left, &body1, &size1);
	size2 = strlen(body2);

	return memu1_comparep(body1, (const byte *)body2, size1, size2);
}

_g int strvect_compare(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_compare_binary(left, body, size);
}

_g int strvect_comparep(addr left, addr right)
{
	const unicode *body;
	size_t size;

	Check(GetType(left) != LISPTYPE_STRING, "type left error");
	Check(GetType(right) != LISPTYPE_STRING, "type right error");
	strvect_posbodylen(right, &body, &size);

	return strvect_comparep_binary(left, body, size);
}


/*
 *  getc/setc
 */
_g unicode strvect_refc(addr pos, size_t index)
{
	const unicode *body;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif
	GetStringUnicode(pos, &body);

	return body[index];
}

_g void strvect_getc(addr pos, size_t index, unicode *c)
{
	*c = strvect_refc(pos, index);
}

_g void strvect_setc(addr pos, size_t index, unicode c)
{
	enum CHARACTER_TYPE type;
	unicode *destroy;
#ifdef LISP_DEBUG
	size_t size;
#endif

	Check(GetType(pos) != LISPTYPE_STRING, "type error");
#ifdef LISP_DEBUG
	strvect_length(pos, &size);
	Check(size <= index, "size error");
#endif

	type = unicode_character_type(RefCharacterType(pos), c);
	if (type == CHARACTER_TYPE_INVALID)
		fmte("Invalid character code.", NULL);
	SetCharacterType(pos, type);
	GetStringUnicode(pos, (const unicode **)&destroy);
	destroy[index] = c;
}

_g void strvect_setall(addr pos, unicode c)
{
	enum CHARACTER_TYPE type;
	unicode *destroy;
	size_t size, i;

	strvect_length(pos, &size);
	if (size == 0) return;
	type = character_type(c);
	if (type == CHARACTER_TYPE_INVALID)
		fmte("Invalid character code.", NULL);
	SetCharacterType(pos, type);
	GetStringUnicode(pos, (const unicode **)&destroy);
	for (i = 0; i < size; i++)
		destroy[i] = c;
}

_g void strvect_get(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode c;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	strvect_length(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(index), NULL);
	strvect_getc(pos, index, &c);
	character_alloc(local, ret, c);
}

_g void strvect_aref(LocalRoot local, addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (! consp(args))
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	if (getindex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	strvect_get(local, pos, index, ret);
}

_g void strvect_set(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	if (! characterp(value))
		fmte("SETF arg ~S must be a character type.", value, NULL);
	strvect_length(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(index), NULL);
	strvect_setc(pos, index, RefCharacter(value));
}

_g void strvect_setf_aref(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_STRING);
	if (GetStatusReadOnly(pos))
		fmte("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	if (getindex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	strvect_set(pos, index, value);
}

_g void strvect_fill(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	unicode c;

	/* argument */
	if (! characterp(item))
		fmte("FILL tem ~S must be a character type.", item, NULL);
	GetCharacter(item, &c);
	strvect_length(pos, &index1);
	sequence_start_end(start, end, index1, &index1, &index2);

	/* fill */
	for (; index1 < index2; index1++)
		strvect_setc(pos, index1, c);
	strvect_update_character_type(pos);
}

_g void strvect_subseq_index(addr *ret, addr pos, size_t index1, size_t index2)
{
	unicode *data1;
	const unicode *data2;
	addr root;
	size_t diff;

	Check(index2 < index1, "index error");
	diff = index2 - index1;
	strvect_heap(&root, diff);
	GetStringUnicode(root, &data1);
	GetStringUnicode(pos, &data2);
	memcpy(data1, data2 + index1, diff * sizeoft(unicode));
	strvect_update_character_type(pos);
	*ret = root;
}

_g void strvect_subseq(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;

	strvect_length(pos, &index1);
	sequence_start_end(start, end, index1, &index1, &index2);
	strvect_subseq_index(ret, pos, index1, index2);
}

_g void strvect_setget(addr pos1, size_t index1, addr pos2, size_t index2)
{
	unicode value;

	strvect_getc(pos2, index2, &value);
	strvect_setc(pos1, index1, value);
}

_g void strvect_adjust(addr *ret, addr array, size_t size, addr value, addr check)
{
	unicode temp, defvalue;
	addr pos;
	size_t i, arraysize;

	if (value != Unbound) {
		if (! characterp(value))
			fmte("string :initial-value ~S must be a character type.", value, NULL);
		GetCharacter(value, &defvalue);
	}
	strvect_heap(&pos, size);
	if (check == Unbound) {
		strvect_length(array, &arraysize);
		for (i = 0; i < size; i++) {
			if (i < arraysize)
				strvect_getc(array, i, &temp);
			else {
				if (value == Unbound) break;
				temp = defvalue;
			}
			strvect_setc(pos, i, temp);
		}
	}
	*ret = pos;
}

_g void strvect_reverse(LocalRoot local, addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, x, y;

	strvect_length(pos, &size);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		strvect_getc(pos, x, &c);
		strvect_setc(one, y, c);
	}
	*ret = one;
}

_g void strvect_nreverse(addr *ret, addr pos)
{
	unicode a, b;
	size_t size, x, y;

	strvect_length(pos, &size);
	if (size <= 1) return;
	x = 0;
	y = size - 1;
	while (x < y) {
		strvect_getc(pos, x, &a);
		strvect_getc(pos, y, &b);
		strvect_setc(pos, x, b);
		strvect_setc(pos, y, a);
		x++;
		y--;
	}
	*ret = pos;
}


/* string utf16 object */
_g addr string8_size_allocr(LocalRoot local, const char *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF8_size_strlen((const byte *)name, size, &allsize))
		fmte("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_size_makeunicode(destroy, (const byte *)name, size))
		fmte("UTF8 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string8_size_localr(LocalRoot local, const char *name, size_t size)
{
	Check(local == NULL, "local error");
	return string8_size_allocr(local, name, size);
}
_g addr string8_size_heapr(const char *name, size_t size)
{
	return string8_size_allocr(NULL, name, size);
}
_g void string8_size_alloc(LocalRoot local, addr *ret, const char *name, size_t size)
{
	*ret = string8_size_allocr(local, name, size);
}
_g void string8_size_local(LocalRoot local, addr *ret, const char *name, size_t size)
{
	Check(local == NULL, "local error");
	*ret = string8_size_allocr(local, name, size);
}
_g void string8_size_heap(addr *ret, const char *name, size_t size)
{
	*ret = string8_size_allocr(NULL, name, size);
}

_g addr string8_null_allocr(LocalRoot local, const char *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF8_null_strlen((const byte *)name, &size))
		fmte("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_null_makeunicode(destroy, (const byte *)name))
		fmte("UTF8 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string8_null_localr(LocalRoot local, const char *name)
{
	Check(local == NULL, "local error");
	return string8_null_allocr(local, name);
}
_g addr string8_null_heapr(const char *name)
{
	return string8_null_allocr(NULL, name);
}
_g void string8_null_alloc(LocalRoot local, addr *ret, const char *name)
{
	*ret = string8_null_allocr(local, name);
}
_g void string8_null_local(LocalRoot local, addr *ret, const char *name)
{
	Check(local == NULL, "local error");
	*ret = string8_null_allocr(local, name);
}
_g void string8_null_heap(addr *ret, const char *name)
{
	*ret = string8_null_allocr(NULL, name);
}

_g addr string16_size_allocr(LocalRoot local, const byte16 *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF16_size_strlen(name, size, &allsize))
		fmte("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_size_makeunicode(destroy, name, size))
		fmte("UTF16 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string16_size_localr(LocalRoot local, const byte16 *name, size_t size)
{
	Check(local == NULL, "local error");
	return string16_size_allocr(local, name, size);
}
_g addr string16_size_heapr(const byte16 *name, size_t size)
{
	return string16_size_allocr(NULL, name, size);
}
_g void string16_size_alloc(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	*ret = string16_size_allocr(local, name, size);
}
_g void string16_size_local(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	Check(local == NULL, "local error");
	*ret = string16_size_allocr(local, name, size);
}
_g void string16_size_heap(addr *ret, const byte16 *name, size_t size)
{
	*ret = string16_size_allocr(NULL, name, size);
}

_g addr string16_null_allocr(LocalRoot local, const byte16 *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF16_null_strlen(name, &size))
		fmte("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_null_makeunicode(destroy, name))
		fmte("UTF16 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string16_null_localr(LocalRoot local, const byte16 *name)
{
	Check(local == NULL, "local error");
	return string16_null_allocr(local, name);
}
_g addr string16_null_heapr(const byte16 *name)
{
	return string16_null_allocr(NULL, name);
}
_g void string16_null_alloc(LocalRoot local, addr *ret, const byte16 *name)
{
	*ret = string16_null_allocr(local, name);
}
_g void string16_null_local(LocalRoot local, addr *ret, const byte16 *name)
{
	Check(local == NULL, "local error");
	*ret = string16_null_allocr(local, name);
}
_g void string16_null_heap(addr *ret, const byte16 *name)
{
	*ret = string16_null_allocr(NULL, name);
}

