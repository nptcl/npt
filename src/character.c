#include "build.h"
#include "character.h"
#include "character_name.h"
#include "condition.h"
#include "constant.h"
#include "heap.h"
#include "memory.h"
#include "strtype.h"

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

	if (value < LISP_CHARACTER_CACHE) {
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


/* equal */
_g int character_equal_unicode(addr left, unicode right)
{
	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	return RefCharacter(left) == right;
}

_g int character_equalp_unicode(addr left, unicode right)
{
	unicode u;

	Check(GetType(left) != LISPTYPE_CHARACTER, "type error");
	GetCharacter(left, &u);
	return toUpperUnicode(u) == toUpperUnicode(right);
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


_g int character2_equal_unicode(addr left, unicode a, unicode b)
{
	CheckType(left, LISPSYSTEM_CHARACTER2);
	return refcharacter2a(left) == a && refcharacter2b(left) == b;
}

_g int character2_equalp_unicode(addr left, unicode a, unicode b)
{
	unicode c, d;

	CheckType(left, LISPSYSTEM_CHARACTER2);
	getcharacter2a(left, &c);
	getcharacter2b(left, &d);
	a = toUpperUnicode(a);
	b = toUpperUnicode(b);
	c = toUpperUnicode(c);
	d = toUpperUnicode(d);
	return a == c && b == d;
}


/*
 *  character table
 */
static void build_character_cache(void)
{
	addr pos;

	/* character cache */
	heap_array4(&pos, LISPSYSTEM_CHARACTER_CACHE, LISP_CHARACTER_CACHE);
	SetConst(CHARACTER_CACHE, pos);
}

_g void build_character(void)
{
	build_character_cache();
	build_character_name();
}

