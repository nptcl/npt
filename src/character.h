#ifndef __CHARACTER_HEADER__
#define __CHARACTER_HEADER__

#include "character_check.h"
#include "local.h"
#include "typedef.h"

#define LISP_CHARACTER_CACHE		0x80

enum CHARACTER_TYPE {
	CHARACTER_TYPE_EMPTY,
	CHARACTER_TYPE_STANDARD,
	CHARACTER_TYPE_BASE,
	CHARACTER_TYPE_EXTENDED,
	CHARACTER_TYPE_INVALID,
	CHARACTER_TYPE_SIZE
};

#define PtrCharacter_Low(x)			((const unicode *)PtrBodyB2(x))
#define RefCharacter_Low(x)			(*PtrCharacter_Low(x))
#define GetCharacter_Low(x,v)		GetvBodyB2((x),unicode,(v))
#define SetCharacter_Low(x,v)		SetvBodyB2((x),unicode,(v))
#define RefCharacterType(x)			((enum CHARACTER_TYPE)GetUser(x))
#define GetCharacterType(x,v)		(*(v) = RefCharacterType(x))
#define SetCharacterType(x,v)		SetUser((x),(byte)(v))

#ifdef LISP_DEBUG
#define RefCharacter(x)				(*ptrcharacter(x))
#define GetCharacter(x,v)			getcharacter((x),(v))
#define SetCharacter_unsafe(x,v)	setcharacter_unsafe((x),(v))
#else
#define RefCharacter(x)				RefCharacter_Low(x)
#define GetCharacter(x,v)			GetCharacter_Low(x,v)
#define SetCharacter_unsafe(x,v)	SetCharacter_Low(x,v)
#endif

/* character */
_g void make_character_heap(addr *ret, unicode value);
_g void character_alloc(LocalRoot root, addr *ret, unicode value);
_g void character_local(LocalRoot root, addr *ret, unicode value);
_g void character_heap(addr *ret, unicode value);
_g addr characterh(unicode value); /* for debug */

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

_g int character_equal_unicode(addr left, unicode right);
_g int character_equalp_unicode(addr left, unicode right);

/* character2 */
_g void character2_heap(addr *ret, unicode a, unicode b);
_g unicode refcharacter2a(addr pos);
_g unicode refcharacter2b(addr pos);
_g void getcharacter2a(addr pos, unicode *ret);
_g void getcharacter2b(addr pos, unicode *ret);
_g void setcharacter2a(addr pos, unicode value);
_g void setcharacter2b(addr pos, unicode value);

_g int character2_equal_unicode(addr left, unicode a, unicode b);
_g int character2_equalp_unicode(addr left, unicode a, unicode b);

_g void build_character(void);

#endif

