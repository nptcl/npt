#ifndef __CHARACTER_HEADER__
#define __CHARACTER_HEADER__

#include "character_check.h"
#include "local.h"
#include "typedef.h"

#define make_character_heap _n(make_character_heap)
#define character_alloc _n(character_alloc)
#define character_local _n(character_local)
#define character_heap _n(character_heap)
#define characterh _n(characterh)
#define character_unicode_heap _n(character_unicode_heap)
#define make_extended_char_heap_ _n(make_extended_char_heap_)
#define ptrcharacter _n(ptrcharacter)
#define refcharacter _n(refcharacter)
#define getcharacter _n(getcharacter)
#define setcharacter_unsafe _n(setcharacter_unsafe)
#define character_type _n(character_type)
#define ref_character_type _n(ref_character_type)
#define get_character_type _n(get_character_type)
#define isvalidunicode _n(isvalidunicode)
#define standard_char_p _n(standard_char_p)
#define base_char_p _n(base_char_p)
#define extended_char_p _n(extended_char_p)
#define characterp _n(characterp)
#define unicode_equalp _n(unicode_equalp)
#define unicode_comparep _n(unicode_comparep)
#define character_equal _n(character_equal)
#define character_equalp _n(character_equalp)
#define character_equal_char _n(character_equal_char)
#define character_equalp_char _n(character_equalp_char)
#define character_compare _n(character_compare)
#define character_comparep _n(character_comparep)
#define character_unicode_equal _n(character_unicode_equal)
#define character_unicode_equalp _n(character_unicode_equalp)
#define character_unicode_compare _n(character_unicode_compare)
#define character_unicode_comparep _n(character_unicode_comparep)
#define character_equal_unicode _n(character_equal_unicode)
#define character_equalp_unicode _n(character_equalp_unicode)
#define character2_heap _n(character2_heap)
#define refcharacter2a _n(refcharacter2a)
#define refcharacter2b _n(refcharacter2b)
#define getcharacter2a _n(getcharacter2a)
#define getcharacter2b _n(getcharacter2b)
#define setcharacter2a _n(setcharacter2a)
#define setcharacter2b _n(setcharacter2b)
#define character2_equal_unicode _n(character2_equal_unicode)
#define character2_equalp_unicode _n(character2_equalp_unicode)
#define build_character _n(build_character)

#define LISP_CHARACTER_CACHE		0x80

#define PtrCharacter_Low(x)			((const unicode *)PtrBodyB2(x))
#define RefCharacter_Low(x)			(*PtrCharacter_Low(x))
#define GetCharacter_Low(x,v)		GetvBodyB2((x),unicode,(v))
#define SetCharacter_Low(x,v)		SetvBodyB2((x),unicode,(v))

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
void make_character_heap(addr *ret, unicode value);
void character_alloc(LocalRoot root, addr *ret, unicode value);
void character_local(LocalRoot root, addr *ret, unicode value);
void character_heap(addr *ret, unicode value);
addr characterh(unicode value); /* for debug */
int character_unicode_heap(addr *ret, unicode c);
int make_extended_char_heap_(addr *ret, unicode c);

const unicode *ptrcharacter(addr pos);
unicode refcharacter(addr pos);
void getcharacter(addr pos, unicode *value);
void setcharacter_unsafe(addr pos, unicode value);
enum CHARACTER_TYPE character_type(unicode c);
enum CHARACTER_TYPE ref_character_type(addr pos);
void get_character_type(addr pos, enum CHARACTER_TYPE *ret);
int isvalidunicode(unicode c);
int standard_char_p(addr pos);
int base_char_p(addr pos);
int extended_char_p(addr pos);
int characterp(addr pos);

int unicode_equalp(unicode left, unicode right);
int unicode_comparep(unicode left, unicode right);
int character_equal(addr left, addr right);
int character_equalp(addr left, addr right);
int character_equal_char(addr left, const char *right);
int character_equalp_char(addr left, const char *right);
int character_compare(addr left, addr right);
int character_comparep(addr left, addr right);
int character_unicode_equal(addr left, unicode right);
int character_unicode_equalp(addr left, unicode right);
int character_unicode_compare(addr left, unicode right);
int character_unicode_comparep(addr left, unicode right);

int character_equal_unicode(addr left, unicode right);
int character_equalp_unicode(addr left, unicode right);

/* character2 */
void character2_heap(addr *ret, unicode a, unicode b);
unicode refcharacter2a(addr pos);
unicode refcharacter2b(addr pos);
void getcharacter2a(addr pos, unicode *ret);
void getcharacter2b(addr pos, unicode *ret);
void setcharacter2a(addr pos, unicode value);
void setcharacter2b(addr pos, unicode value);

int character2_equal_unicode(addr left, unicode a, unicode b);
int character2_equalp_unicode(addr left, unicode a, unicode b);

void build_character(void);

#endif

