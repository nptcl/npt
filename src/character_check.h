#ifndef __CHARACTER_CHECK_HEADER__
#define __CHARACTER_CHECK_HEADER__

#include "typedef.h"

#define IntegerBaseMax		36
#define isBaseChar(x)       (2 <= (x) && (x) <= IntegerBaseMax)
#define isUpperCase(x)      ('A' <= (x) && (x) <= 'Z')
#define isLowerCase(x)      ('a' <= (x) && (x) <= 'z')
#define isDigitCase(x)      ('0' <= (x) && (x) <= '9')
#define isAlphabetic(x)     (isUpperCase(x) || isLowerCase(x))
#define isAlphanumeric(x)   (isAlphabetic(x) || isDigitCase(x))
#define _isGraphUnicode(x)	(0x21 <= (x) && (x) <= 0x7E)
#define isSpaceUnicode(x) \
	(((x) == ' ') || ((x) == '\t') || ((x) == '\n') || \
	 ((x) == '\v') || ((x) == '\f') || ((x) == '\r'))
#define toUpperUnicode(x)   (isLowerCase(x)? ((x)-'a'+'A'): (x))
#define toLowerUnicode(x)   (isUpperCase(x)? ((x)-'A'+'a'): (x))

#define UnicodeCount		((1UL + 16UL) * 0x010000UL)
#define isSurrogatePair(x)  (0xD800 <= (x) && (x) <= 0xDFFF)
#define isBaseRange(x)		((x) < UnicodeCount)
#define isStandardType(x)   (((x)==0x0A) || (0x20<=(x) && (x)<=0x7E))
#define isBaseType(x)		(isBaseRange(x) && (! isSurrogatePair(x)))
#define isExtendedType(x)	(0x80000000UL <= (x))

/* character check */
_g int isbasechar(unicode x);
_g int isuppercase(unicode x);
_g int islowercase(unicode x);
_g int isdigitcase(unicode x);
_g int isalphabetic(unicode x);
_g int isalphanumeric(unicode x);
_g int isgraphunicode(unicode x);
_g int isspaceunicode(unicode x);
_g unicode toupperunicode(unicode x);
_g unicode tolowerunicode(unicode x);

/* character type */
_g int issurrogatepair(unicode x);
_g int isbaserange(unicode x);
_g int isstandardtype(unicode x);
_g int isbasetype(unicode x);
_g int isextendedtype(unicode x);

#endif

