#ifndef __CHARACTER_CHECK_HEADER__
#define __CHARACTER_CHECK_HEADER__

#include "typedef.h"

#define isbasechar _n(isbasechar)
#define isuppercase _n(isuppercase)
#define islowercase _n(islowercase)
#define isdigitcase _n(isdigitcase)
#define isalphabetic _n(isalphabetic)
#define isalphanumeric _n(isalphanumeric)
#define isgraphunicode _n(isgraphunicode)
#define isspaceunicode _n(isspaceunicode)
#define toupperunicode _n(toupperunicode)
#define tolowerunicode _n(tolowerunicode)
#define issurrogatepair _n(issurrogatepair)
#define isbaserange _n(isbaserange)
#define isstandardtype _n(isstandardtype)
#define isbasetype _n(isbasetype)
#define isextendedtype _n(isextendedtype)

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
int isbasechar(unicode x);
int isuppercase(unicode x);
int islowercase(unicode x);
int isdigitcase(unicode x);
int isalphabetic(unicode x);
int isalphanumeric(unicode x);
int isgraphunicode(unicode x);
int isspaceunicode(unicode x);
unicode toupperunicode(unicode x);
unicode tolowerunicode(unicode x);

/* character type */
int issurrogatepair(unicode x);
int isbaserange(unicode x);
int isstandardtype(unicode x);
int isbasetype(unicode x);
int isextendedtype(unicode x);

#endif

