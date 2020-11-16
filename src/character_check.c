#include "character_check.h"

/*
 *  character check
 */
int isbasechar(unicode x)
{
	return isBaseChar(x);
}

int isuppercase(unicode x)
{
	return isUpperCase(x);
}

int islowercase(unicode x)
{
	return isLowerCase(x);
}

int isdigitcase(unicode x)
{
	return isDigitCase(x);
}

int isalphabetic(unicode x)
{
	return isAlphabetic(x);
}

int isalphanumeric(unicode x)
{
	return isAlphanumeric(x);
}

int isgraphunicode(unicode x)
{
	if (x < 0x80)
		return _isGraphUnicode(x);
	else
		return isBaseType(x);
}

int isspaceunicode(unicode x)
{
	return isSpaceUnicode(x);
}

unicode toupperunicode(unicode x)
{
	return toUpperUnicode(x);
}

unicode tolowerunicode(unicode x)
{
	return toLowerUnicode(x);
}


/*
 *  character type
 */
int issurrogatepair(unicode x)
{
	return isSurrogatePair(x);
}

int isbaserange(unicode x)
{
	return isBaseRange(x);
}

int isstandardtype(unicode x)
{
	return isStandardType(x);
}

int isbasetype(unicode x)
{
	return isBaseType(x);
}

int isextendedtype(unicode x)
{
	return isExtendedType(x);
}

