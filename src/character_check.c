#include "character_check.h"

_g int isbasechar(unicode x)
{
	return isBaseChar(x);
}

_g int isuppercase(unicode x)
{
	return isUpperCase(x);
}

_g int islowercase(unicode x)
{
	return isLowerCase(x);
}

_g int isdigitcase(unicode x)
{
	return isDigitCase(x);
}

_g int isalphabetic(unicode x)
{
	return isAlphabetic(x);
}

_g int isalphanumeric(unicode x)
{
	return isAlphanumeric(x);
}

_g int isgraphunicode(unicode x)
{
	if (x < 0x80)
		return _isGraphUnicode(x);
	else
		return isBaseType(x);
}

_g int isspaceunicode(unicode x)
{
	return isSpaceUnicode(x);
}

_g unicode toupperunicode(unicode x)
{
	return toUpperUnicode(x);
}

_g unicode tolowerunicode(unicode x)
{
	return toLowerUnicode(x);
}


/*
 *  character type
 */
_g int issurrogatepair(unicode x)
{
	return isSurrogatePair(x);
}

_g int isbaserange(unicode x)
{
	return isBaseRange(x);
}

_g int isstandardtype(unicode x)
{
	return isStandardType(x);
}

_g int isbasetype(unicode x)
{
	return isBaseType(x);
}

_g int isextendedtype(unicode x)
{
	return isExtendedType(x);
}

