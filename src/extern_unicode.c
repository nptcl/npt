#include "condition.h"
#include "eastasian.h"
#include "eastasian_unicode.h"
#include "encode_unicode.h"
#include "extern_unicode.h"
#include "strtype.h"

/*
 *  eastasian
 */
static enum EastAsianType lisp_eastasian_todevelop(enum LispEastAsianType type)
{
	switch (type) {
		case LispEastAsianType_N:
			return EastAsian_N;

		case LispEastAsianType_A:
			return EastAsian_A;

		case LispEastAsianType_H:
			return EastAsian_H;

		case LispEastAsianType_W:
			return EastAsian_W;

		case LispEastAsianType_F:
			return EastAsian_F;

		case LispEastAsianType_NA:
			return EastAsian_NA;

		default:
			fmte("Invalid EastAsianType.", NULL);
			return EastAsian_error;
	}
}

void lisp_eastasian_set(enum LispEastAsianType type, unsigned width)
{
	enum EastAsianType index = lisp_eastasian_todevelop(type);
	EastAsianSymbol[index] = width;
}

unsigned lisp_eastasian_get(enum LispEastAsianType type)
{
	enum EastAsianType index = lisp_eastasian_todevelop(type);
	return EastAsianSymbol[index];
}

static enum LispEastAsianType lisp_eastasian_toextern(enum EastAsianType type)
{
	switch (type) {
		case EastAsian_N:
			return LispEastAsianType_N;

		case EastAsian_A:
			return LispEastAsianType_A;

		case EastAsian_H:
			return LispEastAsianType_H;

		case EastAsian_W:
			return LispEastAsianType_W;

		case EastAsian_F:
			return LispEastAsianType_F;

		case EastAsian_NA:
			return LispEastAsianType_NA;

		default:
			return LispEastAsianType_error;
	}
}

enum LispEastAsianType lisp_eastasian_type_unicode(unicode c)
{
	enum EastAsianType type = eastasian_symbol(c);
	return lisp_eastasian_toextern(type);
}

enum LispEastAsianType lisp_eastasian_type_character(addr value)
{
	unicode c;

	if (! characterp(value))
		fmte("Invalid character type ~S.", value, NULL);
	GetCharacter(value, &c);
	return lisp_eastasian_type_unicode(c);
}

unsigned lisp_eastasian_unicode(unicode c)
{
	return eastasian_width(c);
}

unsigned lisp_eastasian_character(addr value)
{
	unicode c;

	if (! characterp(value))
		fmte("Invalid character type ~S.", value, NULL);
	GetCharacter(value, &c);
	return eastasian_width(c);
}

int lisp_eastasian_string(addr value, size_t *ret)
{
	if (! stringp(value))
		fmte("Invalid string type ~S.", value, NULL);
	return eastasian_length(value, ret);
}

int lisp_eastasian_width(addr value, size_t *ret)
{
	unicode c;

	if (characterp(value)) {
		GetCharacter(value, &c);
		return eastasian_width(c);
	}
	if (stringp(value))
		return eastasian_length(value, ret);
	fmte("Invalid string type ~S.", value, NULL);
	return 0;
}


/* UTF-8 */
int lisp_utf8_encode(unicode c, void *ptr, size_t *ret)
{
	return encode_utf8(c, ptr, ret);
}

