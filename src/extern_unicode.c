#include "character.h"
#include "condition.h"
#include "eastasian.h"
#include "eastasian_unicode.h"
#include "encode.h"
#include "encode_unicode.h"
#include "extern_unicode.h"
#include "hold.h"
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
			return EastAsian_error;
	}
}

int lisp_eastasian_set(enum LispEastAsianType type, unsigned width)
{
	enum EastAsianType index;

	index = lisp_eastasian_todevelop(type);
	if (index == EastAsian_error)
		return 1;
	EastAsianSymbol[index] = width;
	return 0;
}

int lisp_eastasian_get(enum LispEastAsianType type, unsigned *ret)
{
	enum EastAsianType index;

	index = lisp_eastasian_todevelop(type);
	if (index == EastAsian_error)
		return 1;
	*ret = EastAsianSymbol[index];
	return 0;
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
	enum EastAsianType type;
	type = eastasian_symbol(c);
	return lisp_eastasian_toextern(type);
}

enum LispEastAsianType lisp_eastasian_type_character(addr value)
{
	unicode c;

	hold_value(value, &value);
	if (! characterp(value))
		return LispEastAsianType_error;

	GetCharacter(value, &c);
	return lisp_eastasian_type_unicode(c);
}

unsigned lisp_eastasian_unicode(unicode c)
{
	return eastasian_width(c);
}

int lisp_eastasian_character_(addr value, unsigned *ret)
{
	unicode c;

	hold_value(value, &value);
	if (! characterp(value))
		return fmte_("Invalid character type ~S.", value, NULL);
	GetCharacter(value, &c);
	return Result(ret, eastasian_width(c));
}

int lisp_eastasian_string_(addr value, size_t *ret)
{
	hold_value(value, &value);
	if (! stringp(value))
		return fmte_("Invalid string type ~S.", value, NULL);
	return eastasian_length_(value, ret, NULL);
}

int lisp_eastasian_width_(addr value, size_t *ret)
{
	unicode c;

	hold_value(value, &value);
	if (characterp(value)) {
		GetCharacter(value, &c);
		return Result(ret, eastasian_width(c));
	}
	if (stringp(value))
		return eastasian_length_(value, ret, NULL);

	return fmte_("Invalid string type ~S.", value, NULL);
}


/* unicode */
int lisp_unicode_count(void)
{
	return UnicodeCount;
}


/* UTF-8 */
int lisp_utf8_encode(unicode c, void *ptr, size_t *ret)
{
	return encode_utf8(c, (byte *)ptr, ret);
}


/* UTF-16 */
int lisp_utf16_range(unicode c)
{
	return UTF16range(c);
}

int lisp_utf16_high(unicode c)
{
	return UTF16high(c);
}

int lisp_utf16_low(unicode c)
{
	return UTF16low(c);
}

unicode lisp_utf16_merge(byte16 high, byte16 low)
{
	return UTF16unicode(high, low);
}

