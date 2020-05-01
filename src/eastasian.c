#include "character.h"
#include "condition.h"
#include "constant.h"
#include "eastasian.h"
#include "eastasian_unicode.h"
#include "integer.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  length
 */
_g int eastasian_length(addr pos, size_t *ret)
{
	int check;
	unicode c;
	unsigned v;
	size_t size, i, count;

	string_length(pos, &size);
	check = 0;
	count = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (UnicodeCount <= c) {
			check = 1;
			continue;
		}
		v = eastasian_width(c);
		if (v == 0) {
			check = 1;
			continue;
		}
		count += (size_t)v;
	}
	*ret = count;

	return check;
}


/*
 *  syscall
 */
static enum EastAsianType eastasian_type(addr pos)
{
	if (characterp(pos)) {
		if (character_equalp_unicode(pos, 'N')) return EastAsian_N;
		if (character_equalp_unicode(pos, 'A')) return EastAsian_A;
		if (character_equalp_unicode(pos, 'H')) return EastAsian_H;
		if (character_equalp_unicode(pos, 'W')) return EastAsian_W;
		if (character_equalp_unicode(pos, 'F')) return EastAsian_F;
		return EastAsian_error;
	}
	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (stringp(pos)) {
		if (string_equalp_char(pos, "N")) return EastAsian_N;
		if (string_equalp_char(pos, "A")) return EastAsian_A;
		if (string_equalp_char(pos, "H")) return EastAsian_H;
		if (string_equalp_char(pos, "W")) return EastAsian_W;
		if (string_equalp_char(pos, "F")) return EastAsian_F;
		if (string_equalp_char(pos, "NA")) return EastAsian_NA;
		return EastAsian_error;
	}

	return EastAsian_error;
}

_g void eastasian_set_syscall(addr pos, addr value, addr errorp, addr *ret)
{
	enum EastAsianType type;
	size_t size;

	/* value */
	if (GetIndex_fixnum(value, &size)) {
		if (errorp)
			fmte("Invalid integer value ~S.", value, NULL);
		*ret = Nil;
		return;
	}
	if (UINT_MAX <= size) {
		if (errorp)
			fmte("The value ~S is too large.", value, NULL);
		*ret = Nil;
		return;
	}

	/* set */
	type = eastasian_type(pos);
	switch (type) {
		case EastAsian_N:
		case EastAsian_A:
		case EastAsian_H:
		case EastAsian_W:
		case EastAsian_F:
		case EastAsian_NA:
			EastAsianSymbol[type] = (unsigned)size;
			break;

		default:
			if (errorp)
				fmte("Inavlid eastasian type ~S.", pos, NULL);
			*ret = Nil;
			return;
	}
	*ret = T;
}

static void eastasian_system_symbol(enum EastAsianType type, addr *ret)
{
	constindex index;

	switch (type) {
		case EastAsian_N: index = CONSTANT_SYSTEM_N; break;
		case EastAsian_A: index = CONSTANT_SYSTEM_A; break;
		case EastAsian_H: index = CONSTANT_SYSTEM_H; break;
		case EastAsian_W: index = CONSTANT_SYSTEM_W; break;
		case EastAsian_F: index = CONSTANT_SYSTEM_F; break;
		case EastAsian_NA: index = CONSTANT_SYSTEM_NA; break;
		default: index = CONSTANT_EMPTY; break;
	}
	if (index == CONSTANT_EMPTY)
		*ret = Nil;
	else
		GetConstant(index, ret);
}

_g void eastasian_get_syscall(addr pos, addr *retsize, addr *retsymbol)
{
	enum EastAsianType type;

	type = eastasian_type(pos);
	eastasian_system_symbol(type, retsymbol);
	if (*retsymbol == Nil)
		fixnum_heap(retsize, 0);
	else
		fixnum_heap(retsize, (fixnum)EastAsianSymbol[type]);
}

static void eastasian_width_character(addr pos, addr *ret, addr *retbool)
{
	unicode c;
	unsigned v;

	GetCharacter(pos, &c);
	if (UnicodeCount <= c)
		goto error;
	v = eastasian_width(c);
	if (v == 0)
		goto error;
	fixnum_heap(ret, (fixnum)v);
	*retbool = T;
	return;

error:
	fixnum_heap(ret, 0);
	*retbool = Nil;
}

static void eastasian_width_integer(addr pos, addr *ret, addr *retbool)
{
	unsigned v;
	size_t size;

	if (GetIndex_integer(pos, &size))
		goto error;
	if (UnicodeCount <= size)
		goto error;
	v = eastasian_width((unicode)size);
	if (v == 0)
		goto error;
	fixnum_heap(ret, (fixnum)v);
	*retbool = T;
	return;

error:
	fixnum_heap(ret, 0);
	*retbool = Nil;
}

static void eastasian_width_string(addr pos, addr *ret, addr *retbool)
{
	int check;
	size_t size;

	check = eastasian_length(pos, &size);
	make_index_integer_heap(ret, size);
	*retbool = check? Nil: T;
}

_g void eastasian_width_syscall(addr pos, addr *ret, addr *retbool)
{
	if (characterp(pos)) {
		eastasian_width_character(pos, ret, retbool);
		return;
	}
	if (integerp(pos)) {
		eastasian_width_integer(pos, ret, retbool);
		return;
	}
	if (stringp(pos)) {
		eastasian_width_string(pos, ret, retbool);
		return;
	}

	/* error */
	fixnum_heap(ret, 0);
	*retbool = Nil;
}

