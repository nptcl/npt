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
_g int eastasian_length_(addr pos, size_t *ret, int *rerrp)
{
	int errorp;
	unicode c;
	unsigned v;
	size_t size, i, count;

	string_length(pos, &size);
	errorp = 0;
	count = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (UnicodeCount <= c) {
			errorp = 1;
			continue;
		}
		v = eastasian_width(c);
		if (v == 0) {
			errorp = 1;
			continue;
		}
		count += (size_t)v;
	}
	*ret = count;
	if (rerrp)
		*rerrp = errorp;

	return 0;
}


/*
 *  syscall
 */
static int eastasian_type_(addr pos, enum EastAsianType *ret)
{
	int check;

	if (characterp(pos)) {
		if (character_equalp_unicode(pos, 'N'))
			return Result(ret, EastAsian_N);
		if (character_equalp_unicode(pos, 'A'))
			return Result(ret, EastAsian_A);
		if (character_equalp_unicode(pos, 'H'))
			return Result(ret, EastAsian_H);
		if (character_equalp_unicode(pos, 'W'))
			return Result(ret, EastAsian_W);
		if (character_equalp_unicode(pos, 'F'))
			return Result(ret, EastAsian_F);
		return Result(ret, EastAsian_error);
	}
	if (symbolp(pos))
		GetNameSymbol(pos, &pos);
	if (stringp(pos)) {
		Return(string_equalp_char_(pos, "N", &check));
		if (check)
			return Result(ret, EastAsian_N);
		Return(string_equalp_char_(pos, "A", &check));
		if (check)
			return Result(ret, EastAsian_A);
		Return(string_equalp_char_(pos, "H", &check));
		if (check)
			return Result(ret, EastAsian_H);
		Return(string_equalp_char_(pos, "W", &check));
		if (check)
			return Result(ret, EastAsian_W);
		Return(string_equalp_char_(pos, "F", &check));
		if (check)
			return Result(ret, EastAsian_F);
		Return(string_equalp_char_(pos, "NA", &check));
		if (check)
			return Result(ret, EastAsian_NA);

		return Result(ret, EastAsian_error);
	}

	return Result(ret, EastAsian_error);
}

_g int eastasian_set_syscall_(addr pos, addr value, addr errorp, addr *ret)
{
	enum EastAsianType type;
	size_t size;

	/* value */
	if (GetIndex_fixnum(value, &size)) {
		if (errorp)
			return fmte_("Invalid integer value ~S.", value, NULL);
		else
			return Result(ret, Nil);
	}
	if (UINT_MAX <= size) {
		if (errorp)
			return fmte_("The value ~S is too large.", value, NULL);
		else
			return Result(ret, Nil);
	}

	/* set */
	Return(eastasian_type_(pos, &type));
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
				return fmte_("Inavlid eastasian type ~S.", pos, NULL);
			else
				return Result(ret, Nil);
	}

	return Result(ret, T);
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

_g int eastasian_get_syscall_(addr pos, addr *retsize, addr *retsymbol)
{
	enum EastAsianType type;

	Return(eastasian_type_(pos, &type));
	eastasian_system_symbol(type, retsymbol);
	if (*retsymbol == Nil)
		fixnum_heap(retsize, 0);
	else
		fixnum_heap(retsize, (fixnum)EastAsianSymbol[type]);

	return 0;
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

static int eastasian_width_string_(addr pos, addr *ret, addr *retbool)
{
	int check;
	size_t size;

	Return(eastasian_length_(pos, &size, &check));
	make_index_integer_heap(ret, size);
	*retbool = check? Nil: T;

	return 0;
}

_g int eastasian_width_syscall_(addr pos, addr *ret, addr *retbool)
{
	if (characterp(pos)) {
		eastasian_width_character(pos, ret, retbool);
		return 0;
	}
	if (integerp(pos)) {
		eastasian_width_integer(pos, ret, retbool);
		return 0;
	}
	if (stringp(pos)) {
		return eastasian_width_string_(pos, ret, retbool);
	}

	/* error */
	fixnum_heap(ret, 0);
	*retbool = Nil;

	return 0;
}

