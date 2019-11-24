#include <stddef.h>
#include "build.h"
#include "eastasian.h"
#include "execute.h"
#include "typedef.h"
#include "thread.h"

static int eastasian_check(const struct eastasian_struct *str, unicode x)
{
	return (str->a <= x) && (x <= str->b);
}

static enum EastAsianType eastasian_symbol(unicode x, size_t ai, size_t bi)
{
	size_t ci;
	const struct eastasian_struct *a, *b, *c;

	Check(EastAsianTable_Size <= ai, "size error: ai");
	Check(EastAsianTable_Size <= bi, "size error: bi");
	ci = (ai + bi) / 2;
	a = EastAsianTable + ai;
	b = EastAsianTable + bi;
	c = EastAsianTable + ci;
	if (eastasian_check(a, x))
		return a->c;
	if (eastasian_check(b, x))
		return b->c;
	if (bi <= ai)
		return EastAsian_error;
	if (x < c->a)
		return eastasian_symbol(x, ai + 1UL, ci);
	else
		return eastasian_symbol(x, ci, bi - 1UL);
}

_g unsigned eastasian_width(unicode x)
{
	enum EastAsianType type;

	if (x < 0x80)
		type = EastAsianAscii[x];
	else
		type = eastasian_symbol(x, 0, EastAsianTable_Size - 1UL);

	return EastAsianSymbol[type];
}

_g void init_eastasian(void)
{
	EastAsianSymbol[EastAsian_error] = 0;
	EastAsianSymbol[EastAsian_N] = 1;
	EastAsianSymbol[EastAsian_A] = 2;
	EastAsianSymbol[EastAsian_H] = 1;
	EastAsianSymbol[EastAsian_W] = 2;
	EastAsianSymbol[EastAsian_F] = 2;
	EastAsianSymbol[EastAsian_NA] = 1;
}
