/*
 *  token
 */
#include "c99.h"
#include "character.h"
#include "charqueue.h"
#include "condition.h"
#include "constant.h"
#include "bigcons.h"
#include "bignum.h"
#include "object.h"
#include "ratio.h"
#include "real_float.h"
#include "strtype.h"
#include "symbol.h"
#include "token.h"
#include <errno.h>
#include <stdlib.h>
#include <locale.h>

#define FLOATSIZE		64
#define FLOATBUFFER		(FLOATSIZE * 2)

/*
 *  integer (fixnum, bignum)
 */
_g int getchar_digit(unsigned v, int upperp, unicode *ret)
{
	if (IntegerBaseMax < v)
		return 1;
	if (v < 10)
		*ret = '0' + v;
	else
		*ret = (upperp? 'A': 'a') + v - 10;

	return 0;
}

_g int getvalue_digit(unsigned base, unicode c, unsigned *ret)
{
	unsigned value;

	if (('0' <= c) && (c <= '9')) {
		value = c - '0';
	}
	else if ('a' <= c && c <= 'z') {
		value = c - 'a' + 10;
	}
	else if ('A' <= c && c <= 'Z') {
		value = c - 'A' + 10;
	}
	else {
		return 1;
	}
	if (base <= value) {
		return 1;
	}
	*ret = value;

	return 0;
}

_g unicode checkchar_digit(unsigned v, int upperp)
{
	unicode value;

	if (getchar_digit(v, upperp, &value)) {
		Abort("character error");
		return 0;
	}

	return value;
}

_g unsigned checkvalue_digit(unsigned base, unicode c)
{
	unsigned value;

	if (getvalue_digit(base, c, &value)) {
		Abort("character error");
		return 0;
	}

	return value;
}

_g void maketoken_integer(LocalRoot local, addr queue, unsigned base, addr *ret)
{
	int sign;
	size_t i, m, size, max;
	addr pos, cons;
	unicode u;
	LocalStack stack;

	GetCharQueueSize(queue, &size);
	GetCharQueueMax(queue, &max);
	GetCharQueueRoot(queue, &pos);
	push_local(local, &stack);
	bigcons_local(local, &cons);

	sign = 0;
	for (i = 0; i < size; i++) {
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		if (u == '+' || u == '.')
			continue;
		if (u == '-') {
			sign = 1;
			continue;
		}
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	integer_cons_heap(ret, sign, cons);
	rollback_local(local, stack);
}


/*
 *  float
 */
static void makefloat_buffer(int sign, const char *fract, int exp, char *ret)
{
	const char *tail;

	if (fract[0] == 0) {
		snprintf(ret, FLOATBUFFER, "%c0.0e0",
				(sign? '-': '+'));
	}
	else {
		tail = fract + 1;
		snprintf(ret, FLOATBUFFER, "%c%c.%se%d",
				(sign? '-': '+'),
				fract[0],
				(*fract && *tail)? tail: "0",
				exp);
	}
}

static int makefloat_single(const char *ptr, addr *ret)
{
	single_float value = check_strtof(ptr, NULL);
	single_float_heap(ret, value);
	return 0;
}

static int makefloat_double(const char *ptr, addr *ret)
{
	double_float value = check_strtod(ptr, NULL);
	double_float_heap(ret, value);
	return 0;
}

static int makefloat_long(const char *ptr, addr *ret)
{
	long_float value = check_strtold(ptr, NULL);
	long_float_heap(ret, value);
	return 0;
}

static int read_default_float_format(Execute ptr)
{
	addr symbol, check;

	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &symbol);
	getspecialcheck_local(ptr, symbol, &symbol);
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (symbol == check) return 'f';
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (symbol == check) return 'd';
	GetConst(COMMON_LONG_FLOAT, &check);
	if (symbol == check) return 'l';
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (symbol == check) return 's';

	return 0;
}

static int makefloat(Execute ptr,
		int sign, const char *fract, int v, int type, addr *ret)
{
	char buffer[FLOATBUFFER];

	makefloat_buffer(sign, fract, v, buffer);
	if (type == 'e')
		type = read_default_float_format(ptr);
	switch (type) {
		case 's': /* short */
		case 'f': /* single */
			return makefloat_single(buffer, ret);

		case 'd': /* double */
			return makefloat_double(buffer, ret);

		case 'l': /* long */
			return makefloat_long(buffer, ret);

		default: /* error */
			return 1;
	}

	return 0;
}

static int atolcheck(const char *str, long *value)
{
	long v;
	char *endp;

	errno = 0;
	v = strtol(str, &endp, 10);
	if (errno == ERANGE || errno == EINVAL)
		return 1;
	if (str == endp)
		return 1;
	*value = v;

	return 0;
}

static int plus_safe(long a, long b, long *result)
{
	if (((b > 0) && (a > (LONG_MAX - b)))
			|| ((b < 0) && (a < (LONG_MIN - b)))) {
		return 1;
	}
	*result = a + b;

	return 0;
}

static int isexponent(unicode c)
{
	/* "defslDEFSL" */
	if (c == 'e' || c == 'E') return 'e'; /* default */
	if (c == 'f' || c == 'F') return 'f'; /* single */
	if (c == 'd' || c == 'D') return 'd'; /* double */
	if (c == 'l' || c == 'L') return 'l'; /* long */
	if (c == 's' || c == 'S') return 's'; /* short */

	return 0;
}

#define NextChar() (i < size? str[i++]: 0)

static int floattable(Execute ptr, const unicode *str, size_t size, addr *ret)
{
	int w, e, ds, zero, sign, type;
	long v;
	char fract[FLOATSIZE];
	char exp[FLOATSIZE];
	unicode c;
	size_t i;

	i = w = e = ds = 0;
	zero = 0;
	sign = 0;
	type = 'e';

	/* start */
	c = NextChar();
	if (c == '+') {
		sign = 0;
		c = NextChar();
	}
	else if (c == '-') {
		sign = 1;
		c = NextChar();
	}

first:
	if (c == 0)
		goto final;
	if (zero == 0 && c == '0') {
		c = NextChar();
		goto first;
	}
	if (isDigitCase(c)) {
		if (zero == 0)
			zero = 1;
		else
			ds++;
		if (w < FLOATSIZE) fract[w] = (char)c;
		w++;
		c = NextChar();
		goto first;
	}
	if (c == '.') {
		goto fract;
	}
	type = isexponent(c);
	if (type)
		goto exponent;
	goto error;

fract:
	c = NextChar();
	if (c == 0)
		goto final;
	if (zero == 0 && c == '0') {
		ds--;
		goto fract;
	}
	if (isDigitCase(c)) {
		if (zero == 0) {
			ds--;
			zero = 1;
		}
		if (w < FLOATSIZE) fract[w] = (char)c;
		w++;
		goto fract;
	}
	type = isexponent(c);
	if (type)
		goto exponent;
	goto error;

exponent:
	c = NextChar();
	if (c == '+') {
		if (e < FLOATSIZE) exp[e++] = '+';
		c = NextChar();
	}
	else if (c == '-') {
		if (e < FLOATSIZE) exp[e++] = '-';
		c = NextChar();
	}
	if (c == 0) {
		if (e < FLOATSIZE) exp[e++] = '0';
		goto final;
	}
	if (isDigitCase(c)) {
		if (e < FLOATSIZE) exp[e++] = (char)c;
		goto expnext;
	}
	goto error;

expnext:
	c = NextChar();
	if (c == 0)
		goto final;
	if (isDigitCase(c)) {
		if (e < FLOATSIZE) exp[e++] = (char)c;
		goto expnext;
	}
	goto error;

error:
	return 1;

final:
	if (zero == 0) {
		fract[0] = 0;
		return makefloat(ptr, sign, fract, 0, type, ret);
	}
	fract[w < FLOATSIZE-1? w: FLOATSIZE-1] = 0;
	exp[e] = 0;

	v = 0;
	if (e && atolcheck(exp, &v))  /* atol(exp) error */
		return 1;
	if (plus_safe(v, ds, &v))  /* v += ds; overflow */
		return 1;

	return makefloat(ptr, sign, fract, (int)v, type, ret);
}

_g void maketoken_float(Execute ptr, addr queue, addr *ret)
{
	const unicode *body;
	size_t size;

	make_charqueue_heap(queue, &queue);
	strvect_posbodylen(queue, &body, &size);
	if (floattable(ptr, body, size, ret))
		fmte("parse-float error", NULL);
}


/*
 *  ratio: [-+]?\\d+/\\d+
 */
_g void maketoken_ratio(LocalRoot local, addr queue, unsigned base, addr *ret)
{
	int sign;
	size_t i, m, size, max;
	addr pos, numer, denom, cons;
	unicode u;
	LocalStack stack;

	GetCharQueueSize(queue, &size);
	GetCharQueueMax(queue, &max);
	GetCharQueueRoot(queue, &pos);
	push_local(local, &stack);
	bigcons_local(local, &cons);

	/* numer */
	sign = signplus_bignum;
	for (i = 0; ; i++) {
		Check(size <= i, "size error");
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		if (u == '+')
			continue;
		if (u == '-') {
			sign = signminus_bignum;
			continue;
		}
		if (u == '/') break;
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	bignum_cons_alloc(local, &numer, signplus_bignum, cons);

	/* denom */
	clear_bigcons(cons);
	for (i++; i < size; i++) {
		m = i % max;
		if (i && m == 0)
			GetCharBitNext(pos, &pos);
		GetCharBitChar(pos, m, &u);
		push_bigcons(local, cons, base, checkvalue_digit(base, u));
	}
	bignum_cons_alloc(local, &denom, signplus_bignum, cons);

	/* result */
	ratio_reduction_heap(local, ret, sign, numer, denom);
	rollback_local(local, stack);
}

