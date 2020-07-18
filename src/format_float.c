/*
 *  Common-Lisp Floating-point formatter
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include "build.h"
#include "c99.h"
#include "define.h"
#include "format_float.h"
#include "stream.h"

#define FMTDECIMAL_FLOAT_PARSE	"%+20.10e"
#define FMTDECIMAL_DOUBLE_PARSE	"%+30.20e"
#ifdef LISP_FLOAT_LONG_64
#define FMTDECIMAL_LONG_PARSE	FMTDECIMAL_DOUBLE_PARSE
#else
#define FMTDECIMAL_LONG_PARSE	"%+40.30Le"
#endif

#ifdef LISP_DEGRADE
#define WriteChar_		fmtfloat_write_char_
#define PrintAscii_		fmtfloat_print_ascii_
_g int fmtfloat_write_char_(addr stream, unicode c);
_g int fmtfloat_print_ascii_(addr stream, const char *);
#else
#define WriteChar_		write_char_stream_
#define PrintAscii_		print_ascii_stream_
#endif


/*****************************************************************************
 *  fmtdecimal
 *****************************************************************************/
static int fmtdecimal_parse(fmtdecimal str, const byte *input)
{
	int sign, exponent;
	byte buffer[FMTDECIMAL_EXPONENT];
	byte *ptr;
	unsigned i, x, y, z;

	ptr = str->fraction;
	sign = exponent = 0;
	i = x = y = z = 0;
	while (*input && *input == ' ') {
		input++;
	}
	if (*input == 0) {
		return 1;
	}
	if (*input == '+') {
		input++;
		goto first;
	}
	if (*input == '-') {
		sign = 1;
		input++;
		goto first;
	}
first:
	if (*input == 0) {
		return 1;
	}
	if (*input == '0') {
		ptr[i++] = *input - '0';
		if (FMTDECIMAL_FRACTION <= i)
			return 1;
		x++;
		input++;
	}
ignore:
	if (*input && *input == '0') {
		input++;
		goto ignore;
	}
loop:
	if (*input == 0) {
		goto return_value;
	}
	if (*input == '.') {
		input++;
		goto loop;
	}
	if (*input == 'e' || *input == 'E') {
		input++;
		goto exponent;
	}
	if (! isdigit(*input)) {
		return 1;
	}
	ptr[i++] = *input - '0';
	if (FMTDECIMAL_FRACTION <= i)
		return 1;
	x++;
	input++;
	goto loop;

exponent:
	if (*input == 0) {
		return 1;
	}
	if (*input == '-') {
		buffer[z++] = *input;
		if (FMTDECIMAL_EXPONENT <= z)
			return 1;
		input++;
		goto exploop;
	}
	if (*input == '+') {
		input++;
		goto exploop;
	}

exploop:
	if (*input == 0) {
		goto expfinish;
	}
	if (! isdigit(*input)) {
		goto reject;
	}
	buffer[z++] = *input;
	if (FMTDECIMAL_EXPONENT <= z)
		return 1;
	input++;
	goto exploop;

reject:
	if (*input && *input == ' ') {
		input++;
		goto reject;
	}
	if (*input) {
		return 1;
	}
	goto expfinish;

expfinish:
	buffer[z] = 0;
	exponent = atoi((const char *)buffer);
	goto return_value;

return_value:
	str->sign = sign;
	str->exponent = exponent;
	str->size = x;

	return 0;
}

static void fmtdecimal_press(fmtdecimal str)
{
	byte *ptr;
	unsigned size;

	ptr = str->fraction;
	size = str->size;
	while (2 <= size) {
		if (ptr[size - 1] != 0)
			break;
		size--;
		ptr[size] = 0;
	}
	str->size = size;
}

static void fmtdecimal_zero(fmtdecimal str)
{
	str->size = 1;
	str->exponent = 0;
	str->fraction[0] = 0;
}

static void fmtdecimal_left(fmtdecimal str)
{
	byte *ptr;
	unsigned i, size, mount;

	/* zero */
	size = str->size;
	if (size <= 1)
		return;

	/* check */
	ptr = str->fraction;
	for (mount = 0; mount < size; mount++) {
		if (ptr[mount] != 0)
			break;
	}
	if (mount == 0)
		return;

	/* shift */
	if (size == mount) {
		fmtdecimal_zero(str);
	}
	else {
		size -= mount;
		for (i = 0; i < size; i++) {
			ptr[i] = ptr[mount + i];
		}
		str->size = size;
		str->exponent -= mount;
	}
}

static void fmtdecimal_right(fmtdecimal str, byte c)
{
	byte *ptr, x;
	unsigned i, size;

	if (c == 0)
		return;
	ptr = str->fraction;
	size = str->size;
	for (i = 0; i < FMTDECIMAL_FRACTION && i < size; i++) {
		x = ptr[i];
		ptr[i] = c;
		c = x;
	}
	if (i < FMTDECIMAL_FRACTION) {
		ptr[i++] = c;
	}
	str->size = i;
	str->exponent++;
}

static int fmtdecimal_input(fmtdecimal str, const byte *input)
{
	if (fmtdecimal_parse(str, input)) {
		/* parse error */
		return 1;
	}
	else {
		fmtdecimal_press(str);
		fmtdecimal_left(str);
		return 0;
	}
}

static void fmtdecimal_carry(fmtdecimal str, unsigned i)
{
	byte *ptr, carry;

	ptr = str->fraction;
	for (carry = 1; carry; i--) {
		carry += ptr[i];
		if (carry <= 9) {
			ptr[i] = carry;
			break;
		}
		else {
			ptr[i] = carry % 10;
			carry = 1;
			if (i == 0) {
				fmtdecimal_right(str, carry);
				break;
			}
		}
	}
}

_g int fmtdecimal_zerop(fmtdecimal str)
{
	return str->size == 1 && str->fraction[0] == 0;
}

_g int fmtdecimal_round(fmtdecimal str, unsigned i)
{
	int check, zerop;
	byte *ptr;

	if (str->size <= i) {
		return 0;
	}
	ptr = str->fraction;
	check = 0;
	zerop = fmtdecimal_zerop(str);
	if (str->size != i) {
		str->size = i;
		check = 1;
	}
	if (ptr[i] <= 4) {
		/* reject 4 */
		if (i == 0) {
			check = ! zerop;
			fmtdecimal_zero(str);
		}
	}
	else {
		/* carryup 5 */
		if (i == 0) {
			ptr[0] = 0;
			ptr[1] = 1;
			str->size = 2;
		}
		else {
			fmtdecimal_carry(str, i - 1);
		}
		check = 1;
	}
	fmtdecimal_press(str);
	fmtdecimal_left(str);

	return check;
}

static int fmtdecimal_single_parse(fmtdecimal str, const char *fmt, single_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

static int fmtdecimal_double_parse(fmtdecimal str, const char *fmt, double_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

static int fmtdecimal_long_parse(fmtdecimal str, const char *fmt, long_float value)
{
	byte buffer[FMTDECIMAL_FRACTION];
	snprintc((char *)buffer, FMTDECIMAL_FRACTION, fmt, value);
	return fmtdecimal_input(str, buffer);
}

_g int fmtdecimal_single_float(fmtdecimal str, single_float value, int round)
{
	if (fmtdecimal_single_parse(str, FMTDECIMAL_FLOAT_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

_g int fmtdecimal_double_float(fmtdecimal str, double_float value, int round)
{
	if (fmtdecimal_double_parse(str, FMTDECIMAL_DOUBLE_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

_g int fmtdecimal_long_float(fmtdecimal str, long_float value, int round)
{
	if (fmtdecimal_long_parse(str, FMTDECIMAL_LONG_PARSE, value))
		return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

static int fmtdecimal_char(byte c)
{
	if (c <= 9)
		return '0' + c;
	else
		return '.';
}

_g void fmtdecimal_dump(FILE *file, fmtdecimal str)
{
	byte *ptr;
	unsigned size, i, c;

	ptr = str->fraction;
	size = str->size;
	if (size == 0) {
		fprintf(file, " 0: %c*.", str->sign? '-': '+');
	}
	else {
		c = fmtdecimal_char(ptr[0]);
		fprintf(file, "%2u: %c%c.", size, str->sign? '-': '+', c);
		for (i = 1; i < FMTDECIMAL_FRACTION; i++) {
			c = fmtdecimal_char(ptr[i]);
			fprintf(file, "%c", (i < size)? c: '*');
		}
	}
	fprintf(file, " e%+03" PRIdF "\n", str->exponent);
	fflush(file);
}


/*****************************************************************************
 *  fmtfloat
 *****************************************************************************/
static int fmtfixed_round_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t i, int *ret)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		Return(fmtfloat_fixed_(stream, fmt, dec));
		return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int fmtexponent_round_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t i, int *ret)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		Return(fmtfloat_exponent_(stream, fmt, dec));
		return Result(ret, 1);
	}
	return Result(ret, 0);
}

static int decimal_stream_(addr stream, byte c)
{
	Check(! (0 <= c && c <= 9), "decimal_stream_ error");
	return WriteChar_(stream, '0' + c);
}

static int decimalcheck_stream_(addr stream, fmtdecimal dec, size_t index)
{
	Check(dec->size <= index, "fmtdecimal index error.");
	return decimal_stream_(stream, dec->fraction[index]);
}

static int decimalzero_stream_(addr stream, fmtdecimal dec, size_t index)
{
	return decimal_stream_(stream, (index < dec->size)? dec->fraction[index]: 0);
}

static int fixed_large_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum count, i;

	count = dec->exponent + fmt->k + 1;
	if (count <= 0)
		return WriteChar_(stream, '0');
	for (i = 0; i < count; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	return 0;
}

static int times_stream_(addr stream, size_t size, unicode u)
{
	size_t i;

	for (i = 0; i < size; i++) {
		Return(WriteChar_(stream, u));
	}

	return 0;
}

static int for_decimalcheck_stream_(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}

	return 0;
}

static int for_decimalzero_stream_(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	return 0;
}

static int fixed_small_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t size, ka;

	size = dec->size;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	if (k < 0) {
		Return(times_stream_(stream, ka, '0'));
		Return(for_decimalcheck_stream_(stream, dec, 0, size));
	}
	else if (ka < size) {
		Return(for_decimalcheck_stream_(stream, dec, ka, size));
	}
	else {
		Return(WriteChar_(stream, '0'));
	}

	return 0;
}

static fixnum getexponent(fmtfloat fmt, fmtdecimal dec)
{
	return dec->exponent - (fmt->k - fmt->k_bias);
}

static int expsign_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;

	value = getexponent(fmt, dec);
	if (value < 0) {
		Return(WriteChar_(stream, '-'));
	}
	else if (fmt->sign_exponent) {
		Return(WriteChar_(stream, '+'));
	}

	return 0;
}

static int exponent_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;
	char buffer[32];
	size_t size, i;

	value = getexponent(fmt, dec);
	if (value < 0)
		value = -value; /* no carry */
	snprintf(buffer, 32, "%" PRIdF, value);

	if (fmt->ep) {
		size = strlen(buffer);
		if (size < fmt->e) {
			size = fmt->e - size;
			Return(WriteChar_(stream, fmt->marker));
			Return(expsign_stream_(stream, fmt, dec));
			for (i = 0; i < size; i++) {
				Return(WriteChar_(stream, '0'));
			}
			return PrintAscii_(stream, buffer);
		}
	}

	Return(WriteChar_(stream, fmt->marker));
	Return(expsign_stream_(stream, fmt, dec));
	return PrintAscii_(stream, buffer);
}

static int sign_stream_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	if (fmt->signbit) {
		Return(WriteChar_(stream, '-'));
	}
	else if (fmt->sign) {
		Return(WriteChar_(stream, '+'));
	}

	return 0;
}

static int fixed_free_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(fixed_small_(stream, fmt, dec));
	/* exponent */
	if (fmt->markerp) {
		Return(WriteChar_(stream, fmt->marker));
		Return(WriteChar_(stream, '0'));
	}

	return 0;
}

static size_t getsignsize(fmtfloat fmt, fmtdecimal dec)
{
	if (dec->sign)
		return 1;
	else if (fmt->sign)
		return 1;
	else
		return 0;
}

static int overflow_stream_(addr stream, fmtfloat fmt)
{
	return times_stream_(stream, fmt->w, fmt->overflow);
}

static int margin_stream_(addr stream, fmtfloat fmt, size_t size)
{
	return times_stream_(stream, size, fmt->pad);
}

static int fixed_width_underflow_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	if (size == kb) {
		fmtdecimal_round(dec, 0);
		Return(times_stream_(stream, size - 1, '0'));
		Return(decimalcheck_stream_(stream, dec, 0));
	}
	else {
		Return(times_stream_(stream, size, '0'));
	}

	return 0;
}

static int fixed_width_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	int check;
	size_t info;

	info = size - kb;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalcheck_stream_(stream, dec, 0, info);
}

static int fixed_width_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	size_t margin, dsize;

	/* no round */
	dsize = dec->size;
	margin = size - dsize - kb - 1;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalcheck_stream_(stream, dec, 0, dsize);
}

static int fixed_width_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t margin, size_t psize, size_t kb)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, psize, &check));
	if (check)
		return 0;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	return for_decimalcheck_stream_(stream, dec, kb, psize);
}

static int fixed_width_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	int check;
	size_t margin, tail;

	Return(fmtfixed_round_(stream, fmt, dec, kb + 1, &check));
	if (check)
		return 0;
	margin = size - kb - 1;
	tail = kb - size;

	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(for_decimalzero_stream_(stream, dec, tail, dec->size));
	Return(WriteChar_(stream, '.'));
	return decimalzero_stream_(stream, dec, kb + 1);
}

static int fixed_width_large3_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t kb)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, kb, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	return WriteChar_(stream, '.');
}

static int fixed_width_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check;
	fixnum k, ke;
	size_t w, kb, size, dsize, psize;

	/* ~w,,,o,cF */
	w = fmt->w;
	k = fmt->k;
	ke = k + dec->exponent + 1;
	kb = (size_t)(ke < 0? -ke: ke);

	/* overflow */
	dsize = dec->size;
	size = getsignsize(fmt, dec) + 1;
	check = fmt->overflowp &&
		((ke < 0 && w < (size + 1)) || (0 <= ke && w < (size + kb)));
	if (check)
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && size <= kb)
		return fixed_width_underflow_(stream, fmt, dec, size, kb);

	/* small2 */
	if (ke <= 0 && (size <= dsize + kb))
		return fixed_width_small2_(stream, fmt, dec, size, kb);

	/* small1 */
	if (ke <= 0)
		return fixed_width_small1_(stream, fmt, dec, size, kb);

	/* large1 */
	psize = size < dsize? size: dsize;
	if (kb < psize)
		return fixed_width_large1_(stream, fmt, dec, size - psize, psize, kb);

	/* large2 */
	if (kb < size)
		return fixed_width_large2_(stream, fmt, dec, size, kb);

	/* large3 (overflow) */
	return fixed_width_large3_(stream, fmt, dec, kb);
}

static int fixed_column_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	return times_stream_(stream, ka, '0');
}

static int fixed_column_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, d - ka, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	return for_decimalzero_stream_(stream, dec, 0, d - ka);
}

static int fixed_column_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;

	Return(fmtfixed_round_(stream, fmt, dec, ka + d, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(fixed_large_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, ka, ka + d);
}

static int fixed_column_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	/* ~,d,,o,cF */
	d = fmt->d;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka)
		return fixed_column_small2_(stream, fmt, dec, ka);

	/* small1 */
	if (k <= 0)
		return fixed_column_small1_(stream, fmt, dec, d, ka);

	/* large1 */
	return fixed_column_large1_(stream, fmt, dec, d, ka);
}

static int fixed_limit_underflow_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t margin;

	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	if (d == kb) {
		fmtdecimal_round(dec, 0);
		Return(times_stream_(stream, d - 1, '0'));
		Return(decimalcheck_stream_(stream, dec, 0));
	}
	else {
		Return(times_stream_(stream, d, '0'));
	}

	return 0;
}

static int fixed_limit_small_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t margin, info;

	info = d - kb;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, kb, '0'));
	return for_decimalzero_stream_(stream, dec, 0, info);
}

static int fixed_limit_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t margin, info;

	info = kb + d;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + kb) < size) {
		margin = size - (d + kb);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(fixed_large_(stream, fmt, dec));
	}
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, kb, d + kb);
}

static int fixed_limit_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int check, zero;
	size_t info;

	info = kb + d;
	Return(fmtfixed_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size) || dec->fraction[0] != 0;
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(fixed_large_(stream, fmt, dec));
	}
	Return(WriteChar_(stream, '.'));
	return for_decimalzero_stream_(stream, dec, kb, kb + d);
}

static int fixed_limit_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int illegal;
	fixnum ke;
	size_t w, d, kb, size, psize;

	/* ~w,,,o,cF */
	w = fmt->w;
	d = fmt->d;
	ke = fmt->k + dec->exponent + 1;
	kb = (size_t)(ke < 0? -ke: ke);

	/* overflow */
	size = getsignsize(fmt, dec) + 1;
	illegal = (w < size + d);
	if (illegal) {
		w = d + size - 1;
	}
	psize = size + d;
	if (0 <= ke)
		psize += kb;
	if (fmt->overflowp && (illegal || (w < psize)))
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && (d <= kb))
		return fixed_limit_underflow_(stream, fmt, dec, d, size, kb);

	/* small */
	if (ke <= 0)
		return fixed_limit_small_(stream, fmt, dec, d, size, kb);

	/* large1 */
	if (0 < ke && (! illegal) && (kb <= (size - d)))
		return fixed_limit_large1_(stream, fmt, dec, d, size, kb);

	/* large2 */
	return fixed_limit_large2_(stream, fmt, dec, d, size, kb);
}

_g int fmtfloat_fixed_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		return fixed_free_(stream, fmt, dec);
	}
	else if (dpnot) {
		return fixed_width_(stream, fmt, dec);
	}
	else if (wpnot) {
		return fixed_column_(stream, fmt, dec);
	}
	else {
		return fixed_limit_(stream, fmt, dec);
	}
}

static int exponent_fraction_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum index, i, first, size;

	index = fmt->k;
	size = (int)dec->size;

	first = 0;
	for (i = index; i <= 0; i++) {
		Return(WriteChar_(stream, '0'));
		if (first == 0) {
			Return(WriteChar_(stream, '.'));
			first = 1;
		}
	}

	for (i = 0; i < index; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}

	for (; i < size; i++) {
		if (first == 0) {
			Return(WriteChar_(stream, '.'));
			first = 1;
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}

	if (first == 0) {
		Return(PrintAscii_(stream, ".0"));
	}

	return 0;
}

static int exponent_free_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	Return(sign_stream_(stream, fmt, dec));
	Return(exponent_fraction_(stream, fmt, dec));
	return exponent_stream_(stream, fmt, dec);
}

static size_t exponent_size(fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;
	char buffer[32];
	size_t size;

	value = getexponent(fmt, dec);
	if (value < 0)
		value = -value; /* no carry */
	snprintf(buffer, 32, "%d", (int)value);
	size = strlen(buffer);
	if (fmt->ep && size < fmt->e)
		size = fmt->e;

	return + 1UL/*marker*/ + 1UL/*sign*/ + size;
}

static int exponent_width_overflow1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t i, margin;

	Return(fmtexponent_round_(stream, fmt, dec, size - ka, &check));
	if (check)
		return 0;
	margin = size - (dec->size + ka  + 1);
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	size = dec->size;
	for (i = 0; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, size - ka, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	size = dec->size;
	for (i = 0; i < size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_nomargin_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i;

	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_small_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - dec->size;
	size = dec->size;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalcheck_stream_(stream, dec, i));
	}
	if (i == k) {
		Return(WriteChar_(stream, '.'));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_large_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - k;
	Return(margin_stream_(stream, fmt, margin));
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < dec->size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	size = k - dec->size;
	Return(times_stream_(stream, size, '0'));
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	if (size <= dec->size) {
		return exponent_width_large1_nomargin_(stream, fmt, dec, size, k);
	}
	else if (k <= dec->size) {
		return exponent_width_large1_small_(stream, fmt, dec, size, k);
	}
	else {
		return exponent_width_large1_large_(stream, fmt, dec, size, k);
	}
}

static int exponent_width_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, fixnum k)
{
	int check;
	fixnum i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_width_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check;
	fixnum k;
	size_t w, ka, size;

	w = fmt->w;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* overflow */
	size = getsignsize(fmt, dec) + 1 + exponent_size(fmt, dec);
	check = fmt->overflowp &&
		((k < 0 && w <= (size + ka)) || (0 <= k && w < (size + ka)));
	if (check)
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka)
		return exponent_width_overflow1_(stream, fmt, dec, ka);

	/* small2 */
	if (k <= 0 && (dec->size + ka < size))
		return exponent_width_small2_(stream, fmt, dec, size, ka);

	/* small1 */
	if (k <= 0)
		return exponent_width_small1_(stream, fmt, dec, size, ka);

	/* large1 */
	if (0 < k && ka < size)
		return exponent_width_large1_(stream, fmt, dec, size, ka);

	/* large2 */
	if (0 < k && size <= ka)
		return exponent_width_large2_(stream, fmt, dec, k);

	/* throw */
	return 0;
}

static int exponent_column_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	int check;
	size_t size, i;

	size = d - ka;
	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(PrintAscii_(stream, "0."));
	Return(times_stream_(stream, ka, '0'));
	for (i = 0; i < dec->size; i++) {
		Return(decimalcheck_stream_(stream, dec, i));
	}
	Return(times_stream_(stream, size - dec->size, '0'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t k)
{
	int check;
	size_t i, size;

	size = d + 1;
	Return(fmtexponent_round_(stream, fmt, dec, size, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < size; i++) {
		if (i == k) {
			Return(WriteChar_(stream, '.'));
		}
		Return(decimalzero_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_column_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	d = fmt->d;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka)
		return exponent_column_small2_(stream, fmt, dec, ka);

	/* small1 */
	if (k <= 0 && ka < d)
		return exponent_column_small1_(stream, fmt, dec, d, ka);

	/* large1 */
	if (0 < k && ka <= d)
		return exponent_column_large1_(stream, fmt, dec, d, ka);

	/* large2 */
	if (0 < k && d < ka)
		return exponent_column_large2_(stream, fmt, dec, ka);

	/* throw */
	return 0;
}

static int exponent_limit_overflow1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	int check;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalzero_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_small2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	int check;
	size_t margin;

	Return(fmtexponent_round_(stream, fmt, dec, 1, &check));
	if (check)
		return 0;
	if (ka + 2 < size) {
		margin = size - (ka + 2);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (size != ka + 1) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(decimalcheck_stream_(stream, dec, 0));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_small1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka, size_t d)
{
	int check, zero;
	size_t margin, info;

	info = d - ka;
	Return(fmtexponent_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	if (zero) {
		Return(WriteChar_(stream, '0'));
	}
	Return(WriteChar_(stream, '.'));
	Return(times_stream_(stream, ka, '0'));
	Return(for_decimalzero_stream_(stream, dec, 0, info));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_large1_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k, size_t d)
{
	int check;
	size_t margin, info, i;

	info = d + 1;
	Return(fmtexponent_round_(stream, fmt, dec, info, &check));
	if (check)
		return 0;
	if (d + 1 < size) {
		margin = size - (d + 1);
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	for (; i < info; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_large2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	int check;
	size_t margin, i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	if (k < size) {
		margin = size - k;
		Return(margin_stream_(stream, fmt, margin));
	}
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_overflow2_(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	int check;
	size_t i;

	Return(fmtexponent_round_(stream, fmt, dec, k, &check));
	if (check)
		return 0;
	Return(sign_stream_(stream, fmt, dec));
	for (i = 0; i < k; i++) {
		Return(decimalzero_stream_(stream, dec, i));
	}
	Return(WriteChar_(stream, '.'));
	return exponent_stream_(stream, fmt, dec);
}

static int exponent_limit_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int check, illegal;
	fixnum k;
	size_t w, d, ka, size;

	w = fmt->w;
	d = fmt->d;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* overflow */
	size = getsignsize(fmt, dec) + 1 + exponent_size(fmt, dec);
	illegal = (w < size + d);
	if (illegal) {
		w = d + size - 1;
	}
	check = ((k < 0 && w <= (size + ka)) || (0 <= k && w < (size + ka)));
	if (fmt->overflowp && (illegal || check))
		return overflow_stream_(stream, fmt);
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka)
		return exponent_limit_overflow1_(stream, fmt, dec, ka);

	/* small2 */
	if (k <= 0 && d <= ka)
		return exponent_limit_small2_(stream, fmt, dec, size, ka);

	/* small1 */
	if (k <= 0)
		return exponent_limit_small1_(stream, fmt, dec, size, ka, d);

	/* large1 */
	if (0 < k && ka <= d)
		return exponent_limit_large1_(stream, fmt, dec, size, ka, d);

	/* large2 */
	if (0 < k && ka <= size)
		return exponent_limit_large2_(stream, fmt, dec, size, ka);

	/* overflow2 */
	if (0 < k && size < ka)
		return exponent_limit_overflow2_(stream, fmt, dec, ka);

	/* throw */
	return 0;
}

_g int fmtfloat_exponent_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		return exponent_free_(stream, fmt, dec);
	}
	else if (dpnot) {
		return exponent_width_(stream, fmt, dec);
	}
	else if (wpnot) {
		return exponent_column_(stream, fmt, dec);
	}
	else {
		return exponent_limit_(stream, fmt, dec);
	}
}

static int fmtfloat_general_fixedp(fmtdecimal dec, fmtfloat fmt)
{
	fixnum e;

	e = dec->exponent;
	if (e < -1) /* dec < 0.1 */
		return 0; /* ~E */
	if (! fmt->dp)
		return 1; /* ~F */
	if (e < 0 || ((size_t)e) <= fmt->d)
		return 1; /* ~F */
	else
		return 0; /* ~E */
}

_g int fmtfloat_general_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum n, ww, dd, d, ee, q;

	/* n = arg? (fixnum)(floorf(log10f(arg)) + 1.0f): 0; */
	n = dec->exponent;
	d = fmt->d;
	/* ee */
	ee = fmt->ep? fmt->e + 2: 4;
	/* ww */
	if (fmt->wp) {
		ww = fmt->w - ee;
		if (ww < 0) ww = 0;
	}
	else {
		ww = -1;
	}

	/* format */
	if (fmtfloat_general_fixedp(dec, fmt)) {
		q = (fixnum)dec->size;
		d = (n < 7)? n: 7;
		d = (q > d)? q: d;
		dd = d - n;
		/* call fmtfloat_fixed_ */
		if (ww < 0) {
			fmt->wp = 0;
			fmt->w = 0;
		}
		else {
			fmt->wp = 1;
			fmt->w = ww;
		}
		fmt->d = dd;
		fmt->k = 0;  /* default */
		fmt->k_bias = 0; /* 0 if fixed */
		fmt->markerp = 0;
		Return(fmtfloat_fixed_(stream, fmt, dec));
		return times_stream_(stream, ee, ' ');
	}
	else {
		/* call fmtfloat_exponent_ */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = 1;
		fmt->sign_exponent = 1; /* for prin1 */
		return fmtfloat_exponent_(stream, fmt, dec);
	}
}

_g int fmtfloat_monetary_(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum e, round, index;
	size_t d, n, w, sign, n1, n2, size, p, i;

	d = fmt->dp? fmt->d: 2;
	n = fmt->np? fmt->n: 1;
	w = fmt->wp? fmt->w: 0;

	/* round */
	round = dec->exponent + d + 1;
	if (0 <= round)
		fmtdecimal_round(dec, (unsigned)round);
	e = dec->exponent;

	/* size */
	sign = getsignsize(fmt, dec);
	n2 = (e < 0)? 0: (1UL + (size_t)e);
	n1 = (n2 < n)? (n - n2): 0;
	size = sign + n2 + n1 + 1 + d;
	p = (size < w)? (w - size): 0;

	/* output */
	if (fmt->markerp) {
		/* sign -> padding */
		Return(sign_stream_(stream, fmt, dec));
		Return(margin_stream_(stream, fmt, p));
	}
	else {
		/* padding -> sign*/
		Return(margin_stream_(stream, fmt, p));
		Return(sign_stream_(stream, fmt, dec));
	}
	/* large */
	Return(times_stream_(stream, n1, '0'));
	Return(for_decimalzero_stream_(stream, dec, 0, n2));
	/* dot */
	Return(WriteChar_(stream, '.'));
	/* small */
	for (i = 0; i < d; i++) {
		index = ((int)i) + e + 1;
		if (index < 0) {
			Return(WriteChar_(stream, '0'));
		}
		else {
			Return(decimalzero_stream_(stream, dec, index));
		}
	}

	return 0;
}


/*
 *  ~F  Fixed floating-point
 */
_g int fmtfloat_fixed_float_(addr stream, single_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum k,
		unicode overflow,
		unicode pad)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.k = k; /* default 0 */
	fmt.k_bias = 0; /* 0 if fixed */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';
	fmt.markerp = 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		Abort("fmtdecimal_single_float error");
	}

	return fmtfloat_fixed_(stream, &fmt, &dec);
}

_g int fmtfloat_fixed_double_(addr stream, double_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum k,
		unicode overflow,
		unicode pad)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.k = k; /* default 0 */
	fmt.k_bias = 0; /* 0 if fixed */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';
	fmt.markerp = 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		Abort("fmtdecimal_double_float error");
	}

	return fmtfloat_fixed_(stream, &fmt, &dec);
}


/*
 *  ~E  Exponential floating-point
 */
_g int fmtfloat_exponent_float_(addr stream, single_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum e,
		fixnum k,
		unicode overflow,
		unicode pad,
		unicode exponent)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;

	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.ep = 1;
	fmt.e = e; /* default 2 -> "+1" */
	fmt.k = k; /* default 1 */
	fmt.k_bias = 1; /* 1 if exponential. */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';

	fmt.markerp = 1;
	fmt.marker = exponent? exponent: 'E';
	fmt.sign_exponent = 1; /* for prin1 */

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		Abort("fmtdecimal_single_float error");
	}

	return fmtfloat_exponent_(stream, &fmt, &dec);
}

_g int fmtfloat_exponent_double_(addr stream, double_float value,
		int sign,
		fixnum w,
		fixnum d,
		fixnum e,
		fixnum k,
		unicode overflow,
		unicode pad,
		unicode exponent)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;

	fmt.sign = sign;
	fmt.w = (size_t)w;
	fmt.wp = (0 <= w);
	fmt.d = (size_t)d;
	fmt.dp = (0 <= d);
	fmt.ep = 1;
	fmt.e = e; /* default 2 -> "+1" */
	fmt.k = k; /* default 1 */
	fmt.k_bias = 1; /* 1 if exponential. */
	fmt.overflow = overflow;
	fmt.overflowp = overflow != 0;
	fmt.pad = pad? pad: ' ';

	fmt.markerp = 1;
	fmt.marker = exponent? exponent: 'E';
	fmt.sign_exponent = 1; /* for prin1 */

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		Abort("fmtdecimal_double_float error");
	}

	return fmtfloat_exponent_(stream, &fmt, &dec);
}


/*****************************************************************************
 *  princ / prin1
 *****************************************************************************/
static int fmtfloat_princ_fixedp(fmtdecimal dec)
{
	/* return 1e-3 <= fabs(dec) < 1e7 */
	return -3 <= dec->exponent && dec->exponent < 7;
}

static int fmtfloat_princ_(addr stream, fmtfloat fmt, fmtdecimal dec,
		int markerp, unicode marker)
{
	if (fmtfloat_princ_fixedp(dec)) {
		fmt->k = 0;  /* default */
		fmt->k_bias = 0; /* 0 if fixed */
		fmt->markerp = markerp;
		if (markerp) {
			fmt->ep = 1;
			fmt->e = 1;
			fmt->markerp = 1;
			fmt->marker = marker;
			fmt->sign_exponent = 0; /* 0 if princ / prin1 */
		}
		return fmtfloat_fixed_(stream, fmt, dec);
	}
	else {
		fmt->k = 1; /* default 1 */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = markerp;
		fmt->ep = 1;
		fmt->e = 1;
		fmt->markerp = 1;
		fmt->marker = marker;
		fmt->sign_exponent = 0; /* 0 if princ / prin1 */
		return fmtfloat_exponent_(stream, fmt, dec);
	}
}

_g int fmtfloat_princ_single_float_(addr stream,
		single_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}

_g int fmtfloat_princ_double_float_(addr stream,
		double_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}

_g int fmtfloat_princ_long_float_(addr stream,
		long_float value, int markerp, unicode marker, int *ret)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG))
		return Result(ret, 1);
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_long = value;
	fmt.signbit = signbit(value)? 1: 0;
	Return(fmtfloat_princ_(stream, &fmt, &dec, markerp, marker));

	return Result(ret, 0);
}

