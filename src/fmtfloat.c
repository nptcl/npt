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
#include "fmtfloat.h"
#include "stream.h"

#define FMTDECIMAL_FLOAT_PARSE	"%+20.10e"
#define FMTDECIMAL_DOUBLE_PARSE	"%+30.20e"
#ifdef LISP_FLOAT_LONG_64
#define FMTDECIMAL_LONG_PARSE	FMTDECIMAL_DOUBLE_PARSE
#else
#define FMTDECIMAL_LONG_PARSE	"%+40.30Le"
#endif

#ifdef LISP_DEGRADE
#define WriteChar		fmtfloat_write_char
#define PrintAscii		fmtfloat_print_ascii
_g void fmtfloat_write_char(addr stream, unicode c);
_g void fmtfloat_print_ascii(addr stream, const char *);
#else
#define WriteChar		write_char_stream
#define PrintAscii		print_ascii_stream
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
		if (FMTDECIMAL_FRACTION <= i) return 1;
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
	if (FMTDECIMAL_FRACTION <= i) return 1;
	x++;
	input++;
	goto loop;

exponent:
	if (*input == 0) {
		return 1;
	}
	if (*input == '-') {
		buffer[z++] = *input;
		if (FMTDECIMAL_EXPONENT <= z) return 1;
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
	if (FMTDECIMAL_EXPONENT <= z) return 1;
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
		if (ptr[size - 1] != 0) break;
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
	if (size <= 1) return;

	/* check */
	ptr = str->fraction;
	for (mount = 0; mount < size; mount++) {
		if (ptr[mount] != 0) break;
	}
	if (mount == 0) return;

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

	if (c == 0) return;
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
	if (fmtdecimal_single_parse(str, FMTDECIMAL_FLOAT_PARSE, value)) return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

_g int fmtdecimal_double_float(fmtdecimal str, double_float value, int round)
{
	if (fmtdecimal_double_parse(str, FMTDECIMAL_DOUBLE_PARSE, value)) return 1;
	if (0 <= round)
		fmtdecimal_round(str, (unsigned)round);
	return 0;
}

_g int fmtdecimal_long_float(fmtdecimal str, long_float value, int round)
{
	if (fmtdecimal_long_parse(str, FMTDECIMAL_LONG_PARSE, value)) return 1;
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
static int fmtfixed_round(addr stream, fmtfloat fmt, fmtdecimal dec, size_t i)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		fmtfloat_fixed(stream, fmt, dec);
		return 1;
	}
	return 0;
}

static int fmtexponent_round(addr stream, fmtfloat fmt, fmtdecimal dec, size_t i)
{
	if (fmtdecimal_round(dec, (unsigned)i)) {
		fmtfloat_exponent(stream, fmt, dec);
		return 1;
	}
	return 0;
}

static void decimal_stream(addr stream, byte c)
{
	Check(! (0 <= c && c <= 9), "decimal_stream error");
	WriteChar(stream, '0' + c);
}

static void decimalcheck_stream(addr stream, fmtdecimal dec, size_t index)
{
	Check(dec->size <= index, "fmtdecimal index error.");
	decimal_stream(stream, dec->fraction[index]);
}

static void decimalzero_stream(addr stream, fmtdecimal dec, size_t index)
{
	decimal_stream(stream, (index < dec->size)? dec->fraction[index]: 0);
}

static void fixed_large(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum count, i;

	count = dec->exponent + fmt->k + 1;
	if (count <= 0) {
		WriteChar(stream, '0');
		return;
	}
	for (i = 0; i < count; i++) {
		decimalzero_stream(stream, dec, i);
	}
}

static void times_stream(addr stream, size_t size, unicode u)
{
	size_t i;

	for (i = 0; i < size; i++)
		WriteChar(stream, u);
}

static void for_decimalcheck_stream(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++)
		decimalcheck_stream(stream, dec, i);
}

static void for_decimalzero_stream(addr stream,
		fmtdecimal dec, size_t index, size_t size)
{
	size_t i;

	for (i = index; i < size; i++)
		decimalzero_stream(stream, dec, i);
}

static void fixed_small(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t size, ka;

	size = dec->size;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	if (k < 0) {
		times_stream(stream, ka, '0');
		for_decimalcheck_stream(stream, dec, 0, size);
	}
	else if (ka < size) {
		for_decimalcheck_stream(stream, dec, ka, size);
	}
	else {
		WriteChar(stream, '0');
	}
}

static fixnum getexponent(fmtfloat fmt, fmtdecimal dec)
{
	return dec->exponent - (fmt->k - fmt->k_bias);
}

static void expsign_stream(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;

	value = getexponent(fmt, dec);
	if (value < 0)
		WriteChar(stream, '-');
	else if (fmt->sign_exponent)
		WriteChar(stream, '+');
}

static void exponent_stream(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum value;
	char buffer[32];
	size_t size, i;

	if (fmt->sign_exponent == 0)
		return;
	value = getexponent(fmt, dec);
	if (value < 0)
		value = -value; /* no carry */
	snprintf(buffer, 32, "%" PRIdF, value);

	if (fmt->ep) {
		size = strlen(buffer);
		if (size < fmt->e) {
			size = fmt->e - size;
			WriteChar(stream, fmt->marker);
			expsign_stream(stream, fmt, dec);
			for (i = 0; i < size; i++)
				WriteChar(stream, '0');
			PrintAscii(stream, buffer);
			return;
		}
	}

	WriteChar(stream, fmt->marker);
	expsign_stream(stream, fmt, dec);
	PrintAscii(stream, buffer);
}

static void sign_stream(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	if (fmt->signbit)
		WriteChar(stream, '-');
	else if (fmt->sign)
		WriteChar(stream, '+');
}

static void fixed_free(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	sign_stream(stream, fmt, dec);
	fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
	fixed_small(stream, fmt, dec);
	/* exponent */
	if (fmt->markerp)
		exponent_stream(stream, fmt, dec);
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

static void overflow_stream(addr stream, fmtfloat fmt)
{
	times_stream(stream, fmt->w, fmt->overflow);
}

static void margin_stream(addr stream, fmtfloat fmt, size_t size)
{
	times_stream(stream, size, fmt->pad);
}

static void fixed_width_underflow(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	sign_stream(stream, fmt, dec);
	WriteChar(stream, '.');
	if (size == kb) {
		fmtdecimal_round(dec, 0);
		times_stream(stream, size - 1, '0');
		decimalcheck_stream(stream, dec, 0);
	}
	else {
		times_stream(stream, size, '0');
	}
}

static void fixed_width_small2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	size_t info;

	info = size - kb;
	if (fmtfixed_round(stream, fmt, dec, info)) return;
	sign_stream(stream, fmt, dec);
	WriteChar(stream, '.');
	times_stream(stream, kb, '0');
	for_decimalcheck_stream(stream, dec, 0, info);
}

static void fixed_width_small1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	size_t margin, dsize;

	/* no round */
	dsize = dec->size;
	margin = size - dsize - kb - 1;
	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, kb, '0');
	for_decimalcheck_stream(stream, dec, 0, dsize);
}

static void fixed_width_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t margin, size_t psize, size_t kb)
{
	if (fmtfixed_round(stream, fmt, dec, psize)) return;
	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
	for_decimalcheck_stream(stream, dec, kb, psize);
}

static void fixed_width_large2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t kb)
{
	size_t margin, tail;

	if (fmtfixed_round(stream, fmt, dec, kb + 1)) return;
	margin = size - kb - 1;
	tail = kb - size;

	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	fixed_large(stream, fmt, dec);
	for_decimalzero_stream(stream, dec, tail, dec->size);
	WriteChar(stream, '.');
	decimalzero_stream(stream, dec, kb + 1);
}

static void fixed_width_large3(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t kb)
{
	if (fmtfixed_round(stream, fmt, dec, kb)) return;
	sign_stream(stream, fmt, dec);
	fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
}

static void fixed_width(addr stream, fmtfloat fmt, fmtdecimal dec)
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
	if (check) {
		overflow_stream(stream, fmt);
		return;
	}
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && size <= kb) {
		fixed_width_underflow(stream, fmt, dec, size, kb);
		return;
	}

	/* small2 */
	if (ke <= 0 && (size <= dsize + kb)) {
		fixed_width_small2(stream, fmt, dec, size, kb);
		return;
	}

	/* small1 */
	if (ke <= 0) {
		fixed_width_small1(stream, fmt, dec, size, kb);
		return;
	}

	/* large1 */
	psize = size < dsize? size: dsize;
	if (kb < psize) {
		fixed_width_large1(stream, fmt, dec, size - psize, psize, kb);
		return;
	}

	/* large2 */
	if (kb < size) {
		fixed_width_large2(stream, fmt, dec, size, kb);
		return;
	}

	/* large3 (overflow) */
	else {
		fixed_width_large3(stream, fmt, dec, kb);
		return;
	}
}

static void fixed_column_small2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, ka, '0');
}

static void fixed_column_small1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	if (fmtfixed_round(stream, fmt, dec, d - ka)) return;
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, ka, '0');
	for_decimalzero_stream(stream, dec, 0, d - ka);
}

static void fixed_column_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	if (fmtfixed_round(stream, fmt, dec, ka + d)) return;
	sign_stream(stream, fmt, dec);
	fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
	for_decimalzero_stream(stream, dec, ka, ka + d);
}

static void fixed_column(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	/* ~,d,,o,cF */
	d = fmt->d;
	k = dec->exponent + fmt->k + 1;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka) {
		fixed_column_small2(stream, fmt, dec, ka);
		return;
	}

	/* small1 */
	if (k <= 0) {
		fixed_column_small1(stream, fmt, dec, d, ka);
		return;
	}

	/* large1 */
	else {
		fixed_column_large1(stream, fmt, dec, d, ka);
		return;
	}
}

static void fixed_limit_underflow(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t margin;

	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	if (zero)
		WriteChar(stream, '0');
	WriteChar(stream, '.');
	if (d == kb) {
		fmtdecimal_round(dec, 0);
		times_stream(stream, d - 1, '0');
		decimalcheck_stream(stream, dec, 0);
	}
	else {
		times_stream(stream, d, '0');
	}
}

static void fixed_limit_small(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t margin, info;

	info = d - kb;
	if (fmtfixed_round(stream, fmt, dec, info)) return;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	if (zero)
		WriteChar(stream, '0');
	WriteChar(stream, '.');
	times_stream(stream, kb, '0');
	for_decimalzero_stream(stream, dec, 0, info);
}

static void fixed_limit_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t margin, info;

	info = kb + d;
	if (fmtfixed_round(stream, fmt, dec, info)) return;
	zero = (d < size);
	if ((d + kb) < size) {
		margin = size - (d + kb);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	if (zero)
		fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
	for_decimalzero_stream(stream, dec, kb, d + kb);
}

static void fixed_limit_large2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t size, size_t kb)
{
	int zero;
	size_t info;

	info = kb + d;
	if (fmtfixed_round(stream, fmt, dec, info)) return;
	zero = (d < size) || dec->fraction[0] != 0;
	sign_stream(stream, fmt, dec);
	if (zero)
		fixed_large(stream, fmt, dec);
	WriteChar(stream, '.');
	for_decimalzero_stream(stream, dec, kb, kb + d);
}

static void fixed_limit(addr stream, fmtfloat fmt, fmtdecimal dec)
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
	if (0 <= ke) psize += kb;
	if (fmt->overflowp && (illegal || (w < psize))) {
		overflow_stream(stream, fmt);
		return;
	}
	size = (w <= size)? 0: (w - size);

	/* underflow */
	if (ke < 0 && (d <= kb)) {
		fixed_limit_underflow(stream, fmt, dec, d, size, kb);
		return;
	}

	/* small */
	if (ke <= 0) {
		fixed_limit_small(stream, fmt, dec, d, size, kb);
		return;
	}

	/* large1 */
	if (0 < ke && (! illegal) && (kb <= (size - d))) {
		fixed_limit_large1(stream, fmt, dec, d, size, kb);
		return;
	}

	/* large2 */
	else {
		fixed_limit_large2(stream, fmt, dec, d, size, kb);
		return;
	}
}

_g void fmtfloat_fixed(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		fixed_free(stream, fmt, dec);
	}
	else if (dpnot) {
		fixed_width(stream, fmt, dec);
	}
	else if (wpnot) {
		fixed_column(stream, fmt, dec);
	}
	else {
		fixed_limit(stream, fmt, dec);
	}
}

static void exponent_fraction(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum index, i, first, size;

	index = fmt->k;
	size = (int)dec->size;

	first = 0;
	for (i = index; i <= 0; i++) {
		WriteChar(stream, '0');
		if (first == 0) {
			WriteChar(stream, '.');
			first = 1;
		}
	}

	for (i = 0; i < index; i++) {
		decimalzero_stream(stream, dec, i);
	}

	for (; i < size; i++) {
		if (first == 0) {
			WriteChar(stream, '.');
			first = 1;
		}
		decimalcheck_stream(stream, dec, i);
	}

	if (first == 0) {
		PrintAscii(stream, ".0");
	}
}

static void exponent_free(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	sign_stream(stream, fmt, dec);
	exponent_fraction(stream, fmt, dec);
	exponent_stream(stream, fmt, dec);
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

static void exponent_width_overflow1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	if (fmtexponent_round(stream, fmt, dec, 1)) return;
	sign_stream(stream, fmt, dec);
	WriteChar(stream, '.');
	times_stream(stream, ka, '0');
	decimalcheck_stream(stream, dec, 0);
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_small2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	size_t i, margin;

	if (fmtexponent_round(stream, fmt, dec, size - ka)) return;
	margin = size - (dec->size + ka  + 1);
	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, ka, '0');
	size = dec->size;
	for (i = 0; i < size; i++)
		decimalcheck_stream(stream, dec, i);
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_small1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	size_t i;

	if (fmtexponent_round(stream, fmt, dec, size - ka)) return;
	sign_stream(stream, fmt, dec);
	WriteChar(stream, '.');
	times_stream(stream, ka, '0');
	size = dec->size;
	for (i = 0; i < size; i++)
		decimalcheck_stream(stream, dec, i);
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_large1_nomargin(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i;

	sign_stream(stream, fmt, dec);
	for (i = 0; i < size; i++) {
		if (i == k)
			WriteChar(stream, '.');
		decimalcheck_stream(stream, dec, i);
	}
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_large1_small(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - dec->size;
	size = dec->size;
	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	for (i = 0; i < size; i++) {
		if (i == k)
			WriteChar(stream, '.');
		decimalcheck_stream(stream, dec, i);
	}
	if (i == k)
		WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_large1_large(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t i, margin;

	margin = size - k;
	margin_stream(stream, fmt, margin);
	sign_stream(stream, fmt, dec);
	for (i = 0; i < dec->size; i++)
		decimalcheck_stream(stream, dec, i);
	size = k - dec->size;
	times_stream(stream, size, '0');
	WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_width_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	if (fmtexponent_round(stream, fmt, dec, size)) {
		return;
	}
	if (size <= dec->size) {
		exponent_width_large1_nomargin(stream, fmt, dec, size, k);
	}
	else if (k <= dec->size) {
		exponent_width_large1_small(stream, fmt, dec, size, k);
	}
	else {
		exponent_width_large1_large(stream, fmt, dec, size, k);
	}
}

static void exponent_width_large2(addr stream,
		fmtfloat fmt, fmtdecimal dec, fixnum k)
{
	fixnum i;

	if (fmtexponent_round(stream, fmt, dec, k)) return;
	sign_stream(stream, fmt, dec);
	for (i = 0; i < k; i++)
		decimalzero_stream(stream, dec, i);
	WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_width(addr stream, fmtfloat fmt, fmtdecimal dec)
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
	if (check) {
		overflow_stream(stream, fmt);
		return;
	}
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka) {
		exponent_width_overflow1(stream, fmt, dec, ka);
		return;
	}

	/* small2 */
	if (k <= 0 && (dec->size + ka < size)) {
		exponent_width_small2(stream, fmt, dec, size, ka);
		return;
	}

	/* small1 */
	if (k <= 0) {
		exponent_width_small1(stream, fmt, dec, size, ka);
		return;
	}

	/* large1 */
	if (0 < k && ka < size) {
		exponent_width_large1(stream, fmt, dec, size, ka);
		return;
	}

	/* large2 */
	if (0 < k && size <= ka) {
		exponent_width_large2(stream, fmt, dec, k);
		return;
	}
}

static void exponent_column_small2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	if (fmtexponent_round(stream, fmt, dec, 1)) return;
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, ka, '0');
	decimalcheck_stream(stream, dec, 0);
	exponent_stream(stream, fmt, dec);
}

static void exponent_column_small1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t ka)
{
	size_t size, i;

	size = d - ka;
	if (fmtexponent_round(stream, fmt, dec, size)) return;
	sign_stream(stream, fmt, dec);
	PrintAscii(stream, "0.");
	times_stream(stream, ka, '0');
	for (i = 0; i < dec->size; i++)
		decimalcheck_stream(stream, dec, i);
	times_stream(stream, size - dec->size, '0');
	exponent_stream(stream, fmt, dec);
}

static void exponent_column_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t d, size_t k)
{
	size_t i, size;

	size = d + 1;
	if (fmtexponent_round(stream, fmt, dec, size)) return;
	sign_stream(stream, fmt, dec);
	for (i = 0; i < size; i++) {
		if (i == k)
			WriteChar(stream, '.');
		decimalzero_stream(stream, dec, i);
	}
	exponent_stream(stream, fmt, dec);
}

static void exponent_column_large2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	size_t i;

	if (fmtexponent_round(stream, fmt, dec, k)) return;
	sign_stream(stream, fmt, dec);
	for (i = 0; i < k; i++)
		decimalzero_stream(stream, dec, i);
	WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_column(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	fixnum k;
	size_t d, ka;

	d = fmt->d;
	k = fmt->k;
	ka = (size_t)(k < 0? -k: k);

	/* small2 */
	if (k < 0 && d <= ka) {
		exponent_column_small2(stream, fmt, dec, ka);
		return;
	}

	/* small1 */
	if (k <= 0 && ka < d) {
		exponent_column_small1(stream, fmt, dec, d, ka);
		return;
	}

	/* large1 */
	if (0 < k && ka <= d) {
		exponent_column_large1(stream, fmt, dec, d, ka);
		return;
	}

	/* large2 */
	if (0 < k && d < ka) {
		exponent_column_large2(stream, fmt, dec, ka);
		return;
	}
}

static void exponent_limit_overflow1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t ka)
{
	if (fmtexponent_round(stream, fmt, dec, 1)) return;
	sign_stream(stream, fmt, dec);
	WriteChar(stream, '.');
	times_stream(stream, ka, '0');
	decimalzero_stream(stream, dec, 0);
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit_small2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka)
{
	size_t margin;

	if (fmtexponent_round(stream, fmt, dec, 1)) return;
	if (ka + 2 < size) {
		margin = size - (ka + 2);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	if (size != ka + 1)
		WriteChar(stream, '0');
	WriteChar(stream, '.');
	times_stream(stream, ka, '0');
	decimalcheck_stream(stream, dec, 0);
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit_small1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t ka, size_t d)
{
	int zero;
	size_t margin, info;

	info = d - ka;
	if (fmtexponent_round(stream, fmt, dec, info)) return;
	zero = (d < size);
	if ((d + zero) < size) {
		margin = size - (d + zero);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	if (zero)
		WriteChar(stream, '0');
	WriteChar(stream, '.');
	times_stream(stream, ka, '0');
	for_decimalzero_stream(stream, dec, 0, info);
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit_large1(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k, size_t d)
{
	size_t margin, info, i;

	info = d + 1;
	if (fmtexponent_round(stream, fmt, dec, info)) return;
	if (d + 1 < size) {
		margin = size - (d + 1);
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	for (i = 0; i < k; i++)
		decimalzero_stream(stream, dec, i);
	WriteChar(stream, '.');
	for (; i < info; i++)
		decimalzero_stream(stream, dec, i);
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit_large2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t size, size_t k)
{
	size_t margin, i;

	if (fmtexponent_round(stream, fmt, dec, k)) return;
	if (k < size) {
		margin = size - k;
		margin_stream(stream, fmt, margin);
	}
	sign_stream(stream, fmt, dec);
	for (i = 0; i < k; i++)
		decimalzero_stream(stream, dec, i);
	WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit_overflow2(addr stream,
		fmtfloat fmt, fmtdecimal dec, size_t k)
{
	size_t i;

	if (fmtexponent_round(stream, fmt, dec, k)) return;
	sign_stream(stream, fmt, dec);
	for (i = 0; i < k; i++)
		decimalzero_stream(stream, dec, i);
	WriteChar(stream, '.');
	exponent_stream(stream, fmt, dec);
}

static void exponent_limit(addr stream, fmtfloat fmt, fmtdecimal dec)
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
	if (fmt->overflowp && (illegal || check)) {
		overflow_stream(stream, fmt);
		return;
	}
	size = (w <= size)? 0: (w - size);

	/* overflow1 */
	if (k < 0 && size <= ka) {
		exponent_limit_overflow1(stream, fmt, dec, ka);
		return;
	}

	/* small2 */
	if (k <= 0 && d <= ka) {
		exponent_limit_small2(stream, fmt, dec, size, ka);
		return;
	}

	/* small1 */
	if (k <= 0) {
		exponent_limit_small1(stream, fmt, dec, size, ka, d);
		return;
	}

	/* large1 */
	if (0 < k && ka <= d) {
		exponent_limit_large1(stream, fmt, dec, size, ka, d);
		return;
	}

	/* large2 */
	if (0 < k && ka <= size) {
		exponent_limit_large2(stream, fmt, dec, size, ka);
		return;
	}

	/* overflow2 */
	if (0 < k && size < ka) {
		exponent_limit_overflow2(stream, fmt, dec, ka);
		return;
	}
}

_g void fmtfloat_exponent(addr stream, fmtfloat fmt, fmtdecimal dec)
{
	int wp, dp, wpnot, dpnot;

	wp = fmt->wp;
	dp = fmt->dp;
	wpnot = ! wp;
	dpnot = ! dp;

	if (wpnot && dpnot) {
		exponent_free(stream, fmt, dec);
	}
	else if (dpnot) {
		exponent_width(stream, fmt, dec);
	}
	else if (wpnot) {
		exponent_column(stream, fmt, dec);
	}
	else {
		exponent_limit(stream, fmt, dec);
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

_g void fmtfloat_general(addr stream, fmtfloat fmt, fmtdecimal dec)
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
		/* call fmtfloat_fixed */
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
		fmtfloat_fixed(stream, fmt, dec);
		times_stream(stream, ee, ' ');
	}
	else {
		/* call fmtfloat_exponent */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = 1;
		fmt->sign_exponent = 1; /* for prin1 */
		fmtfloat_exponent(stream, fmt, dec);
	}
}

_g void fmtfloat_dollars(addr stream, fmtfloat fmt, fmtdecimal dec)
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
		sign_stream(stream, fmt, dec);
		margin_stream(stream, fmt, p);
	}
	else {
		/* padding -> sign*/
		margin_stream(stream, fmt, p);
		sign_stream(stream, fmt, dec);
	}
	/* large */
	times_stream(stream, n1, '0');
	for_decimalzero_stream(stream, dec, 0, n2);
	/* dot */
	WriteChar(stream, '.');
	/* small */
	for (i = 0; i < d; i++) {
		index = ((int)i) + e + 1;
		if (index < 0)
			WriteChar(stream, '0');
		else
			decimalzero_stream(stream, dec, index);
	}
}


/*
 *  ~F  Fixed floating-point
 */
_g void fmtfloat_fixed_float(addr stream, single_float value,
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
	fmtfloat_fixed(stream, &fmt, &dec);
}

_g void fmtfloat_fixed_double(addr stream, double_float value,
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
	fmtfloat_fixed(stream, &fmt, &dec);
}


/*
 *  ~E  Exponential floating-point
 */
_g void fmtfloat_exponent_float(addr stream, single_float value,
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
	fmtfloat_exponent(stream, &fmt, &dec);
}

_g void fmtfloat_exponent_double(addr stream, double_float value,
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
	fmtfloat_exponent(stream, &fmt, &dec);
}


/*****************************************************************************
 *  princ / prin1
 *****************************************************************************/
static int fmtfloat_princ_fixedp(fmtdecimal dec)
{
	/* return 1e-3 <= fabs(dec) < 1e7 */
	return -3 <= dec->exponent && dec->exponent < 7;
}

static void fmtfloat_princ(addr stream, fmtfloat fmt, fmtdecimal dec,
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
		fmtfloat_fixed(stream, fmt, dec);
	}
	else {
		fmt->k = 1; /* default 1 */
		fmt->k_bias = 1; /* 1 if exponential. */
		fmt->markerp = markerp;
		if (markerp) {
			fmt->ep = 1;
			fmt->e = 1;
			fmt->markerp = 1;
			fmt->marker = marker;
			fmt->sign_exponent = 1; /* 0 if princ / prin1 */
		}
		fmtfloat_exponent(stream, fmt, dec);
	}
}

_g int fmtfloat_princ_single_float(addr stream,
		single_float value, int markerp, unicode marker)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) return 1;
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_single = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmtfloat_princ(stream, &fmt, &dec, markerp, marker);

	return 0;
}

_g int fmtfloat_princ_double_float(addr stream,
		double_float value, int markerp, unicode marker)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) return 1;
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_double = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmtfloat_princ(stream, &fmt, &dec, markerp, marker);

	return 0;
}

_g int fmtfloat_princ_long_float(addr stream,
		long_float value, int markerp, unicode marker)
{
	struct fmtfloat_struct fmt;
	struct fmtdecimal_struct dec;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG)) return 1;
	memset(&fmt, 0, sizeoft(struct fmtfloat_struct));
	fmt.u.value_long = value;
	fmt.signbit = signbit(value)? 1: 0;
	fmtfloat_princ(stream, &fmt, &dec, markerp, marker);

	return 0;
}

