#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "bignum_output.h"
#include "character.h"
#include "character_check.h"
#include "condition.h"
#include "local.h"
#include "integer.h"
#include "integer_calc.h"
#include "integer_common.h"
#include "stream_string.h"
#include "strtype.h"
#include "token.h"
#include "typedef.h"

/*
 *  output
 */
int output_nosign_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_fixnum_(stream, RefFixnum(pos), base, upperp);

		case LISPTYPE_BIGNUM:
			return output_nosign_bignum_(local, stream, pos, base, upperp);

		default:
			return TypeError_(pos, INTEGER);
	}
}

int output_nosign_comma_integer_(LocalRoot local, addr stream,
		addr pos, unsigned base, int upperp, size_t range, unicode comma)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return output_nosign_comma_fixnum_(local, stream, RefFixnum(pos),
					base, upperp, range, comma);

		case LISPTYPE_BIGNUM:
			return output_nosign_comma_bignum_(local, stream, pos,
					base, upperp, range, comma);

		default:
			return TypeError_(pos, INTEGER);
	}
}

#define INTEGER_STREAM_SIZE		64
int string_nosign_comma_integer_(LocalRoot local, addr *ret, addr pos,
		unsigned base, int upperp, size_t range, unicode comma)
{
	addr stream;

	open_output_string_stream(&stream, INTEGER_STREAM_SIZE);
	Return(output_nosign_comma_integer_(local,
				stream, pos, base, upperp, range, comma));
	Return(string_stream_alloc_(local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  integer-length
 */
static size_t integer_length_fixed(fixed value)
{
	size_t size;

	for (size = 0; value; size++)
		value >>= 1;

	return size;
}

static size_t inverse_length_fixed(fixed value)
{
	if (value <= 1)
		return 0;
	else
		return integer_length_fixed(value - 1);
}

static void integer_length_fixnum(addr pos, size_t *ret)
{
	int sign;
	fixed value;

	castfixed_fixnum(pos, &sign, &value);
	if (IsPlus(sign))
		*ret = integer_length_fixed(value);
	else
		*ret = inverse_length_fixed(value);
}

static size_t integer_length_bigdata(addr pos)
{
	size_t size, size1, size2;
	fixed *data;

	GetSizeBignum(pos, &size);
	GetDataBignum(pos, &data);
	if (size == 1 && data[0] == 0)
		return 0;
	size--;
	size1 = size * BIGNUM_FULLBIT;
	size2 = integer_length_fixed(data[size]);

	return size1 + size2;
}

static int check_length_bignum(addr pos)
{
	int sign;
	fixed *data;
	size_t size, i;

	GetSignBignum(pos, &sign);
	if (IsPlus(sign))
		return 0;
	GetSizeBignum(pos, &size);
	size--;
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++) {
		if (data[i] != 0)
			return 0;
	}

	return data[i] == 1;
}

static void integer_length_bignum(addr pos, size_t *ret)
{
	size_t size;

	size = integer_length_bigdata(pos);
	if (check_length_bignum(pos))
		size--;
	*ret = size;
}

int integer_length_value_(addr pos, size_t *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			integer_length_fixnum(pos, ret);
			return 0;

		case LISPTYPE_BIGNUM:
			integer_length_bignum(pos, ret);
			return 0;

		default:
			*ret = 0;
			return TypeError_(pos, INTEGER);
	}
}

int integer_length_common_(addr pos, addr *ret)
{
	size_t size;

	Return(integer_length_value_(pos, &size));
	make_index_integer_heap(ret, size);

	return 0;
}


/*
 *  ash
 */
static int ash_mm_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	Check(shift == 0, "shift error");

	/* size over */
	size = integer_length_bigdata(pos);
	if (size <= shift) {
		fixnum_heap(ret, -1);
		return 0;
	}

	/* shiftdown */
	shiftdown_minus_bigdata(local, &pos, pos, shift);
	SetSignBignum(pos, SignMinus);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_up_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	int sign;

	GetSignBignum(pos, &sign);
	shiftup_bigdata_alloc(local, &pos, pos, shift);
	SetSignBignum(pos, sign);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_down_common_(LocalRoot local, addr pos, size_t shift, addr *ret)
{
	int sign;

	GetSignBignum(pos, &sign);
	shiftdown_bigdata_alloc(local, &pos, pos, shift);
	SetSignBignum(pos, sign);
	bignum_result_heap(pos, ret);

	return 0;
}

static int ash_left_common_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int check;

	*value = Nil;
	*ret = 0;
	Return(zerop_integer_(pos, &check));
	if (check) {
		*value = pos;
		return Result(ret, 1);
	}
	if (fixnump(pos))
		bignum_fixnum_local(local, &pos, pos);
	if (! bignump(pos))
		return TypeError_(pos, INTEGER);

	*value = pos;
	return Result(ret, 0);
}

static int ash_right_common_(LocalRoot local,
		addr count, size_t *value, int *sign, int *ret)
{
	int check;
	size_t shift;

	*value = 0;
	*sign = 0;
	*ret = 0;
	Return(getindex_sign_integer_(count, sign, &shift, &check));
	if (check)
		return fmte_("Too large shift value ~S.", count, NULL);
	if (shift == 0)
		return Result(ret, 1);

	*value = shift;
	return Result(ret, 0);
}

int ash_integer_common_(LocalRoot local, addr pos, addr count, addr *ret)
{
	int sign1, sign2, check;
	addr value;
	size_t shift;
	LocalStack stack;

	push_local(local, &stack);
	/* left */
	Return(ash_left_common_(local, pos, &value, &check));
	if (check) {
		*ret = pos;
		goto finish;
	}
	/* right */
	Return(ash_right_common_(local, count, &shift, &sign2, &check));
	if (check) {
		*ret = pos;
		goto finish;
	}

	/* sign */
	GetSignBignum(value, &sign1);
	if (IsPlus(sign1) && IsPlus(sign2)) {
		Return(ash_up_common_(local, value, shift, ret));
	}
	else if (IsPlus(sign1)) {
		Return(ash_down_common_(local, value, shift, ret));
	}
	else if (IsPlus(sign2)) {
		Return(ash_up_common_(local, value, shift, ret));
	}
	else {
		Return(ash_mm_common_(local, value, shift, ret));
	}
finish:
	rollback_local(local, stack);

	return 0;
}


/*
 *  parse-integer
 */
struct parse_integer_struct {
	unsigned sign, radix;
	addr input, cons;
	size_t end, index;
};

static int parse_integer_error_(LocalRoot local,
		struct parse_integer_struct *str, addr *ret, addr *rpos)
{
	unicode c;
	unsigned v;

	*ret = *rpos = Nil;
first:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto first;
	}
	/* sign */
	if (c == '+') {
		str->index++;
		str->sign = SignPlus;
		goto sign;
	}
	if (c == '-') {
		str->index++;
		str->sign = SignMinus;
		goto sign;
	}
	/* digit */
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

sign:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);

loop:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto last;
	}
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

last:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto last;
	}
	goto error;

finish:
	integer_cons_heap(ret, str->sign, str->cons);
	make_index_integer_heap(rpos, str->index);
	return 0;

error:
	*ret = *rpos = Nil;
	return call_simple_parse_error_va_(NULL,
			"The string ~S is not integer.", str->input, NULL);
}

static int parse_integer_junk_allowed_(LocalRoot local,
		struct parse_integer_struct *str, addr *ret, addr *rpos)
{
	unicode c;
	unsigned v;

	*ret = *rpos = Nil;
first:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c)) {
		str->index++;
		goto first;
	}
	/* sign */
	if (c == '+') {
		str->index++;
		str->sign = SignPlus;
		goto sign;
	}
	if (c == '-') {
		str->index++;
		str->sign = SignMinus;
		goto sign;
	}
	/* digit */
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

sign:
	if (str->end <= str->index)
		goto error;
	Return(string_getc_(str->input, str->index, &c));
	if (getvalue_digit(str->radix, c, &v))
		goto error;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);

loop:
	if (str->end <= str->index)
		goto finish;
	Return(string_getc_(str->input, str->index, &c));
	if (isSpaceUnicode(c))
		goto finish;
	if (getvalue_digit(str->radix, c, &v))
		goto finish;
	str->index++;
	push_bigcons(local, str->cons, str->radix, v);
	goto loop;

finish:
	integer_cons_heap(ret, str->sign, str->cons);
	make_index_integer_heap(rpos, str->index);
	return 0;

error:
	*ret = Nil;
	make_index_integer_heap(rpos, str->index);
	return 0;
}

int parse_integer_clang_(LocalRoot local,
		addr input, size_t start, size_t end, unsigned radix, int junk,
		addr *ret, addr *rpos)
{
	addr cons;
	LocalStack stack;
	struct parse_integer_struct str;

	push_local(local, &stack);
	bigcons_local(local, &cons);
	str.sign = SignPlus;
	str.input = input;
	str.cons = cons;
	str.index = start;
	str.end = end;
	str.radix = radix;
	if (! junk) {
		Return(parse_integer_error_(local, &str, ret, rpos));
	}
	else {
		Return(parse_integer_junk_allowed_(local, &str, ret, rpos));
	}
	rollback_local(local, stack);

	return 0;
}

