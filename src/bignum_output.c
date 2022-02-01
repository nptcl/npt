#include "character.h"
#include "character_check.h"
#include "character_queue.h"
#include "condition.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_output.h"
#include "token.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_string.h"

/*
 *  output
 */
int decimal_charqueue_fixnum_local_(LocalRoot local, addr pos, addr queue)
{
	char buffer[256];

	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	snprintf(buffer, 256, "%" PRIdF, RefFixnum(pos));
	return pushchar_charqueue_local_(local, queue, buffer);
}

int decimal_charqueue_bignum_local_(LocalRoot local, addr pos, addr queue)
{
	addr stream, value;

	Check(GetType(pos) != LISPTYPE_BIGNUM, "type error");
	/* zero */
	if (zerop_bignum(pos))
		return push_charqueue_local_(local, queue, '0');

	/* sign */
	if (minusp_bignum(pos)) {
		Return(push_charqueue_local_(local, queue, '-'));
	}

	/* body */
	open_output_string_stream(&stream, 0);
	Return(output_nosign_bignum_(local, stream, pos, 10, 0));
	Return(string_stream_heap_(stream, &value));
	clear_output_string_stream(stream);
	return pushstring_charqueue_local_(local, queue, value);
}

int decimal_charqueue_integer_local_(LocalRoot local, addr pos, addr queue)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return decimal_charqueue_fixnum_local_(local, pos, queue);

		case LISPTYPE_BIGNUM:
			return decimal_charqueue_bignum_local_(local, pos, queue);

		default:
			return TypeError_(pos, INTEGER);
	}
}


/*
 *  print
 */
/* base=2, 64bit -> 1+64+1 -> 66+padding -> 72? */
#define FIXNUM_BUFFER_SIZE  128
int output_nosign_index_(addr stream, size_t n, unsigned base, int upperp)
{
	size_t m;
	char buffer[FIXNUM_BUFFER_SIZE], *ptr, chara;

	/* zero */
	if (n == 0)
		return write_char_stream_(stream, '0');

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_SIZE - 1;
	*(ptr--) = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		*(ptr--) = (char)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
	}

	return print_ascii_stream_(stream, ptr + 1);
}

int output_nosign_fixnum_(addr stream, fixnum value, unsigned base, int upperp)
{
	int sign;
	fixed m, n;
	char buffer[FIXNUM_BUFFER_SIZE], *ptr, chara;

	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* fixnum -> fixed */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_SIZE - 1;
	*(ptr--) = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		*(ptr--) = (char)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
	}

	return print_ascii_stream_(stream, ptr + 1);
}

static int charbit_nil_p(addr pos)
{
	size_t size;

	if (pos == Nil)
		return 1;
	GetCharBitSize(pos, &size);

	return size == 0;
}

static void charqueue_nreverse(addr pos, addr *ret)
{
	addr tail, next;

	if (charbit_nil_p(pos)) {
		*ret = pos;
		return;
	}

	for (tail = Nil; ; tail = pos, pos = next) {
		GetCharBitNext(pos, &next);
		SetCharBitNext(pos, tail);
		if (charbit_nil_p(next))
			break;
	}
	*ret = pos;
}

static int charqueue_nreverse_output_(addr pos, addr stream)
{
	unicode *ptr;
	size_t size;

	GetCharQueueRoot(pos, &pos);
	charqueue_nreverse(pos, &pos);
	while (pos != Nil) {
		GetCharBitSize(pos, &size);
		if (size == 0)
			break;
		ptr = PtrCharBitChar(pos);
		do {
			size--;
			Return(write_char_stream_(stream, ptr[size]));
		}
		while (size);
		GetCharBitNext(pos, &pos);
	}

	return 0;
}

int output_nosign_bignum_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp)
{
	unicode u;
	addr queue, error_character;
	fixed rem;
	size_t size;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	/* zero */
	if (zerop_bignum(pos))
		return write_char_stream_(stream, '0');

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	size = (0x010000 < size)? 0x010000: size * 20; /* size * (log10(2**64)+1) */
	charqueue_local(local, &queue, size);
	bignum_copy_local(local, &pos, pos);
	do {
		rem = letdiv_half_bigdata(pos, (fixed)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			return fmte_("Invalid digit character ~S.", error_character, NULL);
		}
		Return(push_charqueue_local_(local, queue, u));
	}
	while (! zerop_bignum(pos));

	/* output */
	Return(charqueue_nreverse_output_(queue, stream));
	rollback_local(local, stack);

	return 0;
}

#define FIXNUM_BUFFER_DOUBLE_SIZE  (FIXNUM_BUFFER_SIZE * 2)
int output_nosign_comma_fixnum_(LocalRoot local,
		addr stream, fixnum value, unsigned base, int upperp,
		size_t range, unicode comma)
{
	int sign;
	fixed m, n;
	unicode buffer[FIXNUM_BUFFER_DOUBLE_SIZE], *ptr, chara;
	size_t index;

	Check(range < 2, "ragen error");
	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* fixnum -> fixed */
	castfixed(value, &sign, &n);

	/* loop */
	chara = upperp? 'A': 'a';
	ptr = buffer + FIXNUM_BUFFER_DOUBLE_SIZE - 1;
	*(ptr--) = 0;
	index = 0;
	while (n != 0) {
		m = n % base;
		n = n / base;
		if (index && (index % range) == 0)
			*(ptr--) = comma;
		*(ptr--) = (unicode)((m <= 9)? ('0' + m): (chara - 10 + m));
		Check(ptr <= buffer, "buffer error");
		index++;
	}

	return print_unicode_stream_(stream, ptr + 1);
}

int output_nosign_comma_bignum_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp,
		size_t range, unicode comma)
{
	unicode u;
	addr queue, error_character;
	fixed rem;
	size_t size, index;
	LocalStack stack;

	Check(local == NULL, "local error");
	Check(! isBaseChar(base), "base error");
	Check(range < 2, "ragen error");
	/* zero */
	if (zerop_bignum(pos))
		return write_char_stream_(stream, '0');

	/* loop */
	push_local(local, &stack);
	GetSizeBignum(pos, &size);
	size = (0x010000 < size)? 0x010000: size * 20; /* size * (log10(2**64)+1) */
	charqueue_local(local, &queue, size);
	bignum_copy_local(local, &pos, pos);
	index = 0;
	do {
		rem = letdiv_half_bigdata(pos, (fixed)base);
		if (getchar_digit((unsigned)rem, upperp, &u)) {
			character_heap(&error_character, u);
			return fmte_("Invalid digit character ~S.", error_character, NULL);
		}
		if (index && (index % range) == 0) {
			Return(push_charqueue_local_(local, queue, comma));
		}
		Return(push_charqueue_local_(local, queue, u));
		index++;
	}
	while (! zerop_bignum(pos));

	/* output */
	Return(charqueue_nreverse_output_(queue, stream));
	rollback_local(local, stack);

	return 0;
}

