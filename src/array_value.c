#include "array.h"
#include "array_value.h"
#include "bigdata.h"
#include "bignum.h"
#include "bit.h"
#include "copy.h"
#include "define.h"
#include "character.h"
#include "condition.h"
#include "integer.h"
#include "strtype.h"

/*
 *  arrayvalue
 */
_g void arrayvalue_get_character(struct array_value *str, addr x)
{
	if (! characterp(x))
		fmte("~S must be character type.", x, NULL);
	GetCharacter(x, &(str->value.character));
	str->type = ARRAY_TYPE_CHARACTER;
}

_g void arrayvalue_get_bit(struct array_value *str, addr x)
{
	int check;

	if (! fixnump(x))
		fmte("~S must be an integer type.", x, NULL);
	if (bit_getint(x, &check))
		fmte("The integer ~S must be a bit value.", x, NULL);
	str->value.bit = (check != 0);
	str->type = ARRAY_TYPE_BIT;
}

_g void arrayvalue_get_signed8(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str->value.signed8 = (int8_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 8;
	return;

error:
	fmte("Overflow ~S in (signed-byte 8).", x, NULL);
}

_g void arrayvalue_get_signed16(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	str->value.signed16 = (int16_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 16;
	return;

error:
	fmte("Overflow ~S in (signed-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
_g void arrayvalue_get_signed32(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;
	return;

error:
	fmte("Overflow ~S in (signed-byte 32).", x, NULL);
}

_g void arrayvalue_get_signed64(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		fmte("Overflow ~S in (signed-byte 64).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed64 = (int64_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 64;
}
#else
_g void arrayvalue_get_signed32(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		fmte("Overflow ~S in (signed-byte 32).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;
}
#endif

_g void arrayvalue_get_signed(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			arrayvalue_get_signed8(str, x);
			break;

		case 16:
			arrayvalue_get_signed16(str, x);
			break;

		case 32:
			arrayvalue_get_signed32(str, x);
			break;

#ifdef LISP_64BIT
		case 64:
			arrayvalue_get_signed64(str, x);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

_g void arrayvalue_get_unsigned8(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (minusp_integer(x))
		fmte("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT8_MAX < init)
		goto error;
	str->value.unsigned8 = (uint8_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 8;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 8).", x, NULL);
}

_g void arrayvalue_get_unsigned16(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (minusp_integer(x))
		fmte("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT16_MAX < init)
		goto error;
	str->value.unsigned16 = (uint16_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 16;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
_g void arrayvalue_get_unsigned32(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (minusp_integer(x))
		fmte("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT32_MAX < init)
		goto error;
	str->value.unsigned32 = (uint32_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 32;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 32).", x, NULL);
}

_g void arrayvalue_get_unsigned64(struct array_value *str, addr x)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (minusp_integer(x))
		fmte("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned64 = (uint64_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			fmte("Overflow ~S in (unsigned-byte 64).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned64 = (uint64_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return;
	}
	TypeError(x, INTEGER);
}
#else
_g void arrayvalue_get_unsigned32(struct array_value *str, addr x)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(x))
		fmte("~S must be an integer type.", x, NULL);
	if (minusp_integer(x))
		fmte("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned32 = (uint32_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			fmte("Overflow ~S in (unsigned-byte 32).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned32 = (uint32_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return;
	}
	TypeError(x, INTEGER);
}
#endif

_g void arrayvalue_get_unsigned(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			arrayvalue_get_unsigned8(str, x);
			break;

		case 16:
			arrayvalue_get_unsigned16(str, x);
			break;

		case 32:
			arrayvalue_get_unsigned32(str, x);
			break;

#ifdef LISP_64BIT
		case 64:
			arrayvalue_get_unsigned64(str, x);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

_g void arrayvalue_get_single(struct array_value *str, addr x)
{
	if (! single_float_p(x))
		fmte("~S must be single-float type.", x, NULL);
	GetSingleFloat(x, &(str->value.single_value));
	str->type = ARRAY_TYPE_SINGLE_FLOAT;
}

_g void arrayvalue_get_double(struct array_value *str, addr x)
{
	if (! double_float_p(x))
		fmte("~S must be double-float type.", x, NULL);
	GetDoubleFloat(x, &(str->value.double_value));
	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
}

_g void arrayvalue_get_long(struct array_value *str, addr x)
{
	if (! long_float_p(x))
		fmte("~S must be long-float type.", x, NULL);
	GetLongFloat(x, &(str->value.long_value));
	str->type = ARRAY_TYPE_LONG_FLOAT;
}


/*
 *  array_value
 */
static void arrayvalue_make_signed(LocalRoot local,
		addr *ret, const struct array_value *str)
{
	switch (str->size) {
		case 8:
			int8_integer_alloc(local, ret, str->value.signed8);
			break;

		case 16:
			int16_integer_alloc(local, ret, str->value.signed16);
			break;

		case 32:
			int32_integer_alloc(local, ret, str->value.signed32);
			break;

#ifdef LISP_64BIT
		case 64:
			int64_integer_alloc(local, ret, str->value.signed64);
			break;
#endif

		default:
			fmte("size error", NULL);
			return;
	}
}

static void arrayvalue_make_unsigned(LocalRoot local,
		addr *ret, const struct array_value *str)
{
	switch (str->size) {
		case 8:
			uint8_integer_alloc(local, ret, str->value.unsigned8);
			break;

		case 16:
			uint16_integer_alloc(local, ret, str->value.unsigned16);
			break;

		case 32:
			uint32_integer_alloc(local, ret, str->value.unsigned32);
			break;

#ifdef LISP_64BIT
		case 64:
			uint64_integer_alloc(local, ret, str->value.unsigned64);
			break;
#endif

		default:
			fmte("size error", NULL);
			return;
	}
}

_g void arrayvalue_alloc(LocalRoot local, addr *ret, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_T:
			copylocal_object(local, ret, str->value.object);
			break;

		case ARRAY_TYPE_BIT:
			fixnum_alloc(local, ret, (str->value.bit)? 1: 0);
			break;

		case ARRAY_TYPE_CHARACTER:
			character_alloc(local, ret, str->value.character);
			break;

		case ARRAY_TYPE_SIGNED:
			arrayvalue_make_signed(local, ret, str);
			break;

		case ARRAY_TYPE_UNSIGNED:
			arrayvalue_make_unsigned(local, ret, str);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			single_float_alloc(local, ret, str->value.single_value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			double_float_alloc(local, ret, str->value.double_value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			long_float_alloc(local, ret, str->value.long_value);
			break;

		default:
			fmte("Invalid array value.", NULL);
			return;
	}
}

_g void arrayvalue_local(LocalRoot local, addr *ret, const struct array_value *str)
{
	Check(local == NULL, "local error");
	arrayvalue_alloc(local, ret, str);
}

_g void arrayvalue_heap(addr *ret, const struct array_value *str)
{
	arrayvalue_alloc(NULL, ret, str);
}

