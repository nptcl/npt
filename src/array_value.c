#include "array.h"
#include "array_value.h"
#include "bignum_data.h"
#include "bignum_object.h"
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
int arrayvalue_get_character_(struct array_value *str, addr x)
{
	if (! characterp(x))
		return fmte_("~S must be character type.", x, NULL);
	GetCharacter(x, &(str->value.character));
	str->type = ARRAY_TYPE_CHARACTER;

	return 0;
}

int arrayvalue_get_bit_(struct array_value *str, addr x)
{
	int check;

	if (! fixnump(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (bit_getint(x, &check))
		return fmte_("The integer ~S must be a bit value.", x, NULL);
	str->value.bit = (check != 0);
	str->type = ARRAY_TYPE_BIT;

	return 0;
}

int arrayvalue_get_signed8_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str->value.signed8 = (int8_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 8;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 8).", x, NULL);
}

int arrayvalue_get_signed16_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	str->value.signed16 = (int16_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 16;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
int arrayvalue_get_signed32_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;
	return 0;

error:
	return fmte_("Overflow ~S in (signed-byte 32).", x, NULL);
}

int arrayvalue_get_signed64_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		return fmte_("Overflow ~S in (signed-byte 64).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed64 = (int64_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 64;

	return 0;
}
#else
int arrayvalue_get_signed32_(struct array_value *str, addr x)
{
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	if (! fixnump(x))
		return fmte_("Overflow ~S in (signed-byte 32).", x, NULL);
	GetFixnum(x, &init);
	str->value.signed32 = (int32_t)init;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 32;

	return 0;
}
#endif

int arrayvalue_get_signed_(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			return arrayvalue_get_signed8_(str, x);

		case 16:
			return arrayvalue_get_signed16_(str, x);

		case 32:
			return arrayvalue_get_signed32_(str, x);

#ifdef LISP_64BIT
		case 64:
			return arrayvalue_get_signed64_(str, x);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

int arrayvalue_get_unsigned8_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT8_MAX < init)
		goto error;
	str->value.unsigned8 = (uint8_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 8;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 8).", x, NULL);
}

int arrayvalue_get_unsigned16_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT16_MAX < init)
		goto error;
	str->value.unsigned16 = (uint16_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 16;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 16).", x, NULL);
}

#ifdef LISP_64BIT
int arrayvalue_get_unsigned32_(struct array_value *str, addr x)
{
	int check;
	fixnum init;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (! fixnump(x))
		goto error;
	GetFixnum(x, &init);
	if (UINT32_MAX < init)
		goto error;
	str->value.unsigned32 = (uint32_t)init;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 32;
	return 0;

error:
	return fmte_("Overflow ~S in (unsigned-byte 32).", x, NULL);
}

int arrayvalue_get_unsigned64_(struct array_value *str, addr x)
{
	int check;
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned64 = (uint64_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return 0;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			return fmte_("Overflow ~S in (unsigned-byte 64).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned64 = (uint64_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 64;
		return 0;
	}
	return TypeError_(x, INTEGER);
}
#else
int arrayvalue_get_unsigned32_(struct array_value *str, addr x)
{
	int check;
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(x))
		return fmte_("~S must be an integer type.", x, NULL);
	Return(minusp_integer_(x, &check));
	if (check)
		return fmte_("~S must be a non-negative integer.", x, NULL);
	if (fixnump(x)) {
		GetFixnum(x, &init);
		str->value.unsigned32 = (uint32_t)init;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return 0;
	}
	if (bignump(x)) {
		GetSizeBignum(x, &size);
		if (size != 1)
			return fmte_("Overflow ~S in (unsigned-byte 32).", x, NULL);
		getfixed_bignum(x, 0, &bigv);
		str->value.unsigned32 = (uint32_t)bigv;
		str->type = ARRAY_TYPE_UNSIGNED;
		str->size = 32;
		return 0;
	}
	return TypeError_(x, INTEGER);
}
#endif

int arrayvalue_get_unsigned_(struct array_value *str, addr x, unsigned size)
{
	switch (size) {
		case 8:
			return arrayvalue_get_unsigned8_(str, x);

		case 16:
			return arrayvalue_get_unsigned16_(str, x);

		case 32:
			return arrayvalue_get_unsigned32_(str, x);

#ifdef LISP_64BIT
		case 64:
			return arrayvalue_get_unsigned64_(str, x);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

int arrayvalue_get_single_(struct array_value *str, addr x)
{
	if (! single_float_p(x))
		return fmte_("~S must be single-float type.", x, NULL);
	GetSingleFloat(x, &(str->value.single_value));
	str->type = ARRAY_TYPE_SINGLE_FLOAT;

	return 0;
}

int arrayvalue_get_double_(struct array_value *str, addr x)
{
	if (! double_float_p(x))
		return fmte_("~S must be double-float type.", x, NULL);
	GetDoubleFloat(x, &(str->value.double_value));
	str->type = ARRAY_TYPE_DOUBLE_FLOAT;

	return 0;
}

int arrayvalue_get_long_(struct array_value *str, addr x)
{
	if (! long_float_p(x))
		return fmte_("~S must be long-float type.", x, NULL);
	GetLongFloat(x, &(str->value.long_value));
	str->type = ARRAY_TYPE_LONG_FLOAT;

	return 0;
}


/*
 *  array_value
 */
static int arrayvalue_make_signed_(LocalRoot local,
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
			*ret = Nil;
			return fmte_("size error", NULL);
	}

	return 0;
}

static int arrayvalue_make_unsigned_(LocalRoot local,
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
			*ret = Nil;
			return fmte_("size error", NULL);
	}

	return 0;
}

int arrayvalue_alloc_(LocalRoot local, addr *ret, const struct array_value *str)
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
			return arrayvalue_make_signed_(local, ret, str);

		case ARRAY_TYPE_UNSIGNED:
			return arrayvalue_make_unsigned_(local, ret, str);

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
			return fmte_("Invalid array value.", NULL);
	}

	return 0;
}

int arrayvalue_local_(LocalRoot local, addr *ret, const struct array_value *str)
{
	Check(local == NULL, "local error");
	return arrayvalue_alloc_(local, ret, str);
}

int arrayvalue_heap_(addr *ret, const struct array_value *str)
{
	return arrayvalue_alloc_(NULL, ret, str);
}

