#include "array_inplace.h"
#include "bit.h"
#include "cons.h"
#include "condition.h"
#include "integer.h"
#include "sequence_safe.h"
#include "typedef.h"

/* array */
static int get_unsigned8_t_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	return GetByte_integer(pos->value.object, ret);
}

static int get_unsigned8_bit_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	*ret = pos->value.bit;
	return 0;
}

static int get_unsigned8_signed_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_ARCH_64BIT
	int64_t s64;
#endif

	switch (pos->size) {
		case 8:
			s8 = pos->value.signed8;
			if (s8 < 0)
				return 1;
			*ret = (byte)s8;
			break;

		case 16:
			s16 = pos->value.signed16;
			if (! IsByteSign(s16))
				return 1;
			*ret = (byte)s16;
			break;

		case 32:
			s32 = pos->value.signed32;
			if (! IsByteSign(s32))
				return 1;
			*ret = (byte)s32;
			break;

#ifdef LISP_ARCH_64BIT
		case 64:
			s64 = pos->value.signed64;
			if (! IsByteSign(s64))
				return 1;
			*ret = (byte)s64;
			break;
#endif
		default:
			return 1;
	}

	return 0;
}

static int get_unsigned8_unsigned_array(const struct array_value *pos,
		size_t index, byte *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_ARCH_64BIT
	uint64_t u64;
#endif

	switch (pos->size) {
		case 8:
			*ret = pos->value.unsigned8;
			break;

		case 16:
			u16 = pos->value.unsigned16;
			if (! IsByteUnsign(u16))
				return 1;
			*ret = (byte)u16;
			break;

		case 32:
			u32 = pos->value.unsigned32;
			if (! IsByteUnsign(u32))
				return 1;
			*ret = (byte)u32;
			break;

#ifdef LISP_ARCH_64BIT
		case 64:
			u64 = pos->value.unsigned64;
			if (! IsByteUnsign(u64))
				return 1;
			*ret = (byte)u64;
			break;
#endif
		default:
			return 1;
	}

	return 0;
}

static int get_unsigned8_array(addr pos, size_t index, byte *ret)
{
	struct array_value value;

	if (arrayinplace_get_safe(pos, index, &value))
		goto error;
	switch (value.type) {
		case ARRAY_TYPE_T:
			return get_unsigned8_t_array(&value, index, ret);

		case ARRAY_TYPE_BIT:
			return get_unsigned8_bit_array(&value, index, ret);

		case ARRAY_TYPE_SIGNED:
			return get_unsigned8_signed_array(&value, index, ret);

		case ARRAY_TYPE_UNSIGNED:
			return get_unsigned8_unsigned_array(&value, index, ret);

		default:
			goto error;
	}
error:
	*ret = 0;
	return 1;
}

/* list */
static int getelt_list_safe(addr pos, size_t index, addr *ret)
{
	for (;;) {
		if (! consp(pos)) {
			*ret = Nil;
			return 1;
		}
		if (index == 0)
			break;
		GetCdr(pos, &pos);
		index--;
	}
	GetCar(pos, ret);
	return 0;
}

static int get_unsigned8_list(addr pos, size_t index, byte *ret)
{
	if (getelt_list_safe(pos, index, &pos))
		goto error;
	if (GetByte_integer(pos, ret))
		goto error;
	return 0;

error:
	*ret = 0;
	return 1;
}

/* vector */
static int getelt_vector_safe(addr pos, size_t index, addr *ret)
{
	if (lenarrayr(pos) <= index) {
		*ret = 0;
		return 1;
	}
	getarray(pos, index, ret);
	return 0;
}

static int get_unsigned8_vector(addr pos, size_t index, byte *ret)
{
	if (getelt_vector_safe(pos, index, &pos))
		goto error;
	if (GetByte_integer(pos, ret))
		goto error;
	return 0;

error:
	*ret = 0;
	return 1;
}

/* bit-vector */
static int getelt_bitvector_safe(addr pos, size_t index, int *ret)
{
	size_t size;

	Return(bitvector_length_(pos, &size));
	if (size <= index) {
		*ret = 0;
		return fmte_("Index ~S is too large.", intsizeh(index), NULL);
	}

	return bitmemory_getint_(pos, index, ret);
}

static int get_unsigned8_bitvector(addr pos, size_t index, byte *ret)
{
	int bit;

	if (getelt_bitvector_safe(pos, index, &bit)) {
		*ret = 0;
		return 1;
	}
	*ret = (byte)bit;
	return 0;
}

int get_unsigned8_sequence(addr pos, size_t index, byte *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return get_unsigned8_list(pos, index, ret);

		case LISPTYPE_VECTOR:
			return get_unsigned8_vector(pos, index, ret);

		case LISPTYPE_BITVECTOR:
			return get_unsigned8_bitvector(pos, index, ret);

		case LISPTYPE_ARRAY:
			return get_unsigned8_array(pos, index, ret);

		default:
			*ret = 0;
			return TypeError_(pos, SEQUENCE);
	}
}

