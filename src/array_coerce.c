#include "array.h"
#include "array_access.h"
#include "array_coerce.h"
#include "array_copy.h"
#include "array_inplace.h"
#include "array_make.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum.h"
#include "character.h"
#include "float_object.h"
#include "ratio.h"
#include "ratio_equal.h"
#include "strtype.h"
#include "symbol.h"
#include "type_upgraded.h"

/*
 *  make
 */
static void array_coerce_type_struct(addr pos, addr array,
		enum ARRAY_TYPE type, unsigned size)
{
	addr value;
	struct array_struct *str1, *str2;

	/* struct */
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	str1->simple = 1;
	str1->adjustable = 0;
	str1->fillpointer = 0;
	str1->displaced = 0;
	str1->dimension = str2->dimension;
	str1->offset = 0;
	str1->size = str1->front = str2->front; /* fill-pointer */
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, Nil);
	/* type */
	str1->type = type;
	str1->bytesize = size;
	upgraded_array_object(str1->type, str1->bytesize, &value);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	array_set_element_size(pos);
}

static int array_coerce_type_heap_(addr *ret, addr array,
		enum ARRAY_TYPE type, unsigned size)
{
	addr pos;
	struct array_struct *str;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_coerce_type_struct(pos, array, type, size);
	/* dimension */
	Return(array_size_copy_(NULL, pos, array));
	/* allocate */
	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	/* result */
	return Result(ret, pos);
}

_g int array_coerce_t_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_T, 0);
}

_g int array_coerce_bit_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_BIT, 0);
}

_g int array_coerce_character_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_CHARACTER, 0);
}

_g int array_coerce_signed8_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 8);
}

_g int array_coerce_signed16_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 16);
}

_g int array_coerce_signed32_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 32);
}

#ifdef LISP_64BIT
_g int array_coerce_signed64_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SIGNED, 64);
}
#endif

_g int array_coerce_unsigned8_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 8);
}

_g int array_coerce_unsigned16_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 16);
}

_g int array_coerce_unsigned32_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 32);
}

#ifdef LISP_64BIT
_g int array_coerce_unsigned64_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_UNSIGNED, 64);
}
#endif

_g int array_coerce_single_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_SINGLE_FLOAT, 0);
}

_g int array_coerce_double_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_DOUBLE_FLOAT, 0);
}

_g int array_coerce_long_heap_(addr *ret, addr array)
{
	return array_coerce_type_heap_(ret, array, ARRAY_TYPE_LONG_FLOAT, 0);
}

static int vector_coerce_type_heap_(addr *ret,
		enum ARRAY_TYPE type1, unsigned type2, size_t size)
{
	addr pos;
	struct array_struct *str;

	Return(array_va_heap_(&pos, size, 0));
	str = ArrayInfoStruct(pos);
	str->type = type1;
	str->bytesize = type2;
	Return(array_build_(pos));

	return Result(ret, pos);
}

_g int vector_coerce_signed8_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 8, size);
}

_g int vector_coerce_signed16_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 16, size);
}

_g int vector_coerce_signed32_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 32, size);
}

#ifdef LISP_64BIT
_g int vector_coerce_signed64_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SIGNED, 64, size);
}
#endif

_g int vector_coerce_unsigned8_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 8, size);
}

_g int vector_coerce_unsigned16_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 16, size);
}

_g int vector_coerce_unsigned32_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 32, size);
}

#ifdef LISP_64BIT
_g int vector_coerce_unsigned64_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_UNSIGNED, 64, size);
}
#endif

_g int vector_coerce_single_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_SINGLE_FLOAT, 0, size);
}

_g int vector_coerce_double_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_DOUBLE_FLOAT, 0, size);
}

_g int vector_coerce_long_heap_(addr *ret, size_t size)
{
	return vector_coerce_type_heap_(ret, ARRAY_TYPE_LONG_FLOAT, 0, size);
}


/*
 *  array_coerce_bit
 */
_g int array_coerce_bit_t_(addr pos, int *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (v == 0) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (v == 1) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(pos)) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (equal_value_bignum(pos, SignPlus, 1)) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_RATIO:
			if (zerop_ratio(pos)) {
				*rv = 0;
				return Result(ret, 0);
			}
			if (equal_value_ratio(pos, SignPlus, 1, 1)) {
				*rv = 1;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	return Result(ret, 1);
}

static int array_coerce_bit_signed_(struct array_value *ptr, int *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (s8 == 0 || s8 == 1) {
				*rv = (int)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (s16 == 0 || s16 == 1) {
				*rv = (int)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (s32 == 0 || s32 == 1) {
				*rv = (int)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (s64 == 0 || s64 == 1) {
				*rv = (int)s64;
				return Result(ret, 0);
			}
			break;
#endif
		default:
			break;
	}

	return Result(ret, 1);
}

static int array_coerce_bit_unsigned_(struct array_value *ptr, int *rv, int *ret)
{
	uint8_t s8;
	uint16_t s16;
	uint32_t s32;
#ifdef LISP_64BIT
	uint64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.unsigned8;
			if (s8 == 0 || s8 == 1) {
				*rv = (int)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.unsigned16;
			if (s16 == 0 || s16 == 1) {
				*rv = (int)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.unsigned32;
			if (s32 == 0 || s32 == 1) {
				*rv = (int)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.unsigned64;
			if (s64 == 0 || s64 == 1) {
				*rv = (int)s64;
				return Result(ret, 0);
			}
			break;
#endif
		default:
			break;
	}

	return Result(ret, 1);
}

_g int array_coerce_bit_(addr pos, size_t i, int *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_bit_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_bit_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_bit_unsigned_(&value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_character
 */
_g int array_coerce_character_t_(addr pos, unicode *rv, int *ret)
{
	size_t size;

	if (symbolp(pos)) {
		GetNameSymbol(pos, &pos);
	}
	if (stringp(pos)) {
		string_length(pos, &size);
		if (size != 1)
			goto novalue;
		Return(string_getc_(pos, 0, rv));
		return Result(ret, 0);
	}
	if (characterp(pos)) {
		GetCharacter(pos, rv);
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_character_(addr pos, size_t i, unicode *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_character_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_CHARACTER:
			*rv = value.value.character;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed8
 */
_g int array_coerce_signed8_t_(addr pos, int8_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT8_MIN <= v && v <= INT8_MAX) {
		*rv = (int8_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed8_signed_(addr pos,
		size_t i, struct array_value *ptr, int8_t *rv, int *ret)
{
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			s16 = ptr->value.signed16;
			if (INT8_MIN <= s16 && s16 <= INT8_MAX) {
				*rv = (int8_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (INT8_MIN <= s32 && s32 <= INT8_MAX) {
				*rv = (int8_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT8_MIN <= s64 && s64 <= INT8_MAX) {
				*rv = (int8_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed8_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int8_t *rv, int *ret)
{
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			u8 = ptr->value.unsigned8;
			if (u8 <= INT8_MAX) {
				*rv = (int8_t)u8;
				return Result(ret, 0);
			}
			break;

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT8_MAX) {
				*rv = (int8_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT8_MAX) {
				*rv = (int8_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT8_MAX) {
				*rv = (int8_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed8_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int8_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed8_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed8_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed16
 */
_g int array_coerce_signed16_t_(addr pos, int16_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT16_MIN <= v && v <= INT16_MAX) {
		*rv = (int16_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed16_signed_(addr pos,
		size_t i, struct array_value *ptr, int16_t *rv, int *ret)
{
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int16_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			s32 = ptr->value.signed32;
			if (INT16_MIN <= s32 && s32 <= INT16_MAX) {
				*rv = (int16_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT16_MIN <= s64 && s64 <= INT16_MAX) {
				*rv = (int16_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed16_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int16_t *rv, int *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int16_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT16_MAX) {
				*rv = (int16_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT16_MAX) {
				*rv = (int16_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT16_MAX) {
				*rv = (int16_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed16_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int16_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed16_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed16_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_signed32
 */
_g int array_coerce_signed32_t_(addr pos, int32_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (INT32_MIN <= v && v <= INT32_MAX) {
		*rv = (int32_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed32_signed_(addr pos,
		size_t i, struct array_value *ptr, int32_t *rv, int *ret)
{
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int32_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (int32_t)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT32_MIN <= s64 && s64 <= INT32_MAX) {
				*rv = (int32_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed32_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int32_t *rv, int *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (int32_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (int32_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT32_MAX) {
				*rv = (int32_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT32_MAX) {
				*rv = (int32_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed32_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int32_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed32_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed32_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


#ifdef LISP_64BIT
/*
 *  array_coerce_signed64
 */
_g int array_coerce_signed64_t_(addr pos, int64_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}
	*rv = (int64_t)v;
	return Result(ret, 0);

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_signed64_signed_(addr pos,
		size_t i, struct array_value *ptr, int64_t *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (int64_t)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (int64_t)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (int64_t)ptr->value.signed32;
			return Result(ret, 0);

		case 64:
			*rv = ptr->value.signed64;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

static int array_coerce_signed64_unsigned_(addr pos,
		size_t i, struct array_value *ptr, int64_t *rv, int *ret)
{
	uint64_t u64;

	switch (ptr->size) {
		case 8:
			*rv = (int64_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (int64_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (int64_t)ptr->value.unsigned32;
			return Result(ret, 0);

		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT64_MAX) {
				*rv = (int64_t)u64;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed64_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (int64_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed64_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed64_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}
#endif


/*
 *  array_coerce_unsigned8
 */
_g int array_coerce_unsigned8_t_(addr pos, uint8_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT8_MAX) {
		*rv = (uint8_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned8_signed_(addr pos,
		size_t i, struct array_value *ptr, uint8_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint8_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16 && s16 <= UINT8_MAX) {
				*rv = (uint8_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT8_MAX) {
				*rv = (uint8_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT8_MAX) {
				*rv = (uint8_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned8_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint8_t *rv, int *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= UINT8_MAX) {
				*rv = (uint8_t)u16;
				return Result(ret, 0);
			}
			break;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT8_MAX) {
				*rv = (uint8_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT8_MAX) {
				*rv = (uint8_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned8_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint8_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned8_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned8_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned16
 */
_g int array_coerce_unsigned16_t_(addr pos, uint16_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT16_MAX) {
		*rv = (uint16_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned16_signed_(addr pos,
		size_t i, struct array_value *ptr, uint16_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint16_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint16_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT16_MAX) {
				*rv = (uint16_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT16_MAX) {
				*rv = (uint16_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned16_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint16_t *rv, int *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (uint16_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT16_MAX) {
				*rv = (uint16_t)u32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT16_MAX) {
				*rv = (uint16_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned16_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint16_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned16_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned16_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned32
 */
#ifdef LISP_64BIT
_g int array_coerce_unsigned32_t_(addr pos, uint32_t *rv, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (GetFixnum_bignum(pos, &v))
				goto novalue;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v))
				goto novalue;
			break;

		default:
			goto novalue;
	}

	if (0 <= v && v <= UINT32_MAX) {
		*rv = (uint32_t)v;
		return Result(ret, 0);
	}

novalue:
	*rv = 0;
	return Result(ret, 1);
}
#else
_g int array_coerce_unsigned32_t_(addr pos, uint32_t *rv, int *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*rv = (uint32_t)v;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint32_t)u;
			return Result(ret, 0);

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint32_t)u;
			return Result(ret, 0);

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}
#endif

static int array_coerce_unsigned32_signed_(addr pos,
		size_t i, struct array_value *ptr, uint32_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint32_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint32_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*rv = (uint32_t)s32;
				return Result(ret, 0);
			}
			break;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT32_MAX) {
				*rv = (uint32_t)s64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned32_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint32_t *rv, int *ret)
{
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*rv = (uint32_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (uint32_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT32_MAX) {
				*rv = (uint32_t)u64;
				return Result(ret, 0);
			}
			break;
#endif

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

_g int array_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned32_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint32_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned32_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned32_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_unsigned64
 */
#ifdef LISP_64BIT
_g int array_coerce_unsigned64_t_(addr pos, uint64_t *rv, int *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*rv = (uint64_t)v;
				return Result(ret, 0);
			}
			break;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint64_t)u;
			return Result(ret, 0);

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u))
				break;
			if (IsMinus(sign) && (u != 0))
				break;
			*rv = (uint64_t)u;
			return Result(ret, 0);

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned64_signed_(addr pos,
		size_t i, struct array_value *ptr, uint64_t *rv, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
	int64_t s64;

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*rv = (uint64_t)s8;
				return Result(ret, 0);
			}
			break;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*rv = (uint64_t)s16;
				return Result(ret, 0);
			}
			break;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*rv = (uint64_t)s32;
				return Result(ret, 0);
			}
			break;

		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64) {
				*rv = (uint64_t)s64;
				return Result(ret, 0);
			}
			break;

		default:
			break;
	}

	*rv = 0;
	return Result(ret, 1);
}

static int array_coerce_unsigned64_unsigned_(addr pos,
		size_t i, struct array_value *ptr, uint64_t *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (uint64_t)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (uint64_t)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (uint64_t)ptr->value.unsigned32;
			return Result(ret, 0);

		case 64:
			*rv = ptr->value.unsigned64;
			return Result(ret, 0);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

_g int array_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned64_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (uint64_t)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned64_signed_(pos, i, &value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned64_unsigned_(pos, i, &value, rv, ret);

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}
#endif


/*
 *  array_coerce_single
 */
_g int array_coerce_single_t_(addr value, single_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = single_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return single_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return single_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(value, rv);
			return Result(ret, 0);

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_ds_value_(value, rv);

		case LISPTYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ls_value_(value, rv);

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

static int array_coerce_single_signed_(const struct array_value *ptr,
		single_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (single_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (single_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (single_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (single_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

static int array_coerce_single_unsigned_(const struct array_value *ptr,
		single_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (single_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (single_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (single_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (single_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}

_g int array_coerce_single_(addr pos, size_t i, single_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_single_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (single_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_single_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_single_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*rv = value.value.single_value;
			return Result(ret, 0);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_ds_float_(value.value.double_value, rv);

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ls_float_(value.value.long_value, rv);

		default:
			*rv = 0.0f;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_double
 */
_g int array_coerce_double_t_(addr value, double_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = double_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return double_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return double_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sd_value_(value, rv);

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(value, rv);
			return Result(ret, 0);

		case LISPTYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ld_value_(value, rv);

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}

static int array_coerce_double_signed_(const struct array_value *ptr,
		double_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (double_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (double_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (double_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (double_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}

static int array_coerce_double_unsigned_(const struct array_value *ptr,
		double_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (double_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (double_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (double_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (double_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0;
			return Result(ret, 1);
	}
}

_g int array_coerce_double_(addr pos, size_t i, double_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_double_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (double_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_double_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_double_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sd_float_(value.value.single_value, rv);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*rv = value.value.double_value;
			return Result(ret, 0);

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = 0;
			return cast_ld_float_(value.value.long_value, rv);

		default:
			*rv = 0.0;
			return Result(ret, 1);
	}
}


/*
 *  array_coerce_long
 */
_g int array_coerce_long_t_(addr value, long_float *rv, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*rv = long_float_fixnum(value);
			return Result(ret, 0);

		case LISPTYPE_BIGNUM:
			*ret = 0;
			return long_float_bignum_(value, rv);

		case LISPTYPE_RATIO:
			*ret = 0;
			return long_float_ratio_(value, rv);

		case LISPTYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sl_value_(value, rv);

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_dl_value_(value, rv);

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(value, rv);
			return Result(ret, 0);

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

static int array_coerce_long_signed_(const struct array_value *ptr,
		long_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (long_float)ptr->value.signed8;
			return Result(ret, 0);

		case 16:
			*rv = (long_float)ptr->value.signed16;
			return Result(ret, 0);

		case 32:
			*rv = (long_float)ptr->value.signed32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (long_float)ptr->value.signed64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

static int array_coerce_long_unsigned_(const struct array_value *ptr,
		long_float *rv, int *ret)
{
	switch (ptr->size) {
		case 8:
			*rv = (long_float)ptr->value.unsigned8;
			return Result(ret, 0);

		case 16:
			*rv = (long_float)ptr->value.unsigned16;
			return Result(ret, 0);

		case 32:
			*rv = (long_float)ptr->value.unsigned32;
			return Result(ret, 0);

#ifdef LISP_64BIT
		case 64:
			*rv = (long_float)ptr->value.unsigned64;
			return Result(ret, 0);
#endif

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}

_g int array_coerce_long_(addr pos, size_t i, long_float *rv, int *ret)
{
	struct array_value value;

	Return(arrayinplace_get_(pos, i, &value));
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_long_t_(value.value.object, rv, ret);

		case ARRAY_TYPE_BIT:
			*rv = (long_float)value.value.bit;
			return Result(ret, 0);

		case ARRAY_TYPE_SIGNED:
			return array_coerce_long_signed_(&value, rv, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_long_unsigned_(&value, rv, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = 0;
			return cast_sl_float_(value.value.single_value, rv);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = 0;
			return cast_dl_float_(value.value.double_value, rv);

		case ARRAY_TYPE_LONG_FLOAT:
			*rv = value.value.long_value;
			return Result(ret, 0);

		default:
			*rv = 0.0L;
			return Result(ret, 1);
	}
}


/*
 *  vector_coerce_bit
 */
_g int vector_coerce_bit_(addr pos, size_t i, int *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_bit_t_(pos, rv, ret);
}

_g int vector_coerce_character_(addr pos, size_t i, unicode *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_character_t_(pos, rv, ret);
}

_g int vector_coerce_signed8_(addr pos, size_t i, int8_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed8_t_(pos, rv, ret);
}

_g int vector_coerce_signed16_(addr pos, size_t i, int16_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed16_t_(pos, rv, ret);
}

_g int vector_coerce_signed32_(addr pos, size_t i, int32_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed32_t_(pos, rv, ret);
}

#ifdef LISP_64BIT
_g int vector_coerce_signed64_(addr pos, size_t i, int64_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed64_t_(pos, rv, ret);
}
#endif

_g int vector_coerce_unsigned8_(addr pos, size_t i, uint8_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned8_t_(pos, rv, ret);
}

_g int vector_coerce_unsigned16_(addr pos, size_t i, uint16_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned16_t_(pos, rv, ret);
}

_g int vector_coerce_unsigned32_(addr pos, size_t i, uint32_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned32_t_(pos, rv, ret);
}

#ifdef LISP_64BIT
_g int vector_coerce_unsigned64_(addr pos, size_t i, uint64_t *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned64_t_(pos, rv, ret);
}
#endif

_g int vector_coerce_single_(addr pos, size_t i, single_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_single_t_(pos, rv, ret);
}

_g int vector_coerce_double_(addr pos, size_t i, double_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_double_t_(pos, rv, ret);
}

_g int vector_coerce_long_(addr pos, size_t i, long_float *rv, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_long_t_(pos, rv, ret);
}

