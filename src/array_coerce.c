#include "array.h"
#include "array_copy.h"
#include "array_object.h"
#include "bigdata.h"
#include "bignum.h"
#include "ratio.h"
#include "real_float.h"
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
	str1->size = str1->front = str1->refer = str2->front; /* fill-pointer */
	SetArrayInfo(pos, ARRAY_INFO_DISPLACED, Nil);
	/* type */
	str1->type = type;
	str1->bytesize = size;
	upgraded_array_object(str1->type, str1->bytesize, &value);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, value);
	array_element_size(pos);
}

static void array_coerce_type_heap(addr *ret, addr array,
		enum ARRAY_TYPE type, unsigned size)
{
	addr pos;
	struct array_struct *str;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_coerce_type_struct(pos, array, type, size);
	/* dimension */
	array_size_copy(NULL, pos, array);
	/* allocate */
	str = ArrayInfoStruct(pos);
	array_allocate(NULL, pos, str);
	/* result */
	*ret = pos;
}

void array_coerce_t_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_T, 0);
}

void array_coerce_bit_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_BIT, 0);
}

void array_coerce_character_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_CHARACTER, 0);
}

void array_coerce_signed8_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_SIGNED, 8);
}

void array_coerce_signed16_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_SIGNED, 16);
}

void array_coerce_signed32_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_SIGNED, 32);
}

#ifdef LISP_64BIT
void array_coerce_signed64_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_SIGNED, 64);
}
#endif

void array_coerce_unsigned8_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_UNSIGNED, 8);
}

void array_coerce_unsigned16_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_UNSIGNED, 16);
}

void array_coerce_unsigned32_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_UNSIGNED, 32);
}

#ifdef LISP_64BIT
void array_coerce_unsigned64_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_UNSIGNED, 64);
}
#endif

void array_coerce_single_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_SINGLE_FLOAT, 0);
}

void array_coerce_double_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_DOUBLE_FLOAT, 0);
}

void array_coerce_long_heap(addr *ret, addr array)
{
	array_coerce_type_heap(ret, array, ARRAY_TYPE_LONG_FLOAT, 0);
}

static void vector_coerce_type_heap(addr *ret,
		enum ARRAY_TYPE type1, unsigned type2, size_t size)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, size, 0);
	str = ArrayInfoStruct(pos);
	str->type = type1;
	str->bytesize = type2;
	array_build_heap(pos);
	*ret = pos;
}

void vector_coerce_signed8_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_SIGNED, 8, size);
}

void vector_coerce_signed16_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_SIGNED, 16, size);
}

void vector_coerce_signed32_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_SIGNED, 32, size);
}

#ifdef LISP_64BIT
void vector_coerce_signed64_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_SIGNED, 64, size);
}
#endif

void vector_coerce_unsigned8_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_UNSIGNED, 8, size);
}

void vector_coerce_unsigned16_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_UNSIGNED, 16, size);
}

void vector_coerce_unsigned32_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_UNSIGNED, 32, size);
}

#ifdef LISP_64BIT
void vector_coerce_unsigned64_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_UNSIGNED, 64, size);
}
#endif

void vector_coerce_single_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_SINGLE_FLOAT, 0, size);
}

void vector_coerce_double_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_DOUBLE_FLOAT, 0, size);
}

void vector_coerce_long_heap(addr *ret, size_t size)
{
	vector_coerce_type_heap(ret, ARRAY_TYPE_LONG_FLOAT, 0, size);
}


/*
 *  array_coerce_bit
 */
int array_coerce_bit_t(addr pos, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (v == 0) {
				*ret = 0;
				return 0;
			}
			if (v == 1) {
				*ret = 1;
				return 0;
			}
			return 1;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(pos)) {
				*ret = 0;
				return 0;
			}
			if (equal_value_bignum(pos, SignPlus, 1)) {
				*ret = 1;
				return 0;
			}
			return 1;

		case LISPTYPE_RATIO:
			if (zerop_ratio(pos)) {
				*ret = 0;
				return 0;
			}
			if (equal_value_ratio(pos, SignPlus, 1, 1)) {
				*ret = 1;
				return 0;
			}
			return 1;

		default:
			return 1;
	}
}

static int array_coerce_bit_signed(struct array_value *ptr, int *ret)
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
				*ret = (int)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (s16 == 0 || s16 == 1) {
				*ret = (int)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (s32 == 0 || s32 == 1) {
				*ret = (int)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (s64 == 0 || s64 == 1) {
				*ret = (int)s64;
				return 0;
			}
			return 1;
#endif
		default:
			return 1;
	}
}

static int array_coerce_bit_unsigned(struct array_value *ptr, int *ret)
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
				*ret = (int)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.unsigned16;
			if (s16 == 0 || s16 == 1) {
				*ret = (int)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.unsigned32;
			if (s32 == 0 || s32 == 1) {
				*ret = (int)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.unsigned64;
			if (s64 == 0 || s64 == 1) {
				*ret = (int)s64;
				return 0;
			}
			return 1;
#endif
		default:
			return 1;
	}
}

int array_coerce_bit(addr pos, size_t i, int *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_bit_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_bit_signed(&value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_bit_unsigned(&value, ret);

		default:
			return 1;
	}
}


/*
 *  array_coerce_character
 */
int array_coerce_character_t(addr pos, unicode *ret)
{
	size_t size;

	if (symbolp(pos)) {
		GetNameSymbol(pos, &pos);
	}
	if (stringp(pos)) {
		string_length(pos, &size);
		if (size != 1)
			return 1;
		string_getc(pos, 0, ret);
		return 0;
	}
	if (characterp(pos)) {
		GetCharacter(pos, ret);
		return 0;
	}

	return 1;
}

int array_coerce_character(addr pos, size_t i, unicode *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_character_t(value.value.object, ret);

		case ARRAY_TYPE_CHARACTER:
			*ret = value.value.character;
			return 0;

		default:
			return 1;
	}
}


/*
 *  array_coerce_signed8
 */
int array_coerce_signed8_t(addr pos, int8_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (INT8_MIN <= v && v <= INT8_MAX) {
		*ret = (int8_t)v;
		return 0;
	}
	else {
		return 1;
	}
}

static int array_coerce_signed8_signed(addr pos,
		size_t i, struct array_value *ptr, int8_t *ret)
{
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = ptr->value.signed8;
			return 0;

		case 16:
			s16 = ptr->value.signed16;
			if (INT8_MIN <= s16 && s16 <= INT8_MAX) {
				*ret = (int8_t)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (INT8_MIN <= s32 && s32 <= INT8_MAX) {
				*ret = (int8_t)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT8_MIN <= s64 && s64 <= INT8_MAX) {
				*ret = (int8_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_signed8_unsigned(addr pos,
		size_t i, struct array_value *ptr, int8_t *ret)
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
				*ret = (int8_t)u8;
				return 0;
			}
			return 1;

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT8_MAX) {
				*ret = (int8_t)u16;
				return 0;
			}
			return 1;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT8_MAX) {
				*ret = (int8_t)u32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT8_MAX) {
				*ret = (int8_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_signed8(addr pos, size_t i, int8_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed8_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int8_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed8_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed8_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_signed16
 */
int array_coerce_signed16_t(addr pos, int16_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (INT16_MIN <= v && v <= INT16_MAX) {
		*ret = (int16_t)v;
		return 0;
	}
	else {
		return 1;
	}
}

static int array_coerce_signed16_signed(addr pos,
		size_t i, struct array_value *ptr, int16_t *ret)
{
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (int16_t)ptr->value.signed8;
			return 0;

		case 16:
			*ret = ptr->value.signed16;
			return 0;

		case 32:
			s32 = ptr->value.signed32;
			if (INT16_MIN <= s32 && s32 <= INT16_MAX) {
				*ret = (int16_t)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT16_MIN <= s64 && s64 <= INT16_MAX) {
				*ret = (int16_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_signed16_unsigned(addr pos,
		size_t i, struct array_value *ptr, int16_t *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (int16_t)ptr->value.unsigned8;
			return 0;

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= INT16_MAX) {
				*ret = (int16_t)u16;
				return 0;
			}
			return 1;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT16_MAX) {
				*ret = (int16_t)u32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT16_MAX) {
				*ret = (int16_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_signed16(addr pos, size_t i, int16_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed16_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int16_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed16_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed16_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_signed32
 */
int array_coerce_signed32_t(addr pos, int32_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (INT32_MIN <= v && v <= INT32_MAX) {
		*ret = (int32_t)v;
		return 0;
	}
	else {
		return 1;
	}
}

static int array_coerce_signed32_signed(addr pos,
		size_t i, struct array_value *ptr, int32_t *ret)
{
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (int32_t)ptr->value.signed8;
			return 0;

		case 16:
			*ret = (int32_t)ptr->value.signed16;
			return 0;

		case 32:
			*ret = ptr->value.signed32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (INT32_MIN <= s64 && s64 <= INT32_MAX) {
				*ret = (int32_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_signed32_unsigned(addr pos,
		size_t i, struct array_value *ptr, int32_t *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (int32_t)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (int32_t)ptr->value.unsigned16;
			return 0;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= INT32_MAX) {
				*ret = (int32_t)u32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT32_MAX) {
				*ret = (int32_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_signed32(addr pos, size_t i, int32_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed32_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int32_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed32_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed32_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


#ifdef LISP_64BIT
/*
 *  array_coerce_signed64
 */
int array_coerce_signed64_t(addr pos, int64_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}
	*ret = (int64_t)v;

	return 0;
}

static int array_coerce_signed64_signed(addr pos,
		size_t i, struct array_value *ptr, int64_t *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (int64_t)ptr->value.signed8;
			return 0;

		case 16:
			*ret = (int64_t)ptr->value.signed16;
			return 0;

		case 32:
			*ret = (int64_t)ptr->value.signed32;
			return 0;

		case 64:
			*ret = ptr->value.signed64;
			return 0;

		default:
			return 1;
	}
}

static int array_coerce_signed64_unsigned(addr pos,
		size_t i, struct array_value *ptr, int64_t *ret)
{
	uint64_t u64;

	switch (ptr->size) {
		case 8:
			*ret = (int64_t)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (int64_t)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = (int64_t)ptr->value.unsigned32;
			return 0;

		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= INT64_MAX) {
				*ret = (int64_t)u64;
				return 0;
			}
			return 1;

		default:
			return 1;
	}
}

int array_coerce_signed64(addr pos, size_t i, int64_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_signed64_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int64_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_signed64_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_signed64_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}
#endif


/*
 *  array_coerce_unsigned8
 */
int array_coerce_unsigned8_t(addr pos, uint8_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (0 <= v && v <= UINT8_MAX) {
		*ret = (uint8_t)v;
		return 0;
	}
	else {
		return 1;
	}
}

static int array_coerce_unsigned8_signed(addr pos,
		size_t i, struct array_value *ptr, uint8_t *ret)
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
				*ret = (uint8_t)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16 && s16 <= UINT8_MAX) {
				*ret = (uint8_t)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT8_MAX) {
				*ret = (uint8_t)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT8_MAX) {
				*ret = (uint8_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_unsigned8_unsigned(addr pos,
		size_t i, struct array_value *ptr, uint8_t *ret)
{
	uint16_t u16;
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = ptr->value.unsigned8;
			return 0;

		case 16:
			u16 = ptr->value.unsigned16;
			if (u16 <= UINT8_MAX) {
				*ret = (uint8_t)u16;
				return 0;
			}
			return 1;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT8_MAX) {
				*ret = (uint8_t)u32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT8_MAX) {
				*ret = (uint8_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_unsigned8(addr pos, size_t i, uint8_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned8_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (uint8_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned8_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned8_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_unsigned16
 */
int array_coerce_unsigned16_t(addr pos, uint16_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (0 <= v && v <= UINT16_MAX) {
		*ret = (uint16_t)v;
		return 0;
	}
	else {
		return 1;
	}
}

static int array_coerce_unsigned16_signed(addr pos,
		size_t i, struct array_value *ptr, uint16_t *ret)
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
				*ret = (uint16_t)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*ret = (uint16_t)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32 && s32 <= UINT16_MAX) {
				*ret = (uint16_t)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT16_MAX) {
				*ret = (uint16_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_unsigned16_unsigned(addr pos,
		size_t i, struct array_value *ptr, uint16_t *ret)
{
	uint32_t u32;
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (uint16_t)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = ptr->value.unsigned16;
			return 0;

		case 32:
			u32 = ptr->value.unsigned32;
			if (u32 <= UINT16_MAX) {
				*ret = (uint16_t)u32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT16_MAX) {
				*ret = (uint16_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_unsigned16(addr pos, size_t i, uint16_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned16_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (uint16_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned16_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned16_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_unsigned32
 */
#ifdef LISP_64BIT
int array_coerce_unsigned32_t(addr pos, uint32_t *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			break;

		case LISPTYPE_BIGNUM:
			if (getfixnum_bignum(pos, &v)) return 1;
			break;

		case LISPTYPE_RATIO:
			if (getfixnum_ratio(pos, &v)) return 1;
			break;

		default:
			return 1;
	}

	if (0 <= v && v <= UINT32_MAX) {
		*ret = (uint32_t)v;
		return 0;
	}
	else {
		return 1;
	}
}
#else
int array_coerce_unsigned32_t(addr pos, uint32_t *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*ret = (uint32_t)v;
				return 0;
			}
			return 1;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u)) return 1;
			if (IsMinus(sign) && (u != 0)) return 1;
			*ret = (uint32_t)u;
			return 0;

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u)) return 1;
			if (IsMinus(sign) && (u != 0)) return 1;
			*ret = (uint32_t)u;
			return 0;

		default:
			return 1;
	}
}
#endif

static int array_coerce_unsigned32_signed(addr pos,
		size_t i, struct array_value *ptr, uint32_t *ret)
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
				*ret = (uint32_t)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*ret = (uint32_t)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*ret = (uint32_t)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64 && s64 <= UINT32_MAX) {
				*ret = (uint32_t)s64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

static int array_coerce_unsigned32_unsigned(addr pos,
		size_t i, struct array_value *ptr, uint32_t *ret)
{
#ifdef LISP_64BIT
	uint64_t u64;
#endif

	switch (ptr->size) {
		case 8:
			*ret = (uint32_t)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (uint32_t)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = ptr->value.unsigned32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			u64 = ptr->value.unsigned64;
			if (u64 <= UINT32_MAX) {
				*ret = (uint32_t)u64;
				return 0;
			}
			return 1;
#endif

		default:
			return 1;
	}
}

int array_coerce_unsigned32(addr pos, size_t i, uint32_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned32_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (uint32_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned32_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned32_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_unsigned64
 */
#ifdef LISP_64BIT
int array_coerce_unsigned64_t(addr pos, uint64_t *ret)
{
	int sign;
	fixnum v;
	fixed u;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (0 <= v) {
				*ret = (uint64_t)v;
				return 0;
			}
			return 1;

		case LISPTYPE_BIGNUM:
			if (getfixed1_bignum(pos, &sign, &u)) return 1;
			if (IsMinus(sign) && (u != 0)) return 1;
			*ret = (uint64_t)u;
			return 0;

		case LISPTYPE_RATIO:
			if (getfixed1_ratio(pos, &sign, &u)) return 1;
			if (IsMinus(sign) && (u != 0)) return 1;
			*ret = (uint64_t)u;
			return 0;

		default:
			return 1;
	}
}

static int array_coerce_unsigned64_signed(addr pos,
		size_t i, struct array_value *ptr, uint64_t *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
	int64_t s64;

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (0 <= s8) {
				*ret = (uint64_t)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (0 <= s16) {
				*ret = (uint64_t)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (0 <= s32) {
				*ret = (uint64_t)s32;
				return 0;
			}
			return 1;

		case 64:
			s64 = ptr->value.signed64;
			if (0 <= s64) {
				*ret = (uint64_t)s64;
				return 0;
			}
			return 1;

		default:
			return 1;
	}
}

static int array_coerce_unsigned64_unsigned(addr pos,
		size_t i, struct array_value *ptr, uint64_t *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (uint64_t)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (uint64_t)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = (uint64_t)ptr->value.unsigned32;
			return 0;

		case 64:
			*ret = ptr->value.unsigned64;
			return 0;

		default:
			return 1;
	}
}

int array_coerce_unsigned64(addr pos, size_t i, uint64_t *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_unsigned64_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (uint64_t)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_unsigned64_signed(pos, i, &value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_unsigned64_unsigned(pos, i, &value, ret);

		default:
			return 1;
	}

	return 0;
}
#endif


/*
 *  array_coerce_single
 */
int array_coerce_single_t(addr value, single_float *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*ret = single_float_fixnum(value);
			return 0;

		case LISPTYPE_BIGNUM:
			*ret = single_float_bignum(value);
			return 0;

		case LISPTYPE_RATIO:
			*ret = single_float_ratio(value);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			GetSingleFloat(value, ret);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_ds_value(value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ls_value(value);
			return 0;

		default:
			return 1;
	}
}

static int array_coerce_single_signed(const struct array_value *ptr,
		single_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (single_float)ptr->value.signed8;
			return 0;

		case 16:
			*ret = (single_float)ptr->value.signed16;
			return 0;

		case 32:
			*ret = (single_float)ptr->value.signed32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (single_float)ptr->value.signed64;
			return 0;
#endif

		default:
			return 1;
	}
}

static int array_coerce_single_unsigned(const struct array_value *ptr,
		single_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (single_float)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (single_float)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = (single_float)ptr->value.unsigned32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (single_float)ptr->value.unsigned64;
			return 0;
#endif

		default:
			return 1;
	}
}

int array_coerce_single(addr pos, size_t i, single_float *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_single_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (single_float)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_single_signed(&value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_single_unsigned(&value, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = value.value.single_value;
			return 0;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = cast_ds_float(value.value.double_value);
			return 0;

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = cast_ls_float(value.value.long_value);
			return 0;

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_double
 */
int array_coerce_double_t(addr value, double_float *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*ret = double_float_fixnum(value);
			return 0;

		case LISPTYPE_BIGNUM:
			*ret = double_float_bignum(value);
			return 0;

		case LISPTYPE_RATIO:
			*ret = double_float_ratio(value);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sd_value(value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			GetDoubleFloat(value, ret);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ld_value(value);
			return 0;

		default:
			return 1;
	}
}

static int array_coerce_double_signed(const struct array_value *ptr,
		double_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (double_float)ptr->value.signed8;
			return 0;

		case 16:
			*ret = (double_float)ptr->value.signed16;
			return 0;

		case 32:
			*ret = (double_float)ptr->value.signed32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (double_float)ptr->value.signed64;
			return 0;
#endif

		default:
			return 1;
	}
}

static int array_coerce_double_unsigned(const struct array_value *ptr,
		double_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (double_float)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (double_float)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = (double_float)ptr->value.unsigned32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (double_float)ptr->value.unsigned64;
			return 0;
#endif

		default:
			return 1;
	}
}

int array_coerce_double(addr pos, size_t i, double_float *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_double_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (double_float)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_double_signed(&value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_double_unsigned(&value, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = cast_sd_float(value.value.single_value);
			return 0;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = value.value.double_value;
			return 0;

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = cast_ld_float(value.value.long_value);
			return 0;

		default:
			return 1;
	}

	return 0;
}


/*
 *  array_coerce_long
 */
int array_coerce_long_t(addr value, long_float *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_FIXNUM:
			*ret = long_float_fixnum(value);
			return 0;

		case LISPTYPE_BIGNUM:
			*ret = long_float_bignum(value);
			return 0;

		case LISPTYPE_RATIO:
			*ret = long_float_ratio(value);
			return 0;

		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sl_value(value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_dl_value(value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			GetLongFloat(value, ret);
			return 0;

		default:
			return 1;
	}
}

static int array_coerce_long_signed(const struct array_value *ptr,
		long_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (long_float)ptr->value.signed8;
			return 0;

		case 16:
			*ret = (long_float)ptr->value.signed16;
			return 0;

		case 32:
			*ret = (long_float)ptr->value.signed32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (long_float)ptr->value.signed64;
			return 0;
#endif

		default:
			return 1;
	}
}

static int array_coerce_long_unsigned(const struct array_value *ptr,
		long_float *ret)
{
	switch (ptr->size) {
		case 8:
			*ret = (long_float)ptr->value.unsigned8;
			return 0;

		case 16:
			*ret = (long_float)ptr->value.unsigned16;
			return 0;

		case 32:
			*ret = (long_float)ptr->value.unsigned32;
			return 0;

#ifdef LISP_64BIT
		case 64:
			*ret = (long_float)ptr->value.unsigned64;
			return 0;
#endif

		default:
			return 1;
	}
}

int array_coerce_long(addr pos, size_t i, long_float *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return array_coerce_long_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (long_float)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return array_coerce_long_signed(&value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return array_coerce_long_unsigned(&value, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			*ret = cast_sl_float(value.value.single_value);
			return 0;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			*ret = cast_dl_float(value.value.double_value);
			return 0;

		case ARRAY_TYPE_LONG_FLOAT:
			*ret = value.value.long_value;
			return 0;

		default:
			return 1;
	}

	return 0;
}


/*
 *  vector_coerce_bit
 */
int vector_coerce_bit(addr pos, size_t i, int *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_bit_t(pos, ret);
}

int vector_coerce_character(addr pos, size_t i, unicode *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_character_t(pos, ret);
}

int vector_coerce_signed8(addr pos, size_t i, int8_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed8_t(pos, ret);
}

int vector_coerce_signed16(addr pos, size_t i, int16_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed16_t(pos, ret);
}

int vector_coerce_signed32(addr pos, size_t i, int32_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed32_t(pos, ret);
}

#ifdef LISP_64BIT
int vector_coerce_signed64(addr pos, size_t i, int64_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_signed64_t(pos, ret);
}
#endif

int vector_coerce_unsigned8(addr pos, size_t i, uint8_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned8_t(pos, ret);
}

int vector_coerce_unsigned16(addr pos, size_t i, uint16_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned16_t(pos, ret);
}

int vector_coerce_unsigned32(addr pos, size_t i, uint32_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned32_t(pos, ret);
}

#ifdef LISP_64BIT
int vector_coerce_unsigned64(addr pos, size_t i, uint64_t *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_unsigned64_t(pos, ret);
}
#endif

int vector_coerce_single(addr pos, size_t i, single_float *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_single_t(pos, ret);
}

int vector_coerce_double(addr pos, size_t i, double_float *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_double_t(pos, ret);
}

int vector_coerce_long(addr pos, size_t i, long_float *ret)
{
	getarray(pos, i, &pos);
	return array_coerce_long_t(pos, ret);
}

