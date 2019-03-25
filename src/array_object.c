#include "arch.h"
#include "array.h"
#include "array_copy.h"
#include "array_object.h"
#include "bigdata.h"
#include "bignum.h"
#include "bit.h"
#include "control.h"
#include "condition.h"
#include "cons.h"
#include "integer.h"
#include "sequence.h"
#include "type_object.h"
#include "type_upgraded.h"

/*
 *  control
 */
static void array_settype_value(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	addr value;
	struct array_struct *str;

	upgraded_array_object(type, size, &value);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, value);
	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
}

void array_settype(addr pos)
{
	struct array_struct *str;
	addr type;

	str = ArrayInfoStruct(pos);
	upgraded_array_object(str->type, str->bytesize, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
}

void array_element_size(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			str->element = sizeoft(unicode);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			str->element = str->bytesize / 8;
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			str->element = sizeoft(single_float);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			str->element = sizeoft(double_float);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			str->element = sizeoft(long_float);
			break;

		default:
			str->element = 0;
			break;
	}
}

int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size)
{
	if (a->type != type)
		return 0;
	if (a->type == ARRAY_TYPE_SIGNED || a->type == ARRAY_TYPE_UNSIGNED)
		return a->bytesize == size;
	else
		return 1;
}

void array_element_type(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &pos);
	type_object(ret, pos);
}

size_t array_vector_length(addr pos, int fill)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	return fill? str->front: str->size;
}

void array_rowlength(addr pos, size_t *ret)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	*ret = (str->fillpointer)? str->front: str->size;
}

int array_dimension_equal(addr a, addr b)
{
	struct array_struct *str1, *str2;
	size_t size, i;
	const size_t *data1, *data2;

	CheckType(a, LISPTYPE_ARRAY);
	CheckType(b, LISPTYPE_ARRAY);
	str1 = ArrayInfoStruct(a);
	str2 = ArrayInfoStruct(b);
	size = str1->dimension;
	if (size != str2->dimension)
		return 0;
	if (size == 0)
		return 1;
	if (size == 1)
		return str1->front == str2->front;
	data1 = array_ptrsize(a);
	data2 = array_ptrsize(b);
	for (i = 0; i < size; i++) {
		if (data1[i] != data2[i])
			return 0;
	}

	return 1;
}


/*  make-array
 *    element-type, initial-element, initial-contents,
 *    adjustable, fill-pointer, displaced-to, displaced-index-offset
 */
static void array_upgraded(addr pos, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	upgraded_array_value(type, &value, &size);
	array_settype_value(pos, value, size);
}

static void array_getsize(addr pos, size_t *ret)
{
	struct array_struct *str;
	size_t dimension, i, size;
	const size_t *data;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	size = 1;
	for (i = 0; i < dimension; i++) {
		if (multisafe_size(size, data[i], &size))
			fmte("size overflow.", NULL);
	}
	*ret = size;
}

static void array_setsize0(addr pos, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 0;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
	*ret = 1;
}

static void array_setsize1(addr pos, addr value, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (minusp_integer(value))
		fmte("Array index ~A must be a non-negative integer.", value, NULL);
	if (getindex_integer(value, ret))
		fmte("Array index ~A is too large.", value, NULL);
	str->dimension = 1;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
}

static void array_setsize2(LocalRoot local, addr pos, addr value, size_t *ret)
{
	struct array_struct *str;
	addr temp, index;
	size_t size, *data, i;

	str = ArrayInfoStruct(pos);
	size = length_list_safe(value);
	arraysize_alloc(local, &temp, size);
	data = arraysize_ptr(temp);
	for (i = 0; value != Nil; i++) {
		GetCons(value, &index, &value);
		if (! integerp(index))
			fmte("Array index ~A must be an integer.", index, NULL);
		if (minusp_integer(index))
			fmte("Array index ~A must be a non-negative integer.", index, NULL);
		if (getindex_integer(index, &size))
			fmte("Array index ~A is too large.", index, NULL);
		data[i] = size;
	}
	str->dimension = i;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
	array_getsize(pos, ret);
}

static void array_setsize(LocalRoot local, addr pos, addr value)
{
	struct array_struct *str;
	size_t size;

	if (value == Nil)
		array_setsize0(pos, &size);
	else if (integerp(value))
		array_setsize1(pos, value, &size);
	else if (consp(value))
		array_setsize2(local, pos, value, &size);
	else {
		fmte("Array index ~A must be an integer or list.", value, NULL);
		return;
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
}

void array_setsize_heap(addr pos, addr value)
{
	array_setsize(NULL, pos, value);
}

static void array_allocate_t(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	arraygen_alloc(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	bitmemory_unsafe(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

void array_allocate_size(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	size_t size;

	if (multisafe_size(str->size , str->element, &size))
		fmte("size overflow.", NULL);
	arrayspec_alloc(local, &array, size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

void array_allocate(LocalRoot local, addr pos, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			fmte("The array has no element size.", NULL);
			break;

		case ARRAY_TYPE_T:
			array_allocate_t(local, pos, str);
			break;

		case ARRAY_TYPE_BIT:
			array_allocate_bit(local, pos, str);
			break;

		default:
			array_allocate_size(local, pos, str);
			break;
	}
}

static void array_memory_displaced(addr pos, addr displaced, addr offset)
{
	struct array_struct *str1, *str2;
	size_t size;

	/* :displaced-to */
	if (! arrayp(displaced))
		fmte(":displaced-to parameter ~S must be a array type.", displaced, NULL);
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(displaced);
	if (str2->size < str1->size + str1->offset) {
		fmte("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str1, str2->type, str2->bytesize))
		fmte("Array type must be equal to displaced array.", NULL);
	SetArrayInfo(pos, ARRAY_INFO_DISPLACED, displaced);

	/* displaced-index-offset */
	if (! integerp(offset))
		fmte("Array offset ~A must be an integer.", offset, NULL);
	if (minusp_integer(offset))
		fmte("Array offset ~A must be a non-negative integer.", offset, NULL);
	if (getindex_integer(offset, &size))
		fmte("Array offset ~A is too large.", offset, NULL);
	str1->offset = size;
}

static void array_memory_fill(addr pos, addr fill)
{
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(pos);
	if (str->dimension != 1)
		fmte("fill-pointer array must be a 1 dimensional.", NULL);
	if (fill != T) {
		if (! integerp(fill))
			fmte("fill-pointer ~A must be an integer.", fill, NULL);
		if (minusp_integer(fill))
			fmte("fill-pointer ~A must be a non-negative integer.", fill, NULL);
		if (getindex_integer(fill, &size))
			fmte("fill-pointer ~A is too large.", fill, NULL);
		if (str->size < size)
			fmte("fill-pointer ~A must be less than array size.", fill, NULL);
		str->front = size;
	}
}

static void array_memory_extend(LocalRoot local, addr pos,
		addr fill, addr displaced, addr offset)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	/* displaced-to */
	if (str->displaced)
		array_memory_displaced(pos, displaced, offset);
	else
		array_allocate(local, pos, str);
	/* fill-pointer */
	if (str->fillpointer)
		array_memory_fill(pos, fill);
}

void array_memory_make(LocalRoot local, addr pos,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	int check1, check2, check3;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	check1 = (adjustable != Nil);
	check2 = (fillpointer != Nil);
	check3 = (displaced != Nil);
	if (check1 || check2 || check3) {
		str->adjustable = check1;
		str->fillpointer = check2;
		str->displaced = check3;
		str->simple = 0;
		array_memory_extend(local, pos, fillpointer, displaced, offset);
	}
	else {
		str->adjustable = 0;
		str->fillpointer = 0;
		str->displaced = 0;
		str->simple = 1;
		array_allocate(local, pos, str);
	}
}

static void array_initial_t(addr pos, addr value, size_t size)
{
	size_t i;

	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	for (i = 0; i < size; i++)
		arraygen_set(pos, i, value);
}

static void array_initial_bit(addr pos, addr value, size_t size)
{
	fixnum init;

	if (! fixnump(value))
		fmte(":initial-element ~A must be integer type.", value, NULL);
	GetFixnum(value, &init);
	if (init != 0 && init != 1)
		fmte(":initail-element ~A must be 0 or 1.", value, NULL);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	bitmemory_memset(pos, (int)init);
}

static void array_memset(addr pos, const void *src)
{
	struct array_struct *str;
	size_t size, elem, i;
	byte *dst;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	dst = (byte *)arrayspec_ptr(pos);
	for (i = 0; i < size; i++) {
		memcpy(dst, src, elem);
		dst += elem;
	}
}

static void array_initial_character(addr pos, addr value)
{
	unicode u;

	if (! characterp(value))
		fmte(":initial-element ~A must be character type.", value, NULL);
	GetCharacter(value, &u);
	array_memset(pos, (const void *)&u);
}

static void array_initial_signed8(addr pos, addr value)
{
	void *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	data = (void *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 8).", value, NULL);
}

static void array_initial_signed16(addr pos, addr value)
{
	int16_t c;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	c = (int16_t)init;
	array_memset(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void array_initial_signed32(addr pos, addr value)
{
	int32_t c;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	c = (int32_t)init;
	array_memset(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
}

static void array_initial_signed64(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow :initial-element ~A in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	array_memset(pos, (const void *)&init);
}
#else
static void array_initial_signed32(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	array_memset(pos, (const void *)&init);
}
#endif

static void array_initial_signed(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			array_initial_signed8(pos, value);
			break;

		case 16:
			array_initial_signed16(pos, value);
			break;

		case 32:
			array_initial_signed32(pos, value);
			break;

#ifdef LISP_64BIT
		case 64:
			array_initial_signed64(pos, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void array_initial_unsigned8(addr pos, addr value)
{
	byte *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	data = (byte *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 8).", value, NULL);
}

static void array_initial_unsigned16(addr pos, addr value)
{
	uint16_t c;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT16_MAX < init)
		goto error;
	c = (uint16_t)init;
	array_memset(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void array_initial_unsigned32(addr pos, addr value)
{
	uint32_t c;
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT32_MAX < init)
		goto error;
	c = (uint32_t)init;
	array_memset(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}

static void array_initial_unsigned64(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		array_memset(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		array_memset(pos, (const void *)&bigv);
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 64).", value, NULL);
}
#else
static void array_initial_unsigned32(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		array_memset(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		array_memset(pos, (const void *)&bigv);
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}
#endif

static void array_initial_unsigned(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			array_initial_unsigned8(pos, value);
			break;

		case 16:
			array_initial_unsigned16(pos, value);
			break;

		case 32:
			array_initial_unsigned32(pos, value);
			break;

#ifdef LISP_64BIT
		case 64:
			array_initial_unsigned64(pos, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void array_initial_single(addr pos, addr value)
{
	single_float v;

	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		fmte(":initial-element ~A must be single-float type.", value, NULL);
	GetSingleFloat(value, &v);
	array_memset(pos, (const void *)&v);
}

static void array_initial_double(addr pos, addr value)
{
	double_float v;

	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		fmte(":initial-element ~A must be double-float type.", value, NULL);
	GetDoubleFloat(value, &v);
	array_memset(pos, (const void *)&v);
}

static void array_initial_long(addr pos, addr value)
{
	long_float v;

	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		fmte(":initial-element ~A must be long-float type.", value, NULL);
	GetLongFloat(value, &v);
	array_memset(pos, (const void *)&v);
}

static void array_initial(addr pos, addr value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			array_initial_t(pos, value, str->size);
			break;

		case ARRAY_TYPE_BIT:
			array_initial_bit(pos, value, str->size);
			break;

		case ARRAY_TYPE_CHARACTER:
			array_initial_character(pos, value);
			break;

		case ARRAY_TYPE_SIGNED:
			array_initial_signed(pos, value);
			break;

		case ARRAY_TYPE_UNSIGNED:
			array_initial_unsigned(pos, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			array_initial_single(pos, value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			array_initial_double(pos, value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			array_initial_long(pos, value);
			break;

		default:
			fmte("Invalid array type.", NULL);
			break;
	}
}

static void arraymemory_set_character(addr pos, size_t index, addr value)
{
	if (! characterp(value))
		fmte("~S must be character type.", value, NULL);
	((unicode *)arrayspec_ptr(pos))[index] = RefCharacter(value);
}

static void arraymemory_set_signed8(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	((int8_t *)arrayspec_ptr(pos))[index] = (int8_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 8).", value, NULL);
}

static void arraymemory_set_signed16(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	((int16_t *)arrayspec_ptr(pos))[index] = (int16_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void arraymemory_set_signed32(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	((int32_t *)arrayspec_ptr(pos))[index] = (int32_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 32).", value, NULL);
}

static void arraymemory_set_signed64(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow ~S in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	((int64_t *)arrayspec_ptr(pos))[index] = (int64_t)init;
}
#else
static void arraymemory_set_signed32(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow ~S in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	((int32_t *)arrayspec_ptr(pos))[index] = (int32_t)init;
}
#endif

static void arraymemory_set_signed(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			arraymemory_set_signed8(pos, index, value);
			break;

		case 16:
			arraymemory_set_signed16(pos, index, value);
			break;

		case 32:
			arraymemory_set_signed32(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_set_signed64(pos, index, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void arraymemory_set_unsigned8(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT8_MAX < init)
		goto error;
	((uint8_t *)arrayspec_ptr(pos))[index] = (uint8_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 8).", value, NULL);
}

static void arraymemory_set_unsigned16(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT16_MAX < init)
		goto error;
	((uint16_t *)arrayspec_ptr(pos))[index] = (uint16_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void arraymemory_set_unsigned32(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT32_MAX < init)
		goto error;
	((uint32_t *)arrayspec_ptr(pos))[index] = (uint32_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 32).", value, NULL);
}

static void arraymemory_set_unsigned64(addr pos, size_t index, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		((uint64_t *)arrayspec_ptr(pos))[index] = (uint64_t)init;
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		((uint64_t *)arrayspec_ptr(pos))[index] = (uint64_t)bigv;
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 64).", value, NULL);
}
#else
static void arraymemory_set_unsigned32(addr pos, size_t index, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		((uint32_t *)arrayspec_ptr(pos))[index] = (uint32_t)init;
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		((uint32_t *)arrayspec_ptr(pos))[index] = (uint32_t)bigv;
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 32).", value, NULL);
}
#endif

static void arraymemory_set_unsigned(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			arraymemory_set_unsigned8(pos, index, value);
			break;

		case 16:
			arraymemory_set_unsigned16(pos, index, value);
			break;

		case 32:
			arraymemory_set_unsigned32(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_set_unsigned64(pos, index, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void arraymemory_set_single(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		fmte("~S must be single-float type.", value, NULL);
	((single_float *)arrayspec_ptr(pos))[index] = RefSingleFloat(value);
}

static void arraymemory_set_double(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		fmte("~S must be double-float type.", value, NULL);
	((double_float *)arrayspec_ptr(pos))[index] = RefDoubleFloat(value);
}

static void arraymemory_set_long(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		fmte("~S must be long-float type.", value, NULL);
	((long_float *)arrayspec_ptr(pos))[index] = RefLongFloat(value);
}

static void arraymemory_set(addr pos, addr mem, size_t index, addr value)
{
	enum ARRAY_TYPE type;
	unsigned bytesize;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	type = str->type;
	bytesize = str->bytesize;
	Check(str->size <= index, "size error");
	switch (type) {
		case ARRAY_TYPE_T:
			arraygen_set(mem, index, value);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_set(mem, index, value);
			break;

		case ARRAY_TYPE_CHARACTER:
			arraymemory_set_character(mem, index, value);
			break;

		case ARRAY_TYPE_SIGNED:
			arraymemory_set_signed(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_UNSIGNED:
			arraymemory_set_unsigned(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			arraymemory_set_single(mem, index, value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			arraymemory_set_double(mem, index, value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			arraymemory_set_long(mem, index, value);
			break;

		default:
			fmte("(SETF AREF) Invalid array type.", NULL);
			break;
	}
}

static void array_setindex(addr pos, size_t index, addr value)
{
	addr mem;
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	arraymemory_set(pos, mem, index, value);
}


/* (s1 s2 s3 s4) : (n1 n2 n3 n4)
 *  -> (s4*s3*s2*n1) + (s4*s3*n2) + (s4*n3) + n4
 *  -> s4*(...) + n4
 *  -> s4*(s3*(...) + n3) + n4
 *  -> s4*(s3*(s2*n1 + n2) + n3) + n4
 *  [1] n1
 *  [2] s2*... + n2
 *  [3] s3*... + n3
 *  [4] s4*... + n4
 */
static void array_contents_recursive(LocalRoot local, addr, addr,
		const size_t *data, size_t limit, size_t depth, size_t size);

static void array_contents_list(LocalRoot local, addr pos, addr list,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		for (i = 0; list != Nil; i++) {
			if (length <= i)
				fmte("Too many :initial-contents ~S list.", list, NULL);
			getcons(list, &next, &list);
			array_contents_recursive(local, pos, next, data, limit, depth+1, size+i);
		}
		if (i < length)
			fmte("Too few :initial-contents list.", NULL);
	}
	else {
		array_setindex(pos, size, list);
	}
}

static void array_contents_sequence(LocalRoot local, addr pos, addr object,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length, check;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		check = length_sequence(object, 1);
		if (length < check)
			fmte("Too many :initial-contents ~S.", object, NULL);
		if (check < length)
			fmte("Too few :initial-contents ~S.", object, NULL);
		for (i = 0; i < check; i++) {
			getelt_sequence(local, object, i, &next);
			array_contents_recursive(local, pos, next, data, limit, depth+1, size+i);
		}
	}
	else {
		array_setindex(pos, size, object);
	}
}

static void array_contents_object(LocalRoot local, addr pos, addr list,
		size_t limit, size_t depth, size_t size)
{
	if (depth < limit)
		fmte("Too few :initial-contents.", NULL);
	else
		array_setindex(pos, size, list);
}

static void array_contents_recursive(LocalRoot local, addr pos,
		addr object, const size_t *data, size_t limit, size_t depth, size_t size)
{
	switch (GetType(object)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			array_contents_list(local, pos, object, data, limit, depth, size);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			array_contents_sequence(local, pos, object, data, limit, depth, size);
			break;

		default:
			array_contents_object(local, pos, object, limit, depth, size);
			break;
	}
}

static void array_contents_setf(LocalRoot local, addr pos, addr contents)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	array_contents_recursive(local, pos, contents, data, dimension, 0, 0);
}

static void array_initial_contents(LocalRoot local, addr pos, addr contents)
{
	size_t dimension;

	dimension = ArrayInfoStruct(pos)->dimension;
	if (dimension == 0)
		array_setindex(pos, 0, contents);
	else
		array_contents_setf(local, pos, contents);
}

void array_initial_make(LocalRoot local, addr pos, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced && (initial != Unbound || contents != Unbound)) {
		fmte("Displaced array don't have "
				":initial-element or :initial-contents.", NULL);
	}
	if (initial != Unbound && contents != Unbound) {
		fmte("Array parameter cannot have both :initial-element and "
				":initial-contens parameter.", NULL);
	}
	if (initial != Unbound) {
		array_initial(pos, initial);
		return;
	}
	if (contents != Unbound) {
		array_initial_contents(local, pos, contents);
		return;
	}
}

void make_array_common(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_upgraded(pos, type);
	array_element_size(pos);
	/* dimension */
	array_setsize_heap(pos, dimension);
	/* allocate */
	array_memory_make(NULL, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	array_initial_make(NULL, pos, initial, contents);
	/* result */
	*ret = pos;
}


/*
 *  array_contents_heap
 */
static void array_size_contents(LocalRoot local,
		addr pos, addr rankarg, addr contents)
{
	struct array_struct *str;
	size_t i, rank, size, *data;
	addr temp;

	if (getindex_integer(rankarg, &rank))
		fmte("Array rank ~A is too large.", rankarg, NULL);
	str = ArrayInfoStruct(pos);
	if (rank == 0) {
		str->dimension = 0;
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
		size = 1;
	}
	else if (rank == 1) {
		str->dimension = 1;
		size = length_list_safe(contents);
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
	}
	else {
		str->dimension = rank;
		arraysize_alloc(local, &temp, rank);
		data = arraysize_ptr(temp);
		for (i = 0; i < rank; i++) {
			if (! consp(contents))
				fmte("Invalid initial-contents parameter ~S.", contents, NULL);
			data[i] = length_list_safe(contents);
			GetCar(contents, &contents);
		}
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
		array_getsize(pos, &size);
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
}

void array_contents_heap(addr *ret, addr rank, addr contents)
{
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	array_settype_value(pos, ARRAY_TYPE_T, 0);
	array_element_size(pos);
	/* dimension */
	array_size_contents(NULL, pos, rank, contents);
	/* allocate */
	array_memory_make(NULL, pos, Nil, Nil, Nil, Nil);
	/* initial value */
	array_initial_make(NULL, pos, Unbound, contents);
	/* result */
	*ret = pos;
}


/*
 *  array function
 */
static void array_setsimple(struct array_struct *str)
{
	str->simple = (!str->adjustable) && (!str->fillpointer) && (!str->displaced);
}

static void array_checkfill(addr pos, struct array_struct *str)
{
	if (str->fillpointer) {
		if (str->size < str->front)
			fmte("fill-pointer size must be smaller than element-size.", NULL);
		if (! array_vector_p(pos))
			fmte("fill-pointer array must be an one dimension.", NULL);
	}
	else {
		str->front = str->size;
	}
}

void array_character_alloc(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_settype_value(pos, ARRAY_TYPE_CHARACTER, 0);
	array_element_size(pos);
	array_setsimple(str);
	array_checkfill(pos, str);
	array_allocate(local, pos, str);
}

void array_build_alloc(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_settype(pos);
	array_element_size(pos);
	array_setsimple(str);
	array_checkfill(pos, str);
	array_allocate(local, pos, str);
}

void array_build_heap(addr pos)
{
	array_build_alloc(NULL, pos);
}


/*
 *  arraymemory
 */
static void arraymemory_index(addr pos, size_t index, addr *retp, size_t *rets)
{
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, retp);
	*rets = index;
}

static void arraymemory_get(addr pos, size_t index, addr *retp, size_t *rets)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (! str->displaced) {
		arraymemory_index(pos, index, retp, rets);
		return;
	}
	if (str->refer <= index) {
		arraymemory_index(pos, index - str->refer, retp, rets);
	}
	else {
		GetArrayInfo(pos, ARRAY_INFO_DISPLACED, &pos);
		index += str->offset;
		arraymemory_get(pos, index, retp, rets);
	}
}

static void arraymemory_getunicode(addr pos, size_t index, unicode *ret)
{
	*ret = ((unicode *)arrayspec_ptr(pos))[index];
}

static void arraymemory_getcharacter(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode u;
	arraymemory_getunicode(pos, index, &u);
	character_alloc(local, ret, u);
}

static void arraymemory_getsigned8(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int8_t *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getsigned16(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int16_t *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getsigned32(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int32_t *)arrayspec_ptr(pos))[index]);
}

#ifdef LISP_64BIT
static void arraymemory_getsigned64(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int64_t *)arrayspec_ptr(pos))[index]);
}
#endif

static void arraymemory_getsigned(LocalRoot local,
		unsigned bytesize, addr pos, size_t index, addr *ret)
{
	switch (bytesize) {
		case 8:
			arraymemory_getsigned8(local, pos, index, ret);
			break;

		case 16:
			arraymemory_getsigned16(local, pos, index, ret);
			break;

		case 32:
			arraymemory_getsigned32(local, pos, index, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_getsigned64(local, pos, index, ret);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void arraymemory_getunsigned8(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, (fixnum)((uint8_t *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getunsigned16(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, (fixnum)((uint16_t *)arrayspec_ptr(pos))[index]);
}

#ifdef LISP_64BIT
static void arraymemory_getunsigned32(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((uint32_t *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getunsigned64(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	bigtype value;

	value = ((bigtype *)arrayspec_ptr(pos))[index];
	integer_fixed_alloc(local, ret, signplus_bignum, value);
}
#else
static void arraymemory_getunsigned32(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	bigtype value;

	value = ((bigtype *)arrayspec_ptr(pos))[index];
	integer_fixed_alloc(local, ret, signplus_bignum, value);
}
#endif

static void arraymemory_getunsigned(LocalRoot local,
		unsigned bytesize, addr pos, size_t index, addr *ret)
{
	switch (bytesize) {
		case 8:
			arraymemory_getunsigned8(local, pos, index, ret);
			break;

		case 16:
			arraymemory_getunsigned16(local, pos, index, ret);
			break;

		case 32:
			arraymemory_getunsigned32(local, pos, index, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_getunsigned64(local, pos, index, ret);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void arraymemory_getsingle(LocalRoot local, addr pos, size_t index, addr *ret)
{
	single_float_alloc(local, ret, ((single_float *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getdouble(LocalRoot local, addr pos, size_t index, addr *ret)
{
	double_float_alloc(local, ret, ((double_float *)arrayspec_ptr(pos))[index]);
}

static void arraymemory_getlong(LocalRoot local, addr pos, size_t index, addr *ret)
{
	long_float_alloc(local, ret, ((long_float *)arrayspec_ptr(pos))[index]);
}

static void array_getindex(LocalRoot local,
		addr pos, addr mem, size_t index, addr *ret)
{
	enum ARRAY_TYPE type;
	unsigned bytesize;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	type = str->type;
	bytesize = str->bytesize;
	Check(str->size <= index, "size error");
	switch (type) {
		case ARRAY_TYPE_T:
			arraygen_get(mem, index, ret);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_get(local, mem, index, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			arraymemory_getcharacter(local, mem, index, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			arraymemory_getsigned(local, bytesize, mem, index, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			arraymemory_getunsigned(local, bytesize, mem, index, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			arraymemory_getsingle(local, mem, index, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			arraymemory_getdouble(local, mem, index, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			arraymemory_getlong(local, mem, index, ret);
			break;

		default:
			fmte("Invalid array type.", NULL);
			break;
	}
}

int array_get_t(addr pos, size_t index, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_T) return 1;
	arraymemory_get(pos, index, &pos, &index);
	arraygen_get(pos, index, ret);

	return 0;
}

int array_get_bit(addr pos, size_t index, int *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_T) {
		if (array_get_t(pos, index, &pos)) return 1;
		if (bit_getint(pos, ret)) return 1;
		return 0;
	}
	if (str->type != ARRAY_TYPE_BIT) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	bitmemory_getint(pos, index, ret);

	return 0;
}

int array_get_unicode(addr pos, size_t index, unicode *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_T) {
		if (array_get_t(pos, index, &pos)) return 1;
		if (! characterp(pos)) return 1;
		GetCharacter(pos, ret);
		return 0;
	}
	if (str->type != ARRAY_TYPE_CHARACTER) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_getunicode(pos, index, ret);

	return 0;
}

void array_get(LocalRoot local, addr pos, size_t index, addr *ret)
{
	addr mem;
	arraymemory_get(pos, index, &mem, &index);
	array_getindex(local, pos, mem, index, ret);
}

static void arraymemory_setunicode(addr pos, size_t index, unicode value)
{
	((unicode *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setsigned8(addr pos, size_t index, int8_t value)
{
	((int8_t *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setsigned16(addr pos, size_t index, int16_t value)
{
	((int16_t *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setsigned32(addr pos, size_t index, int32_t value)
{
	((int32_t *)arrayspec_ptr(pos))[index] = value;
}

#ifdef LISP_64BIT
static void arraymemory_setsigned64(addr pos, size_t index, int64_t value)
{
	((int64_t *)arrayspec_ptr(pos))[index] = value;
}
#endif

static void arraymemory_setunsigned8(addr pos, size_t index, uint8_t value)
{
	((uint8_t *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setunsigned16(addr pos, size_t index, uint16_t value)
{
	((uint16_t *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setunsigned32(addr pos, size_t index, uint32_t value)
{
	((uint32_t *)arrayspec_ptr(pos))[index] = value;
}

#ifdef LISP_64BIT
static void arraymemory_setunsigned64(addr pos, size_t index, uint64_t value)
{
	((uint64_t *)arrayspec_ptr(pos))[index] = value;
}
#endif

static void arraymemory_setsingle(addr pos, size_t index, single_float value)
{
	((single_float *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setdouble(addr pos, size_t index, double_float value)
{
	((double_float *)arrayspec_ptr(pos))[index] = value;
}

static void arraymemory_setlong(addr pos, size_t index, long_float value)
{
	((long_float *)arrayspec_ptr(pos))[index] = value;
}

int array_set_bit(LocalRoot local, addr pos, size_t index, int value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (value != 0 && value != 1)
		fmte("Value ~A must be a bit type (0 or 1).", fixnumh(value), NULL);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_BIT) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	bitmemory_setint(pos, index, value);

	return 0;
}

int array_set_character(LocalRoot local, addr pos, size_t index, unicode value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		character_alloc(local, &temp, value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_CHARACTER) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setunicode(pos, index, value);

	return 0;
}

int array_set_signed8(LocalRoot local, addr pos, size_t index, int8_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_SIGNED || str->bytesize != 8) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setsigned8(pos, index, value);

	return 0;
}

int array_set_signed16(LocalRoot local, addr pos, size_t index, int16_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_SIGNED || str->bytesize != 16) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setsigned16(pos, index, value);

	return 0;
}

int array_set_signed32(LocalRoot local, addr pos, size_t index, int32_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_SIGNED || str->bytesize != 32) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setsigned32(pos, index, value);

	return 0;
}

#ifdef LISP_64BIT
int array_set_signed64(LocalRoot local, addr pos, size_t index, int64_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_SIGNED || str->bytesize != 64) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setsigned64(pos, index, value);

	return 0;
}
#endif

int array_set_unsigned8(LocalRoot local, addr pos, size_t index, uint8_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_UNSIGNED || str->bytesize != 8) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setunsigned8(pos, index, value);

	return 0;
}

int array_set_unsigned16(LocalRoot local, addr pos, size_t index, uint16_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_UNSIGNED || str->bytesize != 16) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setunsigned16(pos, index, value);

	return 0;
}

int array_set_unsigned32(LocalRoot local, addr pos, size_t index, uint32_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_UNSIGNED || str->bytesize != 32) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setunsigned32(pos, index, value);

	return 0;
}

#ifdef LISP_64BIT
int array_set_unsigned64(LocalRoot local, addr pos, size_t index, uint64_t value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		fixnum_alloc(local, &temp, (fixnum)value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_UNSIGNED || str->bytesize != 64) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setunsigned64(pos, index, value);

	return 0;
}
#endif

int array_set_single(LocalRoot local, addr pos, size_t index, single_float value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		single_float_alloc(local, &temp, value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_SINGLE_FLOAT) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setsingle(pos, index, value);

	return 0;
}

int array_set_double(LocalRoot local, addr pos, size_t index, double_float value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		double_float_alloc(local, &temp, value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_DOUBLE_FLOAT) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setdouble(pos, index, value);

	return 0;
}

int array_set_long(LocalRoot local, addr pos, size_t index, long_float value)
{
	addr temp;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (str->type == ARRAY_TYPE_T) {
		long_float_alloc(local, &temp, value);
		array_set(pos, index, temp);
		return 0;
	}
	if (str->type != ARRAY_TYPE_LONG_FLOAT) {
		return 1;
	}
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setlong(pos, index, value);

	return 0;
}

void array_set(addr pos, size_t index, addr value)
{
	struct array_struct *str;
	addr mem;

	if (GetStatusReadOnly(pos))
		fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	arraymemory_get(pos, index, &mem, &index);
	arraymemory_set(pos, mem, index, value);
}

static void array_setget_size(addr m1, size_t s1, addr m2, size_t s2, size_t e)
{
	byte *data1;
	const byte *data2;

	data1 = (byte *)arrayspec_ptr(m1);
	data2 = (const byte *)arrayspec_ptr(m2);
	data1 += s1 * e;
	data2 += s2 * e;
	memcpy(data1, data2, e);
}

static void array_setget_type(addr p1, addr m1, size_t s1, addr m2, size_t s2)
{
	struct array_struct *str1;
	addr temp;

	str1 = ArrayInfoStruct(p1);
	switch (str1->type) {
		case ARRAY_TYPE_T:
			arraygen_get(m2, s2, &temp);
			arraygen_set(m1, s1, temp);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_setget(m1, s1, m2, s2);
			break;

		default:
			array_setget_size(m1, s1, m2, s2, str1->element);
			break;
	}
}

void array_setget(addr p1, size_t s1, addr p2, size_t s2)
{
	struct array_struct *str1, *str2;
	addr m1, m2;

	if (GetStatusReadOnly(p1))
		fmte("Object ~S is constant.", p1, NULL);
	str1 = ArrayInfoStruct(p1);
	str2 = ArrayInfoStruct(p2);

	if (! array_equal_type(str1, str2->type, str2->bytesize))
		fmte("Array ~S type must be equal to base array ~S.", p1, p2, NULL);
	arraymemory_get(p1, s1, &m1, &s1);
	arraymemory_get(p2, s2, &m2, &s2);
	array_setget_type(p1, m1, s1, m2, s2);
}

static void array_get_inplace_type(addr pos,
		size_t index, size_t element, struct array_value *str)
{
	int value;
	const byte *data;

	switch (str->type) {
		case ARRAY_TYPE_T:
			arraygen_get(pos, index, &(str->value.object));
			return;

		case ARRAY_TYPE_BIT:
			bitmemory_getint(pos, index, &value);
			str->value.bit = value? 1: 0;
			return;

		default:
			break;
	}

	/* memory */
	data = (const byte *)arrayspec_ptr(pos);
	data += element * index;
	memcpy(&(str->value), data, element);
}

void array_get_inplace(addr pos, size_t index, struct array_value *value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	value->type = str->type;
	value->size = str->bytesize;
	arraymemory_get(pos, index, &pos, &index);
	array_get_inplace_type(pos, index, str->element, value);
}

static void array_set_inplace_type(addr pos,
		size_t index, size_t element, const struct array_value *str)
{
	byte *data;

	switch (str->type) {
		case ARRAY_TYPE_T:
			arraygen_set(pos, index, str->value.object);
			return;

		case ARRAY_TYPE_BIT:
			bitmemory_setint(pos, index, str->value.bit);
			return;

		default:
			break;
	}

	/* memory */
	data = (byte *)arrayspec_ptr(pos);
	data += element * index;
	memcpy(data, &(str->value), element);
}

int array_set_inplace(addr pos, size_t index, const struct array_value *value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, value->type, value->size))
		return 1;
	arraymemory_get(pos, index, &pos, &index);
	array_set_inplace_type(pos, index, str->element, value);

	return 0;
}


/*
 *  array
 */
static size_t array1arefindex(addr pos, addr args, struct array_struct *str)
{
	addr arg;
	size_t index;

	if (! consp(args))
		fmte("Subscripts ~S must be list form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		fmte("Subscripts ~S too many arguments.", args, NULL);
	if (getindex_integer(arg, &index))
		fmte("Invalid subscript argument ~S.", arg, NULL);
	if (str->front <= index)
		fmte("Subscript ~S is too large.", arg, NULL);
	return index;
}

static size_t array_arefindex(addr pos, addr args)
{
	struct array_struct *str;
	const size_t *data;
	size_t index, value, depth, dimension;
	addr check, list;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	if (dimension == 1)
		return array1arefindex(pos, args, str);
	data = array_ptrsize(pos);
	index = 0;
	list = args;
	for (depth = 0; list != Nil; depth++) {
		if (! consp(list))
			fmte("Subscripts ~S must be a list type.", list, NULL);
		if (dimension <= depth)
			fmte("Subscripts ~A is too large.", args, NULL);
		GetCons(list, &check, &list);
		if (getindex_integer(check, &value))
			fmte("Invalid index value ~S.", check, NULL);
		index = depth? (index * data[depth]): 0;
		index += value;
	}
	if (depth != str->dimension)
		fmte("Subscript ~S is too few.", args, NULL);

	return index;
}

void array_aref(LocalRoot local, addr pos, addr args, addr *ret)
{
	size_t index = array_arefindex(pos, args);
	array_get(local, pos, index, ret);
}

void array_setf_aref(addr pos, addr args, addr value)
{
	size_t index = array_arefindex(pos, args);
	array_set(pos, index, value);
}

void array_aref_bit(LocalRoot local, addr pos, addr args, addr *ret)
{
	int value;
	size_t index;

	index = array_arefindex(pos, args);
	if (array_get_bit(pos, index, &value))
		fmte("Array must be a bit type.", NULL);
	fixnum_alloc(local, ret, (fixnum)value);
}

void array_setf_aref_bit(addr pos, addr args, addr value)
{
	int check;
	size_t index;

	index = array_arefindex(pos, args);
	if (bit_getint(value, &check))
		fmte("Bit value ~S must be 0 or 1.", value, NULL);
	fixnum_heap(&value, (fixnum)check);
	array_set(pos, index, value);
}

void array_array_dimension(addr array, addr axis, addr *ret)
{
	struct array_struct *str;
	size_t index, dimension;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	dimension = str->dimension;
	if (dimension == 0) {
		fmte("The array have no dimension.", NULL);
	}
	if (getindex_integer(axis, &index)) {
		if (minusp_integer(axis))
			fmte("Index ~A must be a non-negative integer.", axis, NULL);
		else
			fmte("Index ~A is too large.", axis, NULL);
	}
	if (dimension <= index) {
		fmte("The dimension index ~A must be less than "
				"the array-dimension limit ~A.", axis, intsizeh(dimension), NULL);
	}
	index = (array_ptrsize(array))[index];
	make_index_integer_alloc(NULL, ret, index);
}

void array_array_dimensions(addr array, addr *ret)
{
	struct array_struct *str;
	const size_t *data;
	size_t size, i, n;
	addr root, pos;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	root = Nil;
	for (i = 0; i < size; i++) {
		n = size - i - 1;
		make_index_integer_alloc(NULL, &pos, data[n]);
		cons_heap(&root, pos, root);
	}
	*ret = root;
}

void array_array_displacement(addr array, addr *displaced, addr *offset)
{
	struct array_struct *str;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);
	if (str->displaced) {
		GetArrayInfo(array, ARRAY_INFO_DISPLACED, displaced);
		make_index_integer_alloc(NULL, offset, str->offset);
	}
	else {
		*displaced = Nil;
		*offset = fixnumh(0);
	}
}

int array_array_in_bounds_p(addr array, addr rest)
{
	int result;
	struct array_struct *str;
	addr list, pos;
	size_t size, i, check;
	const size_t *data;

	str = ArrayInfoStruct(array);
	size = str->dimension;
	data = array_ptrsize(array);
	result = 1;
	list = rest;
	for (i = 0; i < size; i++) {
		if (list == Nil)
			fmte("The subscripts ~S is too few arguments.", rest, NULL);
		if (! consp(list))
			fmte("Invalid subscripts arguments ~S.", rest, NULL);
		GetCons(list, &pos, &list);
		if (! integerp(pos))
			fmte("The subscript ~S must be integer type.", pos, NULL);
		if (getindex_integer(pos, &check)) {
			/* minus or large value */
			result = 0;
			continue;
		}
		if (data[i] <= check) {
			/* out of range */
			result = 0;
			continue;
		}
	}
	if (list != Nil)
		fmte("The subscripts ~S is too many arguments.", rest, NULL);

	return result;
}

void array_array_row_major_index(addr array, addr rest, addr *ret)
{
	size_t value = array_arefindex(array, rest);
	*ret = intsizeh(value);
}

int array_fill_pointer(addr array, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) return 1;
	*ret = intsizeh(str->front);

	return 0;
}

int array_setf_fill_pointer(addr array, addr value)
{
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) return 1;
	if (getindex_integer(value, &size))
		fmte("Invalid fill-pointer value ~S.", value, NULL);
	if (str->size <= size) {
		fmte("Fill-pointer value ~A must be less than array size ~A.",
				value, intsizeh(str->size), NULL);
	}
	str->front = size;

	return 0;
}

int array_fill_pointer_start(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer) {
		return 1;
	}
	else {
		str->front = 0;
		return 0;
	}
}

int array_fill_pointer_end(addr array)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	else
		return 0;
}

int array_fill_pointer_set(addr array, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	if (! str->fillpointer)
		return 1;
	if (str->size <= size)
		return 1;
	str->front = size;

	return 0;
}


/*
 *  array_value
 */
static void array_value_signed(LocalRoot local,
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

static void array_value_unsigned(LocalRoot local,
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

void array_value_alloc(LocalRoot local, addr *ret, const struct array_value *str)
{
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			fixnum_alloc(local, ret, (str->value.bit)? 1: 0);
			break;

		case ARRAY_TYPE_CHARACTER:
			character_alloc(local, ret, str->value.character);
			break;

		case ARRAY_TYPE_SIGNED:
			array_value_signed(local, ret, str);
			break;

		case ARRAY_TYPE_UNSIGNED:
			array_value_unsigned(local, ret, str);
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
			*ret = str->value.object;
			break;
	}
}

void array_value_local(LocalRoot local, addr *ret, const struct array_value *str)
{
	Check(local == NULL, "local error");
	array_value_alloc(local, ret, str);
}

void array_value_heap(addr *ret, const struct array_value *str)
{
	array_value_alloc(NULL, ret, str);
}

