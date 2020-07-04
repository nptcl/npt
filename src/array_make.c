#include <math.h>
#include "arch.h"
#include "array.h"
#include "array_access.h"
#include "array_copy.h"
#include "array_make.h"
#include "bignum_data.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bit.h"
#include "character.h"
#include "control.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "sequence.h"
#include "strtype.h"
#include "type_object.h"
#include "type_upgraded.h"

/*
 *  control
 */
static void array_set_type_value(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	addr value;
	struct array_struct *str;

	upgraded_array_object(type, size, &value);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
}

_g void array_set_type(addr pos)
{
	struct array_struct *str;
	addr type;

	str = ArrayInfoStruct(pos);
	upgraded_array_object(str->type, str->bytesize, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
}

_g void array_set_element_size(addr pos)
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


/*
 *  array-set-dimension
 */
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

static void array_set_dimension0(addr pos, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 0;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	*ret = 1;
}

static void array_set_dimension1(addr pos, addr value, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (minusp_integer(value))
		fmte("Array index ~A must be a non-negative integer.", value, NULL);
	if (GetIndex_integer(value, ret))
		fmte("Array index ~A is too large.", value, NULL);
	str->dimension = 1;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
}

static void array_set_dimension2(addr pos, addr value, size_t *ret)
{
	struct array_struct *str;
	addr temp, index;
	size_t size, *data, i;

	str = ArrayInfoStruct(pos);
	size = length_list_safe(value);
	if (size == 1) {
		GetCar(value, &value);
		array_set_dimension1(pos, value, ret);
		return;
	}
	arraysize_heap(&temp, size);
	data = arraysize_ptr(temp);
	for (i = 0; value != Nil; i++) {
		GetCons(value, &index, &value);
		if (! integerp(index))
			fmte("Array index ~A must be an integer.", index, NULL);
		if (minusp_integer(index))
			fmte("Array index ~A must be a non-negative integer.", index, NULL);
		if (GetIndex_integer(index, &size))
			fmte("Array index ~A is too large.", index, NULL);
		data[i] = size;
	}
	str->dimension = i;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	array_getsize(pos, ret);
}

_g void array_set_dimension(addr pos, addr value)
{
	struct array_struct *str;
	size_t size;

	if (value == Nil)
		array_set_dimension0(pos, &size);
	else if (integerp(value))
		array_set_dimension1(pos, value, &size);
	else if (consp(value))
		array_set_dimension2(pos, value, &size);
	else {
		fmte("Array index ~A must be an integer or list.", value, NULL);
		return;
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = size;
}


/*
 *  array-allocate
 */
static void array_allocate_t(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	arraygen_alloc(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);
}

_g void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	bitmemory_unsafe(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);
}

_g void array_allocate_size(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	size_t size;

	if (multisafe_size(str->size , str->element, &size))
		fmte("size overflow.", NULL);
	arrayspec_alloc(local, &array, size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);
}

_g void array_allocate(LocalRoot local, addr pos, struct array_struct *str)
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


/*
 *  array-memory
 */
static void array_set_adjustable(struct array_struct *str, addr adjustable)
{
	str->adjustable = (adjustable != Nil);
}

static void array_set_fillpointer(struct array_struct *str, addr fill)
{
	size_t size;

	str->fillpointer = (fill != Nil);
	if (fill == Nil)
		return;
	if (str->dimension != 1)
		fmte("fill-pointer array must be a 1 dimensional.", NULL);
	if (fill == T)
		return;
	if (! integerp(fill))
		fmte("fill-pointer ~A must be an integer.", fill, NULL);
	if (minusp_integer(fill))
		fmte("fill-pointer ~A must be a non-negative integer.", fill, NULL);
	if (GetIndex_integer(fill, &size))
		fmte("fill-pointer ~A is too large.", fill, NULL);
	if (str->size < size)
		fmte("fill-pointer ~A must be less than array size.", fill, NULL);
	str->front = size;
}

static void array_set_displaced_array(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(displaced);
	if (str2->size < str1->size) {
		fmte("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str1, str2->type, str2->bytesize))
		fmte("Array type must be equal to displaced array.", NULL);
	*ret = str2->size;
}

static void array_set_displaced_vector(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	lenarray(displaced, &size);
	if (size < str->size) {
		fmte("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_T, 0))
		fmte("Array type must be equal to displaced array.", NULL);
	*ret = size;
}

static void array_set_displaced_bitvector(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	bitmemory_length(displaced, &size);
	if (size < str->size) {
		fmte("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_BIT, 0))
		fmte("Array type must be equal to displaced array.", NULL);
	*ret = size;
}

static void array_set_displaced_string(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	string_length(displaced, &size);
	if (size < str->size) {
		fmte("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_CHARACTER, 0))
		fmte("Array type must be equal to displaced array.", NULL);
	*ret = size;
}

static void array_set_displaced_value(
		addr pos, size_t size, addr displaced, addr offset)
{
	struct array_struct *str;
	size_t value;

	str = ArrayInfoStruct(pos);
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, displaced);

	/* displaced-index-offset */
	if (! integerp(offset))
		fmte("Array offset ~A must be an integer.", offset, NULL);
	if (minusp_integer(offset))
		fmte("Array offset ~A must be a non-negative integer.", offset, NULL);
	if (GetIndex_integer(offset, &value))
		fmte("Array offset ~A is too large.", offset, NULL);
	if (size < value)
		fmte("Too large offset size ~A.", offset, NULL);
	if (size - value < str->size)
		fmte("Array size is not enough length.", NULL);
	str->offset = value;
}

_g void array_set_displaced(addr pos, addr displaced, addr offset)
{
	struct array_struct *str;
	size_t size;

	/* set structure */
	str = ArrayInfoStruct(pos);
	str->displaced = (displaced != Nil);
	if (displaced == Nil) {
		str->offset = 0;
		return;
	}

	/* type check */
	switch (GetType(displaced)) {
		case LISPTYPE_ARRAY:
			array_set_displaced_array(pos, displaced, offset, &size);
			break;

		case LISPTYPE_VECTOR:
			array_set_displaced_vector(pos, displaced, offset, &size);
			break;

		case LISPTYPE_BITVECTOR:
			array_set_displaced_bitvector(pos, displaced, offset, &size);
			break;

		case LISPTYPE_STRING:
			array_set_displaced_string(pos, displaced, offset, &size);
			break;

		default:
			fmte(":displaced-to parameter ~S must be a array type.", displaced, NULL);
			return;
	}
	array_set_displaced_value(pos, size, displaced, offset);
}

_g void array_set_simple(addr pos)
{
	struct array_struct *str = ArrayInfoStruct(pos);
	str->simple = ! (str->adjustable || str->fillpointer || str->displaced);
}

_g void array_make_memory(addr pos, addr adjust, addr fill, addr displaced, addr offset)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_adjustable(str, adjust);
	array_set_fillpointer(str, fill);
	array_set_displaced(pos, displaced, offset);
	array_set_simple(pos);
	if (str->simple || str->displaced == 0)
		array_allocate(NULL, pos, str);
}


/*
 *  array-initial
 */
static void array_initial_t(addr pos, addr value, size_t size)
{
	size_t i;

	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
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
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	bitmemory_memset(pos, (int)init);
}

#ifdef LISP_DEBUG
static void array_initial_memset_buffer(addr pos, int value)
{
	struct array_struct *str;
	size_t size, elem;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	memset(arrayspec_ptr(pos), value, size * elem);
}
#endif

static void array_initial_memset(addr pos, const void *src)
{
	struct array_struct *str;
	size_t size, elem, i;
	byte *dst;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	dst = (byte *)arrayspec_ptr(pos);
	for (i = 0; i < size; i++) {
		memcpy(dst, src, elem);
		dst += elem;
	}
}

static void array_initial_unicode(addr pos, unicode u)
{
	array_initial_memset(pos, (const void *)&u);
}

static void array_initial_character(addr pos, addr value)
{
	unicode u;

	if (! characterp(value))
		fmte(":initial-element ~A must be character type.", value, NULL);
	GetCharacter(value, &u);
	array_initial_unicode(pos, u);
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
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
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
	array_initial_memset(pos, (const void *)&c);
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
	array_initial_memset(pos, (const void *)&c);
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
	array_initial_memset(pos, (const void *)&init);
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
	array_initial_memset(pos, (const void *)&init);
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
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
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
	array_initial_memset(pos, (const void *)&c);
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
	array_initial_memset(pos, (const void *)&c);
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
		array_initial_memset(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
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
		array_initial_memset(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
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
	array_initial_memset(pos, (const void *)&v);
}

static void array_initial_double(addr pos, addr value)
{
	double_float v;

	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		fmte(":initial-element ~A must be double-float type.", value, NULL);
	GetDoubleFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);
}

static void array_initial_long(addr pos, addr value)
{
	long_float v;

	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		fmte(":initial-element ~A must be long-float type.", value, NULL);
	GetLongFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);
}

static void array_initial_value(addr pos, addr value)
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


/*
 *  array-initial-contents
 *
 *  (s1 s2 s3 s4) : (n1 n2 n3 n4)
 *   -> (s4*s3*s2*n1) + (s4*s3*n2) + (s4*n3) + n4
 *   -> s4*(...) + n4
 *   -> s4*(s3*(...) + n3) + n4
 *   -> s4*(s3*(s2*n1 + n2) + n3) + n4
 *   [1] n1
 *   [2] s2*... + n2
 *   [3] s3*... + n3
 *   [4] s4*... + n4
 */
static void array_contents_recursive(addr, addr,
		const size_t *data, size_t limit, size_t depth, size_t size);

static void array_contents_list(addr pos, addr list,
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
			array_contents_recursive(pos, next, data, limit, depth+1, size+i);
		}
		if (i < length)
			fmte("Too few :initial-contents list.", NULL);
	}
	else {
		array_set(pos, size, list);
	}
}

static void array_contents_sequence(addr pos, addr object,
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
			getelt_sequence(NULL, object, i, &next);
			array_contents_recursive(pos, next, data, limit, depth+1, size+i);
		}
	}
	else {
		array_set(pos, size, object);
	}
}

static void array_contents_object(addr pos, addr list,
		size_t limit, size_t depth, size_t size)
{
	if (depth < limit)
		fmte("Too few :initial-contents.", NULL);
	else
		array_set(pos, size, list);
}

static void array_contents_recursive(addr pos,
		addr object, const size_t *data, size_t limit, size_t depth, size_t size)
{
	switch (GetType(object)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			array_contents_list(pos, object, data, limit, depth, size);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			array_contents_sequence(pos, object, data, limit, depth, size);
			break;

		default:
			array_contents_object(pos, object, limit, depth, size);
			break;
	}
}

static void array_contents_setf(addr pos, addr contents)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	array_contents_recursive(pos, contents, data, dimension, 0, 0);
}

static void array_initial_contents(addr pos, addr contents)
{
	size_t dimension;

	dimension = ArrayInfoStruct(pos)->dimension;
	if (dimension == 0)
		array_set(pos, 0, contents);
	else
		array_contents_setf(pos, contents);
}


/*
 *  array-make-array
 */
static void array_make_initial_clear(addr pos)
{
	struct array_struct *str;
#ifdef LISP_DEBUG
	single_float sv;
	double_float dv;
	long_float lv;
#endif

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_CHARACTER:
			array_initial_unicode(pos, 0);
			break;

#ifdef LISP_DEBUG
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
			bitmemory_memset(pos, 1);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			array_initial_memset_buffer(pos, 0xFF);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			sv = nanf("");
			array_initial_memset(pos, (const void *)&sv);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			dv = nan("");
			array_initial_memset(pos, (const void *)&dv);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			lv = nanl("");
			array_initial_memset(pos, (const void *)&lv);
			break;
#endif

		default:
			break;
	}
}

_g void array_make_initial(addr pos, addr initial, addr contents)
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
		array_initial_value(pos, initial);
		return;
	}
	if (contents != Unbound) {
		array_initial_contents(pos, contents);
		return;
	}
	if (str->displaced == 0)
		array_make_initial_clear(pos);
}

static void array_set_type_upgraded(addr pos, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	upgraded_array_value(type, &value, &size);
	array_set_type_value(pos, value, size);
}

_g void array_make_array(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	array_set_type_upgraded(pos, type);
	array_set_element_size(pos);
	array_set_dimension(pos, dimension);
	array_make_memory(pos, adjustable, fillpointer, displaced, offset);
	array_make_initial(pos, initial, contents);
	*ret = pos;
}


/*
 *  array_contents_heap
 */
static void array_contents_size(addr pos, addr rankarg, addr contents)
{
	struct array_struct *str;
	size_t i, rank, size, *data;
	addr temp;

	if (GetIndex_integer(rankarg, &rank))
		fmte("Array rank ~A is too large.", rankarg, NULL);
	str = ArrayInfoStruct(pos);
	if (rank == 0) {
		str->dimension = 0;
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
		size = 1;
	}
	else if (rank == 1) {
		str->dimension = 1;
		size = length_list_safe(contents);
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	}
	else {
		str->dimension = rank;
		arraysize_heap(&temp, rank);
		data = arraysize_ptr(temp);
		for (i = 0; i < rank; i++) {
			if (! consp(contents))
				fmte("Invalid initial-contents parameter ~S.", contents, NULL);
			data[i] = length_list_safe(contents);
			GetCar(contents, &contents);
		}
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
		array_getsize(pos, &size);
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = size;
}

_g void array_contents_heap(addr *ret, addr rank, addr contents)
{
	addr pos;

	array_empty_heap(&pos);
	array_set_type_value(pos, ARRAY_TYPE_T, 0);
	array_set_element_size(pos);
	array_contents_size(pos, rank, contents);
	array_make_memory(pos, Nil, Nil, Nil, Nil);
	array_make_initial(pos, Unbound, contents);
	*ret = pos;
}


/*
 *  array function
 */
static void array_check_fillpointer(addr pos, struct array_struct *str)
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

_g void array_character_alloc(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type_value(pos, ARRAY_TYPE_CHARACTER, 0);
	array_set_element_size(pos);
	array_set_simple(pos);
	array_check_fillpointer(pos, str);
	array_allocate(local, pos, str);
}

_g void array_build(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type(pos);
	array_set_element_size(pos);
	array_set_simple(pos);
	array_check_fillpointer(pos, str);
	array_allocate(NULL, pos, str);
}

