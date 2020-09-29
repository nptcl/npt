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
static int array_getsize_(addr pos, size_t *ret)
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
			return fmte_("size overflow.", NULL);
	}

	return Result(ret, size);
}

static void array_set_dimension0(addr pos, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 0;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	*ret = 1;
}

static int array_set_dimension1_(addr pos, addr value, size_t *ret)
{
	int check;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(minusp_integer_(value, &check));
	if (check) {
		*ret = 0;
		return fmte_("Array index ~A must be a non-negative integer.", value, NULL);
	}
	if (GetIndex_integer(value, ret)) {
		*ret = 0;
		return fmte_("Array index ~A is too large.", value, NULL);
	}
	str->dimension = 1;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);

	return 0;
}

static int array_set_dimension2_(addr pos, addr value, size_t *ret)
{
	int check;
	struct array_struct *str;
	addr temp, index;
	size_t size, *data, i;

	str = ArrayInfoStruct(pos);
	Return(length_list_safe_(value, &size));
	if (size == 1) {
		GetCar(value, &value);
		return array_set_dimension1_(pos, value, ret);
	}
	Return(arraysize_heap_(&temp, size));
	data = arraysize_ptr(temp);
	for (i = 0; value != Nil; i++) {
		GetCons(value, &index, &value);
		if (! integerp(index)) {
			*ret = 0;
			return fmte_("Array index ~A must be an integer.", index, NULL);
		}
		Return(minusp_integer_(index, &check));
		if (check) {
			*ret = 0;
			return fmte_("Array index ~A must be a non-negative integer.", index, NULL);
		}
		if (GetIndex_integer(index, &size)) {
			*ret = 0;
			return fmte_("Array index ~A is too large.", index, NULL);
		}
		data[i] = size;
	}
	str->dimension = i;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
	return array_getsize_(pos, ret);
}

_g int array_set_dimension_(addr pos, addr value)
{
	struct array_struct *str;
	size_t size;

	if (value == Nil) {
		array_set_dimension0(pos, &size);
	}
	else if (integerp(value)) {
		Return(array_set_dimension1_(pos, value, &size));
	}
	else if (consp(value)) {
		Return(array_set_dimension2_(pos, value, &size));
	}
	else {
		return fmte_("Array index ~A must be an integer or list.", value, NULL);
	}
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;

	return 0;
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

_g int array_allocate_size_(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	size_t size;

	if (multisafe_size(str->size , str->element, &size))
		return fmte_("size overflow.", NULL);
	arrayspec_alloc(local, &array, size);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, array);

	return 0;
}

_g int array_allocate_(LocalRoot local, addr pos, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			return fmte_("The array has no element size.", NULL);

		case ARRAY_TYPE_T:
			array_allocate_t(local, pos, str);
			return 0;

		case ARRAY_TYPE_BIT:
			array_allocate_bit(local, pos, str);
			return 0;

		default:
			return array_allocate_size_(local, pos, str);
	}
}


/*
 *  array-memory
 */
static void array_set_adjustable(struct array_struct *str, addr adjustable)
{
	str->adjustable = (adjustable != Nil);
}

static int array_set_fillpointer_(struct array_struct *str, addr fill)
{
	int check;
	size_t size;

	str->fillpointer = (fill != Nil);
	if (fill == Nil)
		return 0;
	if (str->dimension != 1)
		return fmte_("fill-pointer array must be a 1 dimensional.", NULL);
	if (fill == T)
		return 0;
	if (! integerp(fill))
		return fmte_("fill-pointer ~A must be an integer.", fill, NULL);
	Return(minusp_integer_(fill, &check));
	if (check)
		return fmte_("fill-pointer ~A must be a non-negative integer.", fill, NULL);
	if (GetIndex_integer(fill, &size))
		return fmte_("fill-pointer ~A is too large.", fill, NULL);
	if (str->size < size)
		return fmte_("fill-pointer ~A must be less than array size.", fill, NULL);
	str->front = size;

	return 0;
}

static int array_set_displaced_array_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(displaced);
	if (str2->size < str1->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str1, str2->type, str2->bytesize)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, str2->size);
}

static int array_set_displaced_vector_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	lenarray(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_T, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_bitvector_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	bitmemory_length(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_BIT, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_string_(
		addr pos, addr displaced, addr offset, size_t *ret)
{
	struct array_struct *str;
	size_t size;

	/* :displaced-to */
	str = ArrayInfoStruct(pos);
	string_length(displaced, &size);
	if (size < str->size) {
		*ret = 0;
		return fmte_("Array size and offset must be less than equal to "
				"displaced array size.", NULL);
	}
	if (! array_equal_type(str, ARRAY_TYPE_CHARACTER, 0)) {
		*ret = 0;
		return fmte_("Array type must be equal to displaced array.", NULL);
	}

	return Result(ret, size);
}

static int array_set_displaced_value_(
		addr pos, size_t size, addr displaced, addr offset)
{
	int check;
	struct array_struct *str;
	size_t value;

	str = ArrayInfoStruct(pos);
	SetArrayInfo(pos, ARRAY_INDEX_DISPLACED, displaced);

	/* displaced-index-offset */
	if (! integerp(offset))
		return fmte_("Array offset ~A must be an integer.", offset, NULL);
	Return(minusp_integer_(offset, &check));
	if (check)
		return fmte_("Array offset ~A must be a non-negative integer.", offset, NULL);
	if (GetIndex_integer(offset, &value))
		return fmte_("Array offset ~A is too large.", offset, NULL);
	if (size < value)
		return fmte_("Too large offset size ~A.", offset, NULL);
	if (size - value < str->size)
		return fmte_("Array size is not enough length.", NULL);
	str->offset = value;

	return 0;
}

_g int array_set_displaced_(addr pos, addr displaced, addr offset)
{
	struct array_struct *str;
	size_t size;

	/* set structure */
	str = ArrayInfoStruct(pos);
	str->displaced = (displaced != Nil);
	if (displaced == Nil) {
		str->offset = 0;
		return 0;
	}

	/* type check */
	switch (GetType(displaced)) {
		case LISPTYPE_ARRAY:
			Return(array_set_displaced_array_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_VECTOR:
			Return(array_set_displaced_vector_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_BITVECTOR:
			Return(array_set_displaced_bitvector_(pos, displaced, offset, &size));
			break;

		case LISPTYPE_STRING:
			Return(array_set_displaced_string_(pos, displaced, offset, &size));
			break;

		default:
			return fmte_(":displaced-to parameter ~S "
					"must be a array type.", displaced, NULL);
	}

	return array_set_displaced_value_(pos, size, displaced, offset);
}

_g void array_set_simple(addr pos)
{
	struct array_struct *str;
	str = ArrayInfoStruct(pos);
	str->simple = ! (str->adjustable || str->fillpointer || str->displaced);
}

_g int array_make_memory_(addr pos, addr adjust, addr fill, addr displaced, addr offset)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_adjustable(str, adjust);
	Return(array_set_fillpointer_(str, fill));
	Return(array_set_displaced_(pos, displaced, offset));
	array_set_simple(pos);
	if (str->simple || str->displaced == 0) {
		Return(array_allocate_(NULL, pos, str));
	}

	return 0;
}


/*
 *  array-initial
 */
static int array_initial_t_(addr pos, addr value, size_t size)
{
	size_t i;

	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	for (i = 0; i < size; i++)
		arraygen_set(pos, i, value);

	return 0;
}

static int array_initial_bit_(addr pos, addr value, size_t size)
{
	fixnum init;

	if (! fixnump(value))
		return fmte_(":initial-element ~A must be integer type.", value, NULL);
	GetFixnum(value, &init);
	if (init != 0 && init != 1)
		return fmte_(":initail-element ~A must be 0 or 1.", value, NULL);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	bitmemory_memset(pos, (int)init);

	return 0;
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

static int array_initial_character_(addr pos, addr value)
{
	unicode u;

	if (! characterp(value))
		return fmte_(":initial-element ~A must be character type.", value, NULL);
	GetCharacter(value, &u);
	array_initial_unicode(pos, u);

	return 0;
}

static int array_initial_signed8_(addr pos, addr value)
{
	void *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	data = (void *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 8).", value, NULL);
}

static int array_initial_signed16_(addr pos, addr value)
{
	int16_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	c = (int16_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static int array_initial_signed32_(addr pos, addr value)
{
	int32_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	c = (int32_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
}

static int array_initial_signed64_(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		return fmte_("Overflow :initial-element ~A in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	array_initial_memset(pos, (const void *)&init);

	return 0;
}
#else
static int array_initial_signed32_(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		return fmte_("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	array_initial_memset(pos, (const void *)&init);

	return 0;
}
#endif

static int array_initial_signed_(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			return array_initial_signed8_(pos, value);

		case 16:
			return array_initial_signed16_(pos, value);

		case 32:
			return array_initial_signed32_(pos, value);

#ifdef LISP_64BIT
		case 64:
			return array_initial_signed64_(pos, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

static int array_initial_unsigned8_(addr pos, addr value)
{
	byte *data;
	struct array_struct *str;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT8_MAX < init)
		goto error;
	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	data = (byte *)arrayspec_ptr(pos);
	memset(data, (byte)init, str->size);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 8).", value, NULL);
}

static int array_initial_unsigned16_(addr pos, addr value)
{
	uint16_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT16_MAX < init)
		goto error;
	c = (uint16_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static int array_initial_unsigned32_(addr pos, addr value)
{
	uint32_t c;
	fixnum init;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT32_MAX < init)
		goto error;
	c = (uint32_t)init;
	array_initial_memset(pos, (const void *)&c);
	return 0;

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}

static int array_initial_unsigned64_(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0)
			goto error;
		array_initial_memset(pos, (const void *)&init);
		return 0;
	}
	if (bignump(value)) {
		if (minusp_bignum(value))
			goto error;
		GetSizeBignum(value, &size);
		if (size != 1)
			goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
		return 0;
	}
	return TypeError_(value, INTEGER);

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 64).", value, NULL);
}
#else
static int array_initial_unsigned32_(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		return fmte_(":initial-element ~A must be an integer type.", value, NULL);
	if (minusp_integerp(value)) {
		return fmte_(":initial-element ~A "
				"must be greater than equal to 0.", value, NULL);
	}
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0)
			goto error;
		array_initial_memset(pos, (const void *)&init);
		return 0;
	}
	if (bignump(value)) {
		if (minusp_bignum(value))
			goto error;
		GetSizeBignum(value, &size);
		if (size != 1)
			goto error;
		getfixed_bignum(value, 0, &bigv);
		array_initial_memset(pos, (const void *)&bigv);
		return 0;
	}
	return TypeError_(value, INTEGER);

error:
	return fmte_("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}
#endif

static int array_initial_unsigned_(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			return array_initial_unsigned8_(pos, value);

		case 16:
			return array_initial_unsigned16_(pos, value);

		case 32:
			return array_initial_unsigned32_(pos, value);

#ifdef LISP_64BIT
		case 64:
			return array_initial_unsigned64_(pos, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

static int array_initial_single_(addr pos, addr value)
{
	single_float v;

	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		return fmte_(":initial-element ~A must be single-float type.", value, NULL);
	GetSingleFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_double_(addr pos, addr value)
{
	double_float v;

	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		return fmte_(":initial-element ~A must be double-float type.", value, NULL);
	GetDoubleFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_long_(addr pos, addr value)
{
	long_float v;

	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		return fmte_(":initial-element ~A must be long-float type.", value, NULL);
	GetLongFloat(value, &v);
	array_initial_memset(pos, (const void *)&v);

	return 0;
}

static int array_initial_value_(addr pos, addr value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return array_initial_t_(pos, value, str->size);

		case ARRAY_TYPE_BIT:
			return array_initial_bit_(pos, value, str->size);

		case ARRAY_TYPE_CHARACTER:
			return array_initial_character_(pos, value);

		case ARRAY_TYPE_SIGNED:
			return array_initial_signed_(pos, value);

		case ARRAY_TYPE_UNSIGNED:
			return array_initial_unsigned_(pos, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return array_initial_single_(pos, value);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return array_initial_double_(pos, value);

		case ARRAY_TYPE_LONG_FLOAT:
			return array_initial_long_(pos, value);

		default:
			return fmte_("Invalid array type.", NULL);
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
static int array_contents_recursive_(addr, addr,
		const size_t *data, size_t limit, size_t depth, size_t size);

static int array_contents_list_(addr pos, addr list,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		for (i = 0; list != Nil; i++) {
			if (length <= i)
				return fmte_("Too many :initial-contents ~S list.", list, NULL);
			Return_getcons(list, &next, &list);
			Return(array_contents_recursive_(pos, next, data, limit, depth+1, size+i));
		}
		if (i < length)
			return fmte_("Too few :initial-contents list.", NULL);
	}
	else {
		Return(array_set_(pos, size, list));
	}

	return 0;
}

static int array_contents_sequence_(addr pos, addr object,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	addr next;
	size_t i, length, check;

	if (depth < limit) {
		length = data[depth];
		size = depth? (size * length): 0;
		Return(length_sequence_(object, 1, &check));
		if (length < check)
			return fmte_("Too many :initial-contents ~S.", object, NULL);
		if (check < length)
			return fmte_("Too few :initial-contents ~S.", object, NULL);
		for (i = 0; i < check; i++) {
			Return(getelt_sequence_(NULL, object, i, &next));
			Return(array_contents_recursive_(pos, next, data, limit, depth+1, size+i));
		}
	}
	else {
		Return(array_set_(pos, size, object));
	}

	return 0;
}

static int array_contents_object_(addr pos, addr list,
		size_t limit, size_t depth, size_t size)
{
	if (depth < limit)
		return fmte_("Too few :initial-contents.", NULL);
	else
		return array_set_(pos, size, list);
}

static int array_contents_recursive_(addr pos,
		addr object, const size_t *data, size_t limit, size_t depth, size_t size)
{
	switch (GetType(object)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			return array_contents_list_(pos, object, data, limit, depth, size);

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			return array_contents_sequence_(pos, object, data, limit, depth, size);

		default:
			return array_contents_object_(pos, object, limit, depth, size);
	}
}

static int array_contents_setf_(addr pos, addr contents)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_ptrsize(pos);
	return array_contents_recursive_(pos, contents, data, dimension, 0, 0);
}

static int array_initial_contents_(addr pos, addr contents)
{
	size_t dimension;

	dimension = ArrayInfoStruct(pos)->dimension;
	if (dimension == 0)
		return array_set_(pos, 0, contents);
	else
		return array_contents_setf_(pos, contents);
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

_g int array_make_initial_(addr pos, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced && (initial != Unbound || contents != Unbound)) {
		return fmte_("Displaced array don't have "
				":initial-element or :initial-contents.", NULL);
	}
	if (initial != Unbound && contents != Unbound) {
		return fmte_("Array parameter cannot have both :initial-element and "
				":initial-contens parameter.", NULL);
	}
	if (initial != Unbound) {
		return array_initial_value_(pos, initial);
	}
	if (contents != Unbound) {
		return array_initial_contents_(pos, contents);
	}
	if (str->displaced == 0) {
		array_make_initial_clear(pos);
	}

	return 0;
}

static int array_set_type_upgraded_(addr pos, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	Return(upgraded_array_value_(type, &value, &size));
	array_set_type_value(pos, value, size);

	return 0;
}

_g int array_make_array_(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	array_empty_heap(&pos);
	Return(array_set_type_upgraded_(pos, type));
	array_set_element_size(pos);
	Return(array_set_dimension_(pos, dimension));
	Return(array_make_memory_(pos, adjustable, fillpointer, displaced, offset));
	Return(array_make_initial_(pos, initial, contents));

	return Result(ret, pos);
}


/*
 *  array_contents_heap
 */
static int array_contents_size_(addr pos, addr rankarg, addr contents)
{
	struct array_struct *str;
	size_t i, rank, size, *data;
	addr temp;

	if (GetIndex_integer(rankarg, &rank))
		return fmte_("Array rank ~A is too large.", rankarg, NULL);
	str = ArrayInfoStruct(pos);
	if (rank == 0) {
		str->dimension = 0;
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
		size = 1;
	}
	else if (rank == 1) {
		str->dimension = 1;
		Return(length_list_safe_(contents, &size));
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
	}
	else {
		str->dimension = rank;
		Return(arraysize_heap_(&temp, rank));
		data = arraysize_ptr(temp);
		for (i = 0; i < rank; i++) {
			if (! consp(contents))
				return fmte_("Invalid initial-contents parameter ~S.", contents, NULL);
			Return(length_list_safe_(contents, &(data[i])));
			GetCar(contents, &contents);
		}
		SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, temp);
		Return(array_getsize_(pos, &size));
	}
	str = ArrayInfoStruct(pos);
	str->size = str->front = size;

	return 0;
}

_g int array_contents_heap_(addr *ret, addr rank, addr contents)
{
	addr pos;

	array_empty_heap(&pos);
	array_set_type_value(pos, ARRAY_TYPE_T, 0);
	array_set_element_size(pos);
	Return(array_contents_size_(pos, rank, contents));
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	Return(array_make_initial_(pos, Unbound, contents));

	return Result(ret, pos);
}


/*
 *  array function
 */
static int array_check_fillpointer_(addr pos, struct array_struct *str)
{
	if (str->fillpointer) {
		if (str->size < str->front)
			return fmte_("fill-pointer size must be smaller than element-size.", NULL);
		if (! array_vector_p(pos))
			return fmte_("fill-pointer array must be an one dimension.", NULL);
	}
	else {
		str->front = str->size;
	}

	return 0;
}

_g int array_character_alloc_(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type_value(pos, ARRAY_TYPE_CHARACTER, 0);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_check_fillpointer_(pos, str));
	return array_allocate_(local, pos, str);
}

_g int array_build_(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	array_set_type(pos);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_check_fillpointer_(pos, str));
	return array_allocate_(NULL, pos, str);
}

