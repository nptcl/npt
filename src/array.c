#include "array.h"
#include "bignum.h"
#include "bit.h"
#include "calltype.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "format.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "sequence.h"
#include "type_parse.h"
#include "type_optimize.h"
#include "type_value.h"

void Debug_set_arraygen(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(Low_lenr_arraygen(pos) <= index, "size error");
	Low_set_arraygen(pos, index, value);
}

void Debug_get_arraygen(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Check(Low_lenr_arraygen(pos) <= index, "size error");
	Low_get_arraygen(pos, index, ret);
}

void Debug_len_arraygen(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_GENERAL);
	Low_len_arraygen(pos, ret);
}

size_t Debug_lenr_arraygen(addr pos)
{
	return Low_lenr_arraygen(pos);
}

void Debug_pos_arraymemory(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	Low_pos_arraymemory(pos, ret);
}

addr Debug_ptr_arraymemory(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_SPECIALIZED);
	return Low_ptr_arraymemory(pos);
}

size_t *Debug_PtrArrayDimension(addr pos)
{
	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	return Low_PtrArrayDimension(pos);
}

struct array_struct *Debug_ArrayInfoStruct(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return Low_ArrayInfoStruct(pos);
}

addr Debug_RefArrayInfo(addr pos, size_t index)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return Low_RefArrayInfo(pos, index);
}

void Debug_GetArrayInfo(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Low_GetArrayInfo(pos, index, ret);
}

void Debug_SetArrayInfo(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Check(GetStatusReadOnly(pos), "readonly error");
	Low_SetArrayInfo(pos, index, value);
}

void Debug_LenArrayInfo(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	Low_LenArrayInfo(pos, ret);
}

size_t Debug_LenArrayInfor(addr pos)
{
	CheckType(pos, LISPTYPE_ARRAY);
	return Low_LenArrayInfor(pos);
}

static void arraygen_alloc(LocalRoot local, addr *ret, size_t size)
{
	Low_arraygen_alloc(local, ret, LISPSYSTEM_ARRAY_GENERAL, size);
}

static void arrayspec_alloc(LocalRoot local, addr *ret, size_t size)
{
	Low_arrayspec_alloc(local, ret, LISPSYSTEM_ARRAY_SPECIALIZED, size);
}

static void arraydimension_alloc(LocalRoot local, addr *ret, size_t size)
{
	Low_arraydimension_alloc(local, ret, LISPSYSTEM_ARRAY_DIMENSION, size);
}

static void arrayinfo_alloc(LocalRoot local, addr *ret)
{
	Low_arrayinfo_alloc(local, ret, LISPTYPE_ARRAY,
			ARRAY_INFO_SIZE, sizeoft(struct array_struct));
}

static int multisafe_size(size_t left, size_t right, size_t *result)
{
	size_t temp;

	if (left == 0 || right == 0) {
		*result = 0;
		return 0;
	}
	temp = left * right;
	if (temp / right < left) {
		*result = 0;
		return 1;
	}
	*result = temp;

	return 0;
}

static int plussafe_size(size_t a, size_t b, size_t *result)
{
	if (a > SIZE_MAX - b)
		return 1;
	*result = a + b;

	return 0;
}

static void dimension_alloc(LocalRoot local, addr *ret, size_t index)
{
	if (multisafe_size(IdxSize, index, &index))
		fmte("Index overflow.", NULL);
	arraydimension_alloc(local, ret, index);
}

static void copy_dimension_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	size_t *data1;
	const size_t *data2;

	CheckType(pos, LISPSYSTEM_ARRAY_DIMENSION);
	arraydimension_alloc(local, &one, size);
	data1 = PtrArrayDimension(one);
	data2 = PtrArrayDimension(pos);
	memcpy(data1, data2, IdxSize * size);
	*ret = one;
}

static void array_empty_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct array_struct *str;

	arrayinfo_alloc(local, &pos);
	str = ArrayInfoStruct(pos);
	clearpoint(str);
	str->type = ARRAY_TYPE_EMPTY;
	*ret = pos;
}

static void settype_array(addr pos)
{
	struct array_struct *str;
	addr temp;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			GetCallType(&temp, Array_Bit);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetCallType(&temp, Array_Character);
			break;

		case ARRAY_TYPE_SIGNED:
			switch (str->bytesize) {
				case 8: GetCallType(&temp, Signed8); break;
				case 16: GetCallType(&temp, Signed16); break;
				case 32: GetCallType(&temp, Signed32); break;
#ifdef LISP_64BIT
				case 64: GetCallType(&temp, Signed64); break;
#endif
				default: GetCallType(&temp, Array_T); break;
			}
			break;

		case ARRAY_TYPE_UNSIGNED:
			switch (str->bytesize) {
				case 8: GetCallType(&temp, Unsigned8); break;
				case 16: GetCallType(&temp, Unsigned16); break;
				case 32: GetCallType(&temp, Unsigned32); break;
#ifdef LISP_64BIT
				case 64: GetCallType(&temp, Unsigned64); break;
#endif
				default: GetCallType(&temp, Array_T); break;
			}
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetCallType(&temp, Array_SingleFloat);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetCallType(&temp, Array_DoubleFloat);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetCallType(&temp, Array_LongFloat);
			break;

		default:
			GetCallType(&temp, Array_T);
			break;
	}
	SetArrayInfo(pos, ARRAY_INFO_TYPE, temp);
}

void array_alloc(LocalRoot local, addr *ret, size_t index, size_t size)
{
	addr pos, temp;
	struct array_struct *str;

	/* object */
	array_empty_alloc(local, &pos);
	str = ArrayInfoStruct(pos);

	/* dimension */
	if (2 <= index) {
		dimension_alloc(local, &temp, index);
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
	}
	str->dimension = index;
	str->size = str->front = str->refer = size;

	/* type */
	str->type = ARRAY_TYPE_T;
	settype_array(pos);

	/* result */
	*ret = pos;
}

void array_alloc_stdarg(LocalRoot local, addr *ret, ...)
{
	addr pos, dimension;
	size_t size, i, index, allcount, *data;
	va_list args, dest;

	va_start(args, ret);
	/* index */
	va_copy(dest, args);
	allcount = 1;
	for (index = 0; ; index++) {
		size = (size_t)va_arg(dest, unsigned);
		if (size == 0) break;
		if (multisafe_size(allcount, size, &allcount))
			fmte("size overflow.", NULL);
	}

	/* make */
	array_alloc(local, &pos, index, allcount);
	if (2 <= index) {
		GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &dimension);
		data = PtrArrayDimension(dimension);
		for (i = 0; i < index; i++)
			data[i] = (size_t)va_arg(args, unsigned);
	}
	va_end(args);

	/* result */
	*ret = pos;
}


/*  make-array
 *    element-type, initial-element, initial-contents,
 *    adjustable, fill-pointer, displaced-to, displaced-index-offset
 */
static void type_make_array(addr pos, addr type)
{
	enum ARRAY_TYPE value;
	int size;
	struct array_struct *str;

	size = 0;
	value = upgraded_array_direct(type, &size);
	str = ArrayInfoStruct(pos);
	str->type = value;
	str->bytesize = size;
	settype_array(pos);
}

static void element_make_array(addr pos)
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

static void size_make_array(addr pos, size_t *ret)
{
	struct array_struct *str;
	size_t dimension, i, size;
	const size_t *data;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_dimension_pointer(pos);
	size = 1;
	for (i = 0; i < dimension; i++) {
		if (multisafe_size(size, data[i], &size))
			fmte("size overflow.", NULL);
	}
	*ret = size;
}

static void dimension0_make_array(addr pos, size_t *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 0;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
	*ret = 1;
}

static void dimension1_make_array(addr pos, addr value, size_t *ret)
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

static void dimension2_make_array(LocalRoot local, addr pos, addr value, size_t *ret)
{
	struct array_struct *str;
	addr temp, index;
	size_t size, *data, i;

	str = ArrayInfoStruct(pos);
	size = length_list_safe(value);
	dimension_alloc(local, &temp, size);
	data = PtrArrayDimension(temp);
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
	size_make_array(pos, ret);
}

static void dimension_make_array(LocalRoot local, addr pos, addr value)
{
	struct array_struct *str;
	size_t size;

	if (value == Nil)
		dimension0_make_array(pos, &size);
	else if (integerp(value))
		dimension1_make_array(pos, value, &size);
	else if (consp(value))
		dimension2_make_array(local, pos, value, &size);
	else {
		fmte("Array index ~A must be an integer or list.", value, NULL);
		return;
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
}

static void allocate_element_t(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	arraygen_alloc(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

static void allocate_element_bit(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	bitmemory_unsafe(local, &array, str->size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

static void allocate_element_size(LocalRoot local, addr pos, struct array_struct *str)
{
	addr array;
	size_t size;

	if (multisafe_size(str->size , str->element, &size))
		fmte("size overflow.", NULL);
	arrayspec_alloc(local, &array, size);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, array);
}

static void allocate_element_array(LocalRoot local, addr pos, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			fmte("The array has no element size.", NULL);
			break;

		case ARRAY_TYPE_T:
			allocate_element_t(local, pos, str);
			break;

		case ARRAY_TYPE_BIT:
			allocate_element_bit(local, pos, str);
			break;

		default:
			allocate_element_size(local, pos, str);
			break;
	}
}

static int type_equal_make_array(struct array_struct *a,
		enum ARRAY_TYPE type, unsigned bytesize)
{
	if (a->type != type)
		return 0;
	if (a->type == ARRAY_TYPE_SIGNED || a->type == ARRAY_TYPE_UNSIGNED)
		return a->bytesize == bytesize;
	else
		return 1;
}

static void displaced_make_array(addr pos, addr displaced, addr offset)
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
	if (! type_equal_make_array(str1, str2->type, str2->bytesize))
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

static void fill_make_array(addr pos, addr fill)
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

static void extended_make_array(LocalRoot local, addr pos,
		addr fill, addr displaced, addr offset)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	/* displaced-to */
	if (str->displaced)
		displaced_make_array(pos, displaced, offset);
	else
		allocate_element_array(local, pos, str);
	/* fill-pointer */
	if (str->fillpointer)
		fill_make_array(pos, fill);
}

static void allocate_make_array(LocalRoot local, addr pos,
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
		extended_make_array(local, pos, fillpointer, displaced, offset);
	}
	else {
		str->adjustable = 0;
		str->fillpointer = 0;
		str->displaced = 0;
		str->simple = 1;
		allocate_element_array(local, pos, str);
	}
}

static void initial_t_make_array(addr pos, addr value, size_t size)
{
	size_t i;

	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	for (i = 0; i < size; i++)
		set_arraygen(pos, i, value);
}

static void initial_bit_make_array(addr pos, addr value, size_t size)
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

static void memset_make_array(addr pos, const void *src)
{
	struct array_struct *str;
	size_t size, elem, i;
	byte *dst;

	str = ArrayInfoStruct(pos);
	size = str->size;
	elem = str->element;
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	dst = (byte *)ptr_arraymemory(pos);
	for (i = 0; i < size; i++) {
		memcpy(dst, src, elem);
		dst += elem;
	}
}

static void initial_character_make_array(addr pos, addr value)
{
	unicode u;

	if (! characterp(value))
		fmte(":initial-element ~A must be character type.", value, NULL);
	GetCharacter(value, &u);
	memset_make_array(pos, (const void *)&u);
}

static void initial_signed8_make_array(addr pos, addr value)
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
	data = (void *)ptr_arraymemory(pos);
	memset(data, (byte)init, str->size);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 8).", value, NULL);
}

static void initial_signed16_make_array(addr pos, addr value)
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
	memset_make_array(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void initial_signed32_make_array(addr pos, addr value)
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
	memset_make_array(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
}

static void initial_signed64_make_array(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow :initial-element ~A in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	memset_make_array(pos, (const void *)&init);
}
#else
static void initial_signed32_make_array(addr pos, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow :initial-element ~A in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	memset_make_array(pos, (const void *)&init);
}
#endif

static void initial_signed_make_array(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			initial_signed8_make_array(pos, value);
			break;

		case 16:
			initial_signed16_make_array(pos, value);
			break;

		case 32:
			initial_signed32_make_array(pos, value);
			break;

#ifdef LISP_64BIT
		case 64:
			initial_signed64_make_array(pos, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void initial_unsigned8_make_array(addr pos, addr value)
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
	data = (byte *)ptr_arraymemory(pos);
	memset(data, (byte)init, str->size);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 8).", value, NULL);
}

static void initial_unsigned16_make_array(addr pos, addr value)
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
	memset_make_array(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void initial_unsigned32_make_array(addr pos, addr value)
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
	memset_make_array(pos, (const void *)&c);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}

static void initial_unsigned64_make_array(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		memset_make_array(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		memset_make_array(pos, (const void *)&bigv);
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 64).", value, NULL);
}
#else
static void initial_unsigned32_make_array(addr pos, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte(":initial-element ~A must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		memset_make_array(pos, (const void *)&init);
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		memset_make_array(pos, (const void *)&bigv);
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow :initial-element ~A in (unsigned-byte 32).", value, NULL);
}
#endif

static void initial_unsigned_make_array(addr pos, addr value)
{
	switch (ArrayInfoStruct(pos)->bytesize) {
		case 8:
			initial_unsigned8_make_array(pos, value);
			break;

		case 16:
			initial_unsigned16_make_array(pos, value);
			break;

		case 32:
			initial_unsigned32_make_array(pos, value);
			break;

#ifdef LISP_64BIT
		case 64:
			initial_unsigned64_make_array(pos, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void initial_single_float_make_array(addr pos, addr value)
{
	single_float v;

	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		fmte(":initial-element ~A must be single-float type.", value, NULL);
	GetSingleFloat(value, &v);
	memset_make_array(pos, (const void *)&v);
}

static void initial_double_float_make_array(addr pos, addr value)
{
	double_float v;

	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		fmte(":initial-element ~A must be double-float type.", value, NULL);
	GetDoubleFloat(value, &v);
	memset_make_array(pos, (const void *)&v);
}

static void initial_long_float_make_array(addr pos, addr value)
{
	long_float v;

	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		fmte(":initial-element ~A must be long-float type.", value, NULL);
	GetLongFloat(value, &v);
	memset_make_array(pos, (const void *)&v);
}

static void initial_element_make_array(addr pos, addr value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			initial_t_make_array(pos, value, str->size);
			break;

		case ARRAY_TYPE_BIT:
			initial_bit_make_array(pos, value, str->size);
			break;

		case ARRAY_TYPE_CHARACTER:
			initial_character_make_array(pos, value);
			break;

		case ARRAY_TYPE_SIGNED:
			initial_signed_make_array(pos, value);
			break;

		case ARRAY_TYPE_UNSIGNED:
			initial_unsigned_make_array(pos, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			initial_single_float_make_array(pos, value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			initial_double_float_make_array(pos, value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			initial_long_float_make_array(pos, value);
			break;

		default:
			fmte("Invalid array type.", NULL);
			break;
	}
}

static void setcharacter_arraymemory(addr pos, size_t index, addr value)
{
	if (! characterp(value))
		fmte("~S must be character type.", value, NULL);
	((unicode *)ptr_arraymemory(pos))[index] = RefCharacter(value);
}

static void setsigned8_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT8_MIN || INT8_MAX < init)
		goto error;
	((int8_t *)ptr_arraymemory(pos))[index] = (int8_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 8).", value, NULL);
}

static void setsigned16_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT16_MIN || INT16_MAX < init)
		goto error;
	((int16_t *)ptr_arraymemory(pos))[index] = (int16_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void setsigned32_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (init < INT32_MIN || INT32_MAX < init)
		goto error;
	((int32_t *)ptr_arraymemory(pos))[index] = (int32_t)init;
	return;

error:
	fmte("Overflow ~S in (signed-byte 32).", value, NULL);
}

static void setsigned64_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow ~S in (signed-byte 64).", value, NULL);
	GetFixnum(value, &init);
	((int64_t *)ptr_arraymemory(pos))[index] = (int64_t)init;
}
#else
static void setsigned32_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		fmte("Overflow ~S in (signed-byte 32).", value, NULL);
	GetFixnum(value, &init);
	((int32_t *)ptr_arraymemory(pos))[index] = (int32_t)init;
}
#endif

static void setsigned_arraymemory(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			setsigned8_arraymemory(pos, index, value);
			break;

		case 16:
			setsigned16_arraymemory(pos, index, value);
			break;

		case 32:
			setsigned32_arraymemory(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			setsigned64_arraymemory(pos, index, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void setunsigned8_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT8_MAX < init)
		goto error;
	((uint8_t *)ptr_arraymemory(pos))[index] = (uint8_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 8).", value, NULL);
}

static void setunsigned16_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT16_MAX < init)
		goto error;
	((uint16_t *)ptr_arraymemory(pos))[index] = (uint16_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 16).", value, NULL);
}

#ifdef LISP_64BIT
static void setunsigned32_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (! fixnump(value))
		goto error;
	GetFixnum(value, &init);
	if (UINT32_MAX < init)
		goto error;
	((uint32_t *)ptr_arraymemory(pos))[index] = (uint32_t)init;
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 32).", value, NULL);
}

static void setunsigned64_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		((uint64_t *)ptr_arraymemory(pos))[index] = (uint64_t)init;
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		((uint64_t *)ptr_arraymemory(pos))[index] = (uint64_t)bigv;
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 64).", value, NULL);
}
#else
static void setunsigned32_arraymemory(addr pos, size_t index, addr value)
{
	fixnum init;
	bigtype bigv;
	size_t size;

	if (! integerp(value))
		fmte("~S must be an integer type.", value, NULL);
	if (fixnump(value)) {
		GetFixnum(value, &init);
		if (init < 0) goto error;
		((uint32_t *)ptr_arraymemory(pos))[index] = (uint32_t)init;
		return;
	}
	if (bignump(value)) {
		if (minusp_bignum(value)) goto error;
		GetSizeBignum(value, &size);
		if (size != 1) goto error;
		getfixed_bignum(value, 0, &bigv);
		((uint32_t *)ptr_arraymemory(pos))[index] = (uint32_t)bigv;
		return;
	}
	TypeError(value, INTEGER);
	return;

error:
	fmte("Overflow ~S in (unsigned-byte 32).", value, NULL);
}
#endif

static void setunsigned_arraymemory(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			setunsigned8_arraymemory(pos, index, value);
			break;

		case 16:
			setunsigned16_arraymemory(pos, index, value);
			break;

		case 32:
			setunsigned32_arraymemory(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			setunsigned64_arraymemory(pos, index, value);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void set_single_float_arraymemory(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_SINGLE_FLOAT)
		fmte("~S must be single-float type.", value, NULL);
	((single_float *)ptr_arraymemory(pos))[index] = RefSingleFloat(value);
}

static void set_double_float_arraymemory(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_DOUBLE_FLOAT)
		fmte("~S must be double-float type.", value, NULL);
	((double_float *)ptr_arraymemory(pos))[index] = RefDoubleFloat(value);
}

static void set_long_float_arraymemory(addr pos, size_t index, addr value)
{
	if (GetType(value) != LISPTYPE_LONG_FLOAT)
		fmte("~S must be long-float type.", value, NULL);
	((long_float *)ptr_arraymemory(pos))[index] = RefLongFloat(value);
}

static void set_array_memory(addr pos, addr mem, size_t index, addr value)
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
			set_arraygen(mem, index, value);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_set(mem, index, value);
			break;

		case ARRAY_TYPE_CHARACTER:
			setcharacter_arraymemory(mem, index, value);
			break;

		case ARRAY_TYPE_SIGNED:
			setsigned_arraymemory(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_UNSIGNED:
			setunsigned_arraymemory(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			set_single_float_arraymemory(mem, index, value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			set_double_float_arraymemory(mem, index, value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			set_long_float_arraymemory(mem, index, value);
			break;

		default:
			fmte("(SETF AREF) Invalid array type.", NULL);
			break;
	}
}

static void set_index_array(addr pos, size_t index, addr value)
{
	addr mem;
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	set_array_memory(pos, mem, index, value);
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
static void recursive_contents_array(LocalRoot local, addr, addr,
		const size_t *data, size_t limit, size_t depth, size_t size);

static void list_contents_array(LocalRoot local, addr pos, addr list,
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
			recursive_contents_array(local, pos, next, data, limit, depth+1, size+i);
		}
		if (i < length)
			fmte("Too few :initial-contents list.", NULL);
	}
	else {
		set_index_array(pos, size, list);
	}
}

static void sequence_contents_array(LocalRoot local, addr pos, addr object,
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
			recursive_contents_array(local, pos, next, data, limit, depth+1, size+i);
		}
	}
	else {
		set_index_array(pos, size, object);
	}
}

static void object_contents_array(LocalRoot local, addr pos, addr list,
		size_t limit, size_t depth, size_t size)
{
	if (depth < limit)
		fmte("Too few :initial-contents.", NULL);
	else
		set_index_array(pos, size, list);
}

static void recursive_contents_array(LocalRoot local, addr pos,
		addr object, const size_t *data, size_t limit, size_t depth, size_t size)
{
	switch (GetType(object)) {
		case LISPTYPE_NIL:
		case LISPTYPE_CONS:
			list_contents_array(local, pos, object, data, limit, depth, size);
			break;

		case LISPTYPE_VECTOR:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
		case LISPTYPE_BITVECTOR:
			sequence_contents_array(local, pos, object, data, limit, depth, size);
			break;

		default:
			object_contents_array(local, pos, object, limit, depth, size);
			break;
	}
}

static void setf_contents_array(LocalRoot local, addr pos, addr contents)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	data = array_dimension_pointer(pos);
	recursive_contents_array(local, pos, contents, data, dimension, 0, 0);
}

static void contents_element_make_array(LocalRoot local, addr pos, addr contents)
{
	size_t dimension;

	dimension = ArrayInfoStruct(pos)->dimension;
	if (dimension == 0)
		set_index_array(pos, 0, contents);
	else
		setf_contents_array(local, pos, contents);
}

static void initial_make_array(LocalRoot local, addr pos, addr initial, addr contents)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->displaced && (initial != Unbound || contents != Unbound)) {
		fmte("Displaced array don't has "
				":initial-element or :initial-contents.", NULL);
	}
	if (initial != Unbound && contents != Unbound) {
		fmte("Array parameter cannot has both :initial-element and "
				":initial-contens parameter.", NULL);
	}
	if (initial != Unbound) {
		initial_element_make_array(pos, initial);
		return;
	}
	if (contents != Unbound) {
		contents_element_make_array(local, pos, contents);
		return;
	}
}

void array_make_array(LocalRoot local, addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	type_make_array(pos, type);
	element_make_array(pos);
	/* dimension */
	dimension_make_array(local, pos, dimension);
	/* allocate */
	allocate_make_array(local, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	initial_make_array(local, pos, initial, contents);
	/* result */
	*ret = pos;
}

static void dimension_array_contents(LocalRoot local,
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
		dimension_alloc(local, &temp, rank);
		data = PtrArrayDimension(temp);
		for (i = 0; i < rank; i++) {
			if (! consp(contents))
				fmte("Invalid initial-contents parameter ~S.", contents, NULL);
			data[i] = length_list_safe(contents);
			GetCar(contents, &contents);
		}
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
		size_make_array(pos, &size);
	}

	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
}

void array_contents_alloc(LocalRoot local, addr *ret, addr rank, addr contents)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	type_make_array(pos, T);
	element_make_array(pos);
	/* dimension */
	dimension_array_contents(local, pos, rank, contents);
	/* allocate */
	allocate_make_array(local, pos, Nil, Nil, Nil, Nil);
	/* initial value */
	initial_make_array(local, pos, Unbound, contents);
	/* result */
	*ret = pos;
}


/*
 *  adjust-array
 */
static size_t array_index_dimension(const size_t *data,
		const size_t *dimension, size_t depth)
{
	size_t i, value;

	/*  [1] n1
	 *  [2] s2*... + n2
	 *  [3] s3*... + n3
	 *  [4] s4*... + n4
	 */
	if (depth == 0)
		return 1;
	if (depth == 1)
		return data[0];

	value = data[0];
	for (i = 1; i < depth; i++) {
		if (multisafe_size(value, dimension[i], &value))
			fmte("Too large index value.", NULL);
		if (plussafe_size(value, data[i], &value))
			fmte("Too large index value.", NULL);
	}

	return value;
}

static void array_adjust_copyelement(addr pos, addr array, const size_t *data)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t index1, index2, depth;

	str1 = ArrayInfoStruct(pos);
	data1 = array_dimension_pointer(pos);
	data2 = array_dimension_pointer(array);
	depth = str1->dimension;
	index1 = array_index_dimension(data, data1, depth);
	index2 = array_index_dimension(data, data2, depth);
	array_setget(pos, index1, array, index2);
}

static void array_adjust_copydepth(addr pos, addr array, size_t *data, size_t depth)
{
	struct array_struct *str1;
	const size_t *data1, *data2;
	size_t size1, size2, i;

	str1 = ArrayInfoStruct(pos);
	if (depth < str1->dimension) {
		data1 = array_dimension_pointer(pos);
		data2 = array_dimension_pointer(array);
		size1 = data1[depth];
		size2 = data2[depth];
		for (i = 0; (i < size1) && (i < size2); i++) {
			data[depth] = i;
			array_adjust_copydepth(pos, array, data, depth + 1);
		}
	}
	else {
		array_adjust_copyelement(pos, array, data);
	}
}

static void array_adjust_copydata(addr pos, addr array)
{
	struct array_struct *str;
	size_t size, *data;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	str = ArrayInfoStruct(pos);
	size = str->dimension;
	push_local(local, &stack);
	data = (size_t *)lowlevel_local(local, IdxSize * size);
	memset(data, 0, IdxSize * size);
	array_adjust_copydepth(pos, array, data, 0);
	rollback_local(local, stack);
}

static void array_adjust_copy(addr pos, addr array,
		addr initial, addr contents, addr fillpointer,
		addr displaced, addr offset)
{
	addr adjustable;

	/* allocate */
	adjustable = ArrayInfoStruct(array)->adjustable? T: Nil;
	allocate_make_array(NULL, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	initial_make_array(NULL, pos, initial, contents);
	/* copy array */
	if (array != Nil && contents == Unbound)
		array_adjust_copydata(pos, array);
}

static void array_adjust_make(addr pos, addr array,
		addr initial, addr contents, addr fillpointer, addr displaced, addr offset)
{
	int check1, check2;
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	check1 = (! str->displaced);
	check2 = (displaced == Nil);
	if (check1 && check2) {
		/* not-displaced -> not-displaced */
		array_adjust_copy(pos, array,
				initial, contents, fillpointer, displaced, offset);
	}
	else if (check1) {
		/* not -> displaced */
		array_adjust_copy(pos, Nil,
				initial, contents, fillpointer, displaced, offset);
	}
	else if (check2) {
		/* displaced -> not */
		array_adjust_copy(pos, array,
				initial, contents, fillpointer, displaced, offset);
	}
	else {
		/* displaced -> displaced */
		array_adjust_copy(pos, Nil,
				initial, contents, fillpointer, displaced, offset);
	}
}

static void array_adjust_element_type(addr pos, addr type, addr array)
{
	enum ARRAY_TYPE value;
	int size;
	struct array_struct *str1, *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (type == Unbound) {
		str1->type = str2->type;
		str1->bytesize = str2->bytesize;
	}
	else {
		size = 0;
		value = upgraded_array_direct(type, &size);
		if (! type_equal_make_array(str2, value, size))
			fmte(":element-type ~S must be equal to base array.", type, NULL);
		str1->type = value;
		str1->bytesize = size;
	}
	settype_array(pos);
}

static void array_adjust_dimension(addr pos, addr array)
{
	struct array_struct *str1;
	struct array_struct *str2;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	if (str1->dimension != str2->dimension) {
		fmte("Array rank ~S must be equal to base array ~S.",
				intsizeh(str1->dimension),
				intsizeh(str2->dimension), NULL);
	}
}

static void array_adjust_replace(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	int i;
	addr temp;

	str1 = ArrayInfoStruct(array);
	str2 = ArrayInfoStruct(pos);
	*str1 = *str2;

	for (i = 0; i < ARRAY_INFO_SIZE; i++) {
		GetArrayInfo(pos, i, &temp);
		SetArrayInfo(pos, i, temp);
	}
}

void array_adjust_arraytype(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;

	CheckType(array, LISPTYPE_ARRAY);
	/* object */
	array_empty_alloc(NULL, &pos);
	/* element-type */
	array_adjust_element_type(pos, type, array);
	element_make_array(pos);
	/* dimension */
	dimension_make_array(NULL, pos, dimension);
	/* check */
	array_adjust_dimension(pos, array);
	/* make-adjust */
	array_adjust_make(pos, array, initial, contents, fillpointer, displaced, offset);
	/* result */
	if (ArrayInfoStruct(array)->adjustable) {
		array_adjust_replace(pos, array);
		*ret = array;
	}
	else {
		*ret = pos;
	}
}

static void array_adjust_vectorcheck(addr pos, size_t *ret)
{
	if (pos == Nil)
		fmte("Array rank must be a 1, but 0.", NULL);
	if (singlep(pos))
		GetCar(pos, &pos);
	if (integerp(pos)) {
		if (getindex_integer(pos, ret))
			fmte("Dimension ~A is too large.", pos, NULL);
		return;
	}
	if (consp(pos))
		fmte("Array rank must be a 1.", NULL);
	else
		fmte("Invalid pos type ~S.", pos, NULL);
}

static int array_adjust_simplecheck(addr pos, addr fillpointer, addr displaced)
{
	if (fillpointer != Nil || displaced != Nil) return 0;
	if (! arrayp(pos)) return 1;
	return ! ArrayInfoStruct(pos)->adjustable;
}

static void array_adjust_vector_type(addr pos, addr type, enum ARRAY_TYPE check)
{
	enum ARRAY_TYPE value;
	int size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type == Unbound) {
		str->type = check;
		str->bytesize = 0;
	}
	else {
		size = 0;
		value = upgraded_array_direct(type, &size);
		if (check != value)
			fmte(":element-type ~S must be equal to base array.", type, NULL);
		str->type = value;
		str->bytesize = 0;
	}
	settype_array(pos);
}

static void array_adjust_copy_vector(addr pos, addr array)
{
	struct array_struct *str;
	addr temp;
	size_t size1, size2, i;

	str = ArrayInfoStruct(pos);
	size1 = str->size;
	size2 = length_sequence(array, 0);
	for (i = 0; (i < size1) && (i < size2); i++) {
		getelt_sequence(NULL, array, i, &temp);
		setelt_sequence(pos, i, temp);
	}
}

static void array_adjust_array_vector(addr *ret, addr array, addr dimension,
		addr type, enum ARRAY_TYPE check,
		addr initial, addr contents, addr fillpointer, addr displaced, addr offset)
{
	addr pos, adjustable;

	/* object */
	array_empty_alloc(NULL, &pos);
	/* element-type */
	array_adjust_vector_type(pos, type, check);
	element_make_array(pos);
	/* dimension */
	dimension_make_array(NULL, pos, dimension);
	/* allocate */
	adjustable = ArrayInfoStruct(pos)->adjustable? T: Nil;
	allocate_make_array(NULL, pos, adjustable, fillpointer, displaced, offset);
	/* initial value */
	initial_make_array(NULL, pos, initial, Unbound);
	/* copy-array */
	array_adjust_copy_vector(pos, array);
	/* result */
	*ret = pos;
}

static void array_adjust_vector_contents_list(addr pos, size_t size, addr contents)
{
	addr list, value;
	size_t i;

	list = contents;
	for (i = 0; list != Nil; i++) {
		if (size <= i) {
			fmte("The length of :INITIAL-CONTENTS ~S "
					"must be a new-array size ~S.", contents, intsizeh(size), NULL);
		}
		getcons(list, &value, &list);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_vector_contents_vector(addr pos, size_t size, addr seq)
{
	addr value;
	size_t check, i;

	check = length_sequence(seq, 0);
	if (size <= check) {
		fmte("The length of :INITIAL-CONTENTS ~S "
				"must be a new-array size ~S.", seq, intsizeh(size), NULL);
	}
	for (i = 0; i < size; i++) {
		getelt_sequence(NULL, seq, i, &value);
		setelt_sequence(pos, i, value);
	}
}

static void array_adjust_vector_contents(addr pos, size_t size, addr contents)
{
	if (contents != Unbound) {
		if (listp(contents))
			array_adjust_vector_contents_list(pos, size, contents);
		else
			array_adjust_vector_contents_vector(pos, size, contents);
	}
}

void array_adjust_vector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		vector_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_T,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

void array_adjust_string(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		strvect_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_CHARACTER,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

void array_adjust_bitvector(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	addr pos;
	size_t size;

	array_adjust_vectorcheck(dimension, &size);
	if (array_adjust_simplecheck(array, fillpointer, displaced)) {
		bitmemory_adjust(&pos, array, size, initial, contents);
	}
	else {
		array_adjust_array_vector(&pos, array, dimension, type, ARRAY_TYPE_BIT,
				initial, contents, fillpointer, displaced, offset);
	}
	array_adjust_vector_contents(pos, size, contents);
	*ret = pos;
}

void array_adjust_array(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset)
{
	switch (GetType(array)) {
		case LISPTYPE_ARRAY:
			array_adjust_arraytype(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_VECTOR:
			array_adjust_vector(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_STRING:
			array_adjust_string(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		case LISPTYPE_BITVECTOR:
			array_adjust_bitvector(ret, array, dimension,
					type, initial, contents, fillpointer, displaced, offset);
			break;

		default:
			TypeError(array, ARRAY);
			break;
	}
}


/*
 *  arary_copy
 */
static void array_struct_copy(addr pos, addr array)
{
	struct array_struct *str1, *str2;
	addr temp;

	/* array */
	GetArrayInfo(array, ARRAY_INFO_TYPE, &temp);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, temp);
	GetArrayInfo(array, ARRAY_INFO_DISPLACED, &temp);
	SetArrayInfo(pos, ARRAY_INFO_DISPLACED, temp);

	/* body */
	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(array);
	*str1 = *str2;
}

static void array_dimension_copy(LocalRoot local, addr pos, addr array)
{
	addr temp;
	size_t size;

	GetArrayInfo(array, ARRAY_INFO_DIMENSION, &temp);
	if (temp != Nil) {
		size = ArrayInfoStruct(array)->dimension;
		copy_dimension_alloc(local, &temp, temp, size);
	}
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
}

static void copy_array_general(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one, temp;
	size_t i;

	arraygen_alloc(local, &one, size);
	for (i = 0; i < size; i++) {
		get_arraygen(pos, i, &temp);
		set_arraygen(one, i, temp);
	}
	*ret = one;
}

static void copy_array_specialized(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr one;
	void *data1;
	const void *data2;

	arrayspec_alloc(local, &one, size);
	data1 = (void *)ptr_arraymemory(one);
	data2 = (const void *)ptr_arraymemory(pos);
	memcpy(data1, data2, size);
}

static void array_memory_copy(LocalRoot local, addr pos, addr array)
{
	addr mem;
	struct array_struct *str;

	GetArrayInfo(array, ARRAY_INFO_MEMORY, &mem);
	switch (GetType(mem)) {
		case LISPTYPE_BITVECTOR:
			bitmemory_copy_alloc(local, &mem, mem);
			break;

		case LISPSYSTEM_ARRAY_GENERAL:
			str = ArrayInfoStruct(array);
			copy_array_general(local, &mem, mem, str->size);
			break;

		case LISPSYSTEM_ARRAY_SPECIALIZED:
			str = ArrayInfoStruct(array);
			copy_array_specialized(local, &mem, mem, str->size * str->element);
			break;

		default:
			break;
	}
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, mem);
}

void array_copy_alloc(LocalRoot local, addr *ret, addr array)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	array_struct_copy(pos, array);
	/* dimension */
	array_dimension_copy(local, pos, array);
	/* allocate */
	array_memory_copy(local, pos, array);
	/* result */
	*ret = pos;
}

void array_copy_local(LocalRoot local, addr *ret, addr array)
{
	Check(local == NULL, "local error");
	array_copy_alloc(local, ret, array);
}

void array_copy_heap(addr *ret, addr array)
{
	array_copy_alloc(NULL, ret, array);
}


/*
 *  array function
 */
int arrayp(addr pos)
{
	return GetType(pos) == LISPTYPE_ARRAY;
}

int array_simple_p(addr pos)
{
	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	return ArrayInfoStruct(pos)->simple;
}

int array_vector_p(addr pos)
{
	return GetType(pos) == LISPTYPE_ARRAY &&
		ArrayInfoStruct(pos)->dimension == 1;
}

int array_size_vector_p(addr pos, size_t size)
{
	struct array_struct *str;

	if (GetType(pos) != LISPTYPE_ARRAY) return 0;
	str = ArrayInfoStruct(pos);
	if (str->dimension != 1) return 0;

	return ArrayInfoStruct(pos)->size == size;
}

int array_general_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type == ARRAY_TYPE_T;
}

int array_specialized_p(addr pos)
{
	struct array_struct *str;

	if (! arrayp(pos))
		return 0;
	str = ArrayInfoStruct(pos);

	return str->type != ARRAY_TYPE_T;
}

const size_t *array_dimension_pointer(addr pos)
{
	struct array_struct *str;

	Check(GetType(pos) != LISPTYPE_ARRAY, "type error");
	str = ArrayInfoStruct(pos);
	switch (str->dimension) {
		case 0:
			return NULL;

		case 1:
			return (const size_t *)&(str->size);

		default:
			GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &pos);
			return PtrArrayDimension(pos);
	}
}

void *array_write_pointer(addr pos, size_t index)
{
	enum ARRAY_TYPE type;
	struct array_struct *str;
	size_t size;

	str = ArrayInfoStruct(pos);
	type = str->type;
	size = str->size;
	if (type == ARRAY_TYPE_EMPTY)
		fmte("The array has no memory yet.", NULL);
	if (type == ARRAY_TYPE_T || type == ARRAY_TYPE_BIT)
		fmte("The object is not specialized array.", NULL);
	if (size <= index)
		fmte("Index is too large.", NULL);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);

	return (void *)(ptr_arraymemory(pos) + (index * str->element));
}

const void *array_read_pointer(addr pos, size_t index)
{
	return (const void *)array_write_pointer(pos, index);
}

static void set_simple_array(struct array_struct *str)
{
	str->simple = (!str->adjustable) && (!str->fillpointer) && (!str->displaced);
}

static void set_element_size(addr pos, struct array_struct *str)
{
	addr type;

	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	type_make_array(pos, type);
	element_make_array(pos);
}

static void check_fillpointer(addr pos, struct array_struct *str)
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

void allocate_array_alloc(LocalRoot local, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	set_simple_array(str);
	set_element_size(pos, str);
	check_fillpointer(pos, str);
	allocate_element_array(local, pos, str);
}


/*
 *  array control
 */
void array_element_type(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &pos);
	type_object(NULL, ret, pos);
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
	data1 = array_dimension_pointer(a);
	data2 = array_dimension_pointer(b);
	for (i = 0; i < size; i++) {
		if (data1[i] != data2[i])
			return 0;
	}

	return 1;
}


/*
 *  getmemory
 */
static void array_memory_index(addr pos, size_t index, addr *retp, size_t *rets)
{
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, retp);
	*rets = index;
}

static void array_memory(addr pos, size_t index, addr *retp, size_t *rets)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->size <= index)
		fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
	if (! str->displaced) {
		array_memory_index(pos, index, retp, rets);
		return;
	}
	if (str->refer <= index) {
		array_memory_index(pos, index - str->refer, retp, rets);
	}
	else {
		GetArrayInfo(pos, ARRAY_INFO_DISPLACED, &pos);
		index += str->offset;
		array_memory(pos, index, retp, rets);
	}
}


/*
 *  memory control
 */
static void getunicode_arraymemory(addr pos, size_t index, unicode *ret)
{
	*ret = ((unicode *)ptr_arraymemory(pos))[index];
}

static void getcharacter_arraymemory(LocalRoot local, addr pos, size_t index, addr *ret)
{
	unicode u;
	getunicode_arraymemory(pos, index, &u);
	character_alloc(local, ret, u);
}

static void getsigned8_arraymemory(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int8_t *)ptr_arraymemory(pos))[index]);
}

static void getsigned16_arraymemory(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int16_t *)ptr_arraymemory(pos))[index]);
}

static void getsigned32_arraymemory(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int32_t *)ptr_arraymemory(pos))[index]);
}

#ifdef LISP_64BIT
static void getsigned64_arraymemory(LocalRoot local, addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((int64_t *)ptr_arraymemory(pos))[index]);
}
#endif

static void getsigned_arraymemory(LocalRoot local,
		unsigned bytesize, addr pos, size_t index, addr *ret)
{
	switch (bytesize) {
		case 8:
			getsigned8_arraymemory(local, pos, index, ret);
			break;

		case 16:
			getsigned16_arraymemory(local, pos, index, ret);
			break;

		case 32:
			getsigned32_arraymemory(local, pos, index, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			getsigned64_arraymemory(local, pos, index, ret);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void getunsigned8_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, (fixnum)((uint8_t *)ptr_arraymemory(pos))[index]);
}

static void getunsigned16_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, (fixnum)((uint16_t *)ptr_arraymemory(pos))[index]);
}

#ifdef LISP_64BIT
static void getunsigned32_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	fixnum_alloc(local, ret, ((uint32_t *)ptr_arraymemory(pos))[index]);
}

static void getunsigned64_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	bigtype value;

	value = ((bigtype *)ptr_arraymemory(pos))[index];
	integer_fixed_alloc(local, ret, signplus_bignum, value);
}
#else
static void getunsigned32_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	bigtype value;

	value = ((bigtype *)ptr_arraymemory(pos))[index];
	integer_fixed_alloc(local, ret, signplus_bignum, value);
}
#endif

static void getunsigned_arraymemory(LocalRoot local,
		unsigned bytesize, addr pos, size_t index, addr *ret)
{
	switch (bytesize) {
		case 8:
			getunsigned8_arraymemory(local, pos, index, ret);
			break;

		case 16:
			getunsigned16_arraymemory(local, pos, index, ret);
			break;

		case 32:
			getunsigned32_arraymemory(local, pos, index, ret);
			break;

#ifdef LISP_64BIT
		case 64:
			getunsigned64_arraymemory(local, pos, index, ret);
			break;
#endif
		default:
			fmte("Invalid array size.", NULL);
			break;
	}
}

static void get_single_float_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	single_float_alloc(local, ret, ((single_float *)ptr_arraymemory(pos))[index]);
}

static void get_double_float_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	double_float_alloc(local, ret, ((double_float *)ptr_arraymemory(pos))[index]);
}

static void get_lonf_float_arraymemory(LocalRoot local,
		addr pos, size_t index, addr *ret)
{
	long_float_alloc(local, ret, ((long_float *)ptr_arraymemory(pos))[index]);
}

static void aref_index_array(LocalRoot local,
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
			get_arraygen(mem, index, ret);
			break;

		case ARRAY_TYPE_BIT:
			bitmemory_get(local, mem, index, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			getcharacter_arraymemory(local, mem, index, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			getsigned_arraymemory(local, bytesize, mem, index, ret);
			break;

		case ARRAY_TYPE_UNSIGNED:
			getunsigned_arraymemory(local, bytesize, mem, index, ret);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			get_single_float_arraymemory(local, mem, index, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			get_double_float_arraymemory(local, mem, index, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			get_lonf_float_arraymemory(local, mem, index, ret);
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
	array_memory(pos, index, &pos, &index);
	get_arraygen(pos, index, ret);

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
	array_memory(pos, index, &pos, &index);
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
	array_memory(pos, index, &pos, &index);
	getunicode_arraymemory(pos, index, ret);

	return 0;
}

void array_get(LocalRoot local, addr pos, size_t index, addr *ret)
{
	addr mem;
	array_memory(pos, index, &mem, &index);
	aref_index_array(local, pos, mem, index, ret);
}

static void setunicode_arraymemory(addr pos, size_t index, unicode value)
{
	((unicode *)ptr_arraymemory(pos))[index] = value;
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
	array_memory(pos, index, &pos, &index);
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
	array_memory(pos, index, &pos, &index);
	setunicode_arraymemory(pos, index, value);

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
	array_memory(pos, index, &mem, &index);
	set_array_memory(pos, mem, index, value);
}

static void array_setget_size(addr m1, size_t s1, addr m2, size_t s2, size_t e)
{
	byte *data1;
	const byte *data2;

	data1 = (byte *)ptr_arraymemory(m1);
	data2 = (const byte *)ptr_arraymemory(m2);
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
			get_arraygen(m2, s2, &temp);
			set_arraygen(m1, s1, temp);
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

	if (! type_equal_make_array(str1, str2->type, str2->bytesize))
		fmte("Array ~S type must be equal to base array ~S.", p1, p2, NULL);
	array_memory(p1, s1, &m1, &s1);
	array_memory(p2, s2, &m2, &s2);
	array_setget_type(p1, m1, s1, m2, s2);
}

static void get_inplace_array(addr pos,
		size_t index, size_t element, struct array_value *str)
{
	int value;
	const byte *data;

	switch (str->type) {
		case ARRAY_TYPE_T:
			get_arraygen(pos, index, &(str->value.object));
			return;

		case ARRAY_TYPE_BIT:
			bitmemory_getint(pos, index, &value);
			str->value.bit = value? 1: 0;
			return;

		default:
			break;
	}

	/* memory */
	data = (const byte *)ptr_arraymemory(pos);
	data += element * index;
	memcpy(&(str->value), data, element);
}

void array_get_inplace(addr pos, size_t index, struct array_value *value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	value->type = str->type;
	value->size = str->bytesize;
	array_memory(pos, index, &pos, &index);
	get_inplace_array(pos, index, str->element, value);
}

static void set_inplace_array(addr pos,
		size_t index, size_t element, const struct array_value *str)
{
	byte *data;

	switch (str->type) {
		case ARRAY_TYPE_T:
			set_arraygen(pos, index, str->value.object);
			return;

		case ARRAY_TYPE_BIT:
			bitmemory_setint(pos, index, str->value.bit);
			return;

		default:
			break;
	}

	/* memory */
	data = (byte *)ptr_arraymemory(pos);
	data += element * index;
	memcpy(data, &(str->value), element);
}

int array_set_inplace(addr pos, size_t index, const struct array_value *value)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (! type_equal_make_array(str, value->type, value->size))
		return 1;
	array_memory(pos, index, &pos, &index);
	set_inplace_array(pos, index, str->element, value);

	return 0;
}


/*
 *  array
 */
static size_t aref_list_index_vector(addr pos, addr args, struct array_struct *str)
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

static size_t aref_list_index(addr pos, addr args)
{
	struct array_struct *str;
	const size_t *data;
	size_t index, value, depth, dimension;
	addr check, list;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	if (dimension == 1)
		return aref_list_index_vector(pos, args, str);
	data = array_dimension_pointer(pos);
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
	size_t index = aref_list_index(pos, args);
	array_get(local, pos, index, ret);
}

void array_setf_aref(addr pos, addr args, addr value)
{
	size_t index = aref_list_index(pos, args);
	array_set(pos, index, value);
}

void array_aref_bit(LocalRoot local, addr pos, addr args, addr *ret)
{
	int value;
	size_t index;

	index = aref_list_index(pos, args);
	if (array_get_bit(pos, index, &value))
		fmte("Array must be a bit type.", NULL);
	fixnum_alloc(local, ret, (fixnum)value);
}

void array_setf_aref_bit(addr pos, addr args, addr value)
{
	int check;
	size_t index;

	index = aref_list_index(pos, args);
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
	index = (array_dimension_pointer(array))[index];
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
	data = array_dimension_pointer(array);
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
	data = array_dimension_pointer(array);
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
	size_t value = aref_list_index(array, rest);
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

int array_simple_vector_p(addr array)
{
	struct array_struct *str;

	CheckType(array, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(array);

	return str->simple && str->dimension == 1;
}

void array_vector_pop(LocalRoot local, addr pos, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer)
		type_error_fill_pointer(pos);
	if (str->front == 0)
		type_error_fill_pointer_zero(pos);
	str->front--;
	array_get(local, pos, str->front, ret);
}

void array_vector_push(LocalRoot local, addr pos, addr value, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer)
		type_error_fill_pointer(pos);
	if (str->size <= str->front) {
		*ret = Nil;
	}
	else {
		array_set(pos, str->front, value);
		make_index_integer_alloc(local, ret, str->front);
		str->front++;
	}
}

static void array_resize_t(LocalRoot local, addr pos, size_t prev, size_t next)
{
	addr dst, src, temp;
	size_t i;

	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &src);
	arraygen_alloc(local, &dst, next);
	if (src != Nil) {
		Check(lenarrayr(src) != prev, "size error");
		next = (next < prev)? next: prev;
		for (i = 0; i < next; i++) {
			get_arraygen(src, i, &temp);
			set_arraygen(dst, i, temp);
		}
	}
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, dst);
}

static void array_resize_bit(LocalRoot local, addr pos, size_t prev, size_t next)
{
	addr src, dst;

	bitmemory_unsafe(local, &dst, next);
#ifdef LISP_DEBUG
	bitmemory_memset(dst, 1);
#endif
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &src);
	bitmemory_copy_unsafe(dst, src, (prev < next)? prev: next);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, dst);
}

static void array_resize_size(LocalRoot local, addr pos,
		size_t prev, size_t next, unsigned element)
{
	addr src, dst;
	size_t size;
	byte *data1, *data2;

	prev *= element;
	next *= element;
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &src);
	arrayspec_alloc(local, &dst, next);
	if (src != Nil) {
		Check(lenbodyr(src) != prev, "size error");
		data1 = (byte *)ptr_arraymemory(dst);
		data2 = (byte *)ptr_arraymemory(src);
		size = (next < prev)? next: prev;
#ifdef LISP_DEBUG
		next = (next > prev)? next: prev;
		memcpy(data1, data2, size);
		memset(data1 + size, 0xAA, next - size);
#else
		memcpy(data1, data2, size);
#endif
		memcpy(data1, data2, next);
	}
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, dst);
}

static void array_extend_resize(LocalRoot local, addr pos,
		size_t prev, size_t next, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			fmte("The array has no element size.", NULL);
			break;

		case ARRAY_TYPE_T:
			array_resize_t(local, pos, prev, next);
			break;

		case ARRAY_TYPE_BIT:
			array_resize_bit(local, pos, prev, next);
			break;

		default:
			array_resize_size(local, pos, prev, next, str->element);
			break;
	}
}

static void array_extend_displaced(LocalRoot local, addr pos, addr extension)
{
	struct array_struct *str;
	size_t diff, size, resize;

	/* argument */
	str = ArrayInfoStruct(pos);
	Check(str->size < str->refer, "reference size error");
	diff = str->size - str->refer;
	if (extension == Unbound) {
		size = diff;
	}
	else {
		if (getindex_integer(extension, &size))
			fmte("Invalid extension value ~S.", extension, NULL);
	}
	size += diff;
	if (size < 16) size = 16;
	resize = str->refer + size;
	/* allocate */
	array_extend_resize(local, pos, diff, size, str);
	/* size */
	str->size = resize;
}

static void array_extend_normal(LocalRoot local, addr pos, addr extension)
{
	struct array_struct *str;
	size_t size;

	/* argument */
	str = ArrayInfoStruct(pos);
	if (extension == Unbound) {
		size = str->size;
	}
	else {
		if (getindex_integer(extension, &size))
			fmte("Invalid extension value ~S.", extension, NULL);
	}
	size += str->size;
	if (size < 16) size = 16;
	/* allocate */
	array_extend_resize(local, pos, str->size, size, str);
	/* size */
	str->size = size;
}

void array_vector_push_extend(LocalRoot local,
		addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer)
		type_error_fill_pointer(pos);
	if (! str->adjustable)
		type_error_adjustable(pos);
	if (str->displaced)
		array_extend_displaced(local, pos, extension);
	else
		array_extend_normal(local, pos, extension);
	array_set(pos, str->front, value);
	make_index_integer_alloc(local, ret, str->front);
	str->front++;
}


/*
 *  vector
 */
void vector_get(addr pos, size_t index, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(size), NULL);
	getarray(pos, index, ret);
}

void vector_aref(addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (! consp(args))
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	if (! integerp(arg))
		fmte("AREF argument ~S must be a non-negative integer.", arg, NULL);
	if (getindex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	vector_get(pos, index, ret);
}

void vector_set(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(size), NULL);
	setarray(pos, index, value);
}

void vector_setf_aref(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (GetStatusReadOnly(pos))
		fmte("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		fmte("AREF argument ~S must be (integer) form.", args, NULL);
	if (! integerp(arg))
		fmte("AREF argument ~S must be a non-negative integer.", arg, NULL);
	if (getindex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	vector_set(pos, index, value);
}

void vector_array_dimension(addr pos, addr arg, size_t size, addr *ret)
{
	size_t check;

	if (! integerp(arg))
		fmte("ARRAY-DIMENSION argument ~S must be integer type.", arg, NULL);
	if (getindex_integer(arg, &check))
		fmte("Invalid index arg ~S.", arg, NULL);
	if (check != 0)
		fmte("Array rank ~A must be less than equal to 1.", arg, NULL);
	make_index_integer_alloc(NULL, ret, size);
}

void vector_array_dimensions(size_t size, addr *ret)
{
	addr pos;
	make_index_integer_alloc(NULL, &pos, size);
	conscar_heap(ret, pos);
}

int vector_array_in_bounds_p(addr rest, size_t size)
{
	addr pos;
	size_t check;

	if (! consp(rest))
		fmte("The subscripts ~S is too few argumens.", rest, NULL);
	GetCons(rest, &pos, &rest);
	if (rest != Nil)
		fmte("The subscripts ~S is too many argumens.", rest, NULL);
	if (! integerp(pos))
		fmte("The subscript ~S must be integer type.", pos, NULL);
	if (getindex_integer(pos, &check))
		return 0;

	return check < size;
}

void vector_array_row_major_index(addr rest, size_t size, addr *ret)
{
	if (! vector_array_in_bounds_p(rest, size))
		fmte("Out of range ~S.", intsizeh(size), NULL);
	GetCar(rest, ret);
}

static void typeset_make_vector(addr pos, enum ARRAY_TYPE type, int size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	settype_array(pos);
	element_make_array(pos);
}

static void dimension_make_vector(LocalRoot local, addr pos, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = str->refer = size;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, Nil);
}

static void type_make_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, int bytesize, addr value)
{
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	typeset_make_vector(pos, type, bytesize);
	/* dimension */
	dimension_make_vector(local, pos, size);
	/* allocate */
	allocate_make_array(local, pos, Nil, Nil, Nil, Nil);
	/* initial value */
	initial_make_array(local, pos, value, Unbound);
	/* result */
	*ret = pos;
}

static void signed_make_vector_check(enum ARRAY_TYPE type, int bytesize)
{
	switch (type) {
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			break;

		default:
			fmte("Invalid vector type.", NULL);
			break;
	}

	switch (bytesize) {
		case 8:
		case 16:
		case 32:
#ifdef LISP_64BIT
		case 64:
#endif
			break;

		default:
			fmte("Invalide vector type (size).", NULL);
			break;
	}
}

void signed_make_vector_uninitialize(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, int bytesize)
{
	signed_make_vector_check(type, bytesize);
	type_make_vector(local, ret, size, type, bytesize, Unbound);
}

void signed_make_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, int bytesize, addr value)
{
	signed_make_vector_check(type, bytesize);
	if (value == Unbound)
		fixnum_alloc(local, &value, 0);
	type_make_vector(local, ret, size, type, bytesize, value);
}

void float_make_vector_uninitialize(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			break;

		default:
			fmte("Invalid vector type.", NULL);
			break;
	}
	type_make_vector(local, ret, size, type, 0, Unbound);
}

void float_make_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, addr value)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
			if (value == Unbound)
				single_float_alloc(local, &value, 0.0f);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			if (value == Unbound)
				double_float_alloc(local, &value, 0.0);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			if (value == Unbound)
				long_float_alloc(local, &value, 0.0L);
			break;

		default:
			fmte("Invalid vector type.", NULL);
			break;
	}
	type_make_vector(local, ret, size, type, 0, value);
}

void setelt_vector(addr pos, size_t index, addr value)
{
	size_t size;

	lenarray(pos, &size);
	if (size <= index) {
		fmte("Index ~A must be less than vector size ~A.",
				intsizeh(index), intsizeh(size), NULL);
	}
	setarray(pos, index, value);
}

void vector_adjust(addr *ret, addr array, size_t size, addr value, addr check)
{
	addr pos, temp;
	size_t i, arraysize;

	vector_heap(&pos, size);
	if (check == Unbound) {
		lenarray(array, &arraysize);
		for (i = 0; i < size; i++) {
			if (i < arraysize)
				getarray(array, i, &temp);
			else {
				if (value == Unbound) break;
				temp = value;
			}
			setarray(pos, i, temp);
		}
	}
	*ret = pos;
}

void vector_reverse(LocalRoot local, addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	lenarray(pos, &size);
	vector_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		getarray(pos, x, &temp);
		setarray(one, y, temp);
	}
	*ret = one;
}

void vector_nreverse(addr *ret, addr pos)
{
	addr a, b;
	size_t size, x, y;

	lenarray(pos, &size);
	if (size <= 1) return;
	x = 0;
	y = size - 1;
	while (x < y) {
		getarray(pos, x, &a);
		getarray(pos, y, &b);
		setarray(pos, x, b);
		setarray(pos, y, a);
		x++;
		y--;
	}
	*ret = pos;
}


/*
 *  others
 */
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
			switch (str->size) {
				case 8:
					int8_integer_alloc(local, ret, str->value.signed8); break;
				case 16:
					int16_integer_alloc(local, ret, str->value.signed16); break;
				case 32:
					int32_integer_alloc(local, ret, str->value.signed32); break;
#ifdef LISP_64BIT
				case 64:
					int64_integer_alloc(local, ret, str->value.signed64); break;
#endif
				default:
					fmte("size error"); break;
			}
			break;

		case ARRAY_TYPE_UNSIGNED:
			switch (str->size) {
				case 8:
					uint8_integer_alloc(local, ret, str->value.unsigned8); break;
				case 16:
					uint16_integer_alloc(local, ret, str->value.unsigned16); break;
				case 32:
					uint32_integer_alloc(local, ret, str->value.unsigned32); break;
#ifdef LISP_64BIT
				case 64:
					uint64_integer_alloc(local, ret, str->value.unsigned64); break;
#endif
				default:
					fmte("size error"); break;
			}
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

static void struct_bitcalc_make(addr pos, addr src)
{
	struct array_struct *str1;
	const struct array_struct *str2;
	addr type;

	str1 = ArrayInfoStruct(pos);
	str2 = ArrayInfoStruct(src);
	str1->simple = 1;
	str1->adjustable = 0;
	str1->fillpointer = 0;
	str1->displaced = 0;
	str1->type = str2->type;
	str1->element = str2->element;
	str1->bytesize = str2->bytesize;
	str1->size = str2->front;
	str1->front = str2->front;
	str1->dimension = str2->dimension;
	str1->offset = 0;
	str1->refer = str2->front;
	GetArrayInfo(src, ARRAY_INFO_TYPE, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
}

static void dimension_bitcalc_make(addr pos, addr src)
{
	struct array_struct *str;
	const size_t *data2;
	addr temp;
	size_t size, i, *data1;

	str = ArrayInfoStruct(src);
	data2 = array_dimension_pointer(src);
	size = str->dimension;
	if (2 <= size) {
		dimension_alloc(NULL, &temp, size);
		data1 = PtrArrayDimension(temp);
		for (i = 0; i < size; i++)
			data1[i] = data2[i];
		SetArrayInfo(pos, ARRAY_INFO_DIMENSION, temp);
	}
}

static void array_bitcalc_make(addr src, addr *ret)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_alloc(NULL, &pos);
	/* element-type */
	struct_bitcalc_make(pos, src);
	/* dimension */
	dimension_bitcalc_make(pos, src);
	/* allocate */
	str = ArrayInfoStruct(pos);
	allocate_element_bit(NULL, pos, str);
	/* result */
	*ret = pos;
}

static int bitvector_type(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_BITVECTOR:
			return 0;

		case LISPTYPE_ARRAY:
			return 1;

		default:
			fmte("Argument ~S must be a bit-array type.", pos, NULL);
			return -1;
	}
}

static int bitvector_dimension_equal(addr pos1, addr pos2)
{
	int check1, check2;

	check1 = bitvector_type(pos1);
	check2 = bitvector_type(pos2);
	if (check1 && check2) {
		return array_dimension_equal(pos1, pos2);
	}
	if (check1) {
		if (ArrayInfoStruct(pos1)->dimension != 1)
			return 0;
		GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
		return bitmemory_equal_length(pos1, pos2);
	}
	if (check2) {
		if (ArrayInfoStruct(pos2)->dimension != 1)
			return 0;
		GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
		return bitmemory_equal_length(pos1, pos2);
	}
	else {
		return bitmemory_equal_length(pos1, pos2);
	}
}

static void array_bitcalc_aa(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;

	if (! bitvector_dimension_equal(pos1, pos2))
		fmte("Dimension don't match ~S and ~S.", pos1, pos2, NULL);
	if (opt == Nil) {
		array_bitcalc_make(pos1, &opt);
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos1;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! bitvector_dimension_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos1, NULL);
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos2, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
	GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_ab(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;
	size_t size;

	if (! bitvector_dimension_equal(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos2, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = pos1;
		GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! bitvector_dimension_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos1);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos1, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos1, ARRAY_INFO_MEMORY, &pos1);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_ba(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	struct array_struct *str;
	size_t size;

	if (! bitvector_dimension_equal(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		if (! bitvector_dimension_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos2);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos2, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos2, ARRAY_INFO_MEMORY, &pos2);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

static void array_bitcalc_bb(addr *ret,
		addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	size_t size;

	if (! bitmemory_equal_length(pos1, pos2))
		fmte("Length don't match ~S and ~S", pos1, pos2, NULL);
	if (opt == Nil) {
		bitmemory_length(pos1, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos1;
	}
	else {
		if (! bitvector_dimension_equal(pos1, opt))
			fmte("Length don't match ~S and optional ~S", pos1, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	bitmemory_bitcalc(opt, pos1, pos2, call);
}

void array_bitcalc(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call)
{
	int check1, check2;

	check1 = bitvector_type(pos1);
	check2 = bitvector_type(pos2);
	if (check1 && check2) {
		array_bitcalc_aa(ret, pos1, pos2, opt, call);
		return;
	}
	if (check1) {
		array_bitcalc_ab(ret, pos1, pos2, opt, call);
		return;
	}
	if (check2) {
		array_bitcalc_ba(ret, pos1, pos2, opt, call);
		return;
	}
	else {
		array_bitcalc_bb(ret, pos1, pos2, opt, call);
		return;
	}
}

void array_bitnot_array(addr *ret, addr pos, addr opt)
{
	struct array_struct *str;

	if (opt == Nil) {
		array_bitcalc_make(pos, &opt);
		*ret = opt;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else if (opt == T) {
		*ret = opt = pos;
		GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	else {
		if (! bitvector_dimension_equal(pos, opt))
			fmte("Length don't match ~S and optional ~S", pos, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_BIT)
		fmte("Array ~S must be a bit type.", pos, NULL);
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	bitmemory_bitnot(opt, pos);
}

void array_bitnot_bitmemory(addr *ret, addr pos, addr opt)
{
	size_t size;

	if (opt == Nil) {
		bitmemory_length(pos, &size);
		bitmemory_heap(&opt, size);
		*ret = opt;
	}
	else if (opt == T) {
		*ret = opt = pos;
	}
	else {
		if (! bitvector_dimension_equal(pos, opt))
			fmte("Length don't match ~S and optional ~S", pos, opt, NULL);
		*ret = opt;
		if (arrayp(opt))
			GetArrayInfo(opt, ARRAY_INFO_MEMORY, &opt);
	}
	if (! bitvectorp(*ret))
		fmte("Array ~S must be a bit type.", *ret, NULL);
	bitmemory_bitnot(opt, pos);
}

void array_bitnot(addr *ret, addr pos, addr opt)
{
	if (bitvector_type(pos))
		array_bitnot_array(ret, pos, opt);
	else
		array_bitnot_bitmemory(ret, pos, opt);
}

void array_fill(addr pos, addr item, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	/* argument */
	str = ArrayInfoStruct(pos);
	sequence_start_end(start, end, str->size, &index1, &index2);

	/* fill */
	for (; index1 < index2; index1++)
		array_set(pos, index1, item);
}

static void general_subseq_index(addr *ret, addr pos, size_t index1, size_t index2)
{
	addr root, temp;
	size_t i;

	Check(index2 < index1, "index error");
	vector_heap(&root, index2 - index1);
	for (i = 0; index1 < index2; index1++, i++) {
		if (array_get_t(pos, index1, &temp)) {
			fmte("Invalid array object.", NULL);
			return;
		}
		setarray(root, i, temp);
	}
	*ret = root;
}

static void array_make_specialized_sequence(addr *ret, addr array, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_alloc(NULL, &pos);
	/* element-type */
	struct_bitcalc_make(pos, array);
	str = ArrayInfoStruct(pos);
	str->size = str->front = str->refer = size;
	/* allocate */
	Check(str->dimension != 1, "dimension error");
	allocate_element_size(NULL, pos, str);
	/* result */
	*ret = pos;
}

static void specialized_subseq_index(addr *ret, addr array, size_t index1, size_t index2)
{
	byte *data1;
	const byte *data2;
	addr pos, mem1, mem2;
	size_t element, diff;

	/* make array */
	Check(index2 < index1, "index error");
	diff = index2 - index1;
	array_make_specialized_sequence(&pos, array, diff);

	/* subseq */
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem1);
	GetArrayInfo(array, ARRAY_INFO_MEMORY, &mem2);
	data1 = (byte *)ptr_arraymemory(mem1);
	data2 = (const byte *)ptr_arraymemory(mem2);
	element = ArrayInfoStruct(pos)->element;
	memcpy(data1, data2 + index1 * element, diff * element);
	*ret = pos;
}

static void array_subseq_type(addr *ret, addr pos, size_t index1, size_t index2)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_BIT:
			GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
			bitmemory_subseq_index(ret, pos, index1, index2);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
			strvect_subseq_index(ret, pos, index1, index2);
			break;

		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			specialized_subseq_index(ret, pos, index1, index2);
			break;

		default:
			general_subseq_index(ret, pos, index1, index2);
			break;
	}
}

void array_subseq(addr *ret, addr pos, addr start, addr end)
{
	size_t index1, index2;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	sequence_start_end(start, end, str->size, &index1, &index2);
	array_subseq_type(ret, pos, index1, index2);
}

void array_t_reverse(LocalRoot local, addr *ret, addr pos)
{
	addr one, temp;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	vector_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_t(pos, x, &temp)) {
			fmte("Invalid array ~S.", pos, NULL);
			return;
		}
		setarray(one, y, temp);
	}
	*ret = one;
}

void array_bit_reverse(LocalRoot local, addr *ret, addr pos)
{
	int temp;
	addr one;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	bitmemory_unsafe(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_bit(pos, x, &temp))
			fmte("Invalid array ~S.", pos, NULL);
		bitmemory_setint(one, y, temp);
	}
	*ret = one;
}

void array_character_reverse(LocalRoot local, addr *ret, addr pos)
{
	unicode temp;
	addr one;
	size_t size, x, y;

	size = length_sequence(pos, 1);
	strvect_alloc(local, &one, size);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		if (array_get_unicode(pos, x, &temp)) {
			fmte("Invalid array ~S.", pos, NULL);
			return;
		}
		strvect_setc(one, y, temp);
	}
	*ret = one;
}

static void array_type_simple_vector(addr pos, enum ARRAY_TYPE type, unsigned size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	settype_array(pos);
}

static void array_make_simple_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, unsigned bytesize)
{
	struct array_struct *str;
	addr pos;

	/* object */
	array_empty_alloc(local, &pos);
	/* element-type */
	array_type_simple_vector(pos, type, bytesize);
	element_make_array(pos);
	/* dimension */
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = str->refer = size;
	/* allocate */
	allocate_make_array(local, pos, Nil, Nil, Nil, Nil);
	/* initial value */
	initial_make_array(local, pos, Unbound, Unbound);
	/* result */
	*ret = pos;
}

void array_size_reverse(LocalRoot local, addr *ret, addr pos)
{
	struct array_struct *str;
	addr one;
	size_t size, x, y;
	struct array_value value;

	str = ArrayInfoStruct(pos);
	size = str->size;
	array_make_simple_vector(local, &one, size, str->type, str->bytesize);
	for (x = 0; x < size; x++) {
		y = size - x - 1;
		array_get_inplace(pos, x, &value);
		if (array_set_inplace(one, y, &value))
			fmte("Invalid array ~S.", pos, NULL);
	}
	*ret = one;
}

void array_reverse(LocalRoot local, addr *ret, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Check(str->dimension != 1, "dimension error");
	switch (str->type) {
		case ARRAY_TYPE_T:
			array_t_reverse(local, ret, pos);
			break;

		case ARRAY_TYPE_BIT:
			array_bit_reverse(local, ret, pos);
			break;

		case ARRAY_TYPE_CHARACTER:
			array_character_reverse(local, ret, pos);
			break;

		default:
			array_size_reverse(local, ret, pos);
			break;
	}
}


void array_nreverse(addr *ret, addr pos)
{
	size_t size, x, y;
	struct array_value a, b;

	Check(ArrayInfoStruct(pos)->dimension != 1, "dimension error");
	size = length_sequence(pos, 1);
	if (size <= 1) return;
	x = 0;
	y = size - 1;
	while (x < y) {
		array_get_inplace(pos, x, &a);
		array_get_inplace(pos, y, &b);
		array_set_inplace(pos, x, &b);
		array_set_inplace(pos, y, &a);
		x++;
		y--;
	}
	*ret = pos;
}

