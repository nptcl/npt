#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "integer.h"

/*
 *  vector
 */
static void vector_pop_array(addr pos, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer)
		type_error_fill_pointer(pos);
	if (str->front == 0)
		type_error_fill_pointer_zero(pos);
	str->front--;
	array_get(NULL, pos, str->front, ret);
}

_g void vector_pop_common(addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			vector_pop_array(pos, ret);
			break;

		case LISPTYPE_VECTOR:
			type_error_fill_pointer(pos);
			break;

		default:
			TypeError(pos, VECTOR);
			break;
	}
}

static void vector_push_array(addr pos, addr value, addr *ret)
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
		make_index_integer_alloc(NULL, ret, str->front);
		str->front++;
	}
}

_g void vector_push_common(addr value, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			vector_push_array(pos, value, ret);
			break;

		case LISPTYPE_VECTOR:
			type_error_fill_pointer(pos);
			break;

		default:
			TypeError(pos, VECTOR);
			break;
	}
}

static void vector_push_extend_t(addr pos, size_t prev, size_t next)
{
	addr dst, src, temp;
	size_t i;

	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &src);
	arraygen_heap(&dst, next);
	if (src != Nil) {
		Check(lenarrayr(src) != prev, "size error");
		next = (next < prev)? next: prev;
		for (i = 0; i < next; i++) {
			arraygen_get(src, i, &temp);
			arraygen_set(dst, i, temp);
		}
	}
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, dst);
}

static void vector_push_extend_bit(addr pos, size_t prev, size_t next)
{
	addr src, dst;

	bitmemory_unsafe(NULL, &dst, next);
#ifdef LISP_DEBUG
	bitmemory_memset(dst, 1);
#endif
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &src);
	bitmemory_copy_unsafe(dst, src, (prev < next)? prev: next);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, dst);
}

static void vector_push_extend_size(addr pos,
		size_t prev, size_t next, unsigned element)
{
	addr src, dst;
	size_t size;
	byte *data1, *data2;

	prev *= element;
	next *= element;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &src);
	arrayspec_heap(&dst, next);
	if (src != Nil) {
		Check(lenbodyr(src) != prev, "size error");
		data1 = (byte *)arrayspec_ptr(dst);
		data2 = (byte *)arrayspec_ptr(src);
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
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, dst);
}

static void vector_push_extend_resize(addr pos,
		size_t prev, size_t next, struct array_struct *str)
{
	switch (str->type) {
		case ARRAY_TYPE_EMPTY:
			fmte("The array has no element size.", NULL);
			break;

		case ARRAY_TYPE_T:
			vector_push_extend_t(pos, prev, next);
			break;

		case ARRAY_TYPE_BIT:
			vector_push_extend_bit(pos, prev, next);
			break;

		default:
			vector_push_extend_size(pos, prev, next, str->element);
			break;
	}
}

static int vector_push_extension(addr extension, size_t *ret)
{
	if (extension == Unbound) {
		*ret = 0;
		return 1;
	}
	if (GetIndex_integer(extension, ret))
		fmte("Invalid extension value ~S.", extension, NULL);
	return 0;
}

static void vector_push_extend_displaced(addr pos, addr extension)
{
	struct array_struct *str;
	size_t diff, size, resize;

	/* argument */
	str = ArrayInfoStruct(pos);
	Check(str->size < str->refer, "reference size error");
	diff = str->size - str->refer;
	if (vector_push_extension(extension, &size))
		size = diff;
	size += diff;
	if (size < 16) size = 16;
	resize = str->refer + size;
	/* allocate */
	vector_push_extend_resize(pos, diff, size, str);
	/* size */
	str->size = resize;
}

static void vector_push_extend_normal(addr pos, addr extension)
{
	struct array_struct *str;
	size_t size;

	/* argument */
	str = ArrayInfoStruct(pos);
	if (vector_push_extension(extension, &size))
		size = str->size;
	size += str->size;
	if (size < 16) size = 16;
	/* allocate */
	vector_push_extend_resize(pos, str->size, size, str);
	/* size */
	str->size = size;
}

static void vector_push_extend1(addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer)
		type_error_fill_pointer(pos);
	if (! str->adjustable)
		type_error_adjustable(pos);
	if (str->displaced)
		vector_push_extend_displaced(pos, extension);
	else
		vector_push_extend_normal(pos, extension);
	array_set(pos, str->front, value);
	make_index_integer_heap(ret, str->front);
	str->front++;
}

static void vector_push_extend_array(addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->front < str->size)
		vector_push_array(pos, value, ret);
	else
		vector_push_extend1(pos, value, extension, ret);
}

_g void vector_push_extend_common(addr value, addr pos, addr extension, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			vector_push_extend_array(pos, value, extension, ret);
			break;

		case LISPTYPE_VECTOR:
			type_error_fill_pointer(pos);
			break;

		default:
			TypeError(pos, VECTOR);
			break;
	}
}

_g void vector_get(addr pos, size_t index, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(size), NULL);
	getarray(pos, index, ret);
}

_g void vector_aref(addr pos, addr args, addr *ret)
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
	if (GetIndex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	vector_get(pos, index, ret);
}

_g void vector_set(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		fmte("Out of range ~S.", intsizeh(size), NULL);
	setarray(pos, index, value);
}

_g void vector_setf_aref(addr pos, addr args, addr value)
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
	if (GetIndex_integer(arg, &index))
		fmte("Invalid index arg ~S.", arg, NULL);
	vector_set(pos, index, value);
}

_g void vector_array_dimension(addr pos, addr arg, size_t size, addr *ret)
{
	size_t check;

	if (! integerp(arg))
		fmte("ARRAY-DIMENSION argument ~S must be integer type.", arg, NULL);
	if (GetIndex_integer(arg, &check))
		fmte("Invalid index arg ~S.", arg, NULL);
	if (check != 0)
		fmte("Array rank ~A must be less than equal to 1.", arg, NULL);
	make_index_integer_alloc(NULL, ret, size);
}

_g void vector_array_dimensions(size_t size, addr *ret)
{
	addr pos;
	make_index_integer_alloc(NULL, &pos, size);
	conscar_heap(ret, pos);
}

_g int vector_array_in_bounds_p(addr rest, size_t size)
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
	if (GetIndex_integer(pos, &check))
		return 0;

	return check < size;
}

_g void vector_array_row_major_index(addr rest, size_t size, addr *ret)
{
	if (! vector_array_in_bounds_p(rest, size))
		fmte("Out of range ~S.", intsizeh(size), NULL);
	GetCar(rest, ret);
}

static void vector_settype(addr pos, enum ARRAY_TYPE type, int size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->type = type;
	str->bytesize = size;
	array_set_type(pos);
	array_set_element_size(pos);
}

static void vector_dimension(addr pos, size_t size)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = str->front = str->refer = size;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
}

static void vector_type(addr *ret,
		size_t size, enum ARRAY_TYPE type, int bs, addr value)
{
	addr pos;

	/* object */
	array_empty_heap(&pos);
	/* element-type */
	vector_settype(pos, type, bs);
	/* dimension */
	vector_dimension(pos, size);
	/* allocate */
	array_make_memory(pos, Nil, Nil, Nil, Nil);
	/* initial value */
	array_make_initial(pos, value, Unbound);
	/* result */
	*ret = pos;
}

static void vector_signed_check(enum ARRAY_TYPE type, int bytesize)
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

_g void vector_signed_uninit(addr *ret, size_t size, enum ARRAY_TYPE type, int bs)
{
	vector_signed_check(type, bs);
	vector_type(ret, size, type, bs, Unbound);
}

_g void vector_signed(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value)
{
	vector_signed_check(type, bs);
	if (value == Unbound)
		fixnum_heap(&value, 0);
	vector_type(ret, size, type, bs, value);
}

_g void vector_float_uninit(addr *ret, size_t size, enum ARRAY_TYPE type)
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
	vector_type(ret, size, type, 0, Unbound);
}

_g void vector_float(addr *ret, size_t size, enum ARRAY_TYPE type, addr value)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
			if (value == Unbound)
				single_float_heap(&value, 0.0f);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			if (value == Unbound)
				double_float_heap(&value, 0.0);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			if (value == Unbound)
				long_float_heap(&value, 0.0L);
			break;

		default:
			fmte("Invalid vector type.", NULL);
			break;
	}
	vector_type(ret, size, type, 0, value);
}

_g void vector_setelt(addr pos, size_t index, addr value)
{
	size_t size;

	lenarray(pos, &size);
	if (size <= index) {
		fmte("Index ~A must be less than vector size ~A.",
				intsizeh(index), intsizeh(size), NULL);
	}
	setarray(pos, index, value);
}

_g void vector_reverse(LocalRoot local, addr *ret, addr pos)
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

_g void vector_nreverse(addr *ret, addr pos)
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

