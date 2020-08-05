#include "array.h"
#include "array_access.h"
#include "array_adjust.h"
#include "array_make.h"
#include "array_vector.h"
#include "bit.h"
#include "condition.h"
#include "integer.h"
#include "strtype.h"

_g int vector_type_p(addr pos)
{
	return (GetType(pos) == LISPTYPE_VECTOR)
		|| stringp(pos)
		|| array_vector_p(pos);
}


/*
 *  vector
 */
static int vector_pop_array_(Execute ptr, addr pos, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (str->front == 0) {
		*ret = Nil;
		return call_type_error_fill_pointer_zero_(ptr, pos);
	}
	str->front--;
	return array_get_(NULL, pos, str->front, ret);
}

_g int vector_pop_common_(Execute ptr, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_pop_array_(ptr, pos, ret);

		case LISPTYPE_VECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

static int vector_push_array_(Execute ptr, addr pos, addr value, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (str->size <= str->front) {
		return Result(ret, Nil);
	}

	Return(array_set_(pos, str->front, value));
	make_index_integer_heap(ret, str->front);
	str->front++;

	return 0;
}

_g int vector_push_common_(Execute ptr, addr value, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_push_array_(ptr, pos, value, ret);

		case LISPTYPE_VECTOR:
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

static int vector_push_extend_resize_(addr pos, size_t fill, size_t size)
{
	return array_adjust_array_(&pos, pos, intsizeh(size),
			Unbound, Unbound, Unbound,
			intsizeh(fill), Nil, fixnumh(0));
}

static int vector_push_extension_(addr extension, size_t *rsize, int *ret)
{
	if (extension == Unbound) {
		*rsize = 0;
		return Result(ret, 1);
	}
	if (GetIndex_integer(extension, rsize)) {
		*rsize = 0;
		*ret = 0;
		return fmte_("Invalid extension value ~S.", extension, NULL);
	}

	return Result(ret, 0);
}

static int vector_push_extend_normal_(addr pos, addr extension)
{
	int check;
	struct array_struct *str;
	size_t size;

	/* argument */
	str = ArrayInfoStruct(pos);
	Return(vector_push_extension_(extension, &size, &check));
	if (check)
		size = 16;
	if (size < 16)
		size = 16;
	size += str->size;
	/* allocate */
	Return(vector_push_extend_resize_(pos, str->size, size));
	/* size */
	str->size = size;

	return 0;
}

static int vector_push_extend1_(Execute ptr,
		addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (! str->fillpointer) {
		*ret = Nil;
		return call_type_error_fill_pointer_(ptr, pos);
	}
	if (! str->adjustable) {
		*ret = Nil;
		return call_type_error_adjustable_(ptr, pos);
	}
	Return(vector_push_extend_normal_(pos, extension));
	Return(array_set_(pos, str->front, value));
	make_index_integer_heap(ret, str->front);
	str->front++;

	return 0;
}

static int vector_push_extend_array_(Execute ptr,
		addr pos, addr value, addr extension, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->front < str->size)
		return vector_push_array_(ptr, pos, value, ret);
	else
		return vector_push_extend1_(ptr, pos, value, extension, ret);
}

_g int vector_push_extend_common_(Execute ptr,
		addr value, addr pos, addr extension, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return vector_push_extend_array_(ptr, pos, value, extension, ret);

		case LISPTYPE_VECTOR:
			*ret = Nil;
			return call_type_error_fill_pointer_(ptr, pos);

		default:
			*ret = Nil;
			return TypeError_(pos, VECTOR);
	}
}

_g int vector_get_(addr pos, size_t index, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	}
	getarray(pos, index, ret);

	return 0;
}

_g int vector_aref_(addr pos, addr args, addr *ret)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (! consp(args)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	GetCons(args, &arg, &args);
	if (args != Nil) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	}
	if (! integerp(arg)) {
		*ret = Nil;
		return fmte_("AREF argument ~S must be a non-negative integer.", arg, NULL);
	}
	if (GetIndex_integer(arg, &index)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}

	return vector_get_(pos, index, ret);
}

_g int vector_set_(addr pos, size_t index, addr value)
{
	size_t size;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	if (size <= index)
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	setarray(pos, index, value);

	return 0;
}

_g int vector_setf_aref_(addr pos, addr args, addr value)
{
	addr arg;
	size_t index;

	CheckType(pos, LISPTYPE_VECTOR);
	if (GetStatusReadOnly(pos))
		return fmte_("The object ~S is constant.", pos, NULL);
	if (! consp(args))
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("AREF argument ~S must be (integer) form.", args, NULL);
	if (! integerp(arg))
		return fmte_("AREF argument ~S must be a non-negative integer.", arg, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid index arg ~S.", arg, NULL);

	return vector_set_(pos, index, value);
}

_g int vector_array_dimension_(addr pos, addr arg, size_t size, addr *ret)
{
	size_t check;

	if (! integerp(arg)) {
		*ret = Nil;
		return fmte_("ARRAY-DIMENSION argument ~S must be integer type.", arg, NULL);
	}
	if (GetIndex_integer(arg, &check)) {
		*ret = Nil;
		return fmte_("Invalid index arg ~S.", arg, NULL);
	}
	if (check != 0) {
		*ret = Nil;
		return fmte_("Array rank ~A must be less than equal to 1.", arg, NULL);
	}
	make_index_integer_heap(ret, size);

	return 0;
}

_g void vector_array_dimensions(size_t size, addr *ret)
{
	addr pos;
	make_index_integer_heap(&pos, size);
	conscar_heap(ret, pos);
}

_g int vector_array_in_bounds_p_(addr rest, size_t size, int *ret)
{
	addr pos;
	size_t check;

	if (! consp(rest)) {
		*ret = 0;
		return fmte_("The subscripts ~S is too few argumens.", rest, NULL);
	}
	GetCons(rest, &pos, &rest);
	if (rest != Nil) {
		*ret = 0;
		return fmte_("The subscripts ~S is too many argumens.", rest, NULL);
	}
	if (! integerp(pos)) {
		*ret = 0;
		return fmte_("The subscript ~S must be integer type.", pos, NULL);
	}
	if (GetIndex_integer(pos, &check))
		return Result(ret, 0);
	else
		return Result(ret, check < size);
}

_g int vector_array_row_major_index_(addr rest, size_t size, addr *ret)
{
	int check;

	Return(vector_array_in_bounds_p_(rest, size, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Out of range ~S.", intsizeh(size), NULL);
	}
	GetCar(rest, ret);

	return 0;
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
	str->size = str->front = size;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, Nil);
}

static int vector_type_(addr *ret,
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
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	/* initial value */
	Return(array_make_initial_(pos, value, Unbound));
	/* result */
	return Result(ret, pos);
}

static int vector_signed_check_(enum ARRAY_TYPE type, int bytesize)
{
	switch (type) {
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			break;

		default:
			return fmte_("Invalid vector type.", NULL);
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
			return fmte_("Invalide vector type (size).", NULL);
	}

	return 0;
}

_g int vector_signed_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs)
{
	Return(vector_signed_check_(type, bs));
	return vector_type_(ret, size, type, bs, Unbound);
}

_g int vector_signed_(addr *ret, size_t size, enum ARRAY_TYPE type, int bs, addr value)
{
	Return(vector_signed_check_(type, bs));
	if (value == Unbound)
		fixnum_heap(&value, 0);
	return vector_type_(ret, size, type, bs, value);
}

_g int vector_float_uninit_(addr *ret, size_t size, enum ARRAY_TYPE type)
{
	switch (type) {
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			break;

		default:
			return fmte_("Invalid vector type.", NULL);
	}

	return vector_type_(ret, size, type, 0, Unbound);
}

_g int vector_float_(addr *ret, size_t size, enum ARRAY_TYPE type, addr value)
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
			*ret = Nil;
			return fmte_("Invalid vector type.", NULL);
	}

	return vector_type_(ret, size, type, 0, value);
}

_g int vector_setelt_(addr pos, size_t index, addr value)
{
	size_t size;

	lenarray(pos, &size);
	if (size <= index) {
		return fmte_("Index ~A must be less than vector size ~A.",
				intsizeh(index), intsizeh(size), NULL);
	}
	setarray(pos, index, value);

	return 0;
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
	if (size <= 1)
		return;
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

