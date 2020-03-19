#include "array.h"
#include "array_access.h"
#include "array_inplace.h"
#include "array_make.h"
#include "array_value.h"
#include "bit.h"
#include "bigdata.h"
#include "bignum.h"
#include "condition.h"
#include "integer.h"
#include "sequence.h"
#include "strtype.h"
#include "type_object.h"

/*
 *  arraymemory_get
 */
static int arraymemory_get_memory(addr pos, size_t index, addr *retp, size_t *rets)
{
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, retp);
	*rets = index;
	return array_system_p(*retp);
}

_g int arraymemory_get(addr pos, size_t index, addr *retp, size_t *rets)
{
	struct array_struct *str;

	Check(! arrayp(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (str->size <= index) {
		_fmte("Index ~S must be less than array size.", intsizeh(index), NULL);
		*retp = Nil;
		*rets = 0;
		return 0;
	}
	if (! str->displaced)
		return arraymemory_get_memory(pos, index, retp, rets);

	/* displaced */
	GetArrayInfo(pos, ARRAY_INDEX_DISPLACED, &pos);
	index += str->offset;
	if (arrayp(pos))
		return arraymemory_get(pos, index, retp, rets);

	/* not array */
	*retp = pos;
	*rets = index;
	return 0;
}


/*
 *  arraymemory_set
 */
/* character */
static void arraymemory_setvalue_character(addr pos, size_t index, unicode value)
{
	((unicode *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setcharacter(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_character(&str, value);
	arraymemory_setvalue_character(pos, index, str.value.character);
}

/* singed */
static void arraymemory_setvalue_signed8(addr pos, size_t index, int8_t value)
{
	((int8_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setsigned8(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_signed8(&str, value);
	arraymemory_setvalue_signed8(pos, index, str.value.signed8);
}

static void arraymemory_setvalue_signed16(addr pos, size_t index, int16_t value)
{
	((int16_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setsigned16(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_signed16(&str, value);
	arraymemory_setvalue_signed16(pos, index, str.value.signed16);
}

static void arraymemory_setvalue_signed32(addr pos, size_t index, int32_t value)
{
	((int32_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setsigned32(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_signed32(&str, value);
	arraymemory_setvalue_signed32(pos, index, str.value.signed32);
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_signed64(addr pos, size_t index, int64_t value)
{
	((int64_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setsigned64(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_signed64(&str, value);
	arraymemory_setvalue_signed64(pos, index, str.value.signed64);
}
#endif

static void arraymemory_setsigned(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			arraymemory_setsigned8(pos, index, value);
			break;

		case 16:
			arraymemory_setsigned16(pos, index, value);
			break;

		case 32:
			arraymemory_setsigned32(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_setsigned64(pos, index, value);
			break;
#endif
		default:
			_fmte("Invalid array size.", NULL);
			break;
	}
}

/* unsigned */
static void arraymemory_setvalue_unsigned8(addr pos, size_t index, uint8_t value)
{
	((uint8_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setunsigned8(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_unsigned8(&str, value);
	arraymemory_setvalue_unsigned8(pos, index, str.value.unsigned8);
}

static void arraymemory_setvalue_unsigned16(addr pos, size_t index, uint16_t value)
{
	((uint16_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setunsigned16(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_unsigned16(&str, value);
	arraymemory_setvalue_unsigned16(pos, index, str.value.unsigned16);
}

static void arraymemory_setvalue_unsigned32(addr pos, size_t index, uint32_t value)
{
	((uint32_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setunsigned32(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_unsigned32(&str, value);
	arraymemory_setvalue_unsigned32(pos, index, str.value.unsigned32);
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_unsigned64(addr pos, size_t index, uint64_t value)
{
	((uint64_t *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setunsigned64(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_unsigned64(&str, value);
	arraymemory_setvalue_unsigned64(pos, index, str.value.unsigned64);
}
#endif

static void arraymemory_setunsigned(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			arraymemory_setunsigned8(pos, index, value);
			break;

		case 16:
			arraymemory_setunsigned16(pos, index, value);
			break;

		case 32:
			arraymemory_setunsigned32(pos, index, value);
			break;

#ifdef LISP_64BIT
		case 64:
			arraymemory_setunsigned64(pos, index, value);
			break;
#endif
		default:
			_fmte("Invalid array size.", NULL);
			break;
	}
}

/* single-float */
static void arraymemory_setvalue_single(addr pos, size_t index, single_float value)
{
	((single_float *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setsingle(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_single(&str, value);
	arraymemory_setvalue_single(pos, index, str.value.single_value);
}

/* double-float */
static void arraymemory_setvalue_double(addr pos, size_t index, double_float value)
{
	((double_float *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setdouble(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_double(&str, value);
	arraymemory_setvalue_double(pos, index, str.value.double_value);
}

/* long-float */
static void arraymemory_setvalue_long(addr pos, size_t index, long_float value)
{
	((long_float *)arrayspec_ptr(pos))[index] = value;
}
static void arraymemory_setlong(addr pos, size_t index, addr value)
{
	struct array_value str;
	arrayvalue_get_long(&str, value);
	arraymemory_setvalue_long(pos, index, str.value.long_value);
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
			_fmte("Invalid array object, ~S.", pos, NULL);
			break;

		case ARRAY_TYPE_CHARACTER:
			arraymemory_setcharacter(mem, index, value);
			break;

		case ARRAY_TYPE_SIGNED:
			arraymemory_setsigned(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_UNSIGNED:
			arraymemory_setunsigned(mem, bytesize, index, value);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			arraymemory_setsingle(mem, index, value);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			arraymemory_setdouble(mem, index, value);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			arraymemory_setlong(mem, index, value);
			break;

		default:
			_fmte("(SETF AREF) Invalid array type.", NULL);
			break;
	}
}


/*
 *  array-get/set
 */
_g void array_get_t(addr pos, size_t index, addr *ret)
{
	if (array_type(pos) != ARRAY_TYPE_T)
		_fmte("Invalid array type, ~S.", pos, NULL);
	if (arraymemory_get(pos, index, &pos, &index))
		arraygen_get(pos, index, ret);
	else
		getarray(pos, index, ret);
}

_g void array_get_bit(addr pos, size_t index, int *ret)
{
	if (array_type(pos) != ARRAY_TYPE_BIT)
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	bitmemory_getint(pos, index, ret);
}

static void arraymemory_getunicode(addr pos, size_t index, unicode *ret)
{
	*ret = ((unicode *)arrayspec_ptr(pos))[index];
}

_g void array_get_unicode(addr pos, size_t index, unicode *ret)
{
	if (array_type(pos) != ARRAY_TYPE_CHARACTER)
		_fmte("Invalid array type, ~S.", pos, NULL);
	if (arraymemory_get(pos, index, &pos, &index))
		arraymemory_getunicode(pos, index, ret);
	else
		string_getc(pos, index, ret);
}

_g void array_get(LocalRoot local, addr pos, size_t index, addr *ret)
{
	struct array_value str;
	arrayinplace_get(pos, index, &str);
	arrayvalue_alloc(local, ret, &str);
}

_g void array_set_bit(addr pos, size_t index, int value)
{
	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_BIT)
		_fmte("Invalid array type, ~S.", pos, NULL);
	if (value != 0 && value != 1)
		_fmte("Value ~A must be a bit type (0 or 1).", fixnumh(value), NULL);
	arraymemory_get(pos, index, &pos, &index);
	bitmemory_setint(pos, index, value);
}

_g void array_set_character(addr pos, size_t index, unicode value)
{
	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_CHARACTER)
		_fmte("Invalid array type, ~S.", pos, NULL);
	if (arraymemory_get(pos, index, &pos, &index))
		arraymemory_setvalue_character(pos, index, value);
	else
		string_setc(pos, index, value);
}

_g void array_set_signed8(addr pos, size_t index, int8_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 8))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_signed8(pos, index, value);
}

_g void array_set_signed16(addr pos, size_t index, int16_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 16))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_signed16(pos, index, value);
}

_g void array_set_signed32(addr pos, size_t index, int32_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 32))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_signed32(pos, index, value);
}

_g void array_set_unsigned8(addr pos, size_t index, uint8_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 8))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_unsigned8(pos, index, value);
}

_g void array_set_unsigned16(addr pos, size_t index, uint16_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 16))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_unsigned16(pos, index, value);
}

_g void array_set_unsigned32(addr pos, size_t index, uint32_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 32))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_unsigned32(pos, index, value);
}

#ifdef LISP_64BIT
_g void array_set_signed64(addr pos, size_t index, int64_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 64))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_signed64(pos, index, value);
}

_g void array_set_unsigned64(addr pos, size_t index, uint64_t value)
{
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 64))
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_unsigned64(pos, index, value);
}
#endif

_g void array_set_single(addr pos, size_t index, single_float value)
{
	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_SINGLE_FLOAT)
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_single(pos, index, value);
}

_g void array_set_double(addr pos, size_t index, double_float value)
{
	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_DOUBLE_FLOAT)
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_double(pos, index, value);
}

_g void array_set_long(addr pos, size_t index, long_float value)
{
	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_LONG_FLOAT)
		_fmte("Invalid array type, ~S.", pos, NULL);
	arraymemory_get(pos, index, &pos, &index);
	arraymemory_setvalue_long(pos, index, value);
}

_g void array_set(addr pos, size_t index, addr value)
{
	addr mem;

	if (GetStatusReadOnly(pos))
		_fmte("Object ~S is constant.", pos, NULL);
	if (arraymemory_get(pos, index, &mem, &index))
		arraymemory_set(pos, mem, index, value);
	else
		setelt_sequence(mem, index, value);
}


/*
 *  setget
 */
_g void array_setget(addr p1, size_t s1, addr p2, size_t s2)
{
	struct array_value value;
	struct array_struct *str1, *str2;

	if (GetStatusReadOnly(p1))
		_fmte("Object ~S is constant.", p1, NULL);
	str1 = ArrayInfoStruct(p1);
	str2 = ArrayInfoStruct(p2);
	if (! array_equal_type(str1, str2->type, str2->bytesize))
		_fmte("Array ~S type must be equal to base array ~S.", p1, p2, NULL);
	arrayinplace_get(p2, s2, &value);
	arrayinplace_set(p1, s1, &value);
}


/*
 *  array
 */
static size_t array1arefindex(addr pos, addr args, struct array_struct *str)
{
	addr arg;
	size_t index;

	if (! consp(args))
		_fmte("Subscripts ~S must be list form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		_fmte("Subscripts ~S too many arguments.", args, NULL);
	if (GetIndex_integer(arg, &index))
		_fmte("Invalid subscript argument ~S.", arg, NULL);
	if (str->front <= index)
		_fmte("Subscript ~S is too large.", arg, NULL);
	return index;
}

_g size_t array_arefindex(addr pos, addr args)
{
	struct array_struct *str;
	const size_t *data;
	size_t index, value, depth, dimension, range;
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
			_fmte("Subscripts ~S must be a list type.", list, NULL);
		if (dimension <= depth)
			_fmte("Subscripts ~A is too large.", args, NULL);
		GetCons(list, &check, &list);
		if (GetIndex_integer(check, &value))
			_fmte("Invalid index value ~S.", check, NULL);
		range = data[depth];
		if (range <= value)
			_fmte("Out of range ~S subscripts in ~S array.", list, pos, NULL);
		index = depth? (index * range): 0;
		index += value;
	}
	if (depth != str->dimension)
		_fmte("Subscript ~S is too few.", args, NULL);

	return index;
}

_g void array_aref(LocalRoot local, addr pos, addr args, addr *ret)
{
	size_t index = array_arefindex(pos, args);
	array_get(local, pos, index, ret);
}

_g void array_setf_aref(addr pos, addr args, addr value)
{
	size_t index = array_arefindex(pos, args);
	array_set(pos, index, value);
}

_g void array_aref_bit(LocalRoot local, addr pos, addr args, addr *ret)
{
	int value;
	size_t index;

	index = array_arefindex(pos, args);
	array_get_bit(pos, index, &value);
	fixnum_alloc(local, ret, (fixnum)value);
}

_g void array_setf_aref_bit(addr pos, addr args, addr value)
{
	int check;
	size_t index;

	index = array_arefindex(pos, args);
	bit_getint_error(value, &check);
	fixnum_heap(&value, (fixnum)check);
	array_set(pos, index, value);
}


/*
 *  check
 */
_g int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size)
{
	if (a->type != type)
		return 0;
	if (a->type == ARRAY_TYPE_SIGNED || a->type == ARRAY_TYPE_UNSIGNED)
		return a->bytesize == size;
	else
		return 1;
}

_g int array_equal_dimension(addr a, addr b)
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

_g void array_get_element_type(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &pos);
	type_object(ret, pos);
}

_g size_t array_get_vector_length(addr pos, int fill)
{
	struct array_struct *str;

	Check(! array_vector_p(pos), "type error");
	str = ArrayInfoStruct(pos);
	return fill? str->front: str->size;
}

_g void array_get_rowlength(addr pos, size_t *ret)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	*ret = (str->fillpointer)? str->front: str->size;
}

