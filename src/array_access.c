#include "array.h"
#include "array_access.h"
#include "array_inplace.h"
#include "array_make.h"
#include "array_value.h"
#include "bit.h"
#include "bignum_data.h"
#include "bignum.h"
#include "condition.h"
#include "integer.h"
#include "sequence.h"
#include "strtype.h"
#include "type_object.h"

/*
 *  arraymemory_get
 */
static int arraymemory_get_memory_(addr pos, size_t index,
		addr *retp, size_t *rets, int *ret)
{
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, retp);
	*rets = index;
	*ret = array_system_p(*retp);
	return 0;
}

int arraymemory_get_(addr pos, size_t index, addr *retp, size_t *rets, int *ret)
{
	struct array_struct *str;

	Check(! arrayp(pos), "type error");
	str = ArrayInfoStruct(pos);
	if (str->size <= index) {
		*retp = Nil;
		*rets = 0;
		*ret = 0;
		return fmte_("Index ~S must be less than array size.", intsizeh(index), NULL);
	}
	if (! str->displaced)
		return arraymemory_get_memory_(pos, index, retp, rets, ret);

	/* displaced */
	GetArrayInfo(pos, ARRAY_INDEX_DISPLACED, &pos);
	index += str->offset;
	if (arrayp(pos))
		return arraymemory_get_(pos, index, retp, rets, ret);

	/* not array */
	*retp = pos;
	*rets = index;
	*ret = 0;
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
static int arraymemory_setcharacter_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_character_(&str, value));
	arraymemory_setvalue_character(pos, index, str.value.character);

	return 0;
}

/* singed */
static void arraymemory_setvalue_signed8(addr pos, size_t index, int8_t value)
{
	((int8_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned8_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed8_(&str, value));
	arraymemory_setvalue_signed8(pos, index, str.value.signed8);

	return 0;
}

static void arraymemory_setvalue_signed16(addr pos, size_t index, int16_t value)
{
	((int16_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned16_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed16_(&str, value));
	arraymemory_setvalue_signed16(pos, index, str.value.signed16);

	return 0;
}

static void arraymemory_setvalue_signed32(addr pos, size_t index, int32_t value)
{
	((int32_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned32_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed32_(&str, value));
	arraymemory_setvalue_signed32(pos, index, str.value.signed32);

	return 0;
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_signed64(addr pos, size_t index, int64_t value)
{
	((int64_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsigned64_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_signed64_(&str, value));
	arraymemory_setvalue_signed64(pos, index, str.value.signed64);

	return 0;
}
#endif

static int arraymemory_setsigned_(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			return arraymemory_setsigned8_(pos, index, value);

		case 16:
			return arraymemory_setsigned16_(pos, index, value);

		case 32:
			return arraymemory_setsigned32_(pos, index, value);

#ifdef LISP_64BIT
		case 64:
			return arraymemory_setsigned64_(pos, index, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

/* unsigned */
static void arraymemory_setvalue_unsigned8(addr pos, size_t index, uint8_t value)
{
	((uint8_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned8_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned8_(&str, value));
	arraymemory_setvalue_unsigned8(pos, index, str.value.unsigned8);

	return 0;
}

static void arraymemory_setvalue_unsigned16(addr pos, size_t index, uint16_t value)
{
	((uint16_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned16_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned16_(&str, value));
	arraymemory_setvalue_unsigned16(pos, index, str.value.unsigned16);

	return 0;
}

static void arraymemory_setvalue_unsigned32(addr pos, size_t index, uint32_t value)
{
	((uint32_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned32_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned32_(&str, value));
	arraymemory_setvalue_unsigned32(pos, index, str.value.unsigned32);

	return 0;
}

#ifdef LISP_64BIT
static void arraymemory_setvalue_unsigned64(addr pos, size_t index, uint64_t value)
{
	((uint64_t *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setunsigned64_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_unsigned64_(&str, value));
	arraymemory_setvalue_unsigned64(pos, index, str.value.unsigned64);

	return 0;

}
#endif

static int arraymemory_setunsigned_(addr pos, unsigned size, size_t index, addr value)
{
	switch (size) {
		case 8:
			return arraymemory_setunsigned8_(pos, index, value);

		case 16:
			return arraymemory_setunsigned16_(pos, index, value);

		case 32:
			return arraymemory_setunsigned32_(pos, index, value);

#ifdef LISP_64BIT
		case 64:
			return arraymemory_setunsigned64_(pos, index, value);
#endif
		default:
			return fmte_("Invalid array size.", NULL);
	}
}

/* single-float */
static void arraymemory_setvalue_single(addr pos, size_t index, single_float value)
{
	((single_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setsingle_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_single_(&str, value));
	arraymemory_setvalue_single(pos, index, str.value.single_value);

	return 0;
}

/* double-float */
static void arraymemory_setvalue_double(addr pos, size_t index, double_float value)
{
	((double_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setdouble_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_double_(&str, value));
	arraymemory_setvalue_double(pos, index, str.value.double_value);

	return 0;
}

/* long-float */
static void arraymemory_setvalue_long(addr pos, size_t index, long_float value)
{
	((long_float *)arrayspec_ptr(pos))[index] = value;
}
static int arraymemory_setlong_(addr pos, size_t index, addr value)
{
	struct array_value str;

	Return(arrayvalue_get_long_(&str, value));
	arraymemory_setvalue_long(pos, index, str.value.long_value);

	return 0;
}

static int arraymemory_set_(addr pos, addr mem, size_t index, addr value)
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
			return 0;

		case ARRAY_TYPE_BIT:
			return fmte_("Invalid array object, ~S.", pos, NULL);

		case ARRAY_TYPE_CHARACTER:
			return arraymemory_setcharacter_(mem, index, value);

		case ARRAY_TYPE_SIGNED:
			return arraymemory_setsigned_(mem, bytesize, index, value);

		case ARRAY_TYPE_UNSIGNED:
			return arraymemory_setunsigned_(mem, bytesize, index, value);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return arraymemory_setsingle_(mem, index, value);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return arraymemory_setdouble_(mem, index, value);

		case ARRAY_TYPE_LONG_FLOAT:
			return arraymemory_setlong_(mem, index, value);

		default:
			return fmte_("(SETF AREF) Invalid array type.", NULL);
	}
}


/*
 *  array-get/set
 */
int array_get_t_(addr pos, size_t index, addr *ret)
{
	int check;

	if (array_type(pos) != ARRAY_TYPE_T) {
		*ret = Nil;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check)
		arraygen_get(pos, index, ret);
	else
		getarray(pos, index, ret);

	return 0;
}

int array_get_bit_(addr pos, size_t index, int *ret)
{
	int ignore;

	if (array_type(pos) != ARRAY_TYPE_BIT) {
		*ret = 0;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	return bitmemory_getint_(pos, index, ret);
}

static void arraymemory_getunicode(addr pos, size_t index, unicode *ret)
{
	*ret = ((unicode *)arrayspec_ptr(pos))[index];
}

int array_get_unicode_(addr pos, size_t index, unicode *ret)
{
	int check;

	if (array_type(pos) != ARRAY_TYPE_CHARACTER) {
		*ret = 0;
		return fmte_("Invalid array type, ~S.", pos, NULL);
	}
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check) {
		arraymemory_getunicode(pos, index, ret);
	}
	else {
		Return(string_getc_(pos, index, ret));
	}

	return 0;
}

int array_get_(LocalRoot local, addr pos, size_t index, addr *ret)
{
	struct array_value str;

	Return(arrayinplace_get_(pos, index, &str));
	Return(arrayvalue_alloc_(local, ret, &str));

	return 0;
}

int array_set_bit_(addr pos, size_t index, int value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_BIT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	if (value != 0 && value != 1)
		return fmte_("Value ~A must be a bit type (0 or 1).", fixnumh(value), NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	return bitmemory_setint_(pos, index, value);
}

int array_set_character_(addr pos, size_t index, unicode value)
{
	int check;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_CHARACTER)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &check));
	if (check) {
		arraymemory_setvalue_character(pos, index, value);
	}
	else {
		Return(string_setc_(pos, index, value));
	}

	return 0;
}

int array_set_signed8_(addr pos, size_t index, int8_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 8))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed8(pos, index, value);

	return 0;
}

int array_set_signed16_(addr pos, size_t index, int16_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 16))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed16(pos, index, value);

	return 0;
}

int array_set_signed32_(addr pos, size_t index, int32_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 32))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed32(pos, index, value);

	return 0;
}

int array_set_unsigned8_(addr pos, size_t index, uint8_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 8))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned8(pos, index, value);

	return 0;
}

int array_set_unsigned16_(addr pos, size_t index, uint16_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 16))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned16(pos, index, value);

	return 0;
}

int array_set_unsigned32_(addr pos, size_t index, uint32_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 32))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned32(pos, index, value);

	return 0;
}

#ifdef LISP_64BIT
int array_set_signed64_(addr pos, size_t index, int64_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_SIGNED, 64))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_signed64(pos, index, value);

	return 0;
}

int array_set_unsigned64_(addr pos, size_t index, uint64_t value)
{
	int ignore;
	struct array_struct *str;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	str = ArrayInfoStruct(pos);
	if (! array_equal_type(str, ARRAY_TYPE_UNSIGNED, 64))
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_unsigned64(pos, index, value);

	return 0;
}
#endif

int array_set_single_(addr pos, size_t index, single_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_SINGLE_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_single(pos, index, value);

	return 0;
}

int array_set_double_(addr pos, size_t index, double_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_DOUBLE_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_double(pos, index, value);

	return 0;
}

int array_set_long_(addr pos, size_t index, long_float value)
{
	int ignore;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	if (array_type(pos) != ARRAY_TYPE_LONG_FLOAT)
		return fmte_("Invalid array type, ~S.", pos, NULL);
	Return(arraymemory_get_(pos, index, &pos, &index, &ignore));
	arraymemory_setvalue_long(pos, index, value);

	return 0;
}

int array_set_(addr pos, size_t index, addr value)
{
	int check;
	addr mem;

	if (GetStatusReadOnly(pos))
		return fmte_("Object ~S is constant.", pos, NULL);
	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arraymemory_set_(pos, mem, index, value);
	else
		return setelt_sequence_(mem, index, value);
}


/*
 *  setget
 */
int array_setget_(addr p1, size_t s1, addr p2, size_t s2)
{
	struct array_value value;
	struct array_struct *str1, *str2;

	if (GetStatusReadOnly(p1))
		return fmte_("Object ~S is constant.", p1, NULL);
	str1 = ArrayInfoStruct(p1);
	str2 = ArrayInfoStruct(p2);
	if (! array_equal_type(str1, str2->type, str2->bytesize))
		return fmte_("Array ~S type must be equal to base array ~S.", p1, p2, NULL);
	Return(arrayinplace_get_(p2, s2, &value));
	Return(arrayinplace_set_(p1, s1, &value));

	return 0;
}


/*
 *  array
 */
static int array1arefindex_(addr pos, addr args, struct array_struct *str, size_t *ret)
{
	addr arg;
	size_t index;

	if (! consp(args))
		return fmte_("Subscripts ~S must be list form.", args, NULL);
	GetCons(args, &arg, &args);
	if (args != Nil)
		return fmte_("Subscripts ~S too many arguments.", args, NULL);
	if (GetIndex_integer(arg, &index))
		return fmte_("Invalid subscript argument ~S.", arg, NULL);
	if (str->size <= index)
		return fmte_("Subscript ~S is too large.", arg, NULL);

	return Result(ret, index);
}

int array_arefindex_(addr pos, addr args, size_t *ret)
{
	struct array_struct *str;
	const size_t *data;
	size_t index, value, depth, dimension, range;
	addr check, list;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	dimension = str->dimension;
	if (dimension == 1)
		return array1arefindex_(pos, args, str, ret);
	data = array_ptrsize(pos);
	index = 0;
	list = args;
	for (depth = 0; list != Nil; depth++) {
		if (! consp(list))
			return fmte_("Subscripts ~S must be a list type.", list, NULL);
		if (dimension <= depth)
			return fmte_("Subscripts ~A is too large.", args, NULL);
		GetCons(list, &check, &list);
		if (GetIndex_integer(check, &value))
			return fmte_("Invalid index value ~S.", check, NULL);
		range = data[depth];
		if (range <= value)
			return fmte_("Out of range ~S subscripts in ~S array.", list, pos, NULL);
		index = depth? (index * range): 0;
		index += value;
	}
	if (depth != str->dimension)
		return fmte_("Subscript ~S is too few.", args, NULL);

	return Result(ret, index);
}

int array_aref_(LocalRoot local, addr pos, addr args, addr *ret)
{
	size_t index;
	Return(array_arefindex_(pos, args, &index));
	return array_get_(local, pos, index, ret);
}

int array_setf_aref_(addr pos, addr args, addr value)
{
	size_t index;
	Return(array_arefindex_(pos, args, &index));
	return array_set_(pos, index, value);
}

int array_aref_bit_(LocalRoot local, addr pos, addr args, addr *ret)
{
	int value;
	size_t index;

	Return(array_arefindex_(pos, args, &index));
	Return(array_get_bit_(pos, index, &value));
	fixnum_alloc(local, ret, (fixnum)value);

	return 0;
}

int array_setf_aref_bit_(addr pos, addr args, addr value)
{
	int check;
	size_t index;

	Return(array_arefindex_(pos, args, &index));
	Return(bit_getint_error_(value, &check));
	fixnum_heap(&value, (fixnum)check);
	return array_set_(pos, index, value);
}


/*
 *  check
 */
int array_equal_type(struct array_struct *a, enum ARRAY_TYPE type, unsigned size)
{
	if (a->type != type)
		return 0;
	if (a->type == ARRAY_TYPE_SIGNED || a->type == ARRAY_TYPE_UNSIGNED)
		return a->bytesize == size;
	else
		return 1;
}

int array_equal_dimension(addr a, addr b)
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

int array_get_element_type_(Execute ptr, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_ARRAY);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &pos);
	return type_object_(ptr, ret, pos);
}

int array_get_vector_length_(addr pos, int fill, size_t *ret)
{
	struct array_struct *str;

	if (! array_vector_p(pos)) {
		*ret = 0;
		return TypeError_(pos, VECTOR);
	}
	str = ArrayInfoStruct(pos);
	*ret = fill? str->front: str->size;
	return 0;
}

void array_get_rowlength(addr pos, size_t *ret)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	str = ArrayInfoStruct(pos);
	*ret = (str->fillpointer)? str->front: str->size;
}

