#include "array.h"
#include "array_access.h"
#include "array_inplace.h"
#include "array_value.h"
#include "bit.h"
#include "condition.h"
#include "sequence.h"
#include "strtype.h"

/*
 *  arrayinplace_get
 */
static int arrayinplace_get_general_(addr mem, size_t index, struct array_value *value)
{
	arraygen_get(mem, index, &mem);
	value->value.object = mem;
	value->type = ARRAY_TYPE_T;
	value->size = 0;

	return 0;
}

static int arrayinplace_get_specialized_safe(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	unsigned element;
	const byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		return 1;

	element = (unsigned)str->element;
	data = (const byte *)arrayspec_ptr(mem);
	data += element * index;
	value->type = str->type;
	value->size = str->bytesize;
	memcpy(&(value->value), data, element);

	return 0;
}

static int arrayinplace_get_specialized_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	if (arrayinplace_get_specialized_safe(pos, mem, index, value))
		return fmte_("Invaild array object, ~S.", pos, NULL);
	
	return 0;
}

static int arrayinplace_get_memory_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		return arrayinplace_get_general_(mem, index, value);
	else
		return arrayinplace_get_specialized_(pos, mem, index, value);
}

static int arrayinplace_get_memory_safe(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		return arrayinplace_get_general_(mem, index, value);
	else
		return arrayinplace_get_specialized_safe(pos, mem, index, value);
}

static int arrayinplace_get_t_(addr mem, size_t index, struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	getarray(mem, index, &mem);
	str->value.object = mem;
	str->type = ARRAY_TYPE_T;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_bit_(addr mem, size_t index, struct array_value *str)
{
	int value;

	CheckType(mem, LISPTYPE_BITVECTOR);
	Return(bitmemory_getint_(mem, index, &value));
	str->value.bit = value? 1: 0;
	str->type = ARRAY_TYPE_BIT;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_character_(addr mem, size_t index, struct array_value *str)
{
	unicode c;

	Check(! stringp(mem), "type error");
	Return(string_getc_(mem, index, &c));
	str->value.character = c;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 0;

	return 0;
}

static int arrayinplace_get_object_(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			return arrayinplace_get_t_(mem, index, value);

		case ARRAY_TYPE_BIT:
			return arrayinplace_get_bit_(mem, index, value);

		case ARRAY_TYPE_CHARACTER:
			return arrayinplace_get_character_(mem, index, value);

		default:
			return fmte_("Invalid memory object ~S", pos, NULL);
	}
}

static int arrayinplace_get_object_safe(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			return arrayinplace_get_t_(mem, index, value);

		case ARRAY_TYPE_BIT:
			return arrayinplace_get_bit_(mem, index, value);

		case ARRAY_TYPE_CHARACTER:
			return arrayinplace_get_character_(mem, index, value);

		default:
			return 1;
	}
}

_g int arrayinplace_get_(addr pos, size_t index, struct array_value *str)
{
	int check;
	addr mem;

	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arrayinplace_get_memory_(pos, mem, index, str);
	else
		return arrayinplace_get_object_(pos, mem, index, str);
}

_g int arrayinplace_get_safe(addr pos, size_t index, struct array_value *str)
{
	int check;
	addr mem;

	Return(arraymemory_get_safe(pos, index, &mem, &index, &check));
	if (check)
		return arrayinplace_get_memory_safe(pos, mem, index, str);
	else
		return arrayinplace_get_object_safe(pos, mem, index, str);
}


/*
 *  arrayinplace_set
 */
static int arrayinplace_set_general_(
		addr mem, size_t index, const struct array_value *value)
{
	arraygen_set(mem, index, value->value.object);
	return 0;
}

static int arrayinplace_set_specialized_(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	unsigned element;
	byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		return fmte_("Invaild array object, ~S.", pos, NULL);

	element = (unsigned)str->element;
	data = (byte *)arrayspec_ptr(mem);
	data += element * index;
	memcpy(data, &(value->value), element);

	return 0;
}

static int arrayinplace_set_memory_(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		return arrayinplace_set_general_(mem, index, value);
	else
		return arrayinplace_set_specialized_(pos, mem, index, value);
}

static int arrayinplace_set_t_(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	setarray(mem, index, str->value.object);
	return 0;
}

static int arrayinplace_set_bit_(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_BITVECTOR);
	return bitmemory_setint_(mem, index, str->value.bit);
}

static int arrayinplace_set_character_(
		addr mem, size_t index, const struct array_value *str)
{
	Check(! stringp(mem), "type error");
	return string_setc_(mem, index, str->value.character);
}

static int arrayinplace_set_object_(
		addr pos, addr mem, size_t index, const struct array_value *str)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			return arrayinplace_set_t_(mem, index, str);

		case ARRAY_TYPE_BIT:
			return arrayinplace_set_bit_(mem, index, str);

		case ARRAY_TYPE_CHARACTER:
			return arrayinplace_set_character_(mem, index, str);

		default:
			return fmte_("Invalid memory object ~S", pos, NULL);
	}
}

_g int arrayinplace_set_(addr pos, size_t index, const struct array_value *str)
{
	int check;
	addr mem;

	Return(arraymemory_get_(pos, index, &mem, &index, &check));
	if (check)
		return arrayinplace_set_memory_(pos, mem, index, str);
	else
		return arrayinplace_set_object_(pos, mem, index, str);
}

