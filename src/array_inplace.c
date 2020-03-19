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
static void arrayinplace_get_general(addr mem, size_t index, struct array_value *value)
{
	arraygen_get(mem, index, &mem);
	value->value.object = mem;
	value->type = ARRAY_TYPE_T;
	value->size = 0;
}

static void arrayinplace_get_specialized(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	unsigned element;
	const byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		_fmte("Invaild array object, ~S.", pos, NULL);

	element = (unsigned)str->element;
	data = (const byte *)arrayspec_ptr(mem);
	data += element * index;
	value->type = str->type;
	value->size = str->bytesize;
	memcpy(&(value->value), data, element);
}

static void arrayinplace_get_memory(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		arrayinplace_get_general(mem, index, value);
	else
		arrayinplace_get_specialized(pos, mem, index, value);
}

static void arrayinplace_get_t(addr mem, size_t index, struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	getarray(mem, index, &mem);
	str->value.object = mem;
	str->type = ARRAY_TYPE_T;
	str->size = 0;
}

static void arrayinplace_get_bit(addr mem, size_t index, struct array_value *str)
{
	int value;

	CheckType(mem, LISPTYPE_BITVECTOR);
	bitmemory_getint(mem, index, &value);
	str->value.bit = value? 1: 0;
	str->type = ARRAY_TYPE_BIT;
	str->size = 0;
}

static void arrayinplace_get_character(addr mem, size_t index, struct array_value *str)
{
	unicode c;

	Check(! stringp(mem), "type error");
	string_getc(mem, index, &c);
	str->value.character = c;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 0;
}

static void arrayinplace_get_object(
		addr pos, addr mem, size_t index, struct array_value *value)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			arrayinplace_get_t(mem, index, value);
			break;

		case ARRAY_TYPE_BIT:
			arrayinplace_get_bit(mem, index, value);
			break;

		case ARRAY_TYPE_CHARACTER:
			arrayinplace_get_character(mem, index, value);
			break;

		default:
			_fmte("Invalid memory object ~S", pos, NULL);
			return;
	}
}

_g void arrayinplace_get(addr pos, size_t index, struct array_value *str)
{
	addr mem;

	if (arraymemory_get(pos, index, &mem, &index))
		arrayinplace_get_memory(pos, mem, index, str);
	else
		arrayinplace_get_object(pos, mem, index, str);
}


/*
 *  arrayinplace_set
 */
static void arrayinplace_set_general(
		addr mem, size_t index, const struct array_value *value)
{
	arraygen_set(mem, index, value->value.object);
}

static void arrayinplace_set_specialized(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	unsigned element;
	byte *data;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (str->type == ARRAY_TYPE_BIT)
		_fmte("Invaild array object, ~S.", pos, NULL);

	element = (unsigned)str->element;
	data = (byte *)arrayspec_ptr(mem);
	data += element * index;
	memcpy(data, &(value->value), element);
}

static void arrayinplace_set_memory(
		addr pos, addr mem, size_t index, const struct array_value *value)
{
	if (array_type(pos) == ARRAY_TYPE_T)
		arrayinplace_set_general(mem, index, value);
	else
		arrayinplace_set_specialized(pos, mem, index, value);
}

static void arrayinplace_set_t(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_VECTOR);
	setarray(mem, index, str->value.object);
}

static void arrayinplace_set_bit(addr mem, size_t index, const struct array_value *str)
{
	CheckType(mem, LISPTYPE_BITVECTOR);
	bitmemory_setint(mem, index, str->value.bit);
}

static void arrayinplace_set_character(
		addr mem, size_t index, const struct array_value *str)
{
	Check(! stringp(mem), "type error");
	string_setc(mem, index, str->value.character);
}

static void arrayinplace_set_object(
		addr pos, addr mem, size_t index, const struct array_value *str)
{
	switch (array_type(pos)) {
		case ARRAY_TYPE_T:
			arrayinplace_set_t(mem, index, str);
			break;

		case ARRAY_TYPE_BIT:
			arrayinplace_set_bit(mem, index, str);
			break;

		case ARRAY_TYPE_CHARACTER:
			arrayinplace_set_character(mem, index, str);
			break;

		default:
			_fmte("Invalid memory object ~S", pos, NULL);
			return;
	}
}

_g void arrayinplace_set(addr pos, size_t index, const struct array_value *str)
{
	addr mem;

	if (arraymemory_get(pos, index, &mem, &index))
		arrayinplace_set_memory(pos, mem, index, str);
	else
		arrayinplace_set_object(pos, mem, index, str);
}

