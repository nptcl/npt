#include "array.h"
#include "array_access.h"
#include "array_inplace.h"
#include "array_make.h"
#include "bit.h"
#include "compile_array.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_typedef.h"
#include "compile_write.h"
#include "condition.h"
#include "execute.h"
#include "typedef.h"

/*
 *  array-struct
 */
static int faslwrite_value_array_struct_(addr stream, const struct array_struct *str)
{
	byte v;
	size_t size;

	/* simple, adjustable, fillpointer, displaced */
	v = str->simple
		| (str->adjustable << 1U)
		| (str->fillpointer << 2U)
		| (str->displaced << 3U);
	Return(faslwrite_byte_(stream, v));
	/* type */
	v = (byte)str->type;
	Return(faslwrite_byte_(stream, v));
	/* element */
	v = (byte)str->element;
	Return(faslwrite_byte_(stream, v));
	/* bytesize */
	v = (byte)str->bytesize;
	Return(faslwrite_byte_(stream, v));
	/* size */
	size = str->size;
	Return(faslwrite_buffer_(stream, &size, IdxSize));
	/* front */
	size = str->front;
	Return(faslwrite_buffer_(stream, &size, IdxSize));
	/* dimension */
	size = str->dimension;
	Return(faslwrite_buffer_(stream, &size, IdxSize));
	/* offset */
	size = str->offset;
	Return(faslwrite_buffer_(stream, &size, IdxSize));

	return 0;
}

static int faslread_value_array_struct_(addr stream, struct array_struct *str)
{
	byte v;
	size_t size;

	/* simple, adjustable, fillpointer, displaced */
	Return(faslread_byte_(stream, &v));
	str->simple      = (v & 0x01) != 0;
	str->adjustable  = ((v >> 1U) & 0x01) != 0;
	str->fillpointer = ((v >> 2U) & 0x01) != 0;
	str->displaced   = ((v >> 3U) & 0x01) != 0;
	/* type */
	Return(faslread_byte_(stream, &v));
	str->type = (enum ARRAY_TYPE)v;
	/* element */
	Return(faslread_byte_(stream, &v));
	str->element = (unsigned)v;
	/* bytesize */
	Return(faslread_byte_(stream, &v));
	str->bytesize = (unsigned)v;
	/* size */
	Return(faslread_buffer_(stream, &size, IdxSize));
	str->size = size;
	/* front */
	Return(faslread_buffer_(stream, &size, IdxSize));
	str->front = size;
	/* dimension */
	Return(faslread_buffer_(stream, &size, IdxSize));
	str->dimension = size;
	/* offset */
	Return(faslread_buffer_(stream, &size, IdxSize));
	str->offset = size;

	return 0;
}


/*
 *  array-t
 */
static int faslwrite_value_array_t_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(pos, i, &value));
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslread_value_array_t_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	size = str->front;
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		Return(array_set_(pos, i, value));
	}

	return 0;
}


/*
 *  array-bit
 */
static int faslwrite_value_array_bit_(Execute ptr, addr stream, addr pos)
{
	int value;
	addr cons;
	struct array_struct *str;
	size_t size, i;
	LocalRoot local;
	LocalStack stack;

	str = ArrayInfoStruct(pos);
	size = str->front;
	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, size);
	for (i = 0; i < size; i++) {
		Return(array_get_bit_(pos, i, &value));
		push_bitcons(local, cons, value);
	}

	/* write */
	bitmemory_cons_local(local, &cons, cons);
	Return(faslwrite_value_(ptr, stream, cons));
	rollback_local(local, stack);

	return 0;
}

static int faslread_value_array_bit_(Execute ptr, addr stream, addr pos)
{
	addr value;

	Return(faslread_value_(ptr, stream, &value));
	CheckType(value, LISPTYPE_BITVECTOR);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, value);

	return 0;
}


/*
 *  array-memory
 */
static int faslwrite_value_array_memory_(Execute ptr, addr stream, addr pos)
{
	unsigned element;
	struct array_struct *str;
	struct array_value v;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	size = str->front;
	element = str->element;
	for (i = 0; i < size; i++) {
		Return(arrayinplace_get_(pos, i, &v));
		Return(faslwrite_buffer_(stream, &v.value.voidp, element));
	}

	return 0;
}

static int faslread_value_array_memory_(Execute ptr, addr stream, addr pos)
{
	unsigned element;
	struct array_struct *str;
	struct array_value v;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	Return(array_allocate_(NULL, pos, str));
	size = str->front;
	element = str->element;
	for (i = 0; i < size; i++) {
		Return(faslread_buffer_(stream, &v.value.voidp, element));
		Return(arrayinplace_set_(pos, i, &v));
	}

	return 0;
}


/*
 *  body
 */
static int faslwrite_value_array_body_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return faslwrite_value_array_t_(ptr, stream, pos);

		case ARRAY_TYPE_BIT:
			return faslwrite_value_array_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return faslwrite_value_array_memory_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int faslread_value_array_body_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return faslread_value_array_t_(ptr, stream, pos);

		case ARRAY_TYPE_BIT:
			return faslread_value_array_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return faslread_value_array_memory_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}


/*
 *  dimension
 */
static int faslwrite_value_array_dimension_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t *size;

	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &value);
	size = arraysize_ptr(value);
	Return(faslwrite_buffer_(stream, size, IdxSize * str->dimension));

	return 0;
}

static int faslread_value_array_dimension_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;
	size_t *size;

	str = ArrayInfoStruct(pos);
	Return(arraysize_heap_(&value, str->dimension));
	size = arraysize_ptr(value);
	Return(faslread_buffer_(stream, size, IdxSize * str->dimension));
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, value);

	return 0;
}


/*
 *  info
 */
static int faslwrite_value_array_info_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &value);
	Return(faslwrite_value_(ptr, stream, value));
	if (2 <= str->dimension) {
		Return(faslwrite_value_array_dimension_(ptr, stream, pos));
	}

	return 0;
}

static int faslread_value_array_info_(Execute ptr, addr stream, addr pos)
{
	addr value;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(faslread_value_(ptr, stream, &value));
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, value);
	if (2 <= str->dimension) {
		Return(faslread_value_array_dimension_(ptr, stream, pos));
	}

	return 0;
}


/*
 *  array-write
 */
static int faslwrite_value_array_displaced_(Execute ptr, addr stream, addr pos)
{
	struct array_struct str;

	/* make struct */
	str = *ArrayInfoStruct(pos);
	str.displaced = 0;
	str.simple = str.adjustable == 0 && str.fillpointer == 0;
	str.offset = 0;

	/* write */
	Return(faslwrite_value_array_struct_(stream, &str));
	Return(faslwrite_value_array_info_(ptr, stream, pos));
	Return(faslwrite_value_array_body_(ptr, stream, pos));

	return 0;
}

static int faslwrite_value_array_normal_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	Return(faslwrite_value_array_struct_(stream, str));
	Return(faslwrite_value_array_info_(ptr, stream, pos));
	Return(faslwrite_value_array_body_(ptr, stream, pos));

	return 0;
}

int faslwrite_value_array_(Execute ptr, addr stream, addr pos)
{
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	Return(faslwrite_type_(stream, FaslCode_array));
	str = ArrayInfoStruct(pos);
	if (str->displaced)
		return faslwrite_value_array_displaced_(ptr, stream, pos);
	else
		return faslwrite_value_array_normal_(ptr, stream, pos);
}


/*
 *  array-read
 */
int faslread_value_array_(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	struct array_struct *str;

	array_empty_heap(&pos);
	str = ArrayInfoStruct(pos);
	Return(faslread_value_array_struct_(stream, str));
	Return(faslread_value_array_info_(ptr, stream, pos));
	Return(faslread_value_array_body_(ptr, stream, pos));

	return Result(ret, pos);
}

