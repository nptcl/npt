#include "array_make.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"
#include "type_parse.h"

static int test_array_set_type(void)
{
	struct array_struct *str;
	addr pos, type;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_EMPTY;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "array_set_type.1");

	str->type = ARRAY_TYPE_T;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "array_set_type.2");

	str->type = ARRAY_TYPE_BIT;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_BIT, "array_set_type.3");

	str->type = ARRAY_TYPE_CHARACTER;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "array_set_type.4");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 8;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_set_type.5");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "array_set_type.6");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 16;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_set_type.7");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "array_set_type.8");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_set_type.9");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "array_set_type.10");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_set_type.11");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "array_set_type.12");
#endif

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_set_type.13");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "array_set_type.14");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 16;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_set_type.15");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "array_set_type.16");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 32;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_set_type.17");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "array_set_type.18");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 64;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_set_type.19");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "array_set_type.20");
#endif

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SINGLE_FLOAT, "array_set_type.21");

	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_DOUBLE_FLOAT, "array_set_type.22");

	str->type = ARRAY_TYPE_LONG_FLOAT;
	array_set_type(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_LONG_FLOAT, "array_set_type.23");

	RETURN;
}

static int test_array_set_element_size(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->bytesize = 0;
	str->type = ARRAY_TYPE_CHARACTER;
	array_set_element_size(pos);
	test(str->element == sizeoft(unicode), "array_set_element_size.1");

	str->bytesize = 16;
	str->type = ARRAY_TYPE_SIGNED;
	array_set_element_size(pos);
	test(str->element == 2, "array_set_element_size.2");

	str->bytesize = 64;
	str->type = ARRAY_TYPE_UNSIGNED;
	array_set_element_size(pos);
	test(str->element == 8, "array_set_element_size.3");

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	array_set_element_size(pos);
	test(str->element == sizeoft(single_float), "array_set_element_size.4");

	str->type = ARRAY_TYPE_T;
	array_set_element_size(pos);
	test(str->element == 0, "array_set_element_size.5");

	RETURN;
}

static int test_array_set_dimension(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 10;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, T);
	array_set_dimension_(pos, Nil);
	test(str->dimension == 0, "array_set_dimension.1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_set_dimension.2");
	test(str->size == 1, "array_set_dimension.3");
	test(str->front == 1, "array_set_dimension.4");

	fixnum_heap(&check, 10);
	array_set_dimension_(pos, check);
	test(str->dimension == 1, "array_set_dimension.5");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_set_dimension.6");
	test(str->size == 10, "array_set_dimension.7");
	test(str->front == 10, "array_set_dimension.8");

	list_heap(&check, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	array_set_dimension_(pos, check);
	test(str->dimension == 3, "array_set_dimension.9");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_set_dimension.10");
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_set_dimension.11");
	test(data[1] == 20, "array_set_dimension.12");
	test(data[2] == 30, "array_set_dimension.13");
	test(str->size == 10*20*30, "array_set_dimension.14");
	test(str->front == 10*20*30, "array_set_dimension.15");

	RETURN;
}

static int test_array_allocate_t(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	array_allocate_t(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_allocate_t.1");
	test(lenarrayr(check) == 10, "array_allocate_t.2");

	RETURN;
}

static int test_array_allocate_bit(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);

	str->size = 0;
	array_allocate_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(bitmemoryp(check), "array_allocate_bit.1");
	bitmemory_length(check, &size);
	test(size == 0, "array_allocate_bit.2");

	str->size = 1;
	array_allocate_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 1, "array_allocate_bit.3");

	str->size = 100;
	array_allocate_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 100, "array_allocate_bit.4");

	RETURN;
}

static int test_array_allocate_size(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->element = 4;
	array_allocate_size_(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "array_allocate_size.1");
	lenbody(check, &size);
	test(size == 40, "array_allocate_size.2");

	str->size = 32;
	str->element = 2;
	array_allocate_size_(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	lenbody(check, &size);
	test(size == 64, "array_allocate_size.3");

	RETURN;
}

static int test_array_allocate(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->type = ARRAY_TYPE_T;
	array_allocate_(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_allocate.1");
	lenarray(check, &size);
	test(size == 10, "array_allocate.2");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->type = ARRAY_TYPE_BIT;
	array_allocate_(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(bitmemoryp(check), "array_allocate.3");
	bitmemory_length(check, &size);
	test(size == 10, "array_allocate.4");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->element = 4;
	str->type = ARRAY_TYPE_CHARACTER;
	array_allocate_(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "array_allocate.5");
	lenbody(check, &size);
	test(size == 40, "array_allocate.6");

	RETURN;
}

static int test_array_set_fillpointer(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 10;
	str->front = 0;
	array_set_fillpointer_(str, fixnumh(10));
	test(str->front == 10, "array_set_fillpointer.1");

	str->front = 0;
	array_set_fillpointer_(str, fixnumh(5));
	test(str->front == 5, "array_set_fillpointer.2");

	RETURN;
}

static int test_array_set_displaced(void)
{
	struct array_struct *str1, *str2;
	addr pos1, pos2, check;

	array_empty_alloc(NULL, &pos1);
	array_empty_alloc(NULL, &pos2);
	str1 = ArrayInfoStruct(pos1);
	str2 = ArrayInfoStruct(pos2);
	str1->size = 5;
	str2->size = 10;
	array_set_displaced_(pos1, pos2, fixnumh(0));
	GetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, &check);
	test(check == pos2, "array_set_displaced.1");
	test(str1->offset == 0, "array_set_displaced.2");

	str1->size = 10;
	str2->size = 40;
	SetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, Nil);
	array_set_displaced_(pos1, pos2, fixnumh(5));
	GetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, &check);
	test(check == pos2, "array_set_displaced.3");
	test(str1->offset == 5, "array_set_displaced.4");

	RETURN;
}

static int test_array_make_memory(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_make_memory.1");
	test(lenarrayr(check) == 10, "array_make_memory.2");
	test(str->simple, "array_make_memory.3");

	str->fillpointer = 1;
	array_make_memory_(pos, Nil, fixnumh(5), Nil, fixnumh(0));
	test(str->front == 5, "array_make_memory.4");
	test(! str->simple, "array_make_memory.5");

	RETURN;
}

static int test_array_initial_t(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_t_(pos, T, 3);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	arraygen_get(pos, 0, &check);
	test(check == T, "array_initial_t.1");
	arraygen_get(pos, 2, &check);
	test(check == T, "array_initial_t.2");
	arraygen_get(pos, 3, &check);
	test(check != T, "array_initial_t.3");

	RETURN;
}

static int test_array_initial_bit(void)
{
	int value;
	struct array_struct *str;
	addr pos, mem;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_BIT;
	str->size = 100;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_bit_(pos, fixnumh(0), 100);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	bitmemory_getint_(mem, 0, &value);
	test(value == 0, "array_initial_bit.1");
	bitmemory_getint_(mem, 50, &value);
	test(value == 0, "array_initial_bit.2");

	array_initial_bit_(pos, fixnumh(1), 100);
	bitmemory_getint_(mem, 0, &value);
	test(value == 1, "array_initial_bit.3");
	bitmemory_getint_(mem, 50, &value);
	test(value == 1, "array_initial_bit.4");

	RETURN;
}

static int test_array_initial_memset(void)
{
	struct array_struct *str;
	addr pos, mem;
	unicode c, *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 100;
	str->element = sizeoft(unicode);
	c = 66666;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_memset(pos, (const void *)&c);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (unicode *)arrayspec_ptr(mem);
	test(data[0] == 66666, "array_initial_memset.1");
	test(data[99] == 66666, "array_initial_memset.2");

	RETURN;
}

static int test_array_initial_character(void)
{
	struct array_struct *str;
	addr pos, value, mem;
	unicode *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 100;
	str->element = sizeoft(unicode);
	character_heap(&value, 66666);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_character_(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (unicode *)arrayspec_ptr(mem);
	test(data[0] == 66666, "array_initial_character.1");
	test(data[99] == 66666, "array_initial_character.2");

	RETURN;
}

static int test_array_initial_signed8(void)
{
	struct array_struct *str;
	addr pos, mem;
	int8_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 8;
	str->size = 10;
	str->element = 1;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int8_t *)arrayspec_ptr(mem);

	array_initial_signed8_(pos, fixnumh(-0x80));
	test(data[0] == -0x80, "array_initial_signed8.1");
	test(data[4] == -0x80, "array_initial_signed8.2");

	array_initial_signed8_(pos, fixnumh(0x7F));
	test(data[5] == 0x7F, "array_initial_signed8.3");
	test(data[9] == 0x7F, "array_initial_signed8.4");

	RETURN;
}

static int test_array_initial_signed16(void)
{
	struct array_struct *str;
	addr pos, mem;
	int16_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 16;
	str->size = 10;
	str->element = 2;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int16_t *)arrayspec_ptr(mem);

	array_initial_signed16_(pos, fixnumh(-0x8000));
	test(data[0] == -0x8000, "array_initial_signed16.1");
	test(data[4] == -0x8000, "array_initial_signed16.2");

	array_initial_signed16_(pos, fixnumh(0x7FFF));
	test(data[5] == 0x7FFF, "array_initial_signed16.3");
	test(data[9] == 0x7FFF, "array_initial_signed16.4");

	RETURN;
}

static int test_array_initial_signed32(void)
{
	struct array_struct *str;
	addr pos, mem;
	int32_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	str->size = 10;
	str->element = 4;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int32_t *)arrayspec_ptr(mem);

	array_initial_signed32_(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "array_initial_signed32.1");
	test(data[9] == 0x12345678, "array_initial_signed32.2");

	RETURN;
}

#ifdef LISP_64BIT
static int test_array_initial_signed64(void)
{
	struct array_struct *str;
	addr pos, mem;
	int64_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	str->size = 10;
	str->element = 8;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int64_t *)arrayspec_ptr(mem);

	array_initial_signed64_(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "array_initial_signed64.1");
	test(data[9] == 0x123456789ABCDEFFULL, "array_initial_signed64.2");

	RETURN;
}
#endif

static int test_array_initial_signed(void)
{
	struct array_struct *str;
	addr pos, mem;
	int8_t *data1;
	int16_t *data2;
	int32_t *data3;
#ifdef LISP_64BIT
	int64_t *data4;
#endif

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->size = 10;

	str->bytesize = 8;
	str->element = 1;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (int8_t *)arrayspec_ptr(mem);
	array_initial_signed_(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "array_initial_signed.1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (int16_t *)arrayspec_ptr(mem);
	array_initial_signed_(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "array_initial_signed.2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (int32_t *)arrayspec_ptr(mem);
	array_initial_signed_(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "array_initial_signed.3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (int64_t *)arrayspec_ptr(mem);
	array_initial_signed_(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "array_initial_signed.4");
#endif

	RETURN;
}

static int test_array_initial_unsigned8(void)
{
	struct array_struct *str;
	addr pos, mem;
	uint8_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	str->size = 10;
	str->element = 1;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint8_t *)arrayspec_ptr(mem);

	array_initial_unsigned8_(pos, fixnumh(0x00));
	test(data[0] == 0x00, "array_initial_unsigned8.1");
	test(data[4] == 0x00, "array_initial_unsigned8.2");

	array_initial_unsigned8_(pos, fixnumh(0xAB));
	test(data[0] == 0xAB, "array_initial_unsigned8.3");
	test(data[4] == 0xAB, "array_initial_unsigned8.4");

	array_initial_unsigned8_(pos, fixnumh(0xFF));
	test(data[5] == 0xFF, "array_initial_unsigned8.5");
	test(data[9] == 0xFF, "array_initial_unsigned8.6");

	RETURN;
}

static int test_array_initial_unsigned16(void)
{
	struct array_struct *str;
	addr pos, mem;
	uint16_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 16;
	str->size = 10;
	str->element = 2;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint16_t *)arrayspec_ptr(mem);

	array_initial_unsigned16_(pos, fixnumh(0x0000));
	test(data[0] == 0x0000, "array_initial_unsigned16.1");
	test(data[4] == 0x0000, "array_initial_unsigned16.2");

	array_initial_unsigned16_(pos, fixnumh(0x8000));
	test(data[0] == 0x8000, "array_initial_unsigned16.3");
	test(data[4] == 0x8000, "array_initial_unsigned16.4");

	array_initial_unsigned16_(pos, fixnumh(0xFFFF));
	test(data[5] == 0xFFFF, "array_initial_unsigned16.5");
	test(data[9] == 0xFFFF, "array_initial_unsigned16.6");

	RETURN;
}

static int test_array_initial_unsigned32(void)
{
	struct array_struct *str;
	addr pos, mem;
	uint32_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	str->size = 10;
	str->element = 4;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint32_t *)arrayspec_ptr(mem);

	array_initial_unsigned32_(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "array_initial_unsigned32.1");
	test(data[9] == 0x12345678, "array_initial_unsigned32.2");

	RETURN;
}

#ifdef LISP_64BIT
static int test_array_initial_unsigned64(void)
{
	struct array_struct *str;
	addr pos, mem;
	uint64_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	str->size = 10;
	str->element = 8;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint64_t *)arrayspec_ptr(mem);

	array_initial_unsigned64_(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "array_initial_unsigned64.1");
	test(data[9] == 0x123456789ABCDEFFULL, "array_initial_unsigned64.2");

	RETURN;
}
#endif

static int test_array_initial_unsigned(void)
{
	struct array_struct *str;
	addr pos, mem;
	uint8_t *data1;
	uint16_t *data2;
	uint32_t *data3;
#ifdef LISP_64BIT
	uint64_t *data4;
#endif

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->size = 10;

	str->bytesize = 8;
	str->element = 1;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (uint8_t *)arrayspec_ptr(mem);
	array_initial_unsigned_(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "array_initial_unsigned.1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (uint16_t *)arrayspec_ptr(mem);
	array_initial_unsigned_(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "array_initial_unsigned.2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (uint32_t *)arrayspec_ptr(mem);
	array_initial_unsigned_(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "array_initial_unsigned.3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (uint64_t *)arrayspec_ptr(mem);
	array_initial_unsigned_(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "array_initial_unsigned.4");
#endif

	RETURN;
}

static int test_array_initial_single(void)
{
	struct array_struct *str;
	addr pos, value, mem;
	single_float *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	str->size = 100;
	str->element = sizeoft(single_float);
	single_float_heap(&value, 10.23f);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_single_(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (single_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23f, "array_initial_single.1");
	test(data[99] == 10.23f, "array_initial_single.2");

	RETURN;
}

static int test_array_initial_double(void)
{
	struct array_struct *str;
	addr pos, value, mem;
	double_float *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
	str->size = 100;
	str->element = sizeoft(double_float);
	double_float_heap(&value, 10.23);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_double_(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (double_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23, "array_initial_double.1");
	test(data[99] == 10.23, "array_initial_double.2");

	RETURN;
}

static int test_array_initial_long(void)
{
	struct array_struct *str;
	addr pos, value, mem;
	long_float *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_LONG_FLOAT;
	str->size = 100;
	str->element = sizeoft(long_float);
	long_float_heap(&value, 10.23L);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_long_(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23L, "array_initial_long.1");
	test(data[99] == 10.23L, "array_initial_long.2");

	RETURN;
}

static int test_array_initial_value(void)
{
	struct array_struct *str;
	addr pos, value, mem, check;
	const void *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	str->element = 0;
	index_heap(&value, 10);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_value_(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(check == value, "array_initial_value.1");
	arraygen_get(mem, 9, &check);
	test(check == value, "array_initial_value.2");

	str->type = ARRAY_TYPE_CHARACTER;
	str->element = sizeoft(unicode);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_value_(pos, characterh(100));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(((unicode *)data)[0] == 100, "array_initial_value.3");
	test(((unicode *)data)[9] == 100, "array_initial_value.4");

	RETURN;
}

static int test_array_contents_setf(void)
{
	addr pos, mem, check;

	/* make-array */
	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_set_type_upgraded(pos, check);
	array_set_element_size(pos);
	array_set_dimension_(pos, readr("6"));
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	/* test */
	array_contents_setf_(pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_contents_setf.1");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_contents_setf.2");
	arraygen_get(mem, 2, &check);
	test(check == readr("a"), "array_contents_setf.3");
	arraygen_get(mem, 3, &check);
	test(check == readr("b"), "array_contents_setf.4");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 5, "array_contents_setf.5");
	arraygen_get(mem, 5, &check);
	test(RefFixnum(check) == 6, "array_contents_setf.6");

	array_set_dimension_(pos, readr("(2 3)"));
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_contents_setf_(pos, readr("((1 2 3) (4 5 6))"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_contents_setf.7");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_contents_setf.8");
	arraygen_get(mem, 2, &check);
	test(RefFixnum(check) == 3, "array_contents_setf.9");
	arraygen_get(mem, 3, &check);
	test(RefFixnum(check) == 4, "array_contents_setf.10");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 5, "array_contents_setf.11");
	arraygen_get(mem, 5, &check);
	test(RefFixnum(check) == 6, "array_contents_setf.12");

	array_set_dimension_(pos, readr("(4 2 3)"));
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_contents_setf_(pos,
			readr("(((a b c) (1 2 3))"
				"((d e f) (3 1 2))"
				"((g h i) (2 3 1))"
				"((j k l) (0 0 0))))"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(check == readr("a"), "array_contents_setf.13");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 2, "array_contents_setf.14");
	arraygen_get(mem, 14, &check);
	test(check == readr("i"), "array_contents_setf.15");

	RETURN;
}

static int test_array_initial_contents(void)
{
	addr pos, mem, check;

	/* make-array */
	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_set_type_upgraded(pos, check);
	array_set_element_size(pos);
	array_set_dimension_(pos, readr("6"));
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	/* test */
	array_initial_contents_(pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_initial_contents.1");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_initial_contents.2");

	array_set_dimension_(pos, Nil);
	array_make_memory_(pos, Nil, Nil, Nil, Nil);
	array_initial_contents_(pos, readr("9"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 9, "array_initial_contents.3");

	RETURN;
}

static int test_array_set_type_upgraded(void)
{
	struct array_struct *str;
	addr pos, type;

	GetTypeTable(&type, StandardChar);
	array_empty_alloc(NULL, &pos);
	array_set_type_upgraded(pos, type);
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_CHARACTER, "array_set_type_upgraded.1");
	test(str->bytesize == 0, "array_set_type_upgraded.2");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "array_set_type_upgraded.3");

	type = readr("(mod 256)");
	parse_type_unsafe(&type, type);
	array_set_type_upgraded(pos, type);
	test(str->type == ARRAY_TYPE_UNSIGNED, "array_set_type_upgraded.4");
	test(str->bytesize == 8, "array_set_type_upgraded.5");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_set_type_upgraded.6");

	RETURN;
}

static int test_array_make_array(void)
{
	addr pos;

	GetTypeTable(&pos, T);
	array_make_array_(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(arrayp(pos), "array_make_array.1");

	RETURN;
}

static int test_array_contents_size(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_set_type_upgraded(pos, check);
	array_set_element_size(pos);
	array_contents_size_(pos, fixnumh(0), T);
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "array_contents_size.1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_contents_size.2");

	array_contents_size_(pos, fixnumh(1), readr("(10 20 30)"));
	test(str->dimension == 1, "array_contents_size.3");
	data = array_ptrsize(pos);
	test(data[0] == 3, "array_contents_size.4");

	array_contents_size_(pos, fixnumh(2), readr("((1 2) (2 3) (3 4))"));
	test(str->dimension == 2, "array_contents_size.5");
	data = array_ptrsize(pos);
	test(data[0] == 3, "array_contents_size.6");
	test(data[1] == 2, "array_contents_size.7");

	RETURN;
}


/*
 *  array_make
 */
static int testcase_array_make(void)
{
	TestBreak(test_array_set_type);
	TestBreak(test_array_set_element_size);
	TestBreak(test_array_set_dimension);
	TestBreak(test_array_allocate_t);
	TestBreak(test_array_allocate_bit);
	TestBreak(test_array_allocate_size);
	TestBreak(test_array_allocate);
	TestBreak(test_array_set_fillpointer);
	TestBreak(test_array_set_displaced);
	TestBreak(test_array_make_memory);
	TestBreak(test_array_initial_t);
	TestBreak(test_array_initial_bit);
	TestBreak(test_array_initial_memset);
	TestBreak(test_array_initial_character);
	TestBreak(test_array_initial_signed8);
	TestBreak(test_array_initial_signed16);
	TestBreak(test_array_initial_signed32);
#ifdef LISP_64BIT
	TestBreak(test_array_initial_signed64);
#endif
	TestBreak(test_array_initial_signed);
	TestBreak(test_array_initial_unsigned8);
	TestBreak(test_array_initial_unsigned16);
	TestBreak(test_array_initial_unsigned32);
#ifdef LISP_64BIT
	TestBreak(test_array_initial_unsigned64);
#endif
	TestBreak(test_array_initial_unsigned);
	TestBreak(test_array_initial_single);
	TestBreak(test_array_initial_double);
	TestBreak(test_array_initial_long);
	TestBreak(test_array_initial_value);
	TestBreak(test_array_contents_setf);
	TestBreak(test_array_initial_contents);
	TestBreak(test_array_set_type_upgraded);
	TestBreak(test_array_make_array);
	TestBreak(test_array_contents_size);

	return 0;
}

static void testinit_array_make(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_common();
	build_reader();
}

int test_array_make(void)
{
	TITLE;
	return degrade_code(
			testinit_array_make,
			testcase_array_make);
}

