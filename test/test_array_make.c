#include "array_make.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

#if 0
static int test_array_macro(void)
{
	addr pos, check;

	arraygen_alloc(NULL, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc1");
	arraygen_set(pos, 0, T);
	arraygen_get(pos, 0, &check);
	test(check == T, "set_arraygen1");

	arrayspec_alloc(NULL, &pos, 100);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc1");
	arrayspec_pos(pos, &check);
	test(check == posbodyr(pos), "arrayspec_pos1");
	test(check == arrayspec_ptr(pos), "arraymemory_ptr1");

	arraysize1_alloc(NULL, &pos, 400);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize1_alloc1");
	test(GetStatusSize(pos) == LISPSIZE_BODY4, "arraysize1_alloc2");
	test(arraysize_ptr(pos) == (size_t *)posbodyr(pos), "arraysize_ptr1");

	arrayinfo_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "arrayinfo_alloc2");
	test(RefArrayInfo(pos, 0) == Nil, "RefArrayInfo1");
	SetArrayInfo(pos, 1, T);
	test(RefArrayInfo(pos, 1) == T, "SetArrayInfo1");
	GetArrayInfo(pos, 1, &pos);
	test(pos == T, "GetArrayInfo1");

	RETURN;
}

static int test_multisafe_size(void)
{
	size_t size;

	test(multisafe_size(10, 20, &size) == 0, "multisafe_size1");
	test(size == 200, "multisafe_size2");
	test(multisafe_size(SIZE_MAX, 10, &size), "multisafe_size3");

	RETURN;
}

static int test_array_settype(void)
{
	struct array_struct *str;
	addr pos, type;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_EMPTY;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "array_settype1");

	str->type = ARRAY_TYPE_T;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "array_settype2");

	str->type = ARRAY_TYPE_BIT;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_BIT, "array_settype3");

	str->type = ARRAY_TYPE_CHARACTER;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "array_settype4");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 8;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_settype5");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "array_settype6");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 16;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_settype7");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "array_settype8");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_settype9");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "array_settype10");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "array_settype11");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "array_settype12");
#endif

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_settype13");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "array_settype14");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 16;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_settype15");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "array_settype16");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 32;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_settype17");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "array_settype18");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 64;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_settype19");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "array_settype20");
#endif

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SINGLE_FLOAT, "array_settype21");

	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_DOUBLE_FLOAT, "array_settype22");

	str->type = ARRAY_TYPE_LONG_FLOAT;
	array_settype(pos);
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_LONG_FLOAT, "array_settype23");

	RETURN;
}

static int test_array_upgraded(void)
{
	struct array_struct *str;
	addr pos, type;

	GetTypeTable(&type, StandardChar);
	array_empty_alloc(NULL, &pos);
	array_upgraded(pos, type);
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_CHARACTER, "array_upgraded1");
	test(str->bytesize == 0, "array_upgraded2");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "array_upgraded3");

	type = readr("(mod 256)");
	parse_type_unsafe(&type, type);
	array_upgraded(pos, type);
	test(str->type == ARRAY_TYPE_UNSIGNED, "array_upgraded4");
	test(str->bytesize == 8, "array_upgraded5");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "array_upgraded6");

	RETURN;
}

static int test_array_element_size(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->bytesize = 0;
	str->type = ARRAY_TYPE_CHARACTER;
	array_element_size(pos);
	test(str->element == sizeoft(unicode), "array_element_size1");

	str->bytesize = 16;
	str->type = ARRAY_TYPE_SIGNED;
	array_element_size(pos);
	test(str->element == 2, "array_element_size2");

	str->bytesize = 64;
	str->type = ARRAY_TYPE_UNSIGNED;
	array_element_size(pos);
	test(str->element == 8, "array_element_size3");

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	array_element_size(pos);
	test(str->element == sizeoft(single_float), "array_element_size4");

	str->type = ARRAY_TYPE_T;
	array_element_size(pos);
	test(str->element == 0, "array_element_size5");

	RETURN;
}

static int test_array_setsize(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 10;
	SetArrayInfo(pos, ARRAY_INDEX_DIMENSION, T);
	array_setsize(NULL, pos, Nil);
	test(str->dimension == 0, "array_setsize1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_setsize2");
	test(str->size == 1, "array_setsize3");
	test(str->front == 1, "array_setsize4");
	test(str->refer == 1, "array_setsize5");

	fixnum_heap(&check, 10);
	array_setsize(NULL, pos, check);
	test(str->dimension == 1, "array_setsize6");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_setsize7");
	test(str->size == 10, "array_setsize8");
	test(str->front == 10, "array_setsize9");
	test(str->refer == 10, "array_setsize10");

	list_heap(&check, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	array_setsize(NULL, pos, check);
	test(str->dimension == 3, "array_setsize11");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_setsize12");
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_setsize13");
	test(data[1] == 20, "array_setsize14");
	test(data[2] == 30, "array_setsize15");
	test(str->size == 10*20*30, "array_setsize16");
	test(str->front == 10*20*30, "array_setsize17");
	test(str->refer == 10*20*30, "array_setsize18");

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
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_allocate_t1");
	test(lenarrayr(check) == 10, "array_allocate_t2");

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
	test(bitmemoryp(check), "array_allocate_bit1");
	bitmemory_length(check, &size);
	test(size == 0, "array_allocate_bit2");

	str->size = 1;
	array_allocate_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 1, "array_allocate_bit3");

	str->size = 100;
	array_allocate_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 100, "array_allocate_bit4");

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
	array_allocate_size(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "array_allocate_size1");
	lenbody(check, &size);
	test(size == 40, "array_allocate_size2");

	str->size = 32;
	str->element = 2;
	array_allocate_size(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	lenbody(check, &size);
	test(size == 64, "array_allocate_size3");

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
	array_allocate(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_allocate1");
	lenarray(check, &size);
	test(size == 10, "array_allocate2");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->type = ARRAY_TYPE_BIT;
	array_allocate(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(bitmemoryp(check), "array_allocate3");
	bitmemory_length(check, &size);
	test(size == 10, "array_allocate4");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->element = 4;
	str->type = ARRAY_TYPE_CHARACTER;
	array_allocate(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "array_allocate5");
	lenbody(check, &size);
	test(size == 40, "array_allocate6");

	RETURN;
}

static int test_array_equal_type(void)
{
	struct array_struct *c, *d;
	addr a, b;

	array_empty_alloc(NULL, &a);
	array_empty_alloc(NULL, &b);
	c = ArrayInfoStruct(a);
	d = ArrayInfoStruct(b);

	c->type = ARRAY_TYPE_T;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_BIT;
	d->bytesize = 10;
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type1");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 20;
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type2");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(array_equal_type(c, d->type, d->bytesize), "array_equal_type3");

	c->type = ARRAY_TYPE_UNSIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type4");

	RETURN;
}

static int test_array_memory_displaced(void)
{
	struct array_struct *str1, *str2;
	addr pos1, pos2, check;

	array_empty_alloc(NULL, &pos1);
	array_empty_alloc(NULL, &pos2);
	str1 = ArrayInfoStruct(pos1);
	str2 = ArrayInfoStruct(pos2);
	str1->size = 5;
	str2->size = 10;
	array_memory_displaced(pos1, pos2, fixnumh(0));
	GetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, &check);
	test(check == pos2, "array_memory_displaced1");
	test(str1->offset == 0, "array_memory_displaced2");

	str1->size = 10;
	str2->size = 10;
	SetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, Nil);
	array_memory_displaced(pos1, pos2, fixnumh(20));
	GetArrayInfo(pos1, ARRAY_INDEX_DISPLACED, &check);
	test(check == pos2, "array_memory_displaced3");
	test(str1->offset == 20, "array_memory_displaced4");

	RETURN;
}

static int test_array_memory_fill(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 10;
	str->front = 0;
	array_memory_fill(pos, fixnumh(10));
	test(str->front == 10, "array_memory_fill1");

	str->front = 0;
	array_memory_fill(pos, fixnumh(5));
	test(str->front == 5, "array_memory_fill2");

	RETURN;
}

static int test_array_memory_extend(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	str->fillpointer = 1;
	array_memory_extend(NULL, pos, fixnumh(5), Nil, fixnumh(0));
	test(str->front == 5, "array_memory_extend1");

	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_memory_extend2");

	RETURN;
}

static int test_array_memory_make(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_make_memory1");
	test(lenarrayr(check) == 10, "array_make_memory2");
	test(str->simple, "array_make_memory3");

	str->fillpointer = 1;
	array_make_memory(NULL, pos, Nil, fixnumh(5), Nil, fixnumh(0));
	test(str->front == 5, "array_make_memory4");
	test(! str->simple, "array_make_memory5");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_t(pos, T, 3);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	arraygen_get(pos, 0, &check);
	test(check == T, "array_initial_t1");
	arraygen_get(pos, 2, &check);
	test(check == T, "array_initial_t2");
	arraygen_get(pos, 3, &check);
	test(check != T, "array_initial_t3");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_bit(pos, fixnumh(0), 100);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	bitmemory_getint(mem, 0, &value);
	test(value == 0, "array_initial_bit1");
	bitmemory_getint(mem, 50, &value);
	test(value == 0, "array_initial_bit2");

	array_initial_bit(pos, fixnumh(1), 100);
	bitmemory_getint(mem, 0, &value);
	test(value == 1, "array_initial_bit3");
	bitmemory_getint(mem, 50, &value);
	test(value == 1, "array_initial_bit4");

	RETURN;
}

static int test_array_memset(void)
{
	struct array_struct *str;
	addr pos, mem;
	unicode u, *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 100;
	str->element = sizeoft(unicode);
	u = 66666;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_memset(pos, (const void *)&u);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (unicode *)arrayspec_ptr(mem);
	test(data[0] == 66666, "array_memset1");
	test(data[99] == 66666, "array_memset2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_character(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (unicode *)arrayspec_ptr(mem);
	test(data[0] == 66666, "array_initial_character1");
	test(data[99] == 66666, "array_initial_character2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int8_t *)arrayspec_ptr(mem);

	array_initial_signed8(pos, fixnumh(-0x80));
	test(data[0] == -0x80, "array_initial_signed8-1");
	test(data[4] == -0x80, "array_initial_signed8-2");

	array_initial_signed8(pos, fixnumh(0x7F));
	test(data[5] == 0x7F, "array_initial_signed8-3");
	test(data[9] == 0x7F, "array_initial_signed8-4");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int16_t *)arrayspec_ptr(mem);

	array_initial_signed16(pos, fixnumh(-0x8000));
	test(data[0] == -0x8000, "array_initial_signed16-1");
	test(data[4] == -0x8000, "array_initial_signed16-2");

	array_initial_signed16(pos, fixnumh(0x7FFF));
	test(data[5] == 0x7FFF, "array_initial_signed16-3");
	test(data[9] == 0x7FFF, "array_initial_signed16-4");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int32_t *)arrayspec_ptr(mem);

	array_initial_signed32(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "array_initial_signed32-1");
	test(data[9] == 0x12345678, "array_initial_signed32-2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int64_t *)arrayspec_ptr(mem);

	array_initial_signed64(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "array_initial_signed64-1");
	test(data[9] == 0x123456789ABCDEFFULL, "array_initial_signed64-2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (int8_t *)arrayspec_ptr(mem);
	array_initial_signed(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "array_initial_signed1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (int16_t *)arrayspec_ptr(mem);
	array_initial_signed(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "array_initial_signed2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (int32_t *)arrayspec_ptr(mem);
	array_initial_signed(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "array_initial_signed3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (int64_t *)arrayspec_ptr(mem);
	array_initial_signed(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "array_initial_signed4");
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint8_t *)arrayspec_ptr(mem);

	array_initial_unsigned8(pos, fixnumh(0x00));
	test(data[0] == 0x00, "array_initial_unsigned8-1");
	test(data[4] == 0x00, "array_initial_unsigned8-2");

	array_initial_unsigned8(pos, fixnumh(0xAB));
	test(data[0] == 0xAB, "array_initial_unsigned8-3");
	test(data[4] == 0xAB, "array_initial_unsigned8-4");

	array_initial_unsigned8(pos, fixnumh(0xFF));
	test(data[5] == 0xFF, "array_initial_unsigned8-5");
	test(data[9] == 0xFF, "array_initial_unsigned8-6");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint16_t *)arrayspec_ptr(mem);

	array_initial_unsigned16(pos, fixnumh(0x0000));
	test(data[0] == 0x0000, "array_initial_unsigned16-1");
	test(data[4] == 0x0000, "array_initial_unsigned16-2");

	array_initial_unsigned16(pos, fixnumh(0x8000));
	test(data[0] == 0x8000, "array_initial_unsigned16-3");
	test(data[4] == 0x8000, "array_initial_unsigned16-4");

	array_initial_unsigned16(pos, fixnumh(0xFFFF));
	test(data[5] == 0xFFFF, "array_initial_unsigned16-5");
	test(data[9] == 0xFFFF, "array_initial_unsigned16-6");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint32_t *)arrayspec_ptr(mem);

	array_initial_unsigned32(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "array_initial_unsigned32-1");
	test(data[9] == 0x12345678, "array_initial_unsigned32-2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint64_t *)arrayspec_ptr(mem);

	array_initial_unsigned64(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "array_initial_unsigned64-1");
	test(data[9] == 0x123456789ABCDEFFULL, "array_initial_unsigned64-2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (uint8_t *)arrayspec_ptr(mem);
	array_initial_unsigned(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "array_initial_unsigned1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (uint16_t *)arrayspec_ptr(mem);
	array_initial_unsigned(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "array_initial_unsigned2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (uint32_t *)arrayspec_ptr(mem);
	array_initial_unsigned(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "array_initial_unsigned3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (uint64_t *)arrayspec_ptr(mem);
	array_initial_unsigned(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "array_initial_unsigned4");
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_single(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (single_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23f, "array_initial_single1");
	test(data[99] == 10.23f, "array_initial_single2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_double(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (double_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23, "array_initial_double1");
	test(data[99] == 10.23, "array_initial_double2");

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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_long(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23L, "array_initial_long1");
	test(data[99] == 10.23L, "array_initial_long2");

	RETURN;
}

static int test_array_initial(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(check == value, "array_initial1");
	arraygen_get(mem, 9, &check);
	test(check == value, "array_initial2");

	str->type = ARRAY_TYPE_CHARACTER;
	str->element = sizeoft(unicode);
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, character_heapr(100));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(((unicode *)data)[0] == 100, "array_initial3");
	test(((unicode *)data)[9] == 100, "array_initial4");

	RETURN;
}

static int test_setf_t_index_array(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 3, T);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	arraygen_get(pos, 3, &check);
	test(check == T, "setf_t_index_array1");
	arraygen_get(pos, 2, &check);
	test(check != T, "setf_t_index_array2");

	RETURN;
}

static int test_setf_bit_index_array(void)
{
	struct array_struct *str;
	addr pos, mem;
	size_t i;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_BIT;
	str->size = 100;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_bit(pos, fixnumh(0), 100);
	for (i = 10; i < 20; i++)
		array_setindex(pos, i, fixnumh(1));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	test(! bitmemory_refint(mem, 0), "setf_bit_index_array1");
	test(! bitmemory_refint(mem, 1), "setf_bit_index_array2");
	test(! bitmemory_refint(mem, 9), "setf_bit_index_array3");
	test(  bitmemory_refint(mem, 10), "setf_bit_index_array4");
	test(  bitmemory_refint(mem, 19), "setf_bit_index_array5");
	test(! bitmemory_refint(mem, 20), "setf_bit_index_array6");

	RETURN;
}

static int test_setf_character_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 10, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (unicode *)arrayspec_ptr(mem);
	test(data[10] == 66666, "setf_character_index_array1");
	test(data[11] != 66666, "setf_character_index_array2");

	RETURN;
}

static int test_setf_signed8_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int8_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(-0x80));
	test(data[0] == -0x80, "setf_signed8_index_array1");
	test(data[4] != -0x80, "setf_signed8_index_array2");

	array_setindex(pos, 9, fixnumh(0x7F));
	test(data[5] != 0x7F, "setf_signed8_index_array3");
	test(data[9] == 0x7F, "setf_signed8_index_array4");

	RETURN;
}

static int test_setf_signed16_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int16_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(-0x8000));
	test(data[0] == -0x8000, "setf_signed16_index_array1");
	test(data[4] != -0x8000, "setf_signed16_index_array2");

	array_setindex(pos, 9, fixnumh(0x7FFF));
	test(data[5] != 0x7FFF, "setf_signed16_index_array3");
	test(data[9] == 0x7FFF, "setf_signed16_index_array4");

	RETURN;
}

static int test_setf_signed32_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int32_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "setf_signed32_index_array1");
	test(data[9] != 0x12345678, "setf_signed32_index_array2");

	RETURN;
}

#ifdef LISP_64BIT
static int test_setf_signed64_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (int64_t *)arrayspec_ptr(mem);

	array_setindex(pos, 9, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] != 0x123456789ABCDEFFULL, "setf_signed64_index_array1");
	test(data[9] == 0x123456789ABCDEFFULL, "setf_signed64_index_array2");

	RETURN;
}
#endif

static int test_setf_signed_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (int8_t *)arrayspec_ptr(mem);
	array_setindex(pos, 0, fixnumh(0x55));
	test(data1[0] == 0x55, "setf_signed_index_array1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (int16_t *)arrayspec_ptr(mem);
	array_setindex(pos, 4, fixnumh(0x55));
	test(data2[4] == 0x55, "setf_signed_index_array2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (int32_t *)arrayspec_ptr(mem);
	array_setindex(pos, 9, fixnumh(0x55));
	test(data3[9] == 0x55, "setf_signed_index_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (int64_t *)arrayspec_ptr(mem);
	array_setindex(pos, 7, fixnumh(0x55));
	test(data4[7] == 0x55, "setf_signed_index_array4");
#endif

	RETURN;
}

static int test_setf_unsigned8_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint8_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(0x00));
	array_setindex(pos, 4, fixnumh(0x44));
	test(data[0] == 0x00, "setf_unsigned8_index_array1");

	array_setindex(pos, 4, fixnumh(0xAB));
	test(data[4] == 0xAB, "setf_unsigned8_index_array2");

	array_setindex(pos, 5, fixnumh(0xEE));
	array_setindex(pos, 9, fixnumh(0xFF));
	test(data[9] == 0xFF, "setf_unsigned8_index_array3");

	RETURN;
}

static int test_setf_unsigned16_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint16_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(0x0000));
	test(data[0] == 0x0000, "setf_unsigned16_index_array1");

	array_setindex(pos, 4, fixnumh(0x8000));
	test(data[4] == 0x8000, "setf_unsigned16_index_array2");

	array_setindex(pos, 9, fixnumh(0xFFFF));
	test(data[9] == 0xFFFF, "setf_unsigned16_index_array3");

	RETURN;
}

static int test_setf_unsigned32_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint32_t *)arrayspec_ptr(mem);

	array_setindex(pos, 0, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "setf_unsigned32_index_array1");

	RETURN;
}

#ifdef LISP_64BIT
static int test_setf_unsigned64_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (uint64_t *)arrayspec_ptr(mem);

	array_setindex(pos, 9, fixnumh(0x123456789ABCDEFFULL));
	test(data[9] == 0x123456789ABCDEFFULL, "setf_unsigned64_index_array2");

	RETURN;
}
#endif

static int test_setf_unsigned_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data1 = (uint8_t *)arrayspec_ptr(mem);
	array_setindex(pos, 0, fixnumh(0x55));
	test(data1[0] == 0x55, "setf_unsigned_index_array1");

	str->bytesize = 16;
	str->element = 2;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data2 = (uint16_t *)arrayspec_ptr(mem);
	array_setindex(pos, 4, fixnumh(0x55));
	test(data2[4] == 0x55, "setf_unsigned_index_array2");

	str->bytesize = 32;
	str->element = 4;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data3 = (uint32_t *)arrayspec_ptr(mem);
	array_setindex(pos, 9, fixnumh(0x55));
	test(data3[9] == 0x55, "setf_unsigned_index_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data4 = (uint64_t *)arrayspec_ptr(mem);
	array_setindex(pos, 7, fixnumh(0x55));
	test(data4[7] == 0x55, "setf_unsigned_index_array4");
#endif

	RETURN;
}

static int test_setf_single_float_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (single_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23f, "setf_single_float_index_array1");

	RETURN;
}

static int test_setf_double_float_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 99, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (double_float *)arrayspec_ptr(mem);
	test(data[99] == 10.23, "setf_double_float_index_array1");

	RETURN;
}

static int test_setf_long_float_index_array(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(data[0] == 10.23L, "setf_long_float_index_array1");

	RETURN;
}

static int test_array_setindex(void)
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
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(check == value, "array_setindex1");
	arraygen_get(mem, 9, &check);
	test(check != value, "array_setindex2");

	str->type = ARRAY_TYPE_CHARACTER;
	str->element = sizeoft(unicode);
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 9, character_heapr(100));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	data = (long_float *)arrayspec_ptr(mem);
	test(((unicode *)data)[9] == 100, "array_setindex3");

	RETURN;
}

static int test_array_contents_setf(void)
{
	addr pos, mem, check;
	LocalRoot local;

	local = Local_Thread;
	/* make-array */
	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_upgraded(pos, check);
	array_element_size(pos);
	array_setsize(NULL, pos, readr("6"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	/* test */
	array_contents_setf(local, pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_contents_setf1");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_contents_setf2");
	arraygen_get(mem, 2, &check);
	test(check == readr("a"), "array_contents_setf3");
	arraygen_get(mem, 3, &check);
	test(check == readr("b"), "array_contents_setf4");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 5, "array_contents_setf5");
	arraygen_get(mem, 5, &check);
	test(RefFixnum(check) == 6, "array_contents_setf6");

	array_setsize(NULL, pos, readr("(2 3)"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_contents_setf(local, pos, readr("((1 2 3) (4 5 6))"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_contents_setf7");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_contents_setf8");
	arraygen_get(mem, 2, &check);
	test(RefFixnum(check) == 3, "array_contents_setf9");
	arraygen_get(mem, 3, &check);
	test(RefFixnum(check) == 4, "array_contents_setf10");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 5, "array_contents_setf11");
	arraygen_get(mem, 5, &check);
	test(RefFixnum(check) == 6, "array_contents_setf12");

	array_setsize(NULL, pos, readr("(4 2 3)"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_contents_setf(local, pos,
			readr("(((a b c) (1 2 3))"
				"((d e f) (3 1 2))"
				"((g h i) (2 3 1))"
				"((j k l) (0 0 0))))"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(check == readr("a"), "array_contents_setf13");
	arraygen_get(mem, 4, &check);
	test(RefFixnum(check) == 2, "array_contents_setf14");
	arraygen_get(mem, 14, &check);
	test(check == readr("i"), "array_contents_setf15");

	RETURN;
}

static int test_array_initial_contents(void)
{
	addr pos, mem, check;
	LocalRoot local;

	local = Local_Thread;
	/* make-array */
	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_upgraded(pos, check);
	array_element_size(pos);
	array_setsize(NULL, pos, readr("6"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	/* test */
	array_initial_contents(local, pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 1, "array_initial_contents1");
	arraygen_get(mem, 1, &check);
	test(RefFixnum(check) == 2, "array_initial_contents2");

	array_setsize(NULL, pos, Nil);
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial_contents(local, pos, readr("9"));
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &mem);
	arraygen_get(mem, 0, &check);
	test(RefFixnum(check) == 9, "array_initial_contents3");

	RETURN;
}

static int test_array_make_array(void)
{
	addr pos;

	GetTypeTable(&pos, T);
	array_make_array(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(arrayp(pos), "array_make_array1");

	RETURN;
}

static int test_array_size_contents(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	GetTypeTable(&check, T);
	array_upgraded(pos, check);
	array_element_size(pos);
	array_size_contents(NULL, pos, fixnumh(0), T);
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "array_size_contents1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_size_contents2");

	array_size_contents(NULL, pos, fixnumh(1), readr("(10 20 30)"));
	test(str->dimension == 1, "array_size_contents3");
	data = array_ptrsize(pos);
	test(data[0] == 3, "array_size_contents4");

	array_size_contents(NULL, pos, fixnumh(2), readr("((1 2) (2 3) (3 4))"));
	test(str->dimension == 2, "array_size_contents5");
	data = array_ptrsize(pos);
	test(data[0] == 3, "array_size_contents6");
	test(data[1] == 2, "array_size_contents7");

	RETURN;
}

static int test_array_ptrsize(void)
{
	addr pos;
	const size_t *data;

	array_va_heap(&pos, 0);
	test(array_ptrsize(pos) == NULL, "array_dimension_pointer1");
	array_va_heap(&pos, 10, 0);
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_ptrsize2");
	array_va_heap(&pos, 10, 20, 0);
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_ptrsize3");
	test(data[1] == 20, "array_ptrsize4");

	RETURN;
}

static int test_array_ptrwrite(void)
{
	addr pos, mem;
	byte *ptr;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	arrayspec_alloc(NULL, &mem, 1000);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, mem);
	str->element = 4;
	str->type = ARRAY_TYPE_CHARACTER;

	ptr = (byte *)array_ptrwrite(pos, 0);
	test(ptr == (byte *)posbodyr(mem), "array_ptrwrite1");
	ptr = (byte *)array_ptrwrite(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_ptrwrite2");

	ptr = (byte *)array_ptrread(pos, 0);
	test(ptr == (byte *)posbodyr(mem), "array_ptrread1");
	ptr = (byte *)array_ptrread(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_ptrread2");

	RETURN;
}

static int test_array_setsimple(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 0;
	str->displaced = 0;
	array_setsimple(str);
	test(str->simple == 1, "array_setsimple1");

	str->simple = 0;
	str->adjustable = 1;
	str->fillpointer = 0;
	str->displaced = 0;
	array_setsimple(str);
	test(str->simple == 0, "array_setsimple2");

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 1;
	str->displaced = 0;
	array_setsimple(str);
	test(str->simple == 0, "array_setsimple3");

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 0;
	str->displaced = 1;
	array_setsimple(str);
	test(str->simple == 0, "array_setsimple4");

	str->simple = 0;
	str->adjustable = 1;
	str->fillpointer = 1;
	str->displaced = 1;
	array_setsimple(str);
	test(str->simple == 0, "array_setsimple5");

	RETURN;
}

#if 0
static int test_set_element_size(void)
{
	addr pos, type;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	set_element_size(pos, str);
	test(str->element == 0, "set_element_size1");
	test(str->type == ARRAY_TYPE_T, "set_element_size2");

	GetTypeTable(&type, Bit);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 0, "set_element_size3");
	test(str->type == ARRAY_TYPE_BIT, "set_element_size4");

	GetTypeTable(&type, Character);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
	set_element_size(pos, str);
	test(str->element == sizeoft(unicode), "set_element_size5");
	test(str->type == ARRAY_TYPE_CHARACTER, "set_element_size6");

	type_signed_heap(32, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 4, "set_element_size7");
	test(str->type == ARRAY_TYPE_SIGNED, "set_element_size8");

	type_unsigned_heap(32, &type);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 4, "set_element_size9");
	test(str->type == ARRAY_TYPE_UNSIGNED, "set_element_size10");

	GetTypeTable(&type, DoubleFloat);
	SetArrayInfo(pos, ARRAY_INDEX_TYPE, type);
	set_element_size(pos, str);
	test(str->element == sizeoft(double_float), "set_element_size11");
	test(str->type == ARRAY_TYPE_DOUBLE_FLOAT, "set_element_size12");

	RETURN;
}
#endif

static int test_array_checkfill(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->fillpointer = 1;
	array_checkfill(pos, str);
	test(1, "array_checkfill1");

	str->fillpointer = 0;
	str->front = 0;
	array_checkfill(pos, str);
	test(str->front == 10, "array_checkfill2");

	RETURN;
}

static int test_array_build_alloc(void)
{
	addr pos, check;
	size_t size;

	array_va_heap(&pos, 10, 0);
	array_build_alloc(NULL, pos);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "array_build_alloc1");
	lenarray(check, &size);
	test(size == 10, "array_build_alloc2");

	RETURN;
}


/*
 *  array control
 */
static int test_array_element_type(void)
{
	addr pos, check;

	GetTypeTable(&pos, T);
	array_make_array(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_element_type(pos, &pos);
	test(pos == T, "array_element_type1");

	GetTypeTable(&pos, LongFloat);
	array_make_array(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_element_type(pos, &pos);
	GetConst(COMMON_LONG_FLOAT, &check);
	test(pos == check, "array_element_type2");

	RETURN;
}

static int test_array_vector_length(void)
{
	addr pos;

	GetTypeTable(&pos, T);
	array_make_array(&pos, fixnumh(22), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_vector_length(pos, 0) == 22, "array_vector_length1");

	RETURN;
}

static int test_array_row_length(void)
{
	addr pos, type;
	size_t size;

	GetTypeTable(&type, T);
	array_make_array(&pos, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 1, "array_rowlength1");

	array_make_array(&pos, fixnumh(22), type,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 22, "array_rowlength2");

	list_heap(&pos, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos, pos, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 54, "array_rowlength3");

	RETURN;
}

static int test_array_dimension_equal(void)
{
	addr pos1, pos2, type;

	GetTypeTable(&type, T);
	array_make_array(&pos1, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(&pos2, Nil, type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal1");

	array_make_array(&pos1, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal2");

	array_make_array(&pos1, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal3");

	array_make_array(&pos1, fixnumh(20), type, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal4");

	list_heap(&pos1, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal5");

	list_heap(&pos1, fixnumh(2), fixnumh(10), fixnumh(9), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal6");

	list_heap(&pos1, fixnumh(2), fixnumh(3), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal7");

	RETURN;
}

static void test_aref_array_memory(addr pos, size_t index, addr *ret)
{
	addr mem;
	arraymemory_get(pos, index, &mem, &index);
	array_getindex(NULL, pos, mem, index, ret);
}

static int test_aref_t_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 20;
	str->element = 0;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_setindex(pos, 10, T);

	test_aref_array_memory(pos, 9, &value);
	test(value != T, "aref_t_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(value == T, "aref_t_index_array2");

	RETURN;
}

static int test_aref_bit_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_BIT;
	str->size = 100;
	str->element = 0;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(0));
	array_setindex(pos, 10, fixnumh(1));
	array_setindex(pos, 12, fixnumh(1));
	array_setindex(pos, 15, fixnumh(1));
	array_setindex(pos, 25, fixnumh(1));
	array_setindex(pos, 31, fixnumh(1));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 0, "aref_bit_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 1, "aref_bit_index_array2");
	test_aref_array_memory(pos, 11, &value);
	test(RefFixnum(value) == 0, "aref_bit_index_array3");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 1, "aref_bit_index_array4");
	test_aref_array_memory(pos, 31, &value);
	test(RefFixnum(value) == 1, "aref_bit_index_array5");
	test_aref_array_memory(pos, 32, &value);
	test(RefFixnum(value) == 0, "aref_bit_index_array6");

	RETURN;
}

static int test_aref_character_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_CHARACTER;
	str->size = 20;
	str->element = sizeoft(unicode);
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, character_heapr(0));
	array_setindex(pos, 10, character_heapr(10));
	array_setindex(pos, 12, character_heapr(20));
	array_setindex(pos, 15, character_heapr(3333));
	test_aref_array_memory(pos, 9, &value);
	test(RefCharacter(value) == 0, "aref_character_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefCharacter(value) == 10, "aref_character_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefCharacter(value) == 20, "aref_character_index_array3");
	test_aref_array_memory(pos, 15, &value);
	test(RefCharacter(value) == 3333, "aref_character_index_array4");

	RETURN;
}

static int test_aref_signed8_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 8;
	str->element = 1;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed8_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_signed8_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_signed8_index_array3");

	RETURN;
}

static int test_aref_signed16_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 16;
	str->element = 2;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed16_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_signed16_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_signed16_index_array3");

	RETURN;
}

static int test_aref_signed32_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	str->element = 4;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed32_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_signed32_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_signed32_index_array3");

	RETURN;
}

#ifdef LISP_64BIT
static int test_aref_signed64_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	str->element = 8;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed64_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_signed64_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_signed64_index_array3");

	RETURN;
}
#endif

static int test_aref_signed_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	str->element = 4;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_signed_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_signed_index_array3");

	RETURN;
}

static int test_aref_unsigned8_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	str->element = 1;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_unsigned8_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_unsigned8_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_unsigned8_index_array3");

	RETURN;
}

static int test_aref_unsigned16_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 16;
	str->element = 2;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_unsigned16_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_unsigned16_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_unsigned16_index_array3");

	RETURN;
}

static int test_aref_unsigned32_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 32;
	str->element = 4;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_unsigned32_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_unsigned32_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_unsigned32_index_array3");

	RETURN;
}

#ifdef LISP_64BIT
static int test_aref_unsigned64_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 64;
	str->element = 8;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_signed64_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_unsigned64_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_unsigned64_index_array3");

	RETURN;
}
#endif

static int test_aref_unsigned_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 32;
	str->element = 4;
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, fixnumh(11));
	array_setindex(pos, 10, fixnumh(10));
	array_setindex(pos, 12, fixnumh(20));
	test_aref_array_memory(pos, 9, &value);
	test(RefFixnum(value) == 11, "aref_unsigned_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefFixnum(value) == 10, "aref_unsigned_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefFixnum(value) == 20, "aref_unsigned_index_array3");

	RETURN;
}

static int test_aref_single_float_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	str->element = sizeoft(single_float);
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, single_float_heapr(11.0f));
	array_setindex(pos, 10, single_float_heapr(10.0f));
	array_setindex(pos, 12, single_float_heapr(20.0f));
	test_aref_array_memory(pos, 9, &value);
	test(RefSingleFloat(value) == 11.0f, "aref_single_float_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefSingleFloat(value) == 10.0f, "aref_single_float_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefSingleFloat(value) == 20.0f, "aref_single_float_index_array3");

	RETURN;
}

static int test_aref_double_float_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
	str->element = sizeoft(double_float);
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, double_float_heapr(11.0));
	array_setindex(pos, 10, double_float_heapr(10.0));
	array_setindex(pos, 12, double_float_heapr(20.0));
	test_aref_array_memory(pos, 9, &value);
	test(RefDoubleFloat(value) == 11.0, "aref_double_float_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefDoubleFloat(value) == 10.0, "aref_double_float_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefDoubleFloat(value) == 20.0, "aref_double_float_index_array3");

	RETURN;
}

static int test_aref_long_float_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_LONG_FLOAT;
	str->element = sizeoft(long_float);
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, long_float_heapr(11.0L));
	array_setindex(pos, 10, long_float_heapr(10.0L));
	array_setindex(pos, 12, long_float_heapr(20.0L));
	test_aref_array_memory(pos, 9, &value);
	test(RefLongFloat(value) == 11.0L, "aref_long_float_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefLongFloat(value) == 10.0L, "aref_long_float_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefLongFloat(value) == 20.0L, "aref_long_float_index_array3");

	RETURN;
}

static int test_array_getindex(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_LONG_FLOAT;
	str->element = sizeoft(long_float);
	str->size = 20;
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);
	array_initial(pos, long_float_heapr(11.0L));
	array_setindex(pos, 10, long_float_heapr(10.0L));
	array_setindex(pos, 12, long_float_heapr(20.0L));
	test_aref_array_memory(pos, 9, &value);
	test(RefLongFloat(value) == 11.0L, "array_getindex1");
	test_aref_array_memory(pos, 10, &value);
	test(RefLongFloat(value) == 10.0L, "array_getindex2");
	test_aref_array_memory(pos, 12, &value);
	test(RefLongFloat(value) == 20.0L, "array_getindex3");

	RETURN;
}

static int test_array_arefindex(void)
{
	addr pos, type;
	size_t size;

	array_empty_alloc(NULL, &pos);
	GetTypeTable(&type, T);
	array_upgraded(pos, type);
	array_element_size(pos);
	array_setsize(NULL, pos, Nil);
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);

	size = array_arefindex(pos, Nil);
	test(size == 0, "array_arefindex1");

	array_setsize(NULL, pos, readr("10"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);

	size = array_arefindex(pos, readr("(0)"));
	test(size == 0, "array_arefindex2");

	size = array_arefindex(pos, readr("(7)"));
	test(size == 7, "array_arefindex3");

	array_setsize(NULL, pos, readr("(3 4)"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);

	size = array_arefindex(pos, readr("(0 2)"));
	test(size == 2, "array_arefindex4");
	size = array_arefindex(pos, readr("(0 3)"));
	test(size == 3, "array_arefindex5");

	size = array_arefindex(pos, readr("(1 2)"));
	test(size == 4+2, "array_arefindex6");
	size = array_arefindex(pos, readr("(1 3)"));
	test(size == 4+3, "array_arefindex7");

	size = array_arefindex(pos, readr("(2 2)"));
	test(size == 4+4+2, "array_arefindex8");
	size = array_arefindex(pos, readr("(2 3)"));
	test(size == 4+4+3, "array_arefindex9");

	array_setsize(NULL, pos, readr("(3 4 5)"));
	array_make_memory(NULL, pos, Nil, Nil, Nil, Nil);

	size = array_arefindex(pos, readr("(0 2 3)"));
	test(size == 5+5+3, "array_arefindex10");
	size = array_arefindex(pos, readr("(1 2 3)"));
	test(size == 20+10+3, "array_arefindex11");
	size = array_arefindex(pos, readr("(2 3 4)"));
	test(size == 40+15+4, "array_arefindex12");

	RETURN;
}


/*
 *  adjust-array
 */
static int test_array_index_dimension(void)
{
	size_t data1[10], data2[10], size;

	aatype(data1);
	aatype(data2);
	test(array_index_dimension(data1, data2, 0) == 1, "array_index_dimension1");
	data1[0] = 777;
	test(array_index_dimension(data1, data2, 1) == 777, "array_index_dimension2");
	data1[0] = 2;
	data1[1] = 3;
	data2[0] = 4;
	data2[1] = 5; /* ((x x x x x) (x x x x x) (x x x x x) (x x x x x)) */
	size = 2*(5) + 3;
	test(array_index_dimension(data1, data2, 2) == size, "array_index_dimension3");

	data1[0] = 2;
	data1[1] = 3;
	data1[2] = 4;
	data2[0] = 8;
	data2[1] = 9;
	data2[2] = 10;
	size = 2*(9*10) + 3*(10) + 4;
	test(array_index_dimension(data1, data2, 3) == size, "array_index_dimension4");

	RETURN;
}
#endif


/*
 *  main
 */
static int testbreak_array_make(void)
{
#if 0
	TestBreak(test_array_macro);
	TestBreak(test_multisafe_size);
	TestBreak(test_array_settype);
	TestBreak(test_array_upgraded);
	TestBreak(test_array_element_size);
	TestBreak(test_array_setsize);
	TestBreak(test_array_allocate_t);
	TestBreak(test_array_allocate_bit);
	TestBreak(test_array_allocate_size);
	TestBreak(test_array_allocate);
	TestBreak(test_array_equal_type);
	TestBreak(test_array_memory_displaced);
	TestBreak(test_array_memory_fill);
	TestBreak(test_array_memory_extend);
	TestBreak(test_array_memory_make);
	TestBreak(test_array_initial_t);
	TestBreak(test_array_initial_bit);
	TestBreak(test_array_memset);
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
	TestBreak(test_array_initial);
	TestBreak(test_setf_t_index_array);
	TestBreak(test_setf_bit_index_array);
	TestBreak(test_setf_character_index_array);
	TestBreak(test_setf_signed8_index_array);
	TestBreak(test_setf_signed16_index_array);
	TestBreak(test_setf_signed32_index_array);
#ifdef LISP_64BIT
	TestBreak(test_setf_signed64_index_array);
#endif
	TestBreak(test_setf_signed_index_array);
	TestBreak(test_setf_unsigned8_index_array);
	TestBreak(test_setf_unsigned16_index_array);
	TestBreak(test_setf_unsigned32_index_array);
#ifdef LISP_64BIT
	TestBreak(test_setf_unsigned64_index_array);
#endif
	TestBreak(test_setf_unsigned_index_array);
	TestBreak(test_setf_single_float_index_array);
	TestBreak(test_setf_double_float_index_array);
	TestBreak(test_setf_long_float_index_array);
	TestBreak(test_array_setindex);
	TestBreak(test_array_contents_setf);
	TestBreak(test_array_initial_contents);
	TestBreak(test_array_make_array);
	TestBreak(test_array_size_contents);

	TestBreak(test_array_setsimple);
	//	TestBreak(test_set_element_size);
	TestBreak(test_array_checkfill);
	TestBreak(test_array_build_alloc);
	/* array control */
	TestBreak(test_array_element_type);
	TestBreak(test_array_vector_length);
	TestBreak(test_array_row_length);
	TestBreak(test_array_dimension_equal);
	/* array memory */
	TestBreak(test_aref_t_index_array);
	TestBreak(test_aref_bit_index_array);
	TestBreak(test_aref_character_index_array);
	TestBreak(test_aref_signed8_index_array);
	TestBreak(test_aref_signed16_index_array);
	TestBreak(test_aref_signed32_index_array);
#ifdef LISP_64BIT
	TestBreak(test_aref_signed64_index_array);
#endif
	TestBreak(test_aref_signed_index_array);
	TestBreak(test_aref_unsigned8_index_array);
	TestBreak(test_aref_unsigned16_index_array);
	TestBreak(test_aref_unsigned32_index_array);
#ifdef LISP_64BIT
	TestBreak(test_aref_unsigned64_index_array);
#endif
	TestBreak(test_aref_unsigned_index_array);
	TestBreak(test_aref_single_float_index_array);
	TestBreak(test_aref_double_float_index_array);
	TestBreak(test_aref_long_float_index_array);
	TestBreak(test_array_getindex);
	TestBreak(test_array_arefindex);
	/* adjust-array */
	TestBreak(test_array_index_dimension);
#endif

	return 0;
}

int test_array_make(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
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
		build_readtable();
		lisp_initialize = 1;
		result = testbreak_array_make();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

