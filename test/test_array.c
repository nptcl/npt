#include "array.c"
#include "calltype.h"
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

static int test_array_macro(void)
{
	addr pos, check;

	arraygen_alloc(NULL, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc1");
	set_arraygen(pos, 0, T);
	get_arraygen(pos, 0, &check);
	test(check == T, "set_arraygen1");

	arrayspec_alloc(NULL, &pos, 100);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc1");
	pos_arraymemory(pos, &check);
	test(check == posbodyr(pos), "pos_arraymemory1");
	test(check == ptr_arraymemory(pos), "ptr_arraymemory1");

	arraydimension_alloc(NULL, &pos, 400);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraydimension_alloc1");
	test(GetStatusSize(pos) == LISPSIZE_BODY4, "arraydimension_alloc2");
	test(PtrArrayDimension(pos) == (size_t *)posbodyr(pos), "PtrArrayDimension1");

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

static int test_dimension_alloc(void)
{
	addr pos;

	dimension_alloc(NULL, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "dimension_alloc1");
	test(lenbodyr(pos) == IdxSize * 10, "dimension_alloc2");

	RETURN;
}

static int test_array_empty_alloc(void)
{
	addr pos;

	array_empty_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc1");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc2");

	RETURN;
}

static int test_settype_array(void)
{
	struct array_struct *str;
	addr pos, type;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->type = ARRAY_TYPE_EMPTY;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "settype_array1");

	str->type = ARRAY_TYPE_T;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_T, "settype_array2");

	str->type = ARRAY_TYPE_BIT;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_BIT, "settype_array3");

	str->type = ARRAY_TYPE_CHARACTER;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "settype_array4");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 8;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "settype_array5");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "settype_array6");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 16;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "settype_array7");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "settype_array8");

	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 32;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "settype_array9");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "settype_array10");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_SIGNED;
	str->bytesize = 64;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SIGNED_BYTE, "settype_array11");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "settype_array12");
#endif

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 8;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "settype_array13");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 8, "settype_array14");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 16;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "settype_array15");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 16, "settype_array16");

	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 32;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "settype_array17");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 32, "settype_array18");

#ifdef LISP_64BIT
	str->type = ARRAY_TYPE_UNSIGNED;
	str->bytesize = 64;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "settype_array19");
	GetArrayType(type, 0, &type);
	test(RefFixnum(type) == 64, "settype_array20");
#endif

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_SINGLE_FLOAT, "settype_array21");

	str->type = ARRAY_TYPE_DOUBLE_FLOAT;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_DOUBLE_FLOAT, "settype_array22");

	str->type = ARRAY_TYPE_LONG_FLOAT;
	settype_array(pos);
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_LONG_FLOAT, "settype_array23");

	RETURN;
}

static int test_array_alloc(void)
{
	addr pos, check;
	struct array_struct *str;

	array_alloc(NULL, &pos, 0, 5);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_alloc1");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_alloc2");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc3");
	test(str->dimension == 0, "array_alloc4");
	test(str->size == 5, "array_alloc5");
	test(str->front == 5, "array_alloc6");
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc7");

	array_alloc(NULL, &pos, 1, 5);
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_alloc8");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc9");
	test(str->dimension == 1, "array_alloc10");
	test(str->size == 5, "array_alloc11");
	test(str->front == 5, "array_alloc12");
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc13");

	array_alloc(NULL, &pos, 3, 6);
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_alloc14");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc15");
	test(str->dimension == 3, "array_alloc16");
	test(str->size == 6, "array_alloc17");
	test(str->front == 6, "array_alloc18");
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc19");

	RETURN;
}

static int test_array_alloc_stdarg(void)
{
	struct array_struct *str;
	size_t *psize;
	addr pos, check;

	array_alloc_stdarg(NULL, &pos, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_alloc_stdarg1");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_alloc_stdarg2");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "array_alloc_stdarg3");
	test(str->size == 1, "array_alloc_stdarg4");

	array_alloc_stdarg(NULL, &pos, 10, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_alloc_stdarg5");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_alloc_stdarg6");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 1, "array_alloc_stdarg7");
	test(str->size == 10, "array_alloc_stdarg8");

	array_alloc_stdarg(NULL, &pos, 2, 3, 4, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_alloc_stdarg9");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_alloc_stdarg10");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 3, "array_alloc_stdarg11");
	test(str->size == 24, "array_alloc_stdarg12");
	psize = PtrArrayDimension(check);
	test(psize[0] == 2, "array_alloc_stdarg13");
	test(psize[1] == 3, "array_alloc_stdarg14");
	test(psize[2] == 4, "array_alloc_stdarg15");

	RETURN;
}

static int test_type_make_array(void)
{
	struct array_struct *str;
	addr pos, type, value;

	GetConst(COMMON_STANDARD_CHAR, &type);
	array_empty_alloc(NULL, &pos);
	type_make_array(pos, type);
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_CHARACTER, "type_make_array1");
	test(str->bytesize == 0, "type_make_array2");
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_CHARACTER, "type_make_array3");

	GetConst(COMMON_MOD, &type);
	fixnum_heap(&value, 256);
	list_heap(&type, type, value, NULL);
	type_make_array(pos, type);
	test(str->type == ARRAY_TYPE_UNSIGNED, "type_make_array4");
	test(str->bytesize == 8, "type_make_array5");
	GetArrayInfo(pos, ARRAY_INFO_TYPE, &type);
	test(RefLispDecl(type) == LISPDECL_UNSIGNED_BYTE, "type_make_array6");

	RETURN;
}

static int test_element_make_array(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->bytesize = 0;
	str->type = ARRAY_TYPE_CHARACTER;
	element_make_array(pos);
	test(str->element == sizeoft(unicode), "element_make_array1");

	str->bytesize = 16;
	str->type = ARRAY_TYPE_SIGNED;
	element_make_array(pos);
	test(str->element == 2, "element_make_array2");

	str->bytesize = 64;
	str->type = ARRAY_TYPE_UNSIGNED;
	element_make_array(pos);
	test(str->element == 8, "element_make_array3");

	str->type = ARRAY_TYPE_SINGLE_FLOAT;
	element_make_array(pos);
	test(str->element == sizeoft(single_float), "element_make_array4");

	str->type = ARRAY_TYPE_T;
	element_make_array(pos);
	test(str->element == 0, "element_make_array5");

	RETURN;
}

static int test_dimension_make_array(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 10;
	SetArrayInfo(pos, ARRAY_INFO_DIMENSION, T);
	dimension_make_array(NULL, pos, Nil);
	test(str->dimension == 0, "dimension_make_array1");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "dimension_make_array2");
	test(str->size == 1, "dimension_make_array3");
	test(str->front == 1, "dimension_make_array4");
	test(str->refer == 1, "dimension_make_array5");

	fixnum_heap(&check, 10);
	dimension_make_array(NULL, pos, check);
	test(str->dimension == 1, "dimension_make_array6");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "dimension_make_array7");
	test(str->size == 10, "dimension_make_array8");
	test(str->front == 10, "dimension_make_array9");
	test(str->refer == 10, "dimension_make_array10");

	list_heap(&check, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	dimension_make_array(NULL, pos, check);
	test(str->dimension == 3, "dimension_make_array11");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "dimension_make_array12");
	data = array_dimension_pointer(pos);
	test(data[0] == 10, "dimension_make_array13");
	test(data[1] == 20, "dimension_make_array14");
	test(data[2] == 30, "dimension_make_array15");
	test(str->size == 10*20*30, "dimension_make_array16");
	test(str->front == 10*20*30, "dimension_make_array17");
	test(str->refer == 10*20*30, "dimension_make_array18");

	RETURN;
}

static int test_allocate_element_t(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	allocate_element_t(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "allocate_element_t1");
	test(lenarrayr(check) == 10, "allocate_element_t2");

	RETURN;
}

static int test_allocate_element_bit(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);

	str->size = 0;
	allocate_element_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(bitmemoryp(check), "allocate_element_bit1");
	bitmemory_length(check, &size);
	test(size == 0, "allocate_element_bit2");

	str->size = 1;
	allocate_element_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 1, "allocate_element_bit3");

	str->size = 100;
	allocate_element_bit(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	bitmemory_length(check, &size);
	test(size == 100, "allocate_element_bit4");

	RETURN;
}

static int test_allocate_element_size(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->element = 4;
	allocate_element_size(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "allocate_element_size1");
	lenbody(check, &size);
	test(size == 40, "allocate_element_size2");

	str->size = 32;
	str->element = 2;
	allocate_element_size(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	lenbody(check, &size);
	test(size == 64, "allocate_element_size3");

	RETURN;
}

static int test_allocate_element_array(void)
{
	addr pos, check;
	struct array_struct *str;
	size_t size;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->type = ARRAY_TYPE_T;
	allocate_element_array(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "allocate_element_array1");
	lenarray(check, &size);
	test(size == 10, "allocate_element_array2");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->type = ARRAY_TYPE_BIT;
	allocate_element_array(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(bitmemoryp(check), "allocate_element_array3");
	bitmemory_length(check, &size);
	test(size == 10, "allocate_element_array4");

	str = ArrayInfoStruct(pos);
	str->size = 10;
	str->element = 4;
	str->type = ARRAY_TYPE_CHARACTER;
	allocate_element_array(NULL, pos, str);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_SPECIALIZED, "allocate_element_array5");
	lenbody(check, &size);
	test(size == 40, "allocate_element_array6");

	RETURN;
}

static int test_type_equal_make_array(void)
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
	test(! type_equal_make_array(c, d->type, d->bytesize), "type_equal_make_array1");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 20;
	test(! type_equal_make_array(c, d->type, d->bytesize), "type_equal_make_array2");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(type_equal_make_array(c, d->type, d->bytesize), "type_equal_make_array3");

	c->type = ARRAY_TYPE_UNSIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(! type_equal_make_array(c, d->type, d->bytesize), "type_equal_make_array4");

	RETURN;
}

static int test_displaced_make_array(void)
{
	struct array_struct *str1, *str2;
	addr pos1, pos2, check;

	array_empty_alloc(NULL, &pos1);
	array_empty_alloc(NULL, &pos2);
	str1 = ArrayInfoStruct(pos1);
	str2 = ArrayInfoStruct(pos2);
	str1->size = 5;
	str2->size = 10;
	displaced_make_array(pos1, pos2, fixnumh(0));
	GetArrayInfo(pos1, ARRAY_INFO_DISPLACED, &check);
	test(check == pos2, "displaced_make_array1");
	test(str1->offset == 0, "displaced_make_array2");

	str1->size = 10;
	str2->size = 10;
	SetArrayInfo(pos1, ARRAY_INFO_DISPLACED, Nil);
	displaced_make_array(pos1, pos2, fixnumh(20));
	GetArrayInfo(pos1, ARRAY_INFO_DISPLACED, &check);
	test(check == pos2, "displaced_make_array3");
	test(str1->offset == 20, "displaced_make_array4");

	RETURN;
}

static int test_fill_make_array(void)
{
	struct array_struct *str;
	addr pos;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 10;
	str->front = 0;
	fill_make_array(pos, fixnumh(10));
	test(str->front == 10, "fill_make_array1");

	str->front = 0;
	fill_make_array(pos, fixnumh(5));
	test(str->front == 5, "fill_make_array2");

	RETURN;
}

static int test_extended_make_array(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	str->fillpointer = 1;
	extended_make_array(NULL, pos, fixnumh(5), Nil, fixnumh(0));
	test(str->front == 5, "extended_make_array1");

	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "extended_make_array2");

	RETURN;
}

static int test_allocate_make_array(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "allocate_make_array1");
	test(lenarrayr(check) == 10, "allocate_make_array2");
	test(str->simple, "allocate_make_array3");

	str->fillpointer = 1;
	allocate_make_array(NULL, pos, Nil, fixnumh(5), Nil, fixnumh(0));
	test(str->front == 5, "allocate_make_array4");
	test(! str->simple, "allocate_make_array5");

	RETURN;
}

static int test_initial_t_make_array(void)
{
	struct array_struct *str;
	addr pos, check;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_T;
	str->size = 10;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_t_make_array(pos, T, 3);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	get_arraygen(pos, 0, &check);
	test(check == T, "initial_t_make_array1");
	get_arraygen(pos, 2, &check);
	test(check == T, "initial_t_make_array2");
	get_arraygen(pos, 3, &check);
	test(check != T, "initial_t_make_array3");

	RETURN;
}

static int test_initial_bit_make_array(void)
{
	int value;
	struct array_struct *str;
	addr pos, mem;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_BIT;
	str->size = 100;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_bit_make_array(pos, fixnumh(0), 100);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	bitmemory_getint(mem, 0, &value);
	test(value == 0, "initial_bit_make_array1");
	bitmemory_getint(mem, 50, &value);
	test(value == 0, "initial_bit_make_array2");

	initial_bit_make_array(pos, fixnumh(1), 100);
	bitmemory_getint(mem, 0, &value);
	test(value == 1, "initial_bit_make_array3");
	bitmemory_getint(mem, 50, &value);
	test(value == 1, "initial_bit_make_array4");

	RETURN;
}

static int test_memset_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	memset_make_array(pos, (const void *)&u);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (unicode *)ptr_arraymemory(mem);
	test(data[0] == 66666, "memset_make_array1");
	test(data[99] == 66666, "memset_make_array2");

	RETURN;
}

static int test_initial_character_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_character_make_array(pos, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (unicode *)ptr_arraymemory(mem);
	test(data[0] == 66666, "initial_character_make_array1");
	test(data[99] == 66666, "initial_character_make_array2");

	RETURN;
}

static int test_initial_signed8_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int8_t *)ptr_arraymemory(mem);

	initial_signed8_make_array(pos, fixnumh(-0x80));
	test(data[0] == -0x80, "initial_signed8_make_array1");
	test(data[4] == -0x80, "initial_signed8_make_array2");

	initial_signed8_make_array(pos, fixnumh(0x7F));
	test(data[5] == 0x7F, "initial_signed8_make_array3");
	test(data[9] == 0x7F, "initial_signed8_make_array4");

	RETURN;
}

static int test_initial_signed16_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int16_t *)ptr_arraymemory(mem);

	initial_signed16_make_array(pos, fixnumh(-0x8000));
	test(data[0] == -0x8000, "initial_signed16_make_array1");
	test(data[4] == -0x8000, "initial_signed16_make_array2");

	initial_signed16_make_array(pos, fixnumh(0x7FFF));
	test(data[5] == 0x7FFF, "initial_signed16_make_array3");
	test(data[9] == 0x7FFF, "initial_signed16_make_array4");

	RETURN;
}

static int test_initial_signed32_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int32_t *)ptr_arraymemory(mem);

	initial_signed32_make_array(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "initial_signed32_make_array1");
	test(data[9] == 0x12345678, "initial_signed32_make_array2");

	RETURN;
}

#ifdef LISP_64BIT
static int test_initial_signed64_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int64_t *)ptr_arraymemory(mem);

	initial_signed64_make_array(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "initial_signed64_make_array1");
	test(data[9] == 0x123456789ABCDEFFULL, "initial_signed64_make_array2");

	RETURN;
}
#endif

static int test_initial_signed_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data1 = (int8_t *)ptr_arraymemory(mem);
	initial_signed_make_array(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "initial_signed_make_array1");

	str->bytesize = 16;
	str->element = 2;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data2 = (int16_t *)ptr_arraymemory(mem);
	initial_signed_make_array(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "initial_signed_make_array2");

	str->bytesize = 32;
	str->element = 4;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data3 = (int32_t *)ptr_arraymemory(mem);
	initial_signed_make_array(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "initial_signed_make_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data4 = (int64_t *)ptr_arraymemory(mem);
	initial_signed_make_array(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "initial_signed_make_array4");
#endif

	RETURN;
}

static int test_initial_unsigned8_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint8_t *)ptr_arraymemory(mem);

	initial_unsigned8_make_array(pos, fixnumh(0x00));
	test(data[0] == 0x00, "initial_unsigned8_make_array1");
	test(data[4] == 0x00, "initial_unsigned8_make_array2");

	initial_unsigned8_make_array(pos, fixnumh(0xAB));
	test(data[0] == 0xAB, "initial_unsigned8_make_array3");
	test(data[4] == 0xAB, "initial_unsigned8_make_array4");

	initial_unsigned8_make_array(pos, fixnumh(0xFF));
	test(data[5] == 0xFF, "initial_unsigned8_make_array5");
	test(data[9] == 0xFF, "initial_unsigned8_make_array6");

	RETURN;
}

static int test_initial_unsigned16_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint16_t *)ptr_arraymemory(mem);

	initial_unsigned16_make_array(pos, fixnumh(0x0000));
	test(data[0] == 0x0000, "initial_unsigned16_make_array1");
	test(data[4] == 0x0000, "initial_unsigned16_make_array2");

	initial_unsigned16_make_array(pos, fixnumh(0x8000));
	test(data[0] == 0x8000, "initial_unsigned16_make_array3");
	test(data[4] == 0x8000, "initial_unsigned16_make_array4");

	initial_unsigned16_make_array(pos, fixnumh(0xFFFF));
	test(data[5] == 0xFFFF, "initial_unsigned16_make_array5");
	test(data[9] == 0xFFFF, "initial_unsigned16_make_array6");

	RETURN;
}

static int test_initial_unsigned32_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint32_t *)ptr_arraymemory(mem);

	initial_unsigned32_make_array(pos, fixnumh(0x12345678));
	test(data[0] == 0x12345678, "initial_unsigned32_make_array1");
	test(data[9] == 0x12345678, "initial_unsigned32_make_array2");

	RETURN;
}

#ifdef LISP_64BIT
static int test_initial_unsigned64_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint64_t *)ptr_arraymemory(mem);

	initial_unsigned64_make_array(pos, fixnumh(0x123456789ABCDEFFULL));
	test(data[0] == 0x123456789ABCDEFFULL, "initial_unsigned64_make_array1");
	test(data[9] == 0x123456789ABCDEFFULL, "initial_unsigned64_make_array2");

	RETURN;
}
#endif

static int test_initial_unsigned_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data1 = (uint8_t *)ptr_arraymemory(mem);
	initial_unsigned_make_array(pos, fixnumh(0x55));
	test(data1[0] == 0x55, "initial_unsigned_make_array1");

	str->bytesize = 16;
	str->element = 2;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data2 = (uint16_t *)ptr_arraymemory(mem);
	initial_unsigned_make_array(pos, fixnumh(0x55));
	test(data2[4] == 0x55, "initial_unsigned_make_array2");

	str->bytesize = 32;
	str->element = 4;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data3 = (uint32_t *)ptr_arraymemory(mem);
	initial_unsigned_make_array(pos, fixnumh(0x55));
	test(data3[9] == 0x55, "initial_unsigned_make_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data4 = (uint64_t *)ptr_arraymemory(mem);
	initial_unsigned_make_array(pos, fixnumh(0x55));
	test(data4[7] == 0x55, "initial_unsigned_make_array4");
#endif

	RETURN;
}

static int test_initial_single_float_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_single_float_make_array(pos, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (single_float *)ptr_arraymemory(mem);
	test(data[0] == 10.23f, "initial_single_float_make_array1");
	test(data[99] == 10.23f, "initial_single_float_make_array2");

	RETURN;
}

static int test_initial_double_float_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_double_float_make_array(pos, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (double_float *)ptr_arraymemory(mem);
	test(data[0] == 10.23, "initial_double_float_make_array1");
	test(data[99] == 10.23, "initial_double_float_make_array2");

	RETURN;
}

static int test_initial_long_float_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_long_float_make_array(pos, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (long_float *)ptr_arraymemory(mem);
	test(data[0] == 10.23L, "initial_long_float_make_array1");
	test(data[99] == 10.23L, "initial_long_float_make_array2");

	RETURN;
}

static int test_initial_element_make_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(check == value, "initial_element_make_array1");
	get_arraygen(mem, 9, &check);
	test(check == value, "initial_element_make_array2");

	str->type = ARRAY_TYPE_CHARACTER;
	str->element = sizeoft(unicode);
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, character_heapr(100));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (long_float *)ptr_arraymemory(mem);
	test(((unicode *)data)[0] == 100, "initial_element_make_array3");
	test(((unicode *)data)[9] == 100, "initial_element_make_array4");

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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 3, T);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &pos);
	get_arraygen(pos, 3, &check);
	test(check == T, "setf_t_index_array1");
	get_arraygen(pos, 2, &check);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_bit_make_array(pos, fixnumh(0), 100);
	for (i = 10; i < 20; i++)
		set_index_array(pos, i, fixnumh(1));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 10, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (unicode *)ptr_arraymemory(mem);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int8_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(-0x80));
	test(data[0] == -0x80, "setf_signed8_index_array1");
	test(data[4] != -0x80, "setf_signed8_index_array2");

	set_index_array(pos, 9, fixnumh(0x7F));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int16_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(-0x8000));
	test(data[0] == -0x8000, "setf_signed16_index_array1");
	test(data[4] != -0x8000, "setf_signed16_index_array2");

	set_index_array(pos, 9, fixnumh(0x7FFF));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int32_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(0x12345678));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (int64_t *)ptr_arraymemory(mem);

	set_index_array(pos, 9, fixnumh(0x123456789ABCDEFFULL));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data1 = (int8_t *)ptr_arraymemory(mem);
	set_index_array(pos, 0, fixnumh(0x55));
	test(data1[0] == 0x55, "setf_signed_index_array1");

	str->bytesize = 16;
	str->element = 2;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data2 = (int16_t *)ptr_arraymemory(mem);
	set_index_array(pos, 4, fixnumh(0x55));
	test(data2[4] == 0x55, "setf_signed_index_array2");

	str->bytesize = 32;
	str->element = 4;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data3 = (int32_t *)ptr_arraymemory(mem);
	set_index_array(pos, 9, fixnumh(0x55));
	test(data3[9] == 0x55, "setf_signed_index_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data4 = (int64_t *)ptr_arraymemory(mem);
	set_index_array(pos, 7, fixnumh(0x55));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint8_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(0x00));
	set_index_array(pos, 4, fixnumh(0x44));
	test(data[0] == 0x00, "setf_unsigned8_index_array1");

	set_index_array(pos, 4, fixnumh(0xAB));
	test(data[4] == 0xAB, "setf_unsigned8_index_array2");

	set_index_array(pos, 5, fixnumh(0xEE));
	set_index_array(pos, 9, fixnumh(0xFF));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint16_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(0x0000));
	test(data[0] == 0x0000, "setf_unsigned16_index_array1");

	set_index_array(pos, 4, fixnumh(0x8000));
	test(data[4] == 0x8000, "setf_unsigned16_index_array2");

	set_index_array(pos, 9, fixnumh(0xFFFF));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint32_t *)ptr_arraymemory(mem);

	set_index_array(pos, 0, fixnumh(0x12345678));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (uint64_t *)ptr_arraymemory(mem);

	set_index_array(pos, 9, fixnumh(0x123456789ABCDEFFULL));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data1 = (uint8_t *)ptr_arraymemory(mem);
	set_index_array(pos, 0, fixnumh(0x55));
	test(data1[0] == 0x55, "setf_unsigned_index_array1");

	str->bytesize = 16;
	str->element = 2;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data2 = (uint16_t *)ptr_arraymemory(mem);
	set_index_array(pos, 4, fixnumh(0x55));
	test(data2[4] == 0x55, "setf_unsigned_index_array2");

	str->bytesize = 32;
	str->element = 4;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data3 = (uint32_t *)ptr_arraymemory(mem);
	set_index_array(pos, 9, fixnumh(0x55));
	test(data3[9] == 0x55, "setf_unsigned_index_array3");

#ifdef LISP_64BIT
	str->bytesize = 64;
	str->element = 8;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data4 = (uint64_t *)ptr_arraymemory(mem);
	set_index_array(pos, 7, fixnumh(0x55));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (single_float *)ptr_arraymemory(mem);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 99, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (double_float *)ptr_arraymemory(mem);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (long_float *)ptr_arraymemory(mem);
	test(data[0] == 10.23L, "setf_long_float_index_array1");

	RETURN;
}

static int test_set_index_array(void)
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 0, value);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(check == value, "set_index_array1");
	get_arraygen(mem, 9, &check);
	test(check != value, "set_index_array2");

	str->type = ARRAY_TYPE_CHARACTER;
	str->element = sizeoft(unicode);
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 9, character_heapr(100));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	data = (long_float *)ptr_arraymemory(mem);
	test(((unicode *)data)[9] == 100, "set_index_array1");

	RETURN;
}

static int test_setf_contents_array(void)
{
	addr pos, mem, check;
	LocalRoot local;

	local = Local_Thread;
	/* make-array */
	array_empty_alloc(NULL, &pos);
	type_make_array(pos, T);
	element_make_array(pos);
	dimension_make_array(NULL, pos, readr("6"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	/* test */
	setf_contents_array(local, pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(RefFixnum(check) == 1, "setf_contents_array1");
	get_arraygen(mem, 1, &check);
	test(RefFixnum(check) == 2, "setf_contents_array2");
	get_arraygen(mem, 2, &check);
	test(check == readr("a"), "setf_contents_array3");
	get_arraygen(mem, 3, &check);
	test(check == readr("b"), "setf_contents_array4");
	get_arraygen(mem, 4, &check);
	test(RefFixnum(check) == 5, "setf_contents_array5");
	get_arraygen(mem, 5, &check);
	test(RefFixnum(check) == 6, "setf_contents_array6");

	dimension_make_array(NULL, pos, readr("(2 3)"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	setf_contents_array(local, pos, readr("((1 2 3) (4 5 6))"));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(RefFixnum(check) == 1, "setf_contents_array7");
	get_arraygen(mem, 1, &check);
	test(RefFixnum(check) == 2, "setf_contents_array8");
	get_arraygen(mem, 2, &check);
	test(RefFixnum(check) == 3, "setf_contents_array9");
	get_arraygen(mem, 3, &check);
	test(RefFixnum(check) == 4, "setf_contents_array10");
	get_arraygen(mem, 4, &check);
	test(RefFixnum(check) == 5, "setf_contents_array11");
	get_arraygen(mem, 5, &check);
	test(RefFixnum(check) == 6, "setf_contents_array12");

	dimension_make_array(NULL, pos, readr("(4 2 3)"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	setf_contents_array(local, pos,
			readr("(((a b c) (1 2 3))"
				"((d e f) (3 1 2))"
				"((g h i) (2 3 1))"
				"((j k l) (0 0 0))))"));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(check == readr("a"), "setf_contents_array13");
	get_arraygen(mem, 4, &check);
	test(RefFixnum(check) == 2, "setf_contents_array14");
	get_arraygen(mem, 14, &check);
	test(check == readr("i"), "setf_contents_array15");

	RETURN;
}

static int test_contents_element_make_array(void)
{
	addr pos, mem, check;
	LocalRoot local;

	local = Local_Thread;
	/* make-array */
	array_empty_alloc(NULL, &pos);
	type_make_array(pos, T);
	element_make_array(pos);
	dimension_make_array(NULL, pos, readr("6"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	/* test */
	contents_element_make_array(local, pos, readr("(1 2 a b 5 6)"));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(RefFixnum(check) == 1, "contents_element_make_array1");
	get_arraygen(mem, 1, &check);
	test(RefFixnum(check) == 2, "contents_element_make_array2");

	dimension_make_array(NULL, pos, Nil);
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	contents_element_make_array(local, pos, readr("9"));
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &mem);
	get_arraygen(mem, 0, &check);
	test(RefFixnum(check) == 9, "contents_element_make_array3");

	RETURN;
}

static int test_array_make_array(void)
{
	addr pos;

	array_make_array(NULL, &pos, Nil, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(arrayp(pos), "array_make_array1");

	RETURN;
}

static int test_dimension_array_contents(void)
{
	struct array_struct *str;
	addr pos, check;
	const size_t *data;

	array_empty_alloc(NULL, &pos);
	type_make_array(pos, T);
	element_make_array(pos);
	dimension_array_contents(NULL, pos, fixnumh(0), T);
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "dimension_array_contents1");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "dimension_array_contents2");

	dimension_array_contents(NULL, pos, fixnumh(1), readr("(10 20 30)"));
	test(str->dimension == 1, "dimension_array_contents3");
	data = array_dimension_pointer(pos);
	test(data[0] == 3, "dimension_array_contents4");

	dimension_array_contents(NULL, pos, fixnumh(2), readr("((1 2) (2 3) (3 4))"));
	test(str->dimension == 2, "dimension_array_contents5");
	data = array_dimension_pointer(pos);
	test(data[0] == 3, "dimension_array_contents6");
	test(data[1] == 2, "dimension_array_contents7");

	RETURN;
}

static int test_arrayp(void)
{
	addr pos;

	array_alloc_stdarg(NULL, &pos, 2, 3, 4, 0);
	test(arrayp(pos), "arrayp1");
	test(! arrayp(Nil), "arrayp2");

	RETURN;
}

static int test_array_simple_p(void)
{
	addr pos;
	struct array_struct *str;

	array_alloc_stdarg(NULL, &pos, 2, 3, 4, 0);
	str = ArrayInfoStruct(pos);
	str->simple = 1;
	test(array_simple_p(pos), "array_simple_p1");
	str->simple = 0;
	test(! array_simple_p(pos), "array_simple_p2");

	RETURN;
}

static int test_array_vector_p(void)
{
	addr pos;

	array_alloc_stdarg(NULL, &pos, 0);
	test(! array_vector_p(pos), "array_vector_p1");
	array_alloc_stdarg(NULL, &pos, 10, 0);
	test(array_vector_p(pos), "array_vector_p2");
	array_alloc_stdarg(NULL, &pos, 10, 20, 0);
	test(! array_vector_p(pos), "array_vector_p3");

	RETURN;
}

static int test_array_size_vector_p(void)
{
	struct array_struct *str;
	addr pos;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	test(array_size_vector_p(pos, 10), "array_size_vector_p1");
	test(! array_size_vector_p(pos, 11), "array_size_vector_p2");

	array_alloc_stdarg(NULL, &pos, 10, 20, 0);
	test(! array_size_vector_p(pos, 10), "array_size_vector_p3");

	array_alloc_stdarg(NULL, &pos, 0);
	test(! array_size_vector_p(pos, 0), "array_size_vector_p4");

	array_alloc_stdarg(NULL, &pos, 10, 20, 0);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 44;
	test(array_size_vector_p(pos, 44), "array_size_vector_p5");

	RETURN;
}

static int test_array_dimension_pointer(void)
{
	addr pos;
	const size_t *data;

	array_alloc_stdarg(NULL, &pos, 0);
	test(array_dimension_pointer(pos) == NULL, "array_dimension_pointer1");
	array_alloc_stdarg(NULL, &pos, 10, 0);
	data = array_dimension_pointer(pos);
	test(data[0] == 10, "array_dimension_pointer2");
	array_alloc_stdarg(NULL, &pos, 10, 20, 0);
	data = array_dimension_pointer(pos);
	test(data[0] == 10, "array_dimension_pointer3");
	test(data[1] == 20, "array_dimension_pointer4");

	RETURN;
}

static int test_array_write_pointer(void)
{
	addr pos, mem;
	byte *ptr;
	struct array_struct *str;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	str = ArrayInfoStruct(pos);
	arrayspec_alloc(NULL, &mem, 1000);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, mem);
	str->element = 4;
	str->type = ARRAY_TYPE_CHARACTER;

	ptr = (byte *)array_write_pointer(pos, 0);
	test(ptr == (byte *)posbodyr(mem), "array_write_pointer1");
	ptr = (byte *)array_write_pointer(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_write_pointer2");

	ptr = (byte *)array_read_pointer(pos, 0);
	test(ptr == (byte *)posbodyr(mem), "array_read_pointer1");
	ptr = (byte *)array_read_pointer(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_read_pointer2");

	RETURN;
}

static int test_set_simple_array(void)
{
	addr pos;
	struct array_struct *str;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	str = ArrayInfoStruct(pos);

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 0;
	str->displaced = 0;
	set_simple_array(str);
	test(str->simple == 1, "set_simple_array1");

	str->simple = 0;
	str->adjustable = 1;
	str->fillpointer = 0;
	str->displaced = 0;
	set_simple_array(str);
	test(str->simple == 0, "set_simple_array2");

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 1;
	str->displaced = 0;
	set_simple_array(str);
	test(str->simple == 0, "set_simple_array3");

	str->simple = 0;
	str->adjustable = 0;
	str->fillpointer = 0;
	str->displaced = 1;
	set_simple_array(str);
	test(str->simple == 0, "set_simple_array4");

	str->simple = 0;
	str->adjustable = 1;
	str->fillpointer = 1;
	str->displaced = 1;
	set_simple_array(str);
	test(str->simple == 0, "set_simple_array5");

	RETURN;
}

static int test_set_element_size(void)
{
	addr pos, type;
	struct array_struct *str;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	str = ArrayInfoStruct(pos);
	set_element_size(pos, str);
	test(str->element == 0, "set_element_size1");
	test(str->type == ARRAY_TYPE_T, "set_element_size2");

	type_empty(NULL, LISPDECL_BIT, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 0, "set_element_size3");
	test(str->type == ARRAY_TYPE_BIT, "set_element_size4");

	type_empty(NULL, LISPDECL_CHARACTER, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
	set_element_size(pos, str);
	test(str->element == sizeoft(unicode), "set_element_size5");
	test(str->type == ARRAY_TYPE_CHARACTER, "set_element_size6");

	type_object1(NULL, LISPDECL_SIGNED_BYTE, fixnum_heapr(32), &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 4, "set_element_size7");
	test(str->type == ARRAY_TYPE_SIGNED, "set_element_size8");

	type_object1(NULL, LISPDECL_UNSIGNED_BYTE, fixnum_heapr(23), &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
	set_element_size(pos, str);
	test(str->element == 4, "set_element_size9");
	test(str->type == ARRAY_TYPE_UNSIGNED, "set_element_size10");

	type_aster4(NULL, LISPDECL_DOUBLE_FLOAT, &type);
	SetArrayInfo(pos, ARRAY_INFO_TYPE, type);
	set_element_size(pos, str);
	test(str->element == sizeoft(double_float), "set_element_size11");
	test(str->type == ARRAY_TYPE_DOUBLE_FLOAT, "set_element_size12");

	RETURN;
}

static int test_check_fillpointer(void)
{
	addr pos;
	struct array_struct *str;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	str = ArrayInfoStruct(pos);
	str->fillpointer = 1;
	check_fillpointer(pos, str);
	test(1, "check_fillpointer1");

	str->fillpointer = 0;
	str->front = 0;
	check_fillpointer(pos, str);
	test(str->front == 10, "check_fillpointer2");

	RETURN;
}

static int test_allocate_array_alloc(void)
{
	addr pos, check;
	size_t size;

	array_alloc_stdarg(NULL, &pos, 10, 0);
	allocate_array_alloc(NULL, pos);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_GENERAL, "allocate_array_alloc1");
	lenarray(check, &size);
	test(size == 10, "allocate_array_alloc2");

	RETURN;
}


/*
 *  array control
 */
static int test_array_element_type(void)
{
	addr pos, check;

	array_make_array(NULL, &pos, Nil, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_element_type(pos, &pos);
	test(pos == T, "array_element_type1");

	GetConst(COMMON_LONG_FLOAT, &pos);
	array_make_array(NULL, &pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_element_type(pos, &pos);
	GetConst(COMMON_LONG_FLOAT, &check);
	test(pos == check, "array_element_type2");

	RETURN;
}

static int test_array_vector_length(void)
{
	addr pos;

	array_make_array(NULL, &pos, fixnumh(22), T,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_vector_length(pos, 0) == 22, "array_vector_length1");

	RETURN;
}

static int test_array_row_length(void)
{
	addr pos;
	size_t size;

	array_make_array(NULL, &pos, Nil, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 1, "array_rowlength1");

	array_make_array(NULL, &pos, fixnumh(22), T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 22, "array_rowlength2");

	list_heap(&pos, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(NULL, &pos, pos, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_rowlength(pos, &size);
	test(size == 54, "array_rowlength3");

	RETURN;
}

static int test_array_dimension_equal(void)
{
	addr pos1, pos2;

	array_make_array(NULL, &pos1, Nil, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(NULL, &pos2, Nil, T, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal1");

	array_make_array(NULL, &pos1, Nil, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(NULL, &pos2, fixnumh(10), T, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal2");

	array_make_array(NULL, &pos1, fixnumh(10), T, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(NULL, &pos2, fixnumh(10), T, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal3");

	array_make_array(NULL, &pos1, fixnumh(20), T, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(NULL, &pos2, fixnumh(10), T, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal4");

	list_heap(&pos1, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(NULL, &pos1, pos1, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(NULL, &pos2, pos2, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_dimension_equal(pos1, pos2), "array_dimension_equal5");

	list_heap(&pos1, fixnumh(2), fixnumh(10), fixnumh(9), NULL);
	array_make_array(NULL, &pos1, pos1, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(NULL, &pos2, pos2, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal6");

	list_heap(&pos1, fixnumh(2), fixnumh(3), NULL);
	array_make_array(NULL, &pos1, pos1, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(NULL, &pos2, pos2, T, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_dimension_equal(pos1, pos2), "array_dimension_equal7");

	RETURN;
}

static void test_aref_array_memory(addr pos, size_t index, addr *ret)
{
	addr mem;
	array_memory(pos, index, &mem, &index);
	aref_index_array(NULL, pos, mem, index, ret);
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	set_index_array(pos, 10, T);

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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(0));
	set_index_array(pos, 10, fixnumh(1));
	set_index_array(pos, 12, fixnumh(1));
	set_index_array(pos, 15, fixnumh(1));
	set_index_array(pos, 25, fixnumh(1));
	set_index_array(pos, 31, fixnumh(1));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, character_heapr(0));
	set_index_array(pos, 10, character_heapr(10));
	set_index_array(pos, 12, character_heapr(20));
	set_index_array(pos, 15, character_heapr(3333));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, fixnumh(11));
	set_index_array(pos, 10, fixnumh(10));
	set_index_array(pos, 12, fixnumh(20));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, single_float_heapr(11.0f));
	set_index_array(pos, 10, single_float_heapr(10.0f));
	set_index_array(pos, 12, single_float_heapr(20.0f));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, double_float_heapr(11.0));
	set_index_array(pos, 10, double_float_heapr(10.0));
	set_index_array(pos, 12, double_float_heapr(20.0));
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
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, long_float_heapr(11.0L));
	set_index_array(pos, 10, long_float_heapr(10.0L));
	set_index_array(pos, 12, long_float_heapr(20.0L));
	test_aref_array_memory(pos, 9, &value);
	test(RefLongFloat(value) == 11.0L, "aref_long_float_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefLongFloat(value) == 10.0L, "aref_long_float_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefLongFloat(value) == 20.0L, "aref_long_float_index_array3");

	RETURN;
}

static int test_aref_index_array(void)
{
	struct array_struct *str;
	addr pos, value;

	array_empty_alloc(NULL, &pos);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->type = ARRAY_TYPE_LONG_FLOAT;
	str->element = sizeoft(long_float);
	str->size = 20;
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);
	initial_element_make_array(pos, long_float_heapr(11.0L));
	set_index_array(pos, 10, long_float_heapr(10.0L));
	set_index_array(pos, 12, long_float_heapr(20.0L));
	test_aref_array_memory(pos, 9, &value);
	test(RefLongFloat(value) == 11.0L, "aref_index_array1");
	test_aref_array_memory(pos, 10, &value);
	test(RefLongFloat(value) == 10.0L, "aref_index_array2");
	test_aref_array_memory(pos, 12, &value);
	test(RefLongFloat(value) == 20.0L, "aref_index_array3");

	RETURN;
}

static int test_aref_list_index(void)
{
	addr pos;
	size_t size;

	array_empty_alloc(NULL, &pos);
	type_make_array(pos, T);
	element_make_array(pos);
	dimension_make_array(NULL, pos, Nil);
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);

	size = aref_list_index(pos, Nil);
	test(size == 0, "aref_list_index1");

	dimension_make_array(NULL, pos, readr("10"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);

	size = aref_list_index(pos, readr("(0)"));
	test(size == 0, "aref_list_index2");

	size = aref_list_index(pos, readr("(7)"));
	test(size == 7, "aref_list_index3");

	dimension_make_array(NULL, pos, readr("(3 4)"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);

	size = aref_list_index(pos, readr("(0 2)"));
	test(size == 2, "aref_list_index4");
	size = aref_list_index(pos, readr("(0 3)"));
	test(size == 3, "aref_list_index5");

	size = aref_list_index(pos, readr("(1 2)"));
	test(size == 4+2, "aref_list_index6");
	size = aref_list_index(pos, readr("(1 3)"));
	test(size == 4+3, "aref_list_index7");

	size = aref_list_index(pos, readr("(2 2)"));
	test(size == 4+4+2, "aref_list_index8");
	size = aref_list_index(pos, readr("(2 3)"));
	test(size == 4+4+3, "aref_list_index9");

	dimension_make_array(NULL, pos, readr("(3 4 5)"));
	allocate_make_array(NULL, pos, Nil, Nil, Nil, Nil);

	size = aref_list_index(pos, readr("(0 2 3)"));
	test(size == 5+5+3, "aref_list_index10");
	size = aref_list_index(pos, readr("(1 2 3)"));
	test(size == 20+10+3, "aref_list_index11");
	size = aref_list_index(pos, readr("(2 3 4)"));
	test(size == 40+15+4, "aref_list_index12");

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


/*
 *  main
 */
static int testbreak_array(void)
{
	TestBreak(test_array_macro);
	TestBreak(test_multisafe_size);
	TestBreak(test_dimension_alloc);
	TestBreak(test_array_empty_alloc);
	TestBreak(test_settype_array);
	TestBreak(test_array_alloc);
	TestBreak(test_array_alloc_stdarg);
	TestBreak(test_type_make_array);
	TestBreak(test_element_make_array);
	TestBreak(test_dimension_make_array);
	TestBreak(test_allocate_element_t);
	TestBreak(test_allocate_element_bit);
	TestBreak(test_allocate_element_size);
	TestBreak(test_allocate_element_array);
	TestBreak(test_type_equal_make_array);
	TestBreak(test_displaced_make_array);
	TestBreak(test_fill_make_array);
	TestBreak(test_extended_make_array);
	TestBreak(test_allocate_make_array);
	TestBreak(test_initial_t_make_array);
	TestBreak(test_initial_bit_make_array);
	TestBreak(test_memset_make_array);
	TestBreak(test_initial_character_make_array);
	TestBreak(test_initial_signed8_make_array);
	TestBreak(test_initial_signed16_make_array);
	TestBreak(test_initial_signed32_make_array);
#ifdef LISP_64BIT
	TestBreak(test_initial_signed64_make_array);
#endif
	TestBreak(test_initial_signed_make_array);
	TestBreak(test_initial_unsigned8_make_array);
	TestBreak(test_initial_unsigned16_make_array);
	TestBreak(test_initial_unsigned32_make_array);
#ifdef LISP_64BIT
	TestBreak(test_initial_unsigned64_make_array);
#endif
	TestBreak(test_initial_unsigned_make_array);
	TestBreak(test_initial_single_float_make_array);
	TestBreak(test_initial_double_float_make_array);
	TestBreak(test_initial_long_float_make_array);
	TestBreak(test_initial_element_make_array);
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
	TestBreak(test_set_index_array);
	TestBreak(test_setf_contents_array);
	TestBreak(test_contents_element_make_array);
	TestBreak(test_array_make_array);
	TestBreak(test_dimension_array_contents);

	TestBreak(test_arrayp);
	TestBreak(test_array_simple_p);
	TestBreak(test_array_vector_p);
	TestBreak(test_array_size_vector_p);
	TestBreak(test_array_dimension_pointer);
	TestBreak(test_array_write_pointer);
	TestBreak(test_set_simple_array);
	TestBreak(test_set_element_size);
	TestBreak(test_check_fillpointer);
	TestBreak(test_allocate_array_alloc);
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
	TestBreak(test_aref_index_array);
	TestBreak(test_aref_list_index);
	/* adjust-array */
	TestBreak(test_array_index_dimension);

	return 0;
}

int test_array(void)
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
		build_calltype();
		build_syscall();
		build_common();
		build_readtable();
		lisp_init = 1;
		result = testbreak_array();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

