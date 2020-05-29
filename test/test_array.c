#include "array.c"
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

/*
 *  accessor
 */
static int test_arraygen_set(void)
{
	addr pos, check;
	size_t size;

	arraygen_heap(&pos, 10);
	arraygen_set(pos, 1, T);
	arraygen_get(pos, 1, &check);
	test(check == T, "arraygen_set.1");
	arraygen_len(pos, &size);
	test(size == 10, "arraygen_set.2");
	test(arraygen_lenr(pos) == 10, "arraygen_set.3");

	RETURN;
}

static int test_arrayspec_pos(void)
{
	addr pos, check;

	arrayspec_heap(&pos, 30);
	arrayspec_pos(pos, &check);
	test(arrayspec_ptr(pos) == check, "arrayspec_pos.1");

	RETURN;
}

static int test_arraysize_ptr(void)
{
	addr pos;
	size_t *ptr;

	arraysize_heap(&pos, 10);
	ptr = arraysize_ptr(pos);
	test(ptr == (size_t *)posbodyr(pos), "arraysize_ptr.1");

	RETURN;
}

static int test_ArrayInfoStruct(void)
{
	addr pos;
	struct array_struct *ptr;

	array_empty_heap(&pos);
	ptr = ArrayInfoStruct(pos);
	test(ptr, "ArrayInfoStruct.1");

	RETURN;
}

static int test_setarrayinfo(void)
{
	addr pos, check;

	array_empty_heap(&pos);
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, T);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(check == T, "setarrayinfo.1");
	SetArrayInfo(pos, ARRAY_INDEX_MEMORY, Nil);
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &check);
	test(check == Nil, "setarrayinfo.2");

	RETURN;
}


/*
 *  memory allocate
 */
static int test_arraygen_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	arraygen_alloc(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc.1");
	test(GetStatusDynamic(pos), "arraygen_alloc.2");
	test(lenarrayr(pos) == 10, "arraygen_alloc.3");

	arraygen_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc.4");
	test(! GetStatusDynamic(pos), "arraygen_alloc.5");
	test(lenarrayr(pos) == 9, "arraygen_alloc.6");

	arraygen_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc.7");
	test(GetStatusDynamic(pos), "arraygen_alloc.8");
	test(lenarrayr(pos) == 8, "arraygen_alloc.9");

	arraygen_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc.10");
	test(! GetStatusDynamic(pos), "arraygen_alloc.11");
	test(lenarrayr(pos) == 7, "arraygen_alloc.12");

	rollback_local(local, stack);

	RETURN;
}

static int test_arrayspec_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	arrayspec_alloc(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc.1");
	test(GetStatusDynamic(pos), "arrayspec_alloc.2");
	test(lenbodyr(pos) == 10, "arrayspec_alloc.3");

	arrayspec_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc.4");
	test(! GetStatusDynamic(pos), "arrayspec_alloc.5");
	test(lenbodyr(pos) == 9, "arrayspec_alloc.6");

	arrayspec_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc.7");
	test(GetStatusDynamic(pos), "arrayspec_alloc.8");
	test(lenbodyr(pos) == 8, "arrayspec_alloc.9");

	arrayspec_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc.10");
	test(! GetStatusDynamic(pos), "arrayspec_alloc.11");
	test(lenbodyr(pos) == 7, "arrayspec_alloc.12");

	rollback_local(local, stack);

	RETURN;
}

static int test_arrayinfo_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	arrayinfo_alloc(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc.1");
	test(GetStatusDynamic(pos), "arrayinfo_alloc.2");

	arrayinfo_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc.3");
	test(! GetStatusDynamic(pos), "arrayinfo_alloc.4");

	arrayinfo_local(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc.5");
	test(GetStatusDynamic(pos), "arrayinfo_alloc.6");

	arrayinfo_heap(&pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc.7");
	test(! GetStatusDynamic(pos), "arrayinfo_alloc.8");

	rollback_local(local, stack);

	RETURN;
}

static int test_arraysize_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	arraysize_alloc(local, &pos, 10);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc.1");
	test(GetStatusDynamic(pos), "arraysize_alloc.2");
	test(lenbodyr(pos) == 10*IdxSize, "arraysize_alloc.3");

	arraysize_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc.4");
	test(! GetStatusDynamic(pos), "arraysize_alloc.5");
	test(lenbodyr(pos) == 9*IdxSize, "arraysize_alloc.6");

	arraysize_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc.7");
	test(GetStatusDynamic(pos), "arraysize_alloc.8");
	test(lenbodyr(pos) == 8*IdxSize, "arraysize_alloc.9");

	arraysize_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc.10");
	test(! GetStatusDynamic(pos), "arraysize_alloc.11");
	test(lenbodyr(pos) == 7*IdxSize, "arraysize_alloc.12");

	rollback_local(local, stack);

	RETURN;
}

static int test_arraysize_copy_alloc(void)
{
	addr pos, root;
	LocalRoot local;
	LocalStack stack;
	size_t *size;

	local = Local_Thread;
	push_local(local, &stack);

	arraysize_heap(&root, 5);
	size = arraysize_ptr(root);
	size[0] = 10;
	size[3] = 20;
	size[4] = 40;

	arraysize_copy_alloc(local, &pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc.1");
	test(GetStatusDynamic(pos), "arraysize_copy_alloc.2");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc.3");
	size = arraysize_ptr(pos);
	test(size[0] == 10, "arraysize_copy_alloc.4");
	test(size[3] == 20, "arraysize_copy_alloc.5");
	test(size[4] == 40, "arraysize_copy_alloc.6");

	arraysize_copy_alloc(NULL, &pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc.7");
	test(! GetStatusDynamic(pos), "arraysize_copy_alloc.8");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc.9");

	arraysize_copy_local(local, &pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc.10");
	test(GetStatusDynamic(pos), "arraysize_copy_alloc.11");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc.12");

	arraysize_copy_heap(&pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc.13");
	test(! GetStatusDynamic(pos), "arraysize_copy_alloc.14");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc.15");

	rollback_local(local, stack);

	RETURN;
}

static int test_array_empty_alloc(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	array_empty_alloc(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc.1");
	test(GetStatusDynamic(pos), "array_empty_alloc.2");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc.3");

	array_empty_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc.4");
	test(! GetStatusDynamic(pos), "array_empty_alloc.5");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc.6");

	array_empty_local(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc.7");
	test(GetStatusDynamic(pos), "array_empty_alloc.8");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc.9");

	array_empty_heap(&pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc.10");
	test(! GetStatusDynamic(pos), "array_empty_alloc.11");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc.10");

	rollback_local(local, stack);

	RETURN;
}

static int test_array_alloc(void)
{
	addr pos, check;
	struct array_struct *str;

	array_alloc(NULL, &pos, 0, 5);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_alloc.1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_alloc.2");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc.3");
	test(str->dimension == 0, "array_alloc.4");
	test(str->size == 5, "array_alloc.5");
	test(str->front == 5, "array_alloc.6");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc.7");

	array_alloc(NULL, &pos, 1, 5);
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_alloc.8");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc.9");
	test(str->dimension == 1, "array_alloc.10");
	test(str->size == 5, "array_alloc.11");
	test(str->front == 5, "array_alloc.12");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc.13");

	array_alloc(NULL, &pos, 3, 6);
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_alloc.14");
	str = ArrayInfoStruct(pos);
	test(str->type == ARRAY_TYPE_T, "array_alloc.15");
	test(str->dimension == 3, "array_alloc.16");
	test(str->size == 6, "array_alloc.17");
	test(str->front == 6, "array_alloc.18");
	GetArrayInfo(pos, ARRAY_INDEX_TYPE, &check);
	test(RefLispDecl(check) == LISPDECL_T, "array_alloc.19");

	RETURN;
}

static int test_array_va_alloc(void)
{
	struct array_struct *str;
	size_t *psize;
	addr pos, check;

	array_va_alloc(NULL, &pos, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc.1");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_va_alloc.2");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "array_va_alloc.3");
	test(str->size == 1, "array_va_alloc.4");

	array_va_alloc(NULL, &pos, 10, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc.5");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(check == Nil, "array_va_alloc.6");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 1, "array_va_alloc.7");
	test(str->size == 10, "array_va_alloc.8");

	array_va_alloc(NULL, &pos, 2, 3, 4, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc.9");
	GetArrayInfo(pos, ARRAY_INDEX_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_va_alloc.10");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 3, "array_va_alloc.11");
	test(str->size == 24, "array_va_alloc.12");
	psize = arraysize_ptr(check);
	test(psize[0] == 2, "array_va_alloc.13");
	test(psize[1] == 3, "array_va_alloc.14");
	test(psize[2] == 4, "array_va_alloc.15");

	RETURN;
}


/*
 *  type check
 */
static int test_arrayp(void)
{
	addr pos;

	array_va_heap(&pos, 2, 3, 4, 0);
	test(arrayp(pos), "arrayp.1");
	test(! arrayp(Nil), "arrayp.2");

	RETURN;
}

static int test_array_simple_p(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 2, 3, 4, 0);
	str = ArrayInfoStruct(pos);
	str->simple = 1;
	test(array_simple_p(pos), "array_simple_p.1");
	str->simple = 0;
	test(! array_simple_p(pos), "array_simple_p.2");

	RETURN;
}

static int test_array_vector_p(void)
{
	addr pos;

	array_va_heap(&pos, 0);
	test(! array_vector_p(pos), "array_vector_p.1");
	array_va_heap(&pos, 10, 0);
	test(array_vector_p(pos), "array_vector_p.2");
	array_va_heap(&pos, 10, 20, 0);
	test(! array_vector_p(pos), "array_vector_p.3");

	RETURN;
}

static int test_array_size_vector_p(void)
{
	struct array_struct *str;
	addr pos;

	array_va_heap(&pos, 10, 0);
	test(array_size_vector_p(pos, 10), "array_size_vector_p.1");
	test(! array_size_vector_p(pos, 11), "array_size_vector_p.2");

	array_va_heap(&pos, 10, 20, 0);
	test(! array_size_vector_p(pos, 10), "array_size_vector_p.3");

	array_va_heap(&pos, 0);
	test(! array_size_vector_p(pos, 0), "array_size_vector_p.4");

	array_va_heap(&pos, 10, 20, 0);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 44;
	test(array_size_vector_p(pos, 44), "array_size_vector_p.5");

	RETURN;
}


/*
 *  memory access
 */
static int test_array_ptrsize(void)
{
	addr pos;
	const size_t *data;

	array_va_heap(&pos, 0);
	test(array_ptrsize(pos) == NULL, "array_ptrsize.1");
	array_va_heap(&pos, 10, 0);
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_ptrsize.2");
	array_va_heap(&pos, 10, 20, 0);
	data = array_ptrsize(pos);
	test(data[0] == 10, "array_ptrsize.3");
	test(data[1] == 20, "array_ptrsize.4");

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
	test(ptr == (byte *)posbodyr(mem), "array_ptrwrite.1");
	ptr = (byte *)array_ptrwrite(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_ptrwrite.2");

	ptr = (byte *)array_ptrread(pos, 0);
	test(ptr == (byte *)posbodyr(mem), "array_ptrread.1");
	ptr = (byte *)array_ptrread(pos, 3);
	test(ptr == 3 * 4 + (byte *)posbodyr(mem), "array_ptrread.2");

	RETURN;
}


/*
 *  array
 */
static int testcase_array(void)
{
	/* accessor */
	TestBreak(test_arraygen_set);
	TestBreak(test_arrayspec_pos);
	TestBreak(test_arraysize_ptr);
	TestBreak(test_ArrayInfoStruct);
	TestBreak(test_setarrayinfo);
	/* memory allocate */
	TestBreak(test_arraygen_alloc);
	TestBreak(test_arrayspec_alloc);
	TestBreak(test_arrayinfo_alloc);
	TestBreak(test_arraysize_alloc);
	TestBreak(test_arraysize_copy_alloc);
	TestBreak(test_array_empty_alloc);
	TestBreak(test_array_alloc);
	TestBreak(test_array_va_alloc);
	/* type check */
	TestBreak(test_arrayp);
	TestBreak(test_array_simple_p);
	TestBreak(test_array_vector_p);
	TestBreak(test_array_size_vector_p);
	/* memory access */
	TestBreak(test_array_ptrsize);
	TestBreak(test_array_ptrwrite);

	return 0;
}

static void testinit_array(Execute ptr)
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

int test_array(void)
{
	TITLE;
	return degrade_code(
			testinit_array,
			testcase_array);
}

