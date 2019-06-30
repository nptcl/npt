#include "array.c"
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
	test(check == T, "arraygen_set1");
	arraygen_len(pos, &size);
	test(size == 10, "arraygen_set2");
	test(arraygen_lenr(pos) == 10, "arraygen_set3");

	RETURN;
}

static int test_arrayspec_pos(void)
{
	addr pos, check;

	arrayspec_heap(&pos, 30);
	arrayspec_pos(pos, &check);
	test(arrayspec_ptr(pos) == check, "arrayspec_pos1");

	RETURN;
}

static int test_arraysize_ptr(void)
{
	addr pos;
	size_t *ptr;

	arraysize_heap(&pos, 10);
	ptr = arraysize_ptr(pos);
	test(ptr == (size_t *)posbodyr(pos), "arraysize_ptr1");

	RETURN;
}

static int test_ArrayInfoStruct(void)
{
	addr pos;
	struct array_struct *ptr;

	array_empty_heap(&pos);
	ptr = ArrayInfoStruct(pos);
	test(ptr, "ArrayInfoStruct1");

	RETURN;
}

static int test_setarrayinfo(void)
{
	addr pos, check;

	array_empty_heap(&pos);
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, T);
	GetArrayInfo(pos, ARRAY_INFO_MEMORY, &check);
	test(check == T, "setarrayinfo1");
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, Nil);
	check = RefArrayInfo(pos, ARRAY_INFO_MEMORY);
	test(check == Nil, "setarrayinfo2");

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
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc1");
	test(GetStatusDynamic(pos), "arraygen_alloc2");
	test(lenarrayr(pos) == 10, "arraygen_alloc3");

	arraygen_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc4");
	test(! GetStatusDynamic(pos), "arraygen_alloc5");
	test(lenarrayr(pos) == 9, "arraygen_alloc6");

	arraygen_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc7");
	test(GetStatusDynamic(pos), "arraygen_alloc8");
	test(lenarrayr(pos) == 8, "arraygen_alloc9");

	arraygen_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_GENERAL, "arraygen_alloc10");
	test(! GetStatusDynamic(pos), "arraygen_alloc11");
	test(lenarrayr(pos) == 7, "arraygen_alloc12");

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
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc1");
	test(GetStatusDynamic(pos), "arrayspec_alloc2");
	test(lenbodyr(pos) == 10, "arrayspec_alloc3");

	arrayspec_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc4");
	test(! GetStatusDynamic(pos), "arrayspec_alloc5");
	test(lenbodyr(pos) == 9, "arrayspec_alloc6");

	arrayspec_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc7");
	test(GetStatusDynamic(pos), "arrayspec_alloc8");
	test(lenbodyr(pos) == 8, "arrayspec_alloc9");

	arrayspec_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_SPECIALIZED, "arrayspec_alloc10");
	test(! GetStatusDynamic(pos), "arrayspec_alloc11");
	test(lenbodyr(pos) == 7, "arrayspec_alloc12");

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
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc1");
	test(GetStatusDynamic(pos), "arrayinfo_alloc2");

	arrayinfo_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc3");
	test(! GetStatusDynamic(pos), "arrayinfo_alloc4");

	arrayinfo_local(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc5");
	test(GetStatusDynamic(pos), "arrayinfo_alloc6");

	arrayinfo_heap(&pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "arrayinfo_alloc7");
	test(! GetStatusDynamic(pos), "arrayinfo_alloc8");

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
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc1");
	test(GetStatusDynamic(pos), "arraysize_alloc2");
	test(lenbodyr(pos) == 10*IdxSize, "arraysize_alloc3");

	arraysize_alloc(NULL, &pos, 9);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc4");
	test(! GetStatusDynamic(pos), "arraysize_alloc5");
	test(lenbodyr(pos) == 9*IdxSize, "arraysize_alloc6");

	arraysize_local(local, &pos, 8);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc7");
	test(GetStatusDynamic(pos), "arraysize_alloc8");
	test(lenbodyr(pos) == 8*IdxSize, "arraysize_alloc9");

	arraysize_heap(&pos, 7);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_alloc10");
	test(! GetStatusDynamic(pos), "arraysize_alloc11");
	test(lenbodyr(pos) == 7*IdxSize, "arraysize_alloc12");

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
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc1");
	test(GetStatusDynamic(pos), "arraysize_copy_alloc2");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc3");
	size = arraysize_ptr(pos);
	test(size[0] == 10, "arraysize_copy_alloc4");
	test(size[3] == 20, "arraysize_copy_alloc5");
	test(size[4] == 40, "arraysize_copy_alloc6");

	arraysize_copy_alloc(NULL, &pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc7");
	test(! GetStatusDynamic(pos), "arraysize_copy_alloc8");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc9");

	arraysize_copy_local(local, &pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc10");
	test(GetStatusDynamic(pos), "arraysize_copy_alloc11");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc12");

	arraysize_copy_heap(&pos, root, 5);
	test(GetType(pos) == LISPSYSTEM_ARRAY_DIMENSION, "arraysize_copy_alloc13");
	test(! GetStatusDynamic(pos), "arraysize_copy_alloc14");
	test(lenbodyr(pos) == 5*IdxSize, "arraysize_copy_alloc15");

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
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc1");
	test(GetStatusDynamic(pos), "array_empty_alloc2");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc3");

	array_empty_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc4");
	test(! GetStatusDynamic(pos), "array_empty_alloc5");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc6");

	array_empty_local(local, &pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc7");
	test(GetStatusDynamic(pos), "array_empty_alloc8");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc9");

	array_empty_heap(&pos);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_empty_alloc10");
	test(! GetStatusDynamic(pos), "array_empty_alloc11");
	test(ArrayInfoStruct(pos)->type == ARRAY_TYPE_EMPTY, "array_empty_alloc10");

	rollback_local(local, stack);

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

static int test_array_va_alloc(void)
{
	struct array_struct *str;
	size_t *psize;
	addr pos, check;

	array_va_alloc(NULL, &pos, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc1");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_va_alloc2");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 0, "array_va_alloc3");
	test(str->size == 1, "array_va_alloc4");

	array_va_alloc(NULL, &pos, 10, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc5");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(check == Nil, "array_va_alloc6");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 1, "array_va_alloc7");
	test(str->size == 10, "array_va_alloc8");

	array_va_alloc(NULL, &pos, 2, 3, 4, 0);
	test(GetType(pos) == LISPTYPE_ARRAY, "array_va_alloc9");
	GetArrayInfo(pos, ARRAY_INFO_DIMENSION, &check);
	test(GetType(check) == LISPSYSTEM_ARRAY_DIMENSION, "array_va_alloc10");
	str = ArrayInfoStruct(pos);
	test(str->dimension == 3, "array_va_alloc11");
	test(str->size == 24, "array_va_alloc12");
	psize = arraysize_ptr(check);
	test(psize[0] == 2, "array_va_alloc13");
	test(psize[1] == 3, "array_va_alloc14");
	test(psize[2] == 4, "array_va_alloc15");

	RETURN;
}


/*
 *  type check
 */
static int test_arrayp(void)
{
	addr pos;

	array_va_heap(&pos, 2, 3, 4, 0);
	test(arrayp(pos), "arrayp1");
	test(! arrayp(Nil), "arrayp2");

	RETURN;
}

static int test_array_simple_p(void)
{
	addr pos;
	struct array_struct *str;

	array_va_heap(&pos, 2, 3, 4, 0);
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

	array_va_heap(&pos, 0);
	test(! array_vector_p(pos), "array_vector_p1");
	array_va_heap(&pos, 10, 0);
	test(array_vector_p(pos), "array_vector_p2");
	array_va_heap(&pos, 10, 20, 0);
	test(! array_vector_p(pos), "array_vector_p3");

	RETURN;
}

static int test_array_size_vector_p(void)
{
	struct array_struct *str;
	addr pos;

	array_va_heap(&pos, 10, 0);
	test(array_size_vector_p(pos, 10), "array_size_vector_p1");
	test(! array_size_vector_p(pos, 11), "array_size_vector_p2");

	array_va_heap(&pos, 10, 20, 0);
	test(! array_size_vector_p(pos, 10), "array_size_vector_p3");

	array_va_heap(&pos, 0);
	test(! array_size_vector_p(pos, 0), "array_size_vector_p4");

	array_va_heap(&pos, 10, 20, 0);
	str = ArrayInfoStruct(pos);
	str->dimension = 1;
	str->size = 44;
	test(array_size_vector_p(pos, 44), "array_size_vector_p5");

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
	test(array_ptrsize(pos) == NULL, "array_ptrsize1");
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
	SetArrayInfo(pos, ARRAY_INFO_MEMORY, mem);
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


/*
 *  main
 */
static int testbreak_array(void)
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
		build_syscall();
		build_common();
		build_readtable();
		lisp_initialize = 1;
		result = testbreak_array();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

