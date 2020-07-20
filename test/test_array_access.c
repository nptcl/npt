#include "array_access.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "cons.h"
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
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type.1");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 20;
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type.2");

	c->type = ARRAY_TYPE_SIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(array_equal_type(c, d->type, d->bytesize), "array_equal_type.3");

	c->type = ARRAY_TYPE_UNSIGNED;
	c->bytesize = 10;
	d->type = ARRAY_TYPE_SIGNED;
	d->bytesize = 10;
	test(! array_equal_type(c, d->type, d->bytesize), "array_equal_type.4");

	RETURN;
}

static int test_array_equal_dimension(void)
{
	addr pos1, pos2, type;

	GetTypeTable(&type, T);
	array_make_array(&pos1, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(&pos2, Nil, type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_equal_dimension(pos1, pos2), "array_equal_dimension.1");

	array_make_array(&pos1, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_equal_dimension(pos1, pos2), "array_equal_dimension.2");

	array_make_array(&pos1, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(array_equal_dimension(pos1, pos2), "array_equal_dimension.3");

	array_make_array(&pos1, fixnumh(20), type, Unbound, Unbound, T, Nil, Nil, Nil);
	array_make_array(&pos2, fixnumh(10), type, Unbound, Unbound, T, Nil, Nil, Nil);
	test(! array_equal_dimension(pos1, pos2), "array_equal_dimension.4");

	list_heap(&pos1, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_equal_dimension(pos1, pos2), "array_equal_dimension.5");

	list_heap(&pos1, fixnumh(2), fixnumh(10), fixnumh(9), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_equal_dimension(pos1, pos2), "array_equal_dimension.6");

	list_heap(&pos1, fixnumh(2), fixnumh(3), NULL);
	array_make_array(&pos1, pos1, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	list_heap(&pos2, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos2, pos2, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(! array_equal_dimension(pos1, pos2), "array_equal_dimension.7");

	RETURN;
}

static int test_array_get_element_type(void)
{
	addr pos, check;

	GetTypeTable(&pos, T);
	array_make_array(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_get_element_type_(pos, &pos);
	test(pos == T, "array_get_element_type.1");

	GetTypeTable(&pos, LongFloat);
	array_make_array(&pos, Nil, pos, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_get_element_type_(pos, &pos);
	GetConst(COMMON_LONG_FLOAT, &check);
	test(pos == check, "array_get_element_type.2");

	RETURN;
}

static int test_array_get_vector_length(void)
{
	addr pos;

	GetTypeTable(&pos, T);
	array_make_array(&pos, fixnumh(22), pos,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	test(array_get_vector_length(pos, 0) == 22, "array_get_vector_length.1");

	RETURN;
}

static int test_array_get_rowlength(void)
{
	addr pos, type;
	size_t size;

	GetTypeTable(&type, T);
	array_make_array(&pos, Nil, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_get_rowlength(pos, &size);
	test(size == 1, "array_get_rowlength.1");

	array_make_array(&pos, fixnumh(22), type,
			Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_get_rowlength(pos, &size);
	test(size == 22, "array_get_rowlength.2");

	list_heap(&pos, fixnumh(2), fixnumh(3), fixnumh(9), NULL);
	array_make_array(&pos, pos, type, Unbound, Unbound, Nil, Nil, Nil, Nil);
	array_get_rowlength(pos, &size);
	test(size == 54, "array_get_rowlength.3");

	RETURN;
}


/*
 *  array_access
 */
static int testcase_array_access(void)
{
	TestBreak(test_array_equal_type);
	TestBreak(test_array_equal_dimension);
	TestBreak(test_array_get_element_type);
	TestBreak(test_array_get_vector_length);
	TestBreak(test_array_get_rowlength);

	return 0;
}

static void testinit_array_access(Execute ptr)
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

int test_array_access(void)
{
	TITLE;
	return degrade_code(
			testinit_array_access,
			testcase_array_access);
}

