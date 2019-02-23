#include "clos_type.c"
#include "clos.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"
#include "stream.h"
#include "strtype.h"

static int test_build_clos_type(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	build_clos_table(ptr);
	build_clos_standard(ptr);
	build_clos_type(ptr);
	test(1, "build_clos_type1");

	RETURN;
}

static int symbolequalchar(addr symbol, const char *name)
{
	GetNameSymbol(symbol, &symbol);
	return string_equal_char(symbol, name);
}

static int closequalchar(addr clos, const char *name)
{
	addr check;
	internchar(LISP_COMMON, name, &check);
	return clos == find_class(check);
}

static void underline_to_hyphen(char *buffer, size_t size, const char *str)
{
	int c;
	size_t i;

	size--;
	for (i = 0; i < size; i++) {
		c = str[i];
		if (c == 0) break;
		buffer[i] = (c == '_')? '-': c;
	}
	buffer[i] = 0;
}

static int equalclosname(
		enum CONSTANT_INDEX index_name,
		enum CONSTANT_INDEX index_clos,
		const char *str)
{
	addr name, clos;
	char buffer[128];

	underline_to_hyphen(buffer, 128, str);
	GetConstant(index_name, &name);
	GetConstant(index_clos, &clos);
	return symbolequalchar(name, buffer) && closequalchar(clos, buffer);
}

#define test_equalclosname(name, mesg) { \
	test(equalclosname(CONSTANT_COMMON_##name, CONSTANT_CLOS_##name, #name), mesg); \
}

static int test_classes(void)
{
	addr pos;

	/* class */
	test_equalclosname(T, "classes_t");
	test_equalclosname(CLASS, "classes_class");
	test_equalclosname(STANDARD_OBJECT, "classes_standard_object");
	test_equalclosname(STANDARD_CLASS, "classes_standard_class");
	test_equalclosname(BUILT_IN_CLASS, "classes_built_in_class");
	test_equalclosname(STRUCTURE_CLASS, "classes_structure_classs");
	test_equalclosname(STRUCTURE_OBJECT, "classes_structure_object");
	test_equalclosname(FUNCTION, "classes_function");
	test_equalclosname(GENERIC_FUNCTION, "classes_generic_function");
	test_equalclosname(STANDARD_GENERIC_FUNCTION,
			"classes_standard_generic_function");
	test_equalclosname(METHOD, "classes_method");
	test_equalclosname(STANDARD_METHOD, "classes_standard_method");
	test_equalclosname(METHOD_COMBINATION, "classes_method_combination");

	/* funcallable-standard-class */
	GetConstant(CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_CLASS, &pos);
	test(symbolequalchar(pos, "FUNCALLABLE-STANDARD-CLASS"),
			"classes_funcallable_standard_class1");
	GetConstant(CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS, &pos);
	test(closp(pos), "classes_funcallable_standard_class2");
	clos_elt(pos, Clos_class_name, &pos);
	test(symbolequalchar(pos, "FUNCALLABLE-STANDARD-CLASS"),
			"classes_funcallable_standard_class3");

	/* funcallable-standard-object */
	GetConstant(CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, &pos);
	test(symbolequalchar(pos, "FUNCALLABLE-STANDARD-OBJECT"),
			"classes_funcallable_standard_object1");
	GetConstant(CONSTANT_CLOS_FUNCALLABLE_STANDARD_OBJECT, &pos);
	test(closp(pos), "classes_funcallable_standard_object2");
	clos_elt(pos, Clos_class_name, &pos);
	test(symbolequalchar(pos, "FUNCALLABLE-STANDARD-OBJECT"),
			"classes_funcallable_standard_object3");

	/* built-in-class */
	test_equalclosname(ARRAY, "type_array");
	test_equalclosname(LIST, "type_list");
	test_equalclosname(NULL, "type_null");

	RETURN;
}

static int test_class_of(void)
{
	addr pos, check;

	GetConstant(CONSTANT_COMMON_CLASS, &pos);
	pos = find_class(pos);
	class_of(pos, &pos);
	GetConstant(CONSTANT_CLOS_STANDARD_CLASS, &check);
	test(check == pos, "class_of1");

	fixnum_heap(&pos, 100);
	class_of(pos, &pos);
	GetConstant(CONSTANT_CLOS_FIXNUM, &check);
	test(check == pos, "class_of2");

	RETURN;
}

static int test_class_of_t(void)
{
	addr left, right;

	GetConstant(CONSTANT_CLOS_FIXNUM, &left);
	right = find_class(T);
	test(std_subclass_p(left, right), "class_of_t1");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_type(void)
{
	TestBreak(test_build_clos_type);
	TestBreak(test_classes);
	TestBreak(test_class_of);
	TestBreak(test_class_of_t);

	return 0;
}

int test_clos_type(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_stream();
		lisp_init = 1;
		result = testbreak_clos_type();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

