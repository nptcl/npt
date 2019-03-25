#include "build.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "constant.h"
#include "memory.h"
#include "pathname.h"
#include "symbol.h"

/*
 *  define metaclass
 */
static void define_built_in_class(Execute ptr,
		enum CONSTANT_INDEX index,
		enum CONSTANT_INDEX index_clos,
		addr supers)
{
	addr clos, name, metaclass;

	GetConstant(index, &name);
	GetConst(COMMON_BUILT_IN_CLASS, &metaclass);
	metaclass = find_class(metaclass);
	make_type_class(ptr, &clos, metaclass, name, supers);
	SetConstant(index_clos, clos);
	setf_find_class(name, clos);
}

static void define_built_in_class_0(Execute ptr,
		enum CONSTANT_INDEX index,
		enum CONSTANT_INDEX index_clos)
{
	addr clos;

	list_heap(&clos, find_class(T), NULL);
	define_built_in_class(ptr, index, index_clos, clos);
}

static void define_built_in_class_1(Execute ptr,
		enum CONSTANT_INDEX index,
		enum CONSTANT_INDEX index_clos,
		enum CONSTANT_INDEX super)
{
	addr clos;

	GetConstant(super, &clos);
	list_heap(&clos, find_class(clos), find_class(T), NULL);
	define_built_in_class(ptr, index, index_clos, clos);
}

static void define_built_in_class_2(Execute ptr,
		enum CONSTANT_INDEX index,
		enum CONSTANT_INDEX index_clos,
		enum CONSTANT_INDEX super1,
		enum CONSTANT_INDEX super2)
{
	addr clos1, clos2;

	GetConstant(super1, &clos1);
	GetConstant(super2, &clos2);
	list_heap(&clos1, find_class(clos1), find_class(clos2), find_class(T), NULL);
	define_built_in_class(ptr, index, index_clos, clos1);
}

#define findsetconstant(x) find_class_constant(CONSTANT_COMMON_##x, CONSTANT_CLOS_##x)
#define defbuilt0(p,x) define_built_in_class_0(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x)
#define defbuilt1(p,x,y) define_built_in_class_1(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, \
		CONSTANT_COMMON_##y)
#define defbuilt2(p,x,y,z) define_built_in_class_2(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, \
		CONSTANT_COMMON_##y, CONSTANT_COMMON_##z)

void build_clos_type(Execute ptr)
{
	/* error check */
	findsetconstant(T);
	findsetconstant(CLASS);
	findsetconstant(STANDARD_OBJECT);
	findsetconstant(STANDARD_CLASS);
	findsetconstant(BUILT_IN_CLASS);
	findsetconstant(STRUCTURE_CLASS);
	findsetconstant(STRUCTURE_OBJECT);
	findsetconstant(FUNCTION);
	findsetconstant(GENERIC_FUNCTION);
	findsetconstant(STANDARD_GENERIC_FUNCTION);
	findsetconstant(METHOD);
	findsetconstant(STANDARD_METHOD);
	findsetconstant(METHOD_COMBINATION);
	find_class_constant(
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_OBJECT);
	find_class_constant(
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_CLASS,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);

	defbuilt0(ptr, ARRAY);
	defbuilt0(ptr, CHARACTER);
	defbuilt0(ptr, CONDITION);
	defbuilt0(ptr, HASH_TABLE);
	defbuilt0(ptr, NUMBER);
	defbuilt0(ptr, PACKAGE);
	defbuilt0(ptr, PATHNAME);
	defbuilt0(ptr, RANDOM_STATE);
	defbuilt0(ptr, READTABLE);
	defbuilt0(ptr, RESTART);
	defbuilt0(ptr, SEQUENCE);
	defbuilt0(ptr, STREAM);
	defbuilt0(ptr, SYMBOL);
	defbuilt1(ptr, LOGICAL_PATHNAME, PATHNAME);

	defbuilt1(ptr, LIST, SEQUENCE);
	defbuilt1(ptr, CONS, LIST);
	defbuilt1(ptr, VECTOR, SEQUENCE);
	defbuilt1(ptr, BIT_VECTOR, VECTOR);
	defbuilt2(ptr, NULL, SYMBOL, LIST);
	defbuilt1(ptr, STRING, VECTOR);

	defbuilt1(ptr, COMPLEX, NUMBER);
	defbuilt1(ptr, REAL, NUMBER);
	defbuilt1(ptr, FLOAT, REAL);
	defbuilt1(ptr, RATIONAL, REAL);
	defbuilt1(ptr, INTEGER, RATIONAL);
	defbuilt1(ptr, RATIO, RATIONAL);

	defbuilt1(ptr, BROADCAST_STREAM, STREAM);
	defbuilt1(ptr, CONCATENATED_STREAM, STREAM);
	defbuilt1(ptr, ECHO_STREAM, STREAM);
	defbuilt1(ptr, FILE_STREAM, STREAM);
	defbuilt1(ptr, STRING_STREAM, STREAM);
	defbuilt1(ptr, SYNONYM_STREAM, STREAM);
	defbuilt1(ptr, TWO_WAY_STREAM, STREAM);

	defbuilt1(ptr, BASE_CHAR, CHARACTER);
	defbuilt1(ptr, EXTENDED_CHAR, CHARACTER);
	defbuilt1(ptr, STANDARD_CHAR, CHARACTER);
	defbuilt1(ptr, SIMPLE_ARRAY, ARRAY);
	defbuilt2(ptr, SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY);

	defbuilt1(ptr, BASE_STRING, STRING);
	defbuilt2(ptr, SIMPLE_STRING, STRING, SIMPLE_ARRAY);
	defbuilt2(ptr, SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING);
	defbuilt2(ptr, SIMPLE_BIT_VECTOR, BIT_VECTOR, SIMPLE_ARRAY);
	defbuilt1(ptr, BIGNUM, INTEGER);
	defbuilt1(ptr, FIXNUM, INTEGER);
	defbuilt1(ptr, SHORT_FLOAT, FLOAT);
	defbuilt1(ptr, SINGLE_FLOAT, FLOAT);
	defbuilt1(ptr, DOUBLE_FLOAT, FLOAT);
	defbuilt1(ptr, LONG_FLOAT, FLOAT);
	defbuilt1(ptr, SIGNED_BYTE, INTEGER);
	defbuilt1(ptr, UNSIGNED_BYTE, SIGNED_BYTE);
	defbuilt1(ptr, BIT, UNSIGNED_BYTE);
	defbuilt1(ptr, COMPILED_FUNCTION, FUNCTION);
	defbuilt1(ptr, KEYWORD, SYMBOL);
}


/*
 *  class-of
 */
typedef void (*class_of_calltype)(addr object, addr *ret);
static class_of_calltype class_of_call[LISPTYPE_SIZE];

void class_of(addr object, addr *ret)
{
	enum LISPTYPE type;

	type = GetType(object);
	if (type == LISPTYPE_CLOS) {
		/* clos or structure */
		clos_class_of(object, ret);
		return;
	}

	/* built-in-class */
	(class_of_call[(size_t)type])(object, ret);
}

static void class_of_error(addr object, addr *ret)
{
	Abort("class_of_error: system error.");
}
static void class_of_nil(addr object, addr *ret)
{
	GetConst(CLOS_NULL, ret);
}
static void class_of_symbol(addr object, addr *ret)
{
	addr keyword;

	GetConst(PACKAGE_KEYWORD, &keyword);
	GetPackageSymbol(object, &object);
	if (object == keyword) {
		GetConst(CLOS_KEYWORD, ret);
	}
	else {
		GetConst(CLOS_SYMBOL, ret);
	}
}
static void class_of_cons(addr object, addr *ret)
{
	GetConst(CLOS_CONS, ret);
}
static void class_of_vector(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_VECTOR, ret);
}
static void class_of_character(addr object, addr *ret)
{
	GetConst(CLOS_CHARACTER, ret);
}
static void class_of_string(addr object, addr *ret)
{
	GetConst(CLOS_STRING, ret);
}
static void class_of_hashtable(addr object, addr *ret)
{
	GetConst(CLOS_HASH_TABLE, ret);
}
static void class_of_readtable(addr object, addr *ret)
{
	GetConst(CLOS_READTABLE, ret);
}
static void class_of_fixnum(addr object, addr *ret)
{
	GetConst(CLOS_FIXNUM, ret);
}
static void class_of_bignum(addr object, addr *ret)
{
	GetConst(CLOS_BIGNUM, ret);
}
static void class_of_ratio(addr object, addr *ret)
{
	GetConst(CLOS_RATIO, ret);
}
static void class_of_short_float(addr object, addr *ret)
{
	GetConst(CLOS_SHORT_FLOAT, ret);
}
static void class_of_single_float(addr object, addr *ret)
{
	GetConst(CLOS_SINGLE_FLOAT, ret);
}
static void class_of_double_float(addr object, addr *ret)
{
	GetConst(CLOS_DOUBLE_FLOAT, ret);
}
static void class_of_long_float(addr object, addr *ret)
{
	GetConst(CLOS_LONG_FLOAT, ret);
}
static void class_of_complex(addr object, addr *ret)
{
	GetConst(CLOS_COMPLEX, ret);
}
static void class_of_function(addr object, addr *ret)
{
	GetConst(CLOS_FUNCTION, ret);
}
static void class_of_package(addr object, addr *ret)
{
	GetConst(CLOS_PACKAGE, ret);
}
static void class_of_random_state(addr object, addr *ret)
{
	GetConst(CLOS_RANDOM_STATE, ret);
}
static void class_of_pathname(addr object, addr *ret)
{
	if (pathname_logical_p(object))
		GetConst(CLOS_LOGICAL_PATHNAME, ret);
	else
		GetConst(CLOS_PATHNAME, ret);
}
static void class_of_stream(addr object, addr *ret)
{
	GetConst(CLOS_STREAM, ret);
}

void init_class_of(void)
{
	size_t i;

	/* error */
	for (i = 0; i < (size_t)LISPTYPE_SIZE; i++)
		class_of_call[i] = class_of_error;

	/* class-of */
	class_of_call[LISPTYPE_NIL] = class_of_nil;
	class_of_call[LISPTYPE_T] = class_of_symbol;
	class_of_call[LISPTYPE_CONS] = class_of_cons;
	class_of_call[LISPTYPE_VECTOR] = class_of_vector;
	class_of_call[LISPTYPE_CHARACTER] = class_of_character;
	/*class_of_call[LISPTYPE_ARRAY] = class_of_array;*/
	class_of_call[LISPTYPE_STRING] = class_of_string;
	class_of_call[LISPTYPE_HASHTABLE] = class_of_hashtable;
	class_of_call[LISPTYPE_READTABLE] = class_of_readtable;
	class_of_call[LISPTYPE_SYMBOL] = class_of_symbol;
	class_of_call[LISPTYPE_FIXNUM] = class_of_fixnum;
	class_of_call[LISPTYPE_BIGNUM] = class_of_bignum;
	class_of_call[LISPTYPE_RATIO] = class_of_ratio;
	class_of_call[LISPTYPE_SHORT_FLOAT] = class_of_short_float;
	class_of_call[LISPTYPE_SINGLE_FLOAT] = class_of_single_float;
	class_of_call[LISPTYPE_DOUBLE_FLOAT] = class_of_double_float;
	class_of_call[LISPTYPE_LONG_FLOAT] = class_of_long_float;
	class_of_call[LISPTYPE_COMPLEX] = class_of_complex;
	class_of_call[LISPTYPE_FUNCTION] = class_of_function;
	class_of_call[LISPTYPE_PACKAGE] = class_of_package;
	class_of_call[LISPTYPE_RANDOM_STATE] = class_of_random_state;
	class_of_call[LISPTYPE_PATHNAME] = class_of_pathname;
	class_of_call[LISPTYPE_STREAM] = class_of_stream;
}

