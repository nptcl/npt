#include "array.h"
#include "bit.h"
#include "build.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "condition.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "memory.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "unicode.h"

/*
 *  class-of
 */
typedef void (*class_of_calltype)(addr object, addr *ret);
static class_of_calltype class_of_call[LISPTYPE_SIZE];

static void class_of_error(addr object, addr *ret)
{
	fmte("TYPE ~S cannot covert class type.", NULL);
}

static void class_of_nil(addr object, addr *ret)
{
	GetConst(CLOS_NULL, ret);
}

static void class_of_cons(addr object, addr *ret)
{
	GetConst(CLOS_CONS, ret);
}

static void class_of_array(addr object, addr *ret)
{
	/* bit-vector */
	if (bitvectorp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
		else
			GetConst(CLOS_BIT_VECTOR, ret);
		return;
	}

	/* string */
	if (strarray_base_p(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BASE_STRING, ret);
		else
			GetConst(CLOS_BASE_STRING, ret);
		return;
	}
	if (strarrayp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_STRING, ret);
		else
			GetConst(CLOS_STRING, ret);
		return;
	}

	/* vector */
	if (array_vector_p(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_VECTOR, ret);
		else
			GetConst(CLOS_VECTOR, ret);
		return;
	}

	/* array */
	if (array_simple_p(object))
		GetConst(CLOS_SIMPLE_ARRAY, ret);
	else
		GetConst(CLOS_ARRAY, ret);
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
	if (strvect_base_p(object))
		GetConst(CLOS_SIMPLE_BASE_STRING, ret);
	else
		GetConst(CLOS_SIMPLE_STRING, ret);
}

static void class_of_hashtable(addr object, addr *ret)
{
	GetConst(CLOS_HASH_TABLE, ret);
}

static void class_of_readtable(addr object, addr *ret)
{
	GetConst(CLOS_READTABLE, ret);
}

static void class_of_symbol(addr object, addr *ret)
{
	if (keywordp(object))
		GetConst(CLOS_KEYWORD, ret);
	else
		GetConst(CLOS_SYMBOL, ret);
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
	if (compiled_function_p(object))
		GetConst(CLOS_COMPILED_FUNCTION, ret);
	else
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
	switch (getstreamtype(object)) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
			GetConst(CLOS_FILE_STREAM, ret);
			break;

		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetConst(CLOS_STRING_STREAM, ret);
			break;

		case StreamType_Synonym:
			GetConst(CLOS_SYNONYM_STREAM, ret);
			break;

		case StreamType_BroadCast:
			GetConst(CLOS_BROADCAST_STREAM, ret);
			break;

		case StreamType_Concatenated:
			GetConst(CLOS_CONCATENATED_STREAM, ret);
			break;

		case StreamType_TwoWay:
			GetConst(CLOS_TWO_WAY_STREAM, ret);
			break;

		case StreamType_Echo:
			GetConst(CLOS_ECHO_STREAM, ret);
			break;

		default:
			GetConst(CLOS_STREAM, ret);
			break;
	}
}

static void class_of_restart(addr object, addr *ret)
{
	GetConst(CLOS_RESTART, ret);
}

static void class_of_bit_vector(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
}

_g void init_clos_type(void)
{
	size_t i;

	/* error */
	for (i = 0; i < (size_t)LISPTYPE_SIZE; i++)
		class_of_call[i] = class_of_error;

	/* class-of */
	class_of_call[LISPTYPE_NIL] = class_of_nil;
	class_of_call[LISPTYPE_T] = class_of_symbol;
	class_of_call[LISPTYPE_CONS] = class_of_cons;
	class_of_call[LISPTYPE_ARRAY] = class_of_array;
	class_of_call[LISPTYPE_VECTOR] = class_of_vector;
	class_of_call[LISPTYPE_CHARACTER] = class_of_character;
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
	class_of_call[LISPTYPE_RESTART] = class_of_restart;
	class_of_call[LISPTYPE_BITVECTOR] = class_of_bit_vector;
}

_g void clos_class_of(addr object, addr *ret)
{
	enum LISPTYPE type;

	type = GetType(object);
	if (type == LISPTYPE_CLOS) {
		/* clos or structure */
		GetClassOfClos(object, ret);
		return;
	}

	/* built-in-class */
	(class_of_call[(size_t)type])(object, ret);
}


/*
 *  specializer
 */
_g void clos_intern_specializer(addr object, addr *ret)
{
	addr pos, type;

	clos_find_specializer_nil(object, &pos);
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	/* make eql-specializer */
	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	clos_instance_heap(pos, &pos);
	/* define eql-specializer */
	clos_class_of(object, &type);
	stdset_specializer_object(pos, object);
	stdset_specializer_type(pos, type);
	clos_define_specializer(object, pos);
	/* result */
	*ret = pos;
}

