#include "array.h"
#include "bit.h"
#include "build.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_type.h"
#include "condition.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "memory.h"
#include "pathname_object.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  class-of
 */
typedef int (*class_of_calltype)(addr object, addr *ret);
static class_of_calltype class_of_call[LISPTYPE_SIZE];

static int class_of_error_(addr object, addr *ret)
{
	infobit(object);
	return fmte_("TYPE ~S cannot convert class type.", object, NULL);
}

static int class_of_nil_(addr object, addr *ret)
{
	GetConst(CLOS_NULL, ret);
	return 0;
}

static int class_of_cons_(addr object, addr *ret)
{
	GetConst(CLOS_CONS, ret);
	return 0;
}

static int class_of_array_(addr object, addr *ret)
{
	int check;

	/* bit-vector */
	if (bitvectorp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
		else
			GetConst(CLOS_BIT_VECTOR, ret);
		return 0;
	}

	/* string */
	Return(strarray_base_p_(object, &check));
	if (check) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_BASE_STRING, ret);
		else
			GetConst(CLOS_BASE_STRING, ret);
		return 0;
	}
	if (strarrayp(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_STRING, ret);
		else
			GetConst(CLOS_STRING, ret);
		return 0;
	}

	/* vector */
	if (array_vector_p(object)) {
		if (array_simple_p(object))
			GetConst(CLOS_SIMPLE_VECTOR, ret);
		else
			GetConst(CLOS_VECTOR, ret);
		return 0;
	}

	/* array */
	if (array_simple_p(object))
		GetConst(CLOS_SIMPLE_ARRAY, ret);
	else
		GetConst(CLOS_ARRAY, ret);
	return 0;
}

static int class_of_vector_(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_VECTOR, ret);
	return 0;
}

static int class_of_character_(addr object, addr *ret)
{
	GetConst(CLOS_CHARACTER, ret);
	return 0;
}

static int class_of_string_(addr object, addr *ret)
{
	int check;

	Return(strvect_base_p_(object, &check));
	if (check)
		GetConst(CLOS_SIMPLE_BASE_STRING, ret);
	else
		GetConst(CLOS_SIMPLE_STRING, ret);

	return 0;
}

static int class_of_hashtable_(addr object, addr *ret)
{
	GetConst(CLOS_HASH_TABLE, ret);
	return 0;
}

static int class_of_readtable_(addr object, addr *ret)
{
	GetConst(CLOS_READTABLE, ret);
	return 0;
}

static int class_of_symbol_(addr object, addr *ret)
{
	if (keywordp(object))
		GetConst(CLOS_KEYWORD, ret);
	else
		GetConst(CLOS_SYMBOL, ret);
	return 0;
}

static int class_of_fixnum_(addr object, addr *ret)
{
	GetConst(CLOS_FIXNUM, ret);
	return 0;
}

static int class_of_bignum_(addr object, addr *ret)
{
	GetConst(CLOS_BIGNUM, ret);
	return 0;
}

static int class_of_ratio_(addr object, addr *ret)
{
	GetConst(CLOS_RATIO, ret);
	return 0;
}

static int class_of_short_float_(addr object, addr *ret)
{
	GetConst(CLOS_SHORT_FLOAT, ret);
	return 0;
}

static int class_of_single_float_(addr object, addr *ret)
{
	GetConst(CLOS_SINGLE_FLOAT, ret);
	return 0;
}

static int class_of_double_float_(addr object, addr *ret)
{
	GetConst(CLOS_DOUBLE_FLOAT, ret);
	return 0;
}

static int class_of_long_float_(addr object, addr *ret)
{
	GetConst(CLOS_LONG_FLOAT, ret);
	return 0;
}

static int class_of_complex_(addr object, addr *ret)
{
	GetConst(CLOS_COMPLEX, ret);
	return 0;
}

static int class_of_function_(addr object, addr *ret)
{
	if (compiled_function_p(object))
		GetConst(CLOS_COMPILED_FUNCTION, ret);
	else
		GetConst(CLOS_FUNCTION, ret);
	return 0;
}

static int class_of_package_(addr object, addr *ret)
{
	GetConst(CLOS_PACKAGE, ret);
	return 0;
}

static int class_of_random_state_(addr object, addr *ret)
{
	GetConst(CLOS_RANDOM_STATE, ret);
	return 0;
}

static int class_of_pathname_(addr object, addr *ret)
{
	if (pathname_logical_p(object))
		GetConst(CLOS_LOGICAL_PATHNAME, ret);
	else
		GetConst(CLOS_PATHNAME, ret);
	return 0;
}

static int class_of_stream_(addr object, addr *ret)
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
		case StreamType_Probe:
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

	return 0;
}

static int class_of_restart_(addr object, addr *ret)
{
	GetConst(CLOS_RESTART, ret);
	return 0;
}

static int class_of_bit_vector_(addr object, addr *ret)
{
	GetConst(CLOS_SIMPLE_BIT_VECTOR, ret);
	return 0;
}

void init_clos_type(void)
{
	size_t i;

	/* error */
	for (i = 0; i < (size_t)LISPTYPE_SIZE; i++)
		class_of_call[i] = class_of_error_;

	/* class-of */
	class_of_call[LISPTYPE_NIL] = class_of_nil_;
	class_of_call[LISPTYPE_T] = class_of_symbol_;
	class_of_call[LISPTYPE_CONS] = class_of_cons_;
	class_of_call[LISPTYPE_ARRAY] = class_of_array_;
	class_of_call[LISPTYPE_VECTOR] = class_of_vector_;
	class_of_call[LISPTYPE_CHARACTER] = class_of_character_;
	class_of_call[LISPTYPE_STRING] = class_of_string_;
	class_of_call[LISPTYPE_HASHTABLE] = class_of_hashtable_;
	class_of_call[LISPTYPE_READTABLE] = class_of_readtable_;
	class_of_call[LISPTYPE_SYMBOL] = class_of_symbol_;
	class_of_call[LISPTYPE_FIXNUM] = class_of_fixnum_;
	class_of_call[LISPTYPE_BIGNUM] = class_of_bignum_;
	class_of_call[LISPTYPE_RATIO] = class_of_ratio_;
	class_of_call[LISPTYPE_SHORT_FLOAT] = class_of_short_float_;
	class_of_call[LISPTYPE_SINGLE_FLOAT] = class_of_single_float_;
	class_of_call[LISPTYPE_DOUBLE_FLOAT] = class_of_double_float_;
	class_of_call[LISPTYPE_LONG_FLOAT] = class_of_long_float_;
	class_of_call[LISPTYPE_COMPLEX] = class_of_complex_;
	class_of_call[LISPTYPE_FUNCTION] = class_of_function_;
	class_of_call[LISPTYPE_PACKAGE] = class_of_package_;
	class_of_call[LISPTYPE_RANDOM_STATE] = class_of_random_state_;
	class_of_call[LISPTYPE_PATHNAME] = class_of_pathname_;
	class_of_call[LISPTYPE_STREAM] = class_of_stream_;
	class_of_call[LISPTYPE_RESTART] = class_of_restart_;
	class_of_call[LISPTYPE_BITVECTOR] = class_of_bit_vector_;
}

int clos_class_of_(addr object, addr *ret)
{
	enum LISPTYPE type;

	type = GetType(object);
	if (type == LISPTYPE_CLOS) {
		/* clos or structure */
		GetClassOfClos(object, ret);
		return 0;
	}

	/* built-in-class */
	return (class_of_call[(size_t)type])(object, ret);
}


/*
 *  specializer
 */
int clos_intern_specializer_(addr object, addr *ret)
{
	addr pos, type;

	Return(clos_find_specializer_nil_(object, &pos));
	if (pos != Nil)
		return Result(ret, pos);

	/* make eql-specializer */
	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	Return(clos_instance_heap_(pos, &pos));
	/* define eql-specializer */
	Return(clos_class_of_(object, &type));
	Return(stdset_specializer_object_(pos, object));
	Return(stdset_specializer_type_(pos, type));
	Return(clos_define_specializer_(object, pos));
	/* result */
	return Result(ret, pos);
}

