#include "clos_class.h"
#include "clos_type.h"
#include "constant.h"
#include "condition.h"
#include "function.h"
#include "stream.h"
#include "symbol.h"
#include "type_name.h"

static constindex TypeNameTable[LISPTYPE_SIZE];
#define DefTypeName(x,y) (TypeNameTable[x] = CONSTANT_##y)

static void type_name_clos(addr *ret, addr value)
{
	clos_class_of(value, &value);
	stdget_class_name(value, ret);
}

static void type_name_symbol(addr *ret, addr value)
{
	if (keywordp(value))
		GetConst(COMMON_KEYWORD, ret);
	else
		GetConst(COMMON_SYMBOL, ret);
}

static void type_name_function(addr *ret, addr value)
{
	if (system_function_p(value)) {
		if (macro_function_p(value))
			GetConst(SYSTEM_SYSTEM_MACRO_FUNCTION, ret);
		else
			GetConst(SYSTEM_SYSTEM_FUNCTION, ret);
		return;
	}
	if (interpreted_funcall_function_p(value)) {
		GetConst(COMMON_FUNCTION, ret);
		return;
	}
	if (interpreted_macro_function_p(value)) {
		GetConst(COMMON_MACRO_FUNCTION, ret);
		return;
	}
	if (compiled_funcall_function_p(value)) {
		GetConst(COMMON_COMPILED_FUNCTION, ret);
		return;
	}
	if (compiled_macro_function_p(value)) {
		GetConst(SYSTEM_COMPILED_MACRO_FUNCTION, ret);
		return;
	}
	else {
		GetConst(COMMON_FUNCTION, ret);
		return;
	}
}

static void type_name_stream(addr *ret, addr value)
{
	switch (getstreamtype(value)) {
		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetConst(COMMON_STRING_STREAM, ret);
			return;

		case StreamType_Synonym:
			GetConst(COMMON_SYNONYM_STREAM, ret);
			return;

		case StreamType_BroadCast:
			GetConst(COMMON_BROADCAST_STREAM, ret);
			return;

		case StreamType_Concatenated:
			GetConst(COMMON_CONCATENATED_STREAM, ret);
			return;

		case StreamType_Echo:
			GetConst(COMMON_ECHO_STREAM, ret);
			return;

		case StreamType_TwoWay:
			GetConst(COMMON_TWO_WAY_STREAM, ret);
			return;

		case StreamType_Prompt:
			GetConst(SYSTEM_PROMPT_STREAM, ret);
			return;

		default:
			break;
	}

	GetPathnameStream(value, &value);
	if (value != Nil) {
		GetConst(COMMON_FILE_STREAM, ret);
		return;
	}

	GetConst(COMMON_STREAM, ret);
}

int type_name_p(addr *ret, addr value)
{
	enum LISPTYPE type;
	constindex index;

	/* table */
	type = GetType(value);
	index = TypeNameTable[(int)type];
	if (index != CONSTANT_EMPTY) {
		GetConstant(index, ret);
		return 0;
	}

	/* others */
	switch (type) {
		case LISPTYPE_CLOS:
			type_name_clos(ret, value);
			break;

		case LISPTYPE_SYMBOL:
			type_name_symbol(ret, value);
			break;

		case LISPTYPE_FUNCTION:
			type_name_function(ret, value);
			break;

		case LISPTYPE_STREAM:
			type_name_stream(ret, value);
			break;

		default:
			return 1;
	}

	return 0;
}

void type_name(addr *ret, addr value)
{
	if (type_name_p(ret, value))
		fmte("Invalid value, ~A.", value, NULL);
}

void init_type_name(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TypeNameTable[i] = CONSTANT_EMPTY;

	DefTypeName(LISPTYPE_NIL, COMMON_NIL);
	DefTypeName(LISPTYPE_T, COMMON_T);
	DefTypeName(LISPTYPE_TYPE, COMMON_TYPE);
	/*DefTypeName(LISPTYPE_CLOS, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_CONS, COMMON_CONS);
	DefTypeName(LISPTYPE_ARRAY, COMMON_ARRAY);
	DefTypeName(LISPTYPE_VECTOR, COMMON_VECTOR);
	DefTypeName(LISPTYPE_CHARACTER, COMMON_CHARACTER);
	DefTypeName(LISPTYPE_STRING, COMMON_STRING);
	DefTypeName(LISPTYPE_HASHTABLE, COMMON_HASH_TABLE);
	DefTypeName(LISPTYPE_READTABLE, COMMON_READTABLE);
	/*DefTypeName(LISPTYPE_SYMBOL, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_FIXNUM, COMMON_INTEGER);
	DefTypeName(LISPTYPE_BIGNUM, COMMON_INTEGER);
	DefTypeName(LISPTYPE_RATIO, COMMON_RATIO);
	DefTypeName(LISPTYPE_SHORT_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_SINGLE_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_DOUBLE_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_LONG_FLOAT, COMMON_FLOAT);
	DefTypeName(LISPTYPE_COMPLEX, COMMON_COMPLEX);
	DefTypeName(LISPTYPE_CONTROL, SYSTEM_CONTROL);
	DefTypeName(LISPTYPE_CODE, SYSTEM_CODE);
	DefTypeName(LISPTYPE_CALLNAME, SYSTEM_CALLNAME);
	/*DefTypeName(LISPTYPE_FUNCTION, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_INDEX, SYSTEM_INDEX);
	DefTypeName(LISPTYPE_SYSTEM, SYSTEM_SYSTEM);
	DefTypeName(LISPTYPE_PACKAGE, COMMON_PACKAGE);
	DefTypeName(LISPTYPE_RANDOM_STATE, COMMON_RANDOM_STATE);
	DefTypeName(LISPTYPE_PATHNAME, COMMON_PATHNAME);
	/*DefTypeName(LISPTYPE_STREAM, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_QUOTE, SYSTEM_QUOTE);
	DefTypeName(LISPTYPE_RESTART, COMMON_RESTART);
	DefTypeName(LISPTYPE_EVAL, SYSTEM_EVAL);
	DefTypeName(LISPTYPE_ENVIRONMENT, SYSTEM_ENVIRONMENT);
	DefTypeName(LISPTYPE_BITVECTOR, COMMON_BIT_VECTOR);
	DefTypeName(LISPTYPE_PPRINT, COMMON_PPRINT);
	DefTypeName(LISPTYPE_BYTESPEC, SYSTEM_BYTESPEC);

	DefTypeName(LISPSYSTEM_CHARACTER2, SYSTEM_CHARACTER2);
	DefTypeName(LISPSYSTEM_CHARQUEUE, SYSTEM_CHARQUEUE);
	DefTypeName(LISPSYSTEM_CHARBIT, SYSTEM_CHARBIT);
	DefTypeName(LISPSYSTEM_SYMSTACK, SYSTEM_SYMSTACK);
	DefTypeName(LISPSYSTEM_SYMARRAY, SYSTEM_SYMARRAY);
	DefTypeName(LISPSYSTEM_BITTYPE, SYSTEM_BITTYPE);
	DefTypeName(LISPSYSTEM_READLABEL, SYSTEM_READLABEL);
	DefTypeName(LISPSYSTEM_READINFO, SYSTEM_READINFO_SYMBOL);
	DefTypeName(LISPSYSTEM_READTYPE, SYSTEM_READTYPE);
	DefTypeName(LISPSYSTEM_BITCONS, SYSTEM_BITCONS);
	DefTypeName(LISPSYSTEM_BITBUFFER, SYSTEM_BITBUFFER);
	DefTypeName(LISPSYSTEM_HASHITERATOR, SYSTEM_HASHITERATOR);
	DefTypeName(LISPSYSTEM_PACKAGEITERATOR, SYSTEM_PACKAGEITERATOR);
	DefTypeName(LISPSYSTEM_TAGINFO, SYSTEM_TAGINFO);
	DefTypeName(LISPSYSTEM_ARRAY_DIMENSION, SYSTEM_ARRAY_DIMENSION);
	DefTypeName(LISPSYSTEM_ARRAY_GENERAL, SYSTEM_ARRAY_GENERAL);
	DefTypeName(LISPSYSTEM_ARRAY_SPECIALIZED, SYSTEM_ARRAY_SPECIALIZED);

	DefTypeName(LISPSYSTEM_UNBOUND, SYSTEM_UNBOUND);
	DefTypeName(LISPSYSTEM_SPACE, SYSTEM_SPACE);
	DefTypeName(LISPSYSTEM_SPACE1, SYSTEM_SPACE1);
	DefTypeName(LISPSYSTEM_RESERVED, SYSTEM_RESERVED);
	DefTypeName(LISPSYSTEM_END, SYSTEM_END);
}

