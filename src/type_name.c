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

static int type_name_clos_(addr pos, addr *value, int *ret)
{
	Return(clos_class_of_(pos, &pos));
	Return(stdget_class_name_(pos, value));
	return Result(ret, 0);
}

static int type_name_symbol_(addr pos, addr *value, int *ret)
{
	if (keywordp(pos))
		GetConst(COMMON_KEYWORD, value);
	else
		GetConst(COMMON_SYMBOL, value);

	return Result(ret, 0);
}

static int type_name_function_(addr pos, addr *value, int *ret)
{
	if (interpreted_funcall_function_p(pos)) {
		GetConst(COMMON_FUNCTION, value);
		goto normal;
	}
	if (interpreted_macro_function_p(pos)) {
		GetConst(COMMON_MACRO_FUNCTION, value);
		goto normal;
	}
	if (compiled_funcall_function_p(pos)) {
		GetConst(COMMON_COMPILED_FUNCTION, value);
		goto normal;
	}
	if (compiled_macro_function_p(pos)) {
		GetConst(SYSTEM_COMPILED_MACRO_FUNCTION, value);
		goto normal;
	}
	else {
		GetConst(COMMON_FUNCTION, value);
		goto normal;
	}

normal:
	return Result(ret, 0);
}

static int type_name_stream_(addr pos, addr *value, int *ret)
{
	switch (getstreamtype(pos)) {
		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetConst(COMMON_STRING_STREAM, value);
			goto normal;

		case StreamType_Synonym:
			GetConst(COMMON_SYNONYM_STREAM, value);
			goto normal;

		case StreamType_BroadCast:
			GetConst(COMMON_BROADCAST_STREAM, value);
			goto normal;

		case StreamType_Concatenated:
			GetConst(COMMON_CONCATENATED_STREAM, value);
			goto normal;

		case StreamType_Echo:
			GetConst(COMMON_ECHO_STREAM, value);
			goto normal;

		case StreamType_TwoWay:
			GetConst(COMMON_TWO_WAY_STREAM, value);
			goto normal;

		case StreamType_Prompt:
			GetConst(SYSTEM_PROMPT_STREAM, value);
			goto normal;

		case StreamType_Pretty:
			GetConst(SYSTEM_PRETTY_STREAM, value);
			goto normal;

		default:
			break;
	}

	GetPathnameStream(pos, &pos);
	if (pos != Nil) {
		GetConst(COMMON_FILE_STREAM, value);
		goto normal;
	}

	GetConst(COMMON_STREAM, value);
	goto normal;

normal:
	return Result(ret, 0);
}

_g int type_name_p_(addr pos, addr *value, int *ret)
{
	enum LISPTYPE type;
	constindex index;

	/* table */
	type = GetType(pos);
	index = TypeNameTable[(int)type];
	if (index != CONSTANT_EMPTY) {
		GetConstant(index, value);
		return Result(ret, 0);
	}

	/* others */
	switch (type) {
		case LISPTYPE_CLOS:
			return type_name_clos_(pos, value, ret);

		case LISPTYPE_SYMBOL:
			return type_name_symbol_(pos, value, ret);

		case LISPTYPE_FUNCTION:
			return type_name_function_(pos, value, ret);

		case LISPTYPE_STREAM:
			return type_name_stream_(pos, value, ret);

		default:
			*value = Nil;
			return Result(ret, 1);
	}
}

_g int type_name_(addr pos, addr *value)
{
	int check;

	Return(type_name_p_(pos, value, &check));
	if (check) {
		*value = Nil;
		return fmte_("Invalid value, ~A.", pos, NULL);
	}

	return 0;
}

_g void init_type_name(void)
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
	DefTypeName(LISPTYPE_PACKAGE, COMMON_PACKAGE);
	DefTypeName(LISPTYPE_RANDOM_STATE, COMMON_RANDOM_STATE);
	DefTypeName(LISPTYPE_PATHNAME, COMMON_PATHNAME);
	/*DefTypeName(LISPTYPE_STREAM, COMMON_EMPTY);*/
	DefTypeName(LISPTYPE_QUOTE, SYSTEM_QUOTE);
	DefTypeName(LISPTYPE_RESTART, COMMON_RESTART);
	DefTypeName(LISPTYPE_EVAL, SYSTEM_EVAL);
	DefTypeName(LISPTYPE_ENVIRONMENT, SYSTEM_ENVIRONMENT);
	DefTypeName(LISPTYPE_BITVECTOR, COMMON_BIT_VECTOR);
	DefTypeName(LISPTYPE_BYTESPEC, SYSTEM_BYTESPEC);
	DefTypeName(LISPTYPE_PRINT_DISPATCH, SYSTEM_PRINT_DISPATCH);
	DefTypeName(LISPTYPE_EVAL, SYSTEM_EVAL);

	DefTypeName(LISPSYSTEM_CHARACTER2, SYSTEM_CHARACTER2);
	DefTypeName(LISPSYSTEM_CHARQUEUE, SYSTEM_CHARQUEUE);
	DefTypeName(LISPSYSTEM_CHARBIT, SYSTEM_CHARBIT);
	DefTypeName(LISPSYSTEM_SYMSTACK, SYSTEM_SYMSTACK);
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

