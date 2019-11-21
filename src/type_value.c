#include "clos_type.h"
#include "bit.h"
#include "cmpl.h"
#include "condition.h"
#include "function.h"
#include "integer.h"
#include "rational.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"
#include "type_value.h"

typedef void (*type_value_call)(addr *, addr);
static type_value_call TypeValueTable[LISPTYPE_SIZE];

_g void type_value_nil(addr *ret)
{
	GetTypeTable(ret, Null);
}

_g void type_value_t(addr *ret)
{
	GetTypeTable(ret, Boolean);
}

static void type_value_nil_call(addr *ret, addr value)
{
	GetTypeTable(ret, Null);
}

static void type_value_t_call(addr *ret, addr value)
{
	GetTypeTable(ret, Boolean);
}

static void type_value_type(addr *ret, addr value)
{
	GetTypeTable(ret, Type);
}

_g void type_value_clos(addr *ret, addr value)
{
	clos_class_of(value, &value);
	type_clos_heap(value, ret);
}

static void type_value_cons(addr *ret, addr value)
{
	GetTypeTable(ret, Cons);
}

static void type_value_strarray(addr *ret, addr value)
{
	enum LISPDECL decl;
	enum CHARACTER_TYPE type;
	int simple;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	Check(! strarrayp(value), "type array error");
	simple = ArrayInfoStruct(value)->simple;
	strarray_length(value, &size);
	make_index_integer_heap(&arg, size);

	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			decl = simple? LISPDECL_SIMPLE_BASE_STRING: LISPDECL_BASE_STRING;
			type1_heap(decl, arg, &pos);
			break;

		default:
			decl = simple? LISPDECL_SIMPLE_STRING: LISPDECL_STRING;
			type1_heap(decl, arg, &pos);
			break;
	}
	*ret = pos;
}

static void type_value_array_nil(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	fixnum_heap(&pos, 0);
	type2_heap(decl, type, pos, ret);
}

static void type_value_array_single(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	make_index_integer_heap(&pos, str->size);
	vector4_heap(&array, 1);
	SetArrayA4(array, 0, pos);
	type2_heap(decl, type, array, ret);
}

static void type_value_array_multiple(addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;
	size_t *psize, size, i;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &type);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	size = str->dimension;

	Check(GetType(pos) != LISPSYSTEM_ARRAY_DIMENSION, "type dimension error");
	psize = arraysize_ptr(pos);
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		make_index_integer_heap(&pos, psize[i]);
		SetArrayA4(array, i, pos);
	}
	type2_heap(decl, type, array, ret);
}

_g void type_value_array(addr *ret, addr value)
{
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	size = ArrayInfoStruct(value)->dimension;
	if (array_stringp(value))
		type_value_strarray(ret, value);
	else if (size == 0)
		type_value_array_nil(ret, value);
	else if (size == 1)
		type_value_array_single(ret, value);
	else
		type_value_array_multiple(ret, value);
}

_g void type_value_vector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_VECTOR, "type error");
	lenarray(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_VECTOR, arg, ret);
}

_g void type_value_character(addr *ret, addr value)
{
	addr pos;
	enum CHARACTER_TYPE type;

	Check(GetType(value) != LISPTYPE_CHARACTER, "type error");
	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
			GetTypeTable(&pos, StandardChar);
			break;

		case CHARACTER_TYPE_BASE:
			GetTypeTable(&pos, BaseChar);
			break;

		case CHARACTER_TYPE_EXTENDED:
			GetTypeTable(&pos, ExtendedChar);
			break;

		default:
			fmte("Invalid character type.", NULL);
			return;
	}
	*ret = pos;
}

static void type_value_strvect(addr *ret, addr value)
{
	enum CHARACTER_TYPE type;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_STRING, "type error");
	strvect_length(value, &size);
	make_index_integer_heap(&arg, size);

	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			type1_heap(LISPDECL_SIMPLE_BASE_STRING, arg, &pos);
			break;

		default:
			type1_heap(LISPDECL_SIMPLE_STRING, arg, &pos);
			break;
	}
	*ret = pos;
}

_g void type_value_string(addr *ret, addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_STRING:
			type_value_strvect(ret, value);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(value))
				fmte("The array is not string type.", NULL);
			type_value_strarray(ret, value);
			break;

		default:
			fmte("The value is not string type.", NULL);
			break;
	}
}

static void type_value_hashtable(addr *ret, addr value)
{
	GetTypeTable(ret, Hashtable);
}

static void type_value_readtable(addr *ret, addr value)
{
	GetTypeTable(ret, Readtable);
}

static void type_value_symbol(addr *ret, addr value)
{
	Check(GetType(value) != LISPTYPE_SYMBOL, "type error");
	if (keywordp(value))
		GetTypeTable(ret, Keyword);
	else
		GetTypeTable(ret, Symbol);
}

static void type_realvalue(enum LISPDECL type, addr value, addr *ret)
{
	type4_heap(type, Nil, value, Nil, value, ret);
}

_g void type_value_integer(addr *ret, addr value)
{
	Check(! integerp(value), "type error");
	type_realvalue(LISPDECL_INTEGER, value, ret);
}

_g void type_value_rational(addr *ret, addr value)
{
	Check(! rationalp(value), "type error");
	type_realvalue(LISPDECL_RATIONAL, value, ret);
}

static void type_value_single(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_SINGLE_FLOAT);
	type_realvalue(LISPDECL_SINGLE_FLOAT, value, ret);
}

static void type_value_double(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_DOUBLE_FLOAT);
	type_realvalue(LISPDECL_DOUBLE_FLOAT, value, ret);
}

static void type_value_long(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_LONG_FLOAT);
	type_realvalue(LISPDECL_LONG_FLOAT, value, ret);
}

_g void type_value_float(addr *ret, addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_SINGLE_FLOAT:
			type_value_single(ret, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			type_value_double(ret, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			type_value_long(ret, value);
			break;

		default:
			TypeError(value, FLOAT);
			return;
	}
}

_g void type_value_complex(addr *ret, addr value)
{
	addr real, imag, type;

	GetRealComplex(value, &real);
	GetImagComplex(value, &imag);
	type_value(&real, real);
	type_value(&imag, imag);
	type2or_heap(real, imag, &type);
	type1_heap(LISPDECL_COMPLEX, type, ret);
}

static void type_value_function(addr *ret, addr value)
{
	Check(! functionp(value), "type error");
	gettype_function(value, ret);
	if (*ret == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(ret, CompiledFunction);
		else
			GetTypeTable(ret, Function);
	}
}

static void type_value_package(addr *ret, addr value)
{
	GetTypeTable(ret, Package);
}

static void type_value_random_state(addr *ret, addr value)
{
	GetTypeTable(ret, RandomState);
}

_g void type_value_pathname(addr *ret, addr value)
{
	Check(! pathnamep(value), "type error");
	if (pathname_pathname_p(value))
		GetTypeTable(ret, Pathname);
	else
		GetTypeTable(ret, LogicalPathname);
}

_g void type_value_environment(addr *ret, addr value)
{
	GetTypeTable(ret, Environment);
}

static void type_value_stream(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_STREAM);
	switch (getstreamtype(value)) {
		case StreamType_BroadCast:
			GetTypeTable(ret, BroadcastStream);
			return;

		case StreamType_Concatenated:
			GetTypeTable(ret, ConcatenatedStream);
			return;

		case StreamType_Echo:
			GetTypeTable(ret, EchoStream);
			return;

		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetTypeTable(ret, StringStream);
			return;

		case StreamType_Synonym:
			GetTypeTable(ret, SynonymStream);
			return;

		case StreamType_TwoWay:
			GetTypeTable(ret, TwoWayStream);
			return;

		case StreamType_Prompt:
			GetTypeTable(ret, PromptStream);
			return;

		case StreamType_Pretty:
			GetTypeTable(ret, PrettyStream);
			return;

		default:
			break;
	}

	GetPathnameStream(value, &value);
	if (value != Nil)
		GetTypeTable(ret, FileStream);
	else
		GetTypeTable(ret, Stream);
}

static void type_value_restart(addr *ret, addr value)
{
	GetTypeTable(ret, Restart);
}

_g void type_value_bitvector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_BITVECTOR, "type error");
	bitmemory_length(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_BIT_VECTOR, arg, ret);
}

static void type_value_quote(addr *ret, addr value)
{
	GetTypeTable(ret, Quote);
}

static void type_value_bytespec(addr *ret, addr value)
{
	GetTypeTable(ret, ByteSpec);
}

static void type_value_print_dispatch(addr *ret, addr value)
{
	GetTypeTable(ret, PrintDispatch);
}

static void type_value_argument(addr *ret, addr value)
{
	GetTypeTable(ret, T);
}

static void type_value_error(addr *ret, addr value)
{
	fmte("Invalid type-value ~S.", value, NULL);
}

_g void type_value(addr *ret, addr value)
{
	type_value_call call;

	call = TypeValueTable[GetType(value)];
	if (call == NULL)
		type_value_error(ret, value);
	else
		call(ret, value);
}

_g void init_type_value(void)
{
	TypeValueTable[LISPTYPE_NIL] = type_value_nil_call;
	TypeValueTable[LISPTYPE_T] = type_value_t_call;
	TypeValueTable[LISPTYPE_TYPE] = type_value_type;
	TypeValueTable[LISPTYPE_CLOS] = type_value_clos;
	TypeValueTable[LISPTYPE_CONS] = type_value_cons;
	TypeValueTable[LISPTYPE_ARRAY] = type_value_array;
	TypeValueTable[LISPTYPE_VECTOR] = type_value_vector;
	TypeValueTable[LISPTYPE_CHARACTER] = type_value_character;
	TypeValueTable[LISPTYPE_STRING] = type_value_string;
	TypeValueTable[LISPTYPE_HASHTABLE] = type_value_hashtable;
	TypeValueTable[LISPTYPE_READTABLE] = type_value_readtable;
	TypeValueTable[LISPTYPE_SYMBOL] = type_value_symbol;
	TypeValueTable[LISPTYPE_FIXNUM] = type_value_integer;
	TypeValueTable[LISPTYPE_BIGNUM] = type_value_integer;
	TypeValueTable[LISPTYPE_RATIO] = type_value_rational;
	TypeValueTable[LISPTYPE_SHORT_FLOAT] = type_value_error;
	TypeValueTable[LISPTYPE_SINGLE_FLOAT] = type_value_single;
	TypeValueTable[LISPTYPE_DOUBLE_FLOAT] = type_value_double;
	TypeValueTable[LISPTYPE_LONG_FLOAT] = type_value_long;
	TypeValueTable[LISPTYPE_COMPLEX] = type_value_complex;
	TypeValueTable[LISPTYPE_CONTROL] = type_value_error;
	TypeValueTable[LISPTYPE_CODE] = type_value_error;
	TypeValueTable[LISPTYPE_CALLNAME] = type_value_error;
	TypeValueTable[LISPTYPE_FUNCTION] = type_value_function;
	TypeValueTable[LISPTYPE_INDEX] = type_value_error;
	TypeValueTable[LISPTYPE_SYSTEM] = type_value_error;
	TypeValueTable[LISPTYPE_PACKAGE] = type_value_package;
	TypeValueTable[LISPTYPE_RANDOM_STATE] = type_value_random_state;
	TypeValueTable[LISPTYPE_PATHNAME] = type_value_pathname;
	TypeValueTable[LISPTYPE_STREAM] = type_value_stream;
	TypeValueTable[LISPTYPE_RESTART] = type_value_restart;
	TypeValueTable[LISPTYPE_EVAL] = type_value_error;
	TypeValueTable[LISPTYPE_ENVIRONMENT] = type_value_environment;
	TypeValueTable[LISPTYPE_BITVECTOR] = type_value_bitvector;
	TypeValueTable[LISPTYPE_QUOTE] = type_value_quote;
	TypeValueTable[LISPTYPE_BYTESPEC] = type_value_bytespec;
	TypeValueTable[LISPTYPE_PRINT_DISPATCH] = type_value_print_dispatch;
	TypeValueTable[LISPSYSTEM_ARGUMENT] = type_value_argument;
}

