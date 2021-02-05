#include "character.h"
#include "clos_type.h"
#include "bit.h"
#include "cmpl.h"
#include "condition.h"
#include "function.h"
#include "integer.h"
#include "rational.h"
#include "pathname_object.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"
#include "type_value.h"

typedef int (*type_value_call)(addr *, addr);
static type_value_call TypeValueTable[LISPTYPE_SIZE];

/* nil */
void type_value_nil(addr *ret)
{
	GetTypeTable(ret, Null);
}

static int type_value_nil_(addr *ret, addr value)
{
	GetTypeTable(ret, Null);
	return 0;
}

/* t */
void type_value_t(addr *ret)
{
	GetTypeTable(ret, Boolean);
}

static int type_value_t_(addr *ret, addr value)
{
	GetTypeTable(ret, Boolean);
	return 0;
}

/* clos */
int type_value_clos_(addr *ret, addr value)
{
	Return(clos_class_of_(value, &value));
	type_clos_heap(value, ret);
	return 0;
}

/* cons */
static int type_value_cons_(addr *ret, addr value)
{
	GetTypeTable(ret, Cons);
	return 0;
}

/* array */
static int type_value_strarray_(addr *ret, addr value)
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

	Return(strarray_character_type_(value, &type));
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

	return Result(ret, pos);
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

int type_value_array_(addr *ret, addr value)
{
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	size = ArrayInfoStruct(value)->dimension;
	if (array_stringp(value)) {
		Return(type_value_strarray_(ret, value));
	}
	else if (size == 0) {
		type_value_array_nil(ret, value);
	}
	else if (size == 1) {
		type_value_array_single(ret, value);
	}
	else {
		type_value_array_multiple(ret, value);
	}

	return 0;
}

/* vector */
void type_value_vector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_VECTOR, "type error");
	lenarray(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_VECTOR, arg, ret);
}

static int type_value_vector_(addr *ret, addr value)
{
	type_value_vector(ret, value);
	return 0;
}

/* character */
void type_value_character(addr *ret, addr value)
{
	enum CHARACTER_TYPE type;

	Check(GetType(value) != LISPTYPE_CHARACTER, "type error");
	get_character_type(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
			GetTypeTable(ret, StandardChar);
			break;

		case CHARACTER_TYPE_BASE:
			GetTypeTable(ret, BaseChar);
			break;

		case CHARACTER_TYPE_EXTENDED:
			GetTypeTable(ret, ExtendedChar);
			break;

		default:
			GetTypeTable(ret, Character);
			break;
	}
}

static int type_value_character_(addr *ret, addr value)
{
	type_value_character(ret, value);
	return 0;
}

/* string */
static int type_value_string_(addr *ret, addr value)
{
	enum CHARACTER_TYPE type;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_STRING, "type error");
	strvect_length(value, &size);
	make_index_integer_heap(&arg, size);

	Return(strvect_character_type_(value, &type));
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			type1_heap(LISPDECL_SIMPLE_BASE_STRING, arg, &pos);
			break;

		default:
			type1_heap(LISPDECL_SIMPLE_STRING, arg, &pos);
			break;
	}

	return Result(ret, pos);
}

/* hashtable */
static int type_value_hashtable_(addr *ret, addr value)
{
	GetTypeTable(ret, Hashtable);
	return 0;
}

/* readtable */
static int type_value_readtable_(addr *ret, addr value)
{
	GetTypeTable(ret, Readtable);
	return 0;
}

/* symbol */
static int type_value_symbol_(addr *ret, addr value)
{
	Check(GetType(value) != LISPTYPE_SYMBOL, "type error");
	if (keywordp(value))
		GetTypeTable(ret, Keyword);
	else
		GetTypeTable(ret, Symbol);

	return 0;
}

/* integer */
static void type_realvalue(enum LISPDECL type, addr value, addr *ret)
{
	type4_heap(type, Nil, value, Nil, value, ret);
}

void type_value_integer(addr *ret, addr value)
{
	Check(! integerp(value), "type error");
	type_realvalue(LISPDECL_INTEGER, value, ret);
}

static int type_value_integer_(addr *ret, addr value)
{
	type_value_integer(ret, value);
	return 0;
}

/* rational */
void type_value_rational(addr *ret, addr value)
{
	Check(! rationalp(value), "type error");
	type_realvalue(LISPDECL_RATIONAL, value, ret);
}

static int type_value_rational_(addr *ret, addr value)
{
	type_value_rational(ret, value);
	return 0;
}

/* single */
static int type_value_single_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_SINGLE_FLOAT);
	type_realvalue(LISPDECL_SINGLE_FLOAT, value, ret);
	return 0;
}

/* double */
static int type_value_double_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_DOUBLE_FLOAT);
	type_realvalue(LISPDECL_DOUBLE_FLOAT, value, ret);
	return 0;
}

/* long */
static int type_value_long_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_LONG_FLOAT);
	type_realvalue(LISPDECL_LONG_FLOAT, value, ret);
	return 0;
}

void type_value_float(addr *ret, addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_SINGLE_FLOAT:
			(void)type_value_single_(ret, value);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			(void)type_value_double_(ret, value);
			break;

		case LISPTYPE_LONG_FLOAT:
			(void)type_value_long_(ret, value);
			break;

		default:
			Abort("type error");
			break;
	}
}

/* complex */
int type_value_complex_(addr *ret, addr value)
{
	addr real, imag, type;

	GetRealComplex(value, &real);
	GetImagComplex(value, &imag);
	Return(type_value_(&real, real));
	Return(type_value_(&imag, imag));
	type2or_heap(real, imag, &type);
	type1_heap(LISPDECL_COMPLEX, type, ret);

	return 0;
}

/* function */
static int type_value_function_(addr *ret, addr value)
{
	Check(! functionp(value), "type error");
	gettype_function(value, ret);
	if (*ret == Nil) {
		if (compiled_function_p(value))
			GetTypeTable(ret, CompiledFunction);
		else
			GetTypeTable(ret, Function);
	}

	return 0;
}

/* package */
static int type_value_package_(addr *ret, addr value)
{
	GetTypeTable(ret, Package);
	return 0;
}

/* random-state */
void type_value_random_state(addr *ret, addr value)
{
	GetTypeTable(ret, RandomState);
}

static int type_value_random_state_(addr *ret, addr value)
{
	type_value_random_state(ret, value);
	return 0;
}

/* pathname */
void type_value_pathname(addr *ret, addr value)
{
	Check(! pathnamep(value), "type error");
	if (pathname_pathname_p(value))
		GetTypeTable(ret, Pathname);
	else
		GetTypeTable(ret, LogicalPathname);
}

static int type_value_pathname_(addr *ret, addr value)
{
	type_value_pathname(ret, value);
	return 0;
}

/* environment */
void type_value_environment(addr *ret, addr value)
{
	GetTypeTable(ret, Environment);
}

static int type_value_environment_(addr *ret, addr value)
{
	type_value_environment(ret, value);
	return 0;
}

/* stream */
static int type_value_stream_(addr *ret, addr value)
{
	CheckType(value, LISPTYPE_STREAM);
	switch (getstreamtype(value)) {
		case StreamType_BroadCast:
			GetTypeTable(ret, BroadcastStream);
			return 0;

		case StreamType_Concatenated:
			GetTypeTable(ret, ConcatenatedStream);
			return 0;

		case StreamType_Echo:
			GetTypeTable(ret, EchoStream);
			return 0;

		case StreamType_StringInput:
		case StreamType_StringOutput:
			GetTypeTable(ret, StringStream);
			return 0;

		case StreamType_Synonym:
			GetTypeTable(ret, SynonymStream);
			return 0;

		case StreamType_TwoWay:
			GetTypeTable(ret, TwoWayStream);
			return 0;

		case StreamType_Prompt:
			GetTypeTable(ret, PromptStream);
			return 0;

		case StreamType_Pretty:
			GetTypeTable(ret, PrettyStream);
			return 0;

		default:
			break;
	}

	GetPathnameStream(value, &value);
	if (value != Nil)
		GetTypeTable(ret, FileStream);
	else
		GetTypeTable(ret, Stream);

	return 0;
}

/* restart */
static int type_value_restart_(addr *ret, addr value)
{
	GetTypeTable(ret, Restart);
	return 0;
}

/* eval */
static int type_value_eval_(addr *ret, addr value)
{
	GetTypeTable(ret, Eval);
	return 0;
}

/* bit-vector */
void type_value_bitvector(addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_BITVECTOR, "type error");
	bitmemory_length(value, &size);
	make_index_integer_heap(&arg, size);
	type1_heap(LISPDECL_SIMPLE_BIT_VECTOR, arg, ret);
}

static int type_value_bitvector_(addr *ret, addr value)
{
	type_value_bitvector(ret, value);
	return 0;
}

/* quote */
static int type_value_quote_(addr *ret, addr value)
{
	GetTypeTable(ret, Quote);
	return 0;
}

static int type_value_bytespec_(addr *ret, addr value)
{
	GetTypeTable(ret, ByteSpec);
	return 0;
}

static int type_value_print_dispatch_(addr *ret, addr value)
{
	GetTypeTable(ret, PrintDispatch);
	return 0;
}

static int type_value_argument_(addr *ret, addr value)
{
	GetTypeTable(ret, T);
	return 0;
}

static int type_value_error_(addr *ret, addr value)
{
	infobit(value);
	return fmte_("Invalid type-value.", NULL);
}

int type_value_(addr *ret, addr value)
{
	type_value_call call;

	call = TypeValueTable[GetType(value)];
	if (call == NULL)
		return type_value_error_(ret, value);
	else
		return (*call)(ret, value);
}

void init_type_value(void)
{
	TypeValueTable[LISPTYPE_NIL] = type_value_nil_;
	TypeValueTable[LISPTYPE_T] = type_value_t_;
	TypeValueTable[LISPTYPE_CLOS] = type_value_clos_;
	TypeValueTable[LISPTYPE_CONS] = type_value_cons_;
	TypeValueTable[LISPTYPE_ARRAY] = type_value_array_;
	TypeValueTable[LISPTYPE_VECTOR] = type_value_vector_;
	TypeValueTable[LISPTYPE_CHARACTER] = type_value_character_;
	TypeValueTable[LISPTYPE_STRING] = type_value_string_;
	TypeValueTable[LISPTYPE_HASHTABLE] = type_value_hashtable_;
	TypeValueTable[LISPTYPE_READTABLE] = type_value_readtable_;
	TypeValueTable[LISPTYPE_SYMBOL] = type_value_symbol_;
	TypeValueTable[LISPTYPE_FIXNUM] = type_value_integer_;
	TypeValueTable[LISPTYPE_BIGNUM] = type_value_integer_;
	TypeValueTable[LISPTYPE_RATIO] = type_value_rational_;
	TypeValueTable[LISPTYPE_SHORT_FLOAT] = type_value_error_;
	TypeValueTable[LISPTYPE_SINGLE_FLOAT] = type_value_single_;
	TypeValueTable[LISPTYPE_DOUBLE_FLOAT] = type_value_double_;
	TypeValueTable[LISPTYPE_LONG_FLOAT] = type_value_long_;
	TypeValueTable[LISPTYPE_COMPLEX] = type_value_complex_;
	TypeValueTable[LISPTYPE_CONTROL] = type_value_error_;
	TypeValueTable[LISPTYPE_CODE] = type_value_error_;
	TypeValueTable[LISPTYPE_CALLNAME] = type_value_error_;
	TypeValueTable[LISPTYPE_FUNCTION] = type_value_function_;
	TypeValueTable[LISPTYPE_INDEX] = type_value_error_;
	TypeValueTable[LISPTYPE_PACKAGE] = type_value_package_;
	TypeValueTable[LISPTYPE_RANDOM_STATE] = type_value_random_state_;
	TypeValueTable[LISPTYPE_PATHNAME] = type_value_pathname_;
	TypeValueTable[LISPTYPE_STREAM] = type_value_stream_;
	TypeValueTable[LISPTYPE_RESTART] = type_value_restart_;
	TypeValueTable[LISPTYPE_EVAL] = type_value_eval_;
	TypeValueTable[LISPTYPE_ENVIRONMENT] = type_value_environment_;
	TypeValueTable[LISPTYPE_BITVECTOR] = type_value_bitvector_;
	TypeValueTable[LISPTYPE_QUOTE] = type_value_quote_;
	TypeValueTable[LISPTYPE_BYTESPEC] = type_value_bytespec_;
	TypeValueTable[LISPTYPE_PRINT_DISPATCH] = type_value_print_dispatch_;
	TypeValueTable[LISPSYSTEM_ARGUMENT] = type_value_argument_;
}

