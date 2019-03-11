#include "array.h"
#include "bit.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "clos_type.h"
#include "cmpl.h"
#include "cons.h"
#include "condition.h"
#include "function.h"
#include "integer.h"
#include "pathname.h"
#include "rational.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "type_number.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_value.h"

typedef void (*calltype_type_value)(LocalRoot, addr *, addr);
typedef void (*calltype_type_object)(LocalRoot, addr *, addr);
static calltype_type_value call_type_value[LISPTYPE_SIZE];
static calltype_type_object call_type_object[LISPTYPE_SIZE];
static constindex table_type_name[LISPTYPE_SIZE];

/*
 *  type-value
 */
void type_value_nil(LocalRoot local, addr *ret)
{
	type_empty(local, LISPDECL_NULL, ret);
}

void type_value_t(LocalRoot local, addr *ret)
{
	type_empty(local, LISPDECL_BOOLEAN, ret);
}

static void type_value_nil_call(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_NULL, ret);
}

static void type_value_t_call(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_BOOLEAN, ret);
}

static void type_value_type(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_TYPE, ret);
}

static void type_value_clos(LocalRoot local, addr *ret, addr value)
{
	class_of(value, ret);
}

static void type_value_cons(LocalRoot local, addr *ret, addr value)
{
	type_aster2(local, LISPDECL_CONS, ret);
}

static void type_value_strarray(LocalRoot local, addr *ret, addr value)
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
	fixnum_alloc(local, &arg, (fixnum)size);

	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			decl = simple? LISPDECL_SIMPLE_BASE_STRING: LISPDECL_BASE_STRING;
			type_object1(local, decl, arg, &pos);
			break;

		default:
			decl = simple? LISPDECL_SIMPLE_STRING: LISPDECL_STRING;
			type_object1(local, decl, arg, &pos);
			break;
	}
	*ret = pos;
}

static void type_value_array_nil(LocalRoot local, addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos;

	GetArrayInfo(value, ARRAY_INFO_TYPE, &type);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	fixnum_alloc(local, &pos, 0);
	type_object2(local, decl, type, pos, ret);
}

static void type_value_array_single(LocalRoot local, addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;

	GetArrayInfo(value, ARRAY_INFO_TYPE, &type);
	GetArrayInfo(value, ARRAY_INFO_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	make_index_integer_alloc(local, &pos, str->size);
	vector4_alloc(local, &array, 1);
	SetArrayA4(array, 0, pos);
	type_object2(local, decl, type, array, ret);
}

static void type_value_array_multiple(LocalRoot local, addr *ret, addr value)
{
	enum LISPDECL decl;
	struct array_struct *str;
	addr type, pos, array;
	size_t *psize, size, i;

	GetArrayInfo(value, ARRAY_INFO_TYPE, &type);
	GetArrayInfo(value, ARRAY_INFO_DIMENSION, &pos);
	str = ArrayInfoStruct(value);
	decl = str->simple? LISPDECL_SIMPLE_ARRAY: LISPDECL_ARRAY;
	size = str->dimension;

	Check(GetType(pos) != LISPSYSTEM_ARRAY_DIMENSION, "type dimension error");
	psize = PtrArrayDimension(pos);
	vector4_alloc(local, &array, size);
	for (i = 0; i < size; i++) {
		fixnum_alloc(local, &pos, (fixnum)psize[i]);
		SetArrayA4(array, i, pos);
	}
	type_object2(local, decl, type, array, ret);
}

void type_value_array(LocalRoot local, addr *ret, addr value)
{
	size_t size;

	Check(GetType(value) != LISPTYPE_ARRAY, "type error");
	size = ArrayInfoStruct(value)->dimension;
	if (array_stringp(value))
		type_value_strarray(local, ret, value);
	else if (size == 0)
		type_value_array_nil(local, ret, value);
	else if (size == 1)
		type_value_array_single(local, ret, value);
	else
		type_value_array_multiple(local, ret, value);
}

void type_value_vector(LocalRoot local, addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_VECTOR, "type error");
	lenarray(value, &size);
	fixnum_alloc(local, &arg, (fixnum)size);
	type_object1(local, LISPDECL_SIMPLE_VECTOR, arg, ret);
}

void type_value_character(LocalRoot local, addr *ret, addr value)
{
	addr pos;
	enum CHARACTER_TYPE type;

	Check(GetType(value) != LISPTYPE_CHARACTER, "type error");
	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
			type_empty(local, LISPDECL_STANDARD_CHAR, &pos);
			break;

		case CHARACTER_TYPE_BASE:
			type_empty(local, LISPDECL_BASE_CHAR, &pos);
			break;

		case CHARACTER_TYPE_EXTENDED:
			GetConst(COMMON_EXTENDED_CHAR, &pos);
			parse_type_alloc(local, &pos, pos);
			break;

		default:
			fmte("Invalid character type.", NULL);
			return;
	}
	*ret = pos;
}

static void type_value_strvect(LocalRoot local, addr *ret, addr value)
{
	enum CHARACTER_TYPE type;
	addr arg, pos;
	size_t size;

	Check(GetType(value) != LISPTYPE_STRING, "type error");
	strvect_length(value, &size);
	fixnum_alloc(local, &arg, (fixnum)size);

	GetCharacterType(value, &type);
	switch (type) {
		case CHARACTER_TYPE_STANDARD:
		case CHARACTER_TYPE_BASE:
			type_object1(local, LISPDECL_SIMPLE_BASE_STRING, arg, &pos);
			break;

		default:
			type_object1(local, LISPDECL_SIMPLE_STRING, arg, &pos);
			break;
	}
	*ret = pos;
}

void type_value_string(LocalRoot local, addr *ret, addr value)
{
	switch (GetType(value)) {
		case LISPTYPE_STRING:
			type_value_strvect(local, ret, value);
			break;

		case LISPTYPE_ARRAY:
			if (! strarrayp(value))
				fmte("The array is not string type.", NULL);
			type_value_strarray(local, ret, value);
			break;

		default:
			fmte("The value is not string type.", NULL);
			break;
	}
}

static void type_value_hashtable(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_HASH_TABLE, ret);
}

static void type_value_readtable(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_READTABLE, ret);
}

void type_value_symbol(LocalRoot local, addr *ret, addr value)
{
	addr keyword;

	Check(GetType(value) != LISPTYPE_SYMBOL, "type error");
	GetPackageSymbol(value, &value);
	GetConst(PACKAGE_KEYWORD, &keyword);
	type_empty(local, value == keyword? LISPDECL_KEYWORD: LISPDECL_SYMBOL, ret);
}

void type_value_integer(LocalRoot local, addr *ret, addr value)
{
	Check(! integerp(value), "type error");
	type_realvalue(local, LISPDECL_INTEGER, value, ret);
}

void type_value_rational(LocalRoot local, addr *ret, addr value)
{
	Check(! rationalp(value), "type error");
	type_realvalue(local, LISPDECL_RATIONAL, value, ret);
}

void type_value_real(LocalRoot local, addr *ret, addr value)
{
	Check(! realp(value), "type error");
	type_realvalue(local, LISPDECL_REAL, value, ret);
}

void type_value_float(LocalRoot local, addr *ret, addr value)
{
	enum LISPDECL type;

	switch (GetType(value)) {
		case LISPTYPE_SINGLE_FLOAT:
			type = LISPDECL_SINGLE_FLOAT;
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			type = LISPDECL_DOUBLE_FLOAT;
			break;

		case LISPTYPE_LONG_FLOAT:
			type = LISPDECL_LONG_FLOAT;
			break;

		case LISPTYPE_SHORT_FLOAT:
			type = LISPDECL_SHORT_FLOAT;
			break;

		default:
			Abort("type error");
			return;
	}

	type_realvalue(local, type, value, ret);
}

void type_value_complex(LocalRoot local, addr *ret, addr value)
{
	addr real, imag, type;

	GetRealComplex(value, &real);
	GetImagComplex(value, &imag);
	type_value_alloc(local, &real, real);
	type_value_alloc(local, &imag, imag);
	type_or(local, real, imag, &type);
	type_object1(local, LISPDECL_COMPLEX, type, ret);
}

void type_value_function(LocalRoot local, addr *ret, addr value)
{
	Check(! functionp(value), "type error");
	gettype_function(value, ret);
	if (*ret == Nil) {
		if (compiled_function_p(value))
			type_compiled_function_asterisk(local, ret);
		else
			type_function_asterisk(local, ret);
	}
}

static void type_value_package(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_PACKAGE, ret);
}

static void type_value_random_state(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_RANDOM_STATE, ret);
}

void type_value_pathname(LocalRoot local, addr *ret, addr value)
{
	Check(! pathnamep(value), "type error");
	if (pathname_pathname_p(value))
		type_empty(local, LISPDECL_PATHNAME, ret);
	else
		type_empty(local, LISPDECL_LOGICAL_PATHNAME, ret);
}

static enum LISPDECL lispdecl_stream(addr value)
{
	switch (getstreamtype(value)) {
		case StreamType_StringInput:
		case StreamType_StringOutput:
			return LISPDECL_STRING_STREAM;

		case StreamType_Synonym:
			return LISPDECL_SYNONYM_STREAM;

		case StreamType_BroadCast:
			return LISPDECL_BROADCAST_STREAM;

		case StreamType_Concatenated:
			return LISPDECL_CONCATENATED_STREAM;

		case StreamType_TwoWay:
			return LISPDECL_TWO_WAY_STREAM;

		case StreamType_Echo:
			return LISPDECL_ECHO_STREAM;

		default:
			break;
	}

	GetPathnameStream(value, &value);
	if (value != Nil)
		return LISPDECL_FILE_STREAM;

	return LISPDECL_STREAM;
}

static void type_value_stream(LocalRoot local, addr *ret, addr value)
{
	enum LISPDECL type = lispdecl_stream(value);
	type_empty(local, type, ret);
}

static void type_value_quote(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_QUOTE, ret);
}

static void type_value_byte(LocalRoot local, addr *ret, addr value)
{
	type_empty(local, LISPDECL_BYTESPEC, ret);
}

void type_value_bitvector(LocalRoot local, addr *ret, addr value)
{
	addr arg;
	size_t size;

	Check(GetType(value) != LISPTYPE_BITVECTOR, "type error");
	bitmemory_length(value, &size);
	fixnum_alloc(local, &arg, (fixnum)size);
	type_object1(local, LISPDECL_SIMPLE_BIT_VECTOR, arg, ret);
}

void type_value_alloc(LocalRoot local, addr *ret, addr value)
{
	calltype_type_value call = call_type_value[GetType(value)];
	call(local, ret, value);
}

void type_value_local(LocalRoot local, addr *ret, addr value)
{
	Check(local == NULL, "local error");
	type_value_alloc(local, ret, value);
}

void type_value_heap(addr *ret, addr value)
{
	type_value_alloc(NULL, ret, value);
}

static void type_value_error(LocalRoot local, addr *ret, addr value)
{
	fmte("Invalid type-value ~S.", value, NULL);
}

void init_type_value_table(void)
{
	int i;

	/* error */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		call_type_value[i] = type_value_error;

	/* call */
	call_type_value[LISPTYPE_NIL] = type_value_nil_call;
	call_type_value[LISPTYPE_T] = type_value_t_call;
	call_type_value[LISPTYPE_TYPE] = type_value_type;
	call_type_value[LISPTYPE_CLOS] = type_value_clos;
	call_type_value[LISPTYPE_CONS] = type_value_cons;
	call_type_value[LISPTYPE_ARRAY] = type_value_array;
	call_type_value[LISPTYPE_VECTOR] = type_value_vector;
	call_type_value[LISPTYPE_CHARACTER] = type_value_character;
	call_type_value[LISPTYPE_STRING] = type_value_string;
	call_type_value[LISPTYPE_HASHTABLE] = type_value_hashtable;
	call_type_value[LISPTYPE_READTABLE] = type_value_readtable;
	call_type_value[LISPTYPE_SYMBOL] = type_value_symbol;
	call_type_value[LISPTYPE_FIXNUM] = type_value_integer;
	call_type_value[LISPTYPE_BIGNUM] = type_value_integer;
	call_type_value[LISPTYPE_RATIO] = type_value_rational;
	call_type_value[LISPTYPE_SHORT_FLOAT] = type_value_error;
	call_type_value[LISPTYPE_SINGLE_FLOAT] = type_value_float;
	call_type_value[LISPTYPE_DOUBLE_FLOAT] = type_value_float;
	call_type_value[LISPTYPE_LONG_FLOAT] = type_value_float;
	call_type_value[LISPTYPE_COMPLEX] = type_value_complex;
	call_type_value[LISPTYPE_CONTROL] = type_value_error;
	call_type_value[LISPTYPE_CODE] = type_value_error;
	call_type_value[LISPTYPE_CALLNAME] = type_value_error;
	call_type_value[LISPTYPE_FUNCTION] = type_value_function;
	call_type_value[LISPTYPE_INDEX] = type_value_error;
	call_type_value[LISPTYPE_SYSTEM] = type_value_error;
	call_type_value[LISPTYPE_PACKAGE] = type_value_package;
	call_type_value[LISPTYPE_RANDOM_STATE] = type_value_random_state;
	call_type_value[LISPTYPE_PATHNAME] = type_value_pathname;
	call_type_value[LISPTYPE_STREAM] = type_value_stream;
	call_type_value[LISPTYPE_QUOTE] = type_value_quote;
	call_type_value[LISPTYPE_RESTART] = type_value_error;
	call_type_value[LISPTYPE_EVAL] = type_value_error;
	call_type_value[LISPTYPE_ENVIRONMENT] = type_value_error;
	call_type_value[LISPTYPE_BITVECTOR] = type_value_bitvector;
	call_type_value[LISPTYPE_PPRINT] = type_value_error;
	call_type_value[LISPTYPE_BYTESPEC] = type_value_byte;
}


/*
 *  type-name
 */
#define TypeName(x,y) (table_type_name[x] = CONSTANT_##y)

static void type_name_clos(addr *ret, addr value)
{
	clos_class_of(value, &value);
	class_elt(value, Clos_class_name, ret);
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

		case StreamType_TwoWay:
			GetConst(COMMON_TWO_WAY_STREAM, ret);
			return;

		case StreamType_Echo:
			GetConst(COMMON_ECHO_STREAM, ret);
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

int type_name_check(addr *ret, addr value)
{
	enum LISPTYPE type;
	constindex index;

	/* table */
	type = GetType(value);
	index = table_type_name[(int)type];
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
	if (type_name_check(ret, value))
		fmte("Invalid value, ~A.", value, NULL);
}

void init_type_name(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		table_type_name[i] = CONSTANT_EMPTY;
	TypeName(LISPTYPE_NIL, COMMON_NIL);
	TypeName(LISPTYPE_T, COMMON_T);
	TypeName(LISPTYPE_TYPE, COMMON_TYPE);
	/*TypeName(LISPTYPE_CLOS, COMMON_EMPTY);*/
	TypeName(LISPTYPE_CONS, COMMON_CONS);
	TypeName(LISPTYPE_ARRAY, COMMON_ARRAY);
	TypeName(LISPTYPE_VECTOR, COMMON_VECTOR);
	TypeName(LISPTYPE_CHARACTER, COMMON_CHARACTER);
	TypeName(LISPTYPE_STRING, COMMON_STRING);
	TypeName(LISPTYPE_HASHTABLE, COMMON_HASH_TABLE);
	TypeName(LISPTYPE_READTABLE, COMMON_READTABLE);
	/*TypeName(LISPTYPE_SYMBOL, COMMON_EMPTY);*/
	TypeName(LISPTYPE_FIXNUM, COMMON_INTEGER);
	TypeName(LISPTYPE_BIGNUM, COMMON_INTEGER);
	TypeName(LISPTYPE_RATIO, COMMON_RATIO);
	TypeName(LISPTYPE_SHORT_FLOAT, COMMON_FLOAT);
	TypeName(LISPTYPE_SINGLE_FLOAT, COMMON_FLOAT);
	TypeName(LISPTYPE_DOUBLE_FLOAT, COMMON_FLOAT);
	TypeName(LISPTYPE_LONG_FLOAT, COMMON_FLOAT);
	TypeName(LISPTYPE_COMPLEX, COMMON_COMPLEX);
	TypeName(LISPTYPE_CONTROL, SYSTEM_CONTROL);
	TypeName(LISPTYPE_CODE, SYSTEM_CODE);
	TypeName(LISPTYPE_CALLNAME, SYSTEM_CALLNAME);
	/*TypeName(LISPTYPE_FUNCTION, COMMON_EMPTY);*/
	TypeName(LISPTYPE_INDEX, SYSTEM_INDEX);
	TypeName(LISPTYPE_SYSTEM, SYSTEM_SYSTEM);
	TypeName(LISPTYPE_PACKAGE, COMMON_PACKAGE);
	TypeName(LISPTYPE_RANDOM_STATE, COMMON_RANDOM_STATE);
	TypeName(LISPTYPE_PATHNAME, COMMON_PATHNAME);
	/*TypeName(LISPTYPE_STREAM, COMMON_EMPTY);*/
	TypeName(LISPTYPE_QUOTE, SYSTEM_QUOTE);
	TypeName(LISPTYPE_RESTART, COMMON_RESTART);
	TypeName(LISPTYPE_EVAL, SYSTEM_EVAL);
	TypeName(LISPTYPE_ENVIRONMENT, SYSTEM_ENVIRONMENT);
	TypeName(LISPTYPE_BITVECTOR, COMMON_BIT_VECTOR);
	TypeName(LISPTYPE_PPRINT, COMMON_PPRINT);
	TypeName(LISPTYPE_BYTESPEC, SYSTEM_BYTESPEC);

	TypeName(LISPSYSTEM_CHARACTER2, SYSTEM_CHARACTER2);
	TypeName(LISPSYSTEM_CHARQUEUE, SYSTEM_CHARQUEUE);
	TypeName(LISPSYSTEM_CHARBIT, SYSTEM_CHARBIT);
	TypeName(LISPSYSTEM_SYMSTACK, SYSTEM_SYMSTACK);
	TypeName(LISPSYSTEM_SYMARRAY, SYSTEM_SYMARRAY);
	TypeName(LISPSYSTEM_BITTYPE, SYSTEM_BITTYPE);
	TypeName(LISPSYSTEM_READLABEL, SYSTEM_READLABEL);
	TypeName(LISPSYSTEM_READINFO, SYSTEM_READINFO_SYMBOL);
	TypeName(LISPSYSTEM_READTYPE, SYSTEM_READTYPE);
	TypeName(LISPSYSTEM_BITCONS, SYSTEM_BITCONS);
	TypeName(LISPSYSTEM_BITBUFFER, SYSTEM_BITBUFFER);
	TypeName(LISPSYSTEM_HASHITERATOR, SYSTEM_HASHITERATOR);
	TypeName(LISPSYSTEM_PACKAGEITERATOR, SYSTEM_PACKAGEITERATOR);
	TypeName(LISPSYSTEM_TAGINFO, SYSTEM_TAGINFO);
	TypeName(LISPSYSTEM_ARRAY_DIMENSION, SYSTEM_ARRAY_DIMENSION);
	TypeName(LISPSYSTEM_ARRAY_GENERAL, SYSTEM_ARRAY_GENERAL);
	TypeName(LISPSYSTEM_ARRAY_SPECIALIZED, SYSTEM_ARRAY_SPECIALIZED);

	TypeName(LISPSYSTEM_UNBOUND, SYSTEM_UNBOUND);
	TypeName(LISPSYSTEM_SPACE, SYSTEM_SPACE);
	TypeName(LISPSYSTEM_SPACE1, SYSTEM_SPACE1);
	TypeName(LISPSYSTEM_RESERVED, SYSTEM_RESERVED);
	TypeName(LISPSYSTEM_END, SYSTEM_END);
}


/*
 *  type-object
 */
static void type_object_error(LocalRoot local, addr *ret, addr pos)
{
	fmte("Invalid type ~S.", pos, NULL);
}

static void type_object_name(LocalRoot local, addr *ret, addr pos)
{
	constindex index = typedecl_classname(RefLispDecl(pos));
	GetConstant(index, ret);
}

static void type_object_optimized(LocalRoot local, addr *ret, addr pos)
{
	get_type_optimized(ret, pos);
}

static void type_object_subtypep(LocalRoot local, addr *ret, addr pos)
{
	get_type_subtypep(ret, pos);
}

static void type_object_clos(LocalRoot local, addr *ret, addr pos)
{
	class_of(pos, &pos);
	class_elt(pos, Clos_class_name, ret);
}

static void type_object_vectortype(LocalRoot local, addr *ret, addr name, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		type_object(local, &temp, temp);
		cons_alloc(local, &root, temp, root);
	}
	cons_alloc(local, ret, name, root);
}

static void type_object_and(LocalRoot local, addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_AND, &name);
	type_object_vectortype(local, ret, name, pos);
}

static void type_object_or(LocalRoot local, addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_OR, &name);
	type_object_vectortype(local, ret, name, pos);
}

static void type_object_operator1(LocalRoot local,
		addr *ret, constindex index, addr pos)
{
	addr name;
	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	list_alloc(local, ret, name, pos, NULL);
}

static void type_object_eql(LocalRoot local, addr *ret, addr pos)
{
	type_object_operator1(local, ret, CONSTANT_COMMON_EQL, pos);
}

static void type_object_member(LocalRoot local, addr *ret, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		cons_alloc(local, &root, temp, root);
	}
	GetConst(COMMON_MEMBER, &temp);
	cons_alloc(local, ret, temp, root);
}

static void type_object_mod(LocalRoot local, addr *ret, addr pos)
{
	type_object_operator1(local, ret, CONSTANT_COMMON_MOD, pos);
}

static void type_object_not(LocalRoot local, addr *ret, addr pos)
{
	type_object_operator1(local, ret, CONSTANT_COMMON_NOT, pos);
}

static void type_object_satisfies(LocalRoot local, addr *ret, addr pos)
{
	type_object_operator1(local, ret, CONSTANT_COMMON_SATISFIES, pos);
}

static void type_object_values(LocalRoot local, addr *ret, addr pos)
{
	addr root, list, value;

	/* first */
	GetConst(COMMON_VALUES, &value);
	conscar_alloc(local, &root, value);

	/* var */
	GetArrayType(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &value, &list);
		type_object(local, &value, value);
		cons_alloc(local, &root, value, root);
	}

	/* opt */
	GetArrayType(pos, 1, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_OPTIONAL, &value);
		cons_alloc(local, &root, value, root);
	}
	while (list != Nil) {
		GetCons(list, &value, &list);
		type_object(local, &value, value);
		cons_alloc(local, &root, value, root);
	}

	/* rest */
	GetArrayType(pos, 2, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_REST, &value);
		cons_alloc(local, &root, value, root);
		type_object(local, &list, list);
		cons_alloc(local, &root, list, root);
	}

	/* key */
	GetArrayType(pos, 3, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_KEY, &value);
		cons_alloc(local, &root, value, root);
	}
	while (list != Nil) {
		GetCons(list, &value, &list);
		GetCons(value, &pos, &value);
		type_object(local, &value, value);
		list_alloc(local, &value, pos, value, NULL);
		cons_alloc(local, &root, value, root);
	}

	/* result */
	nreverse_list_unsafe(ret, root);
}

static void type_object_vector(LocalRoot local, addr *ret, addr pos)
{
	addr name, type;

	GetConst(COMMON_VECTOR, &name);
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (asterisk_p(type) && asterisk_p(pos)) {
		*ret = name;
		return;
	}
	type_object(local, &type, type);
	if (asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	list_alloc(local, ret, name, type, pos, NULL);
}

static void type_object_size(LocalRoot local, addr *ret, constindex index, addr pos)
{
	addr name;

	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	if (asterisk_p(pos)) {
		*ret = name;
		return;
	}
	list_alloc(local, ret, name, pos, NULL);
}

static void type_object_simple_vector(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_SIMPLE_VECTOR, pos);
}

static void type_object_bit_vector(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_BIT_VECTOR, pos);
}

static void type_object_simple_bit_vector(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_SIMPLE_BIT_VECTOR, pos);
}

static void type_object_string(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_STRING, pos);
}

static void type_object_base_string(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_BASE_STRING, pos);
}

static void type_object_simple_string(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_SIMPLE_STRING, pos);
}

static void type_object_simple_base_string(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_SIMPLE_BASE_STRING, pos);
}

static void type_object_signed_byte(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_SIGNED_BYTE, pos);
}

static void type_object_unsigned_byte(LocalRoot local, addr *ret, addr pos)
{
	type_object_size(local, ret, CONSTANT_COMMON_UNSIGNED_BYTE, pos);
}

static void type_object_cons(LocalRoot local, addr *ret, addr pos)
{
	addr name, car, cdr;

	GetConst(COMMON_CONS, &name);
	GetArrayType(pos, 0, &car);
	GetArrayType(pos, 1, &cdr);
	if (asterisk_p(car) && asterisk_p(cdr)) {
		*ret = name;
		return;
	}
	type_object(local, &car, car);
	type_object(local, &cdr, cdr);
	list_alloc(local, ret, name, car, cdr);
}

static void type_object_function_args(LocalRoot local, addr *ret, addr type)
{
	addr root, list, pos;
	addr var, opt, rest, key, value;

	Check(GetType(type) != LISPTYPE_VECTOR, "type error");
	/* var */
	GetArrayA2(type, 0, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		type_object(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse_list_unsafe(&var, root);

	/* opt */
	GetArrayA2(type, 1, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		type_object(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse_list_unsafe(&opt, root);

	/* rest */
	GetArrayA2(type, 2, &rest);
	if (rest != Nil)
		type_object(local, &rest, rest);

	/* key */
	GetArrayA2(type, 3, &key);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &value);
		type_object(local, &value, value);
		list_alloc(local, &pos, pos, value, NULL);
		cons_alloc(local, &root, pos, root);
	}
	nreverse_list_unsafe(&key, root);

	list_alloc(local, ret, var, opt, rest, key, NULL);
}

static void type_object_functiontype(LocalRoot local,
		addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (asterisk_p(type) && asterisk_p(pos)) {
		*ret = name;
		return;
	}
	if (asterisk_p(type)) {
		GetConst(COMMON_ASTERISK, &type);
	}
	else {
		type_object_function_args(local, &type, type);
	}
	/* values */
	if (asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	else
		type_object(local, &pos, pos);
	/* result */
	list_alloc(local, ret, name, type, pos, NULL);
}

static void type_object_function(LocalRoot local, addr *ret, addr pos)
{
	type_object_functiontype(local, ret, CONSTANT_COMMON_FUNCTION, pos);
}

static void type_object_compiled_function(LocalRoot local, addr *ret, addr pos)
{
	type_object_functiontype(local, ret, CONSTANT_COMMON_COMPILED_FUNCTION, pos);
}

static void type_object_arraydimension(LocalRoot local, addr pos, addr *ret)
{
	addr root, temp;
	size_t size;

	if (GetType(pos) == LISPTYPE_FIXNUM) {
		*ret = pos;
		return;
	}
	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(pos, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(pos, size, &temp);
		cons_alloc(local, &root, temp, root);
	}
	*ret = root;
}

static void type_object_arraytype(LocalRoot local,
		addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* type */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (asterisk_p(type) && asterisk_p(pos)) {
		*ret = name;
		return;
	}
	if (asterisk_p(type))
		GetConst(COMMON_ASTERISK, &type);
	/* dimension */
	if (asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	else
		type_object_arraydimension(local, pos, &pos);
	/* result */
	list_alloc(local, ret, name, type, pos, NULL);
}

static void type_object_array(LocalRoot local, addr *ret, addr pos)
{
	type_object_arraytype(local, ret, CONSTANT_COMMON_ARRAY, pos);
}

static void type_object_simple_array(LocalRoot local, addr *ret, addr pos)
{
	type_object_arraytype(local, ret, CONSTANT_COMMON_SIMPLE_ARRAY, pos);
}

static void type_object_realtype(LocalRoot local,
		addr *ret, constindex index, addr pos)
{
	addr name, left1, left2, right1, right2;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &left1);
	GetArrayType(pos, 1, &left2);
	GetArrayType(pos, 2, &right1);
	GetArrayType(pos, 3, &right2);
	if (asterisk_p(left1) && asterisk_p(right1)) {
		*ret = name;
		return;
	}
	/* left */
	if (asterisk_p(left1))
		GetConst(COMMON_ASTERISK, &left2);
	else if (left1 == T)
		conscar_alloc(local, &left2, left2);
	/* right */
	if (asterisk_p(right1))
		GetConst(COMMON_ASTERISK, &right2);
	else if (left1 == T)
		conscar_alloc(local, &right2, right2);
	/* result */
	list_alloc(local, ret, name, left2, right2, NULL);
}

static void type_object_real(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_REAL, pos);
}

static void type_object_rational(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_RATIONAL, pos);
}

static void type_object_integer(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_INTEGER, pos);
}

static void type_object_float(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_FLOAT, pos);
}

static void type_object_short_float(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_SHORT_FLOAT, pos);
}

static void type_object_single_float(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_SINGLE_FLOAT, pos);
}

static void type_object_double_float(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_DOUBLE_FLOAT, pos);
}

static void type_object_long_float(LocalRoot local, addr *ret, addr pos)
{
	type_object_realtype(local, ret, CONSTANT_COMMON_LONG_FLOAT, pos);
}

static void type_object_complex(LocalRoot local, addr *ret, addr pos)
{
	addr name;

	GetConst(COMMON_COMPLEX, &name);
	GetArrayType(pos, 0, &pos);
	if (asterisk_p(pos)) {
		*ret = name;
		return;
	}
	type_object(local, &pos, pos);
	list_alloc(local, ret, name, pos, NULL);
}

void type_object(LocalRoot local, addr *ret, addr pos)
{
	calltype_type_object call;
	addr result, notp;

	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	call = call_type_object[(int)RefLispDecl(pos)];
	call(local, &result, pos);
	if (RefNotDecl(pos)) {
		GetConst(COMMON_NOT, &notp);
		list_alloc(local, ret, notp, result, NULL);
	}
	else {
		*ret = result;
	}
	Check(*ret == Unbound, "unbound error");
}

void init_type_object(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		call_type_object[i] = type_object_name;
		
	/* object */
	call_type_object[LISPDECL_EMPTY] = type_object_error;
	call_type_object[LISPDECL_OPTIMIZED] = type_object_optimized;
	call_type_object[LISPDECL_SUBTYPEP] = type_object_subtypep;
	call_type_object[LISPDECL_TYPE] = type_object_error;
	call_type_object[LISPDECL_CLOS] = type_object_clos;
	call_type_object[LISPDECL_ASTERISK] = type_object_name;
	/* Compound-type */
	call_type_object[LISPDECL_AND] = type_object_and;
	call_type_object[LISPDECL_OR] = type_object_or;
	call_type_object[LISPDECL_EQL] = type_object_eql;
	call_type_object[LISPDECL_MEMBER] = type_object_member;
	call_type_object[LISPDECL_MOD] = type_object_mod;
	call_type_object[LISPDECL_NOT] = type_object_not;
	call_type_object[LISPDECL_SATISFIES] = type_object_satisfies;
	call_type_object[LISPDECL_VALUES] = type_object_values;
	/* Extract-type */
	call_type_object[LISPDECL_ATOM] = type_object_name;
	call_type_object[LISPDECL_LIST] = type_object_name;
	call_type_object[LISPDECL_BOOLEAN] = type_object_name;
	call_type_object[LISPDECL_VECTOR] = type_object_vector;
	call_type_object[LISPDECL_SIMPLE_VECTOR] = type_object_simple_vector;
	call_type_object[LISPDECL_BIT_VECTOR] = type_object_bit_vector;
	call_type_object[LISPDECL_SIMPLE_BIT_VECTOR] = type_object_simple_bit_vector;
	call_type_object[LISPDECL_EXTENDED_CHAR] = type_object_name;
	call_type_object[LISPDECL_STRING] = type_object_string;
	call_type_object[LISPDECL_BASE_STRING] = type_object_base_string;
	call_type_object[LISPDECL_SIMPLE_STRING] = type_object_simple_string;
	call_type_object[LISPDECL_SIMPLE_BASE_STRING] = type_object_simple_base_string;
	call_type_object[LISPDECL_SIGNED_BYTE] = type_object_signed_byte;
	call_type_object[LISPDECL_UNSIGNED_BYTE] = type_object_unsigned_byte;
	call_type_object[LISPDECL_BIT] = type_object_name;
	call_type_object[LISPDECL_FIXNUM] = type_object_name;
	call_type_object[LISPDECL_BIGNUM] = type_object_name;
	/* Atomic-type */
	call_type_object[LISPDECL_NIL] = type_object_name;
	call_type_object[LISPDECL_T] = type_object_name;
	call_type_object[LISPDECL_NULL] = type_object_name;
	call_type_object[LISPDECL_CONS] = type_object_cons;
	call_type_object[LISPDECL_HASH_TABLE] = type_object_name;
	call_type_object[LISPDECL_SYMBOL] = type_object_name;
	call_type_object[LISPDECL_KEYWORD] = type_object_name;
	call_type_object[LISPDECL_PACKAGE] = type_object_name;
	call_type_object[LISPDECL_RANDOM_STATE] = type_object_name;
	call_type_object[LISPDECL_READTABLE] = type_object_name;
	call_type_object[LISPDECL_FUNCTION] = type_object_function;
	call_type_object[LISPDECL_COMPILED_FUNCTION] = type_object_compiled_function;
	call_type_object[LISPDECL_PATHNAME] = type_object_name;
	call_type_object[LISPDECL_LOGICAL_PATHNAME] = type_object_name;
	call_type_object[LISPDECL_SEQUENCE] = type_object_name;
	call_type_object[LISPDECL_ARRAY] = type_object_array;
	call_type_object[LISPDECL_SIMPLE_ARRAY] = type_object_simple_array;
	call_type_object[LISPDECL_CHARACTER] = type_object_name;
	call_type_object[LISPDECL_BASE_CHAR] = type_object_name;
	call_type_object[LISPDECL_STANDARD_CHAR] = type_object_name;
	call_type_object[LISPDECL_NUMBER] = type_object_name;
	call_type_object[LISPDECL_REAL] = type_object_real;
	call_type_object[LISPDECL_RATIONAL] = type_object_rational;
	call_type_object[LISPDECL_RATIO] = type_object_name;
	call_type_object[LISPDECL_INTEGER] = type_object_integer;
	call_type_object[LISPDECL_COMPLEX] = type_object_complex;
	call_type_object[LISPDECL_FLOAT] = type_object_float;
	call_type_object[LISPDECL_SHORT_FLOAT] = type_object_short_float;
	call_type_object[LISPDECL_SINGLE_FLOAT] = type_object_single_float;
	call_type_object[LISPDECL_DOUBLE_FLOAT] = type_object_double_float;
	call_type_object[LISPDECL_LONG_FLOAT] = type_object_long_float;
	call_type_object[LISPDECL_RESTART] = type_object_name;
	call_type_object[LISPDECL_ENVIRONMENT] = type_object_name;
	call_type_object[LISPDECL_STREAM] = type_object_name;
	call_type_object[LISPDECL_BROADCAST_STREAM] = type_object_name;
	call_type_object[LISPDECL_CONCATENATED_STREAM] = type_object_name;
	call_type_object[LISPDECL_ECHO_STREAM] = type_object_name;
	call_type_object[LISPDECL_FILE_STREAM] = type_object_name;
	call_type_object[LISPDECL_STRING_STREAM] = type_object_name;
	call_type_object[LISPDECL_SYNONYM_STREAM] = type_object_name;
	call_type_object[LISPDECL_TWO_WAY_STREAM] = type_object_name;
	call_type_object[LISPDECL_QUOTE] = type_object_error;
	call_type_object[LISPDECL_BYTESPEC] = type_object_error;
}


/*
 *  initialize
 */
void init_type_value(void)
{
	init_type_value_table();
	init_type_name();
	init_type_object();
}

