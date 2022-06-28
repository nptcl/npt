#include "array.h"
#include "bignum.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bit.h"
#include "character.h"
#include "cmpl.h"
#include "clos_instance.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "equal.h"
#include "float_equal.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "pathname_object.h"
#include "rational.h"
#include "rational_equal.h"
#include "real_equal.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "subtypep.h"
#include "symbol.h"
#include "type.h"
#include "type_call.h"
#include "type_delay.h"
#include "type_parse.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_upgraded.h"

static int typep_call_(Execute ptr, addr value, addr type, int asterisk, int *ret);
typedef int (*call_type_typep)(Execute ptr, addr value, addr type, int *ret);
static call_type_typep TypeTypep[LISPDECL_SIZE];

static int typep_invalid_(Execute ptr, addr value, addr type, int *ret)
{
	infobit(type);
	*ret = 0;
	return fmte_("Invalid type.", NULL);
}

static int typep_delay_(Execute ptr, addr value, addr type, int *ret)
{
	Return(get_delay_type_(ptr, type, &type));
	return typep_call_(ptr, value, type, 1, ret);
}

static int typep_type_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_TYPE);
	return 0;
}

static int typep_clos_(Execute ptr, addr value, addr type, int *ret)
{
	if (GetType(value) != LISPTYPE_CLOS)
		return Result(ret, 0);
	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);

	return clos_subtype_p_(value, type, ret);
}

static int typep_asterisk_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 1);
}

static int typep_optimized_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	return typep_call_(ptr, value, type, 1, ret);
}


/*
 *  Compound-type
 */
static int typep_and_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		Return(typep_call_(ptr, value, check, 1, &result));
		if (! result)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int typep_or_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		Return(typep_call_(ptr, value, check, 1, &result));
		if (result)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int typep_eql_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	*ret = eql_function(value, type);
	return 0;
}

static int typep_member_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (eql_function(value, check))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int typep_mod_(Execute ptr, addr value, addr type, int *ret)
{
	int check;

	if (! integerp(value))
		return Result(ret, 0);
	Return(minusp_integer_(value, &check));
	if (check)
		return Result(ret, 0);
	GetArrayType(type, 0, &type);

	return less_integer_(value, type, ret);
}

static int typep_not_(Execute ptr, addr value, addr type, int *ret)
{
	int check;

	GetArrayType(type, 0, &type);
	Return(typep_call_(ptr, value, type, 1, &check));
	return Result(ret, ! check);
}

static int typep_satisfies_(Execute ptr, addr value, addr type, int *ret)
{
	GetArrayType(type, 0, &type);
	Return(funcall1_control_(ptr, &type, type, value, NULL));
	return Result(ret, (type != Nil));
}

static int typep_values_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = 0;
	return fmte_("The values type don't use in typep context.", NULL);
}


/*
 *  Extract-type
 */
static int typep_atom_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (! IsCons(value)));
}

static int typep_list_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, IsList(value));
}

static int typep_boolean_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (value == Nil || value == T));
}

static int typep_vector_vector_(addr value, addr type, int *ret)
{
	enum LISPDECL left;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &left);
	if ((left != LISPDECL_ASTERISK) && (left != LISPDECL_T))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	lenarray(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_string_(addr value, addr type, int *ret)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (! decl_character_p(decl)))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	string_length(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_bitvector_(addr value, addr type, int *ret)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (decl != LISPDECL_BIT))
		return Result(ret, 0);
	GetArrayType(type, 1, &check);
	if (type_asterisk_p(check))
		return Result(ret, 1);
	bitmemory_length(value, &size1);
	GetFixnum(check, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_vector_dimension_(addr value, addr type, int *ret)
{
	/* asterisk */
	if (type_asterisk_p(type)) {
		*ret = array_vector_p(value);
		return 0;
	}

	/* fixnum */
	if (GetType(type) == LISPTYPE_FIXNUM) {
		*ret = array_size_vector_p(value, (size_t)RefFixnum(type));
		return 0;
	}

	/* error */
	*ret = 0;
	return fmte_("type error", NULL);
}

static int typep_vector_array_(addr value, addr type, int *ret)
{
	addr left, right;

	GetArrayType(type, 0, &left);
	GetArrayInfo(value, ARRAY_INDEX_TYPE, &right);
	if ((! type_asterisk_p(left)) && (! upgraded_array0_equal(left, right)))
		return Result(ret, 0);
	GetArrayType(type, 1, &type);
	return typep_vector_dimension_(value, type, ret);
}

static int typep_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_vector_vector_(value, type, ret);

		case LISPTYPE_STRING:
			return typep_vector_string_(value, type, ret);

		case LISPTYPE_ARRAY:
			if (strarrayp(value))
				return typep_vector_string_(value, type, ret);
			return typep_vector_array_(value, type, ret);

		case LISPTYPE_BITVECTOR:
			return typep_vector_bitvector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_simple_vector_vector_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	lenarray(value, &size1);
	GetFixnum(type, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_type_vector_array_(addr value, addr type, enum LISPDECL decl, int *ret)
{
	addr check;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &check);
	if ((! type_asterisk_p(check)) && (RefLispDecl(check) != decl))
		return Result(ret, 0);
	GetArrayType(type, 0, &check);
	return typep_vector_dimension_(value, check, ret);
}

static int typep_simple_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_simple_vector_vector_(value, type, ret);

		case LISPTYPE_ARRAY:
			if (! array_simple_p(value))
				return Result(ret, 0);
			return typep_type_vector_array_(value, type, LISPDECL_T, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_bit_vector_bit_vector_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	bitmemory_length(value, &size1);
	GetFixnum(type, &size2);
	return Result(ret, (size1 == (size_t)size2));
}

static int typep_bit_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return typep_type_vector_array_(value, type, LISPDECL_BIT, ret);

		case LISPTYPE_BITVECTOR:
			return typep_bit_vector_bit_vector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_simple_bit_vector_(Execute ptr, addr value, addr type, int *ret)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			if (! array_simple_p(value))
				return Result(ret, 0);
			return typep_type_vector_array_(value, type, LISPDECL_BIT, ret);

		case LISPTYPE_BITVECTOR:
			return typep_bit_vector_bit_vector_(value, type, ret);

		default:
			return Result(ret, 0);
	}
}

static int typep_extended_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = extended_char_p(value);
	return 0;
}

static int typep_string_size_(addr value, addr type, int *ret)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type))
		return Result(ret, 1);
	string_length(value, &size1);
	GetFixnum(type, &size2);

	return Result(ret, (size1 == (size_t)size2));
}

static int typep_string_(Execute ptr, addr value, addr type, int *ret)
{
	if (! stringp(value))
		return Result(ret, 0);

	return typep_string_size_(value, type, ret);
}

static int typep_base_string_size_(addr value, addr type, int *ret)
{
	enum CHARACTER_TYPE check;

	Return(string_character_type_(value, &check));
	if (check == CHARACTER_TYPE_EMPTY ||
			check == CHARACTER_TYPE_STANDARD ||
			check == CHARACTER_TYPE_BASE) {
		return typep_string_size_(value, type, ret);
	}

	return Result(ret, 0);
}

static int typep_base_string_(Execute ptr, addr value, addr type, int *ret)
{
	if (! stringp(value))
		return Result(ret, 0);

	return typep_base_string_size_(value, type, ret);
}

static int typep_simple_string_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING)
		return typep_string_size_(value, type, ret);
	if (strarrayp(value) && array_simple_p(value))
		return typep_string_size_(value, type, ret);

	return Result(ret, 0);
}

static int typep_simple_base_string_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING)
		return typep_base_string_size_(value, type, ret);
	if (strarrayp(value) && array_simple_p(value))
		return typep_base_string_size_(value, type, ret);

	return Result(ret, 0);
}

static int typep_signed_byte_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, 1);
		*ret = fixnum_signed_byte_p(value, RefFixnum(check));
		return 0;
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, 1);
		*ret = bignum_signed_byte_p(value, RefFixnum(check));
		return 0;
	}

	return Result(ret, 0);
}

static int typep_unsigned_byte_(Execute ptr, addr value, addr type, int *ret)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, (0 <= RefFixnum(value)));
		*ret = fixnum_unsigned_byte_p(value, RefFixnum(check));
		return 0;
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (type_asterisk_p(check))
			return Result(ret, zerop_or_plusp_bignum(value));
		*ret = bignum_unsigned_byte_p(value, RefFixnum(check));
		return 0;
	}

	return Result(ret, 0);
}

static int typep_bit_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;
	fixnum check;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM) {
		GetFixnum(value, &check);
		return Result(ret, (check == 0 || check == 1));
	}

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = zerop_bignum(value) || equal_value_bignum(value, signplus_bignum, 1);
		return 0;
	}

	return Result(ret, 0);
}

static int fbf_bignum(fixnum left, addr value, fixnum right)
{
	return compare_value_bignum(left, value) <= 0
		&& compare_bignum_value(value, right) <= 0;
}

static int typep_fixnum_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM)
		return Result(ret, 1);

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX);
		return 0;
	}

	return Result(ret, 0);
}

static int typep_bignum_(Execute ptr, addr value, addr type, int *ret)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM)
		return Result(ret, 0);

	if (lisptype == LISPTYPE_BIGNUM) {
		*ret = (! fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX));
		return 0;
	}

	return Result(ret, 0);
}


/*
 *  Atomic-type
 */
static int typep_nil_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 0);
}

static int typep_t_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, 1);
}

static int typep_null_(Execute ptr, addr value, addr type, int *ret)
{
	return Result(ret, (value == Nil));
}

static int typep_cons_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr left, check;

	if (! IsCons(value))
		return Result(ret, 0);
	GetCons(value, &left, &value);
	GetArrayType(type, 0, &check);
	Return(typep_call_(ptr, left, check, 1, &result));
	if (! result)
		return Result(ret, 0);
	GetArrayType(type, 1, &check);

	return typep_call_(ptr, value, check, 1, ret);
}

static int typep_hash_table_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_HASHTABLE);
	return 0;
}

static int typep_symbol_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = symbolp(value);
	return 0;
}

static int typep_keyword_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = keywordp(value);
	return 0;
}

static int typep_package_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PACKAGE);
	return 0;
}

static int typep_random_state_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RANDOM_STATE);
	return 0;
}

static int typep_readtable_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_READTABLE);
	return 0;
}

static int typep_pathname_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pathnamep(value);
	return 0;
}

static int typep_logical_pathname_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pathname_logical_p(value);
	return 0;
}

static int typep_sequence_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = sequencep(value);
	return 0;
}

static int equal_array_dimension(addr value, addr right)
{
	addr left, check;
	size_t i, rank, index, *psize;
	struct array_struct *str;

	/* rank check */
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &left);
	str = ArrayInfoStruct(value);
	rank = ArrayInfoStruct(value)->dimension;
	LenArrayA4(right, &i);
	if (rank != i)
		return 0;

	/* no-dimension */
	if (rank == 0) {
		return 0;
	}

	/* sequence */
	if (rank == 1) {
		GetArrayA4(right, 0, &check);
		if (type_asterisk_p(check))
			return 1;
		if (GetIndex_integer(check, &index))
			return 0;
		return str->size == index;
	}

	/* multi-dimension */
	CheckType(left, LISPSYSTEM_ARRAY_DIMENSION);
	psize = arraysize_ptr(left);
	for (i = 0; i < rank; i++) {
		GetArrayA4(right, i, &check);
		if (type_asterisk_p(check))
			continue;
		if (GetIndex_integer(check, &index))
			return 0;
		if (psize[i] != index)
			return 0;
	}
	return 1;
}

static int typep_array_dimension(addr value, addr type)
{
	/* asterisk */
	if (type_asterisk_p(type)) {
		return 1;
	}

	/* fixnum */
	if (GetType(type) == LISPTYPE_FIXNUM) {
		return ArrayInfoStruct(value)->dimension == (size_t)RefFixnum(type);
	}

	/* arraydimension */
	if (GetType(type) == LISPTYPE_VECTOR) {
		return equal_array_dimension(value, type);
	}

	/* error */
	Abort("type error");
	return 0;
}

static int typep_array_array(addr value, addr type)
{
	addr left, right;

	GetArrayType(type, 0, &left);
	GetArrayInfo(value, ARRAY_INDEX_TYPE, &right);
	if ((! type_asterisk_p(left)) && (! upgraded_array0_equal(left, right)))
		return 0;
	GetArrayType(type, 1, &type);

	return typep_array_dimension(value, type);
}

static int equal_fixnum_index(addr left, size_t right)
{
	fixnum value;

	GetFixnum(left, &value);
	if (value < 0)
		return 0;
	return (size_t)value == right;
}

static int typep_array_vector(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_T)
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		return equal_fixnum_index(left, lenarrayr(value));
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_string(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if ((decl != LISPDECL_ASTERISK) && (! decl_character_p(decl)))
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		string_length(value, &size);
		return equal_fixnum_index(left, size);
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_bitvector(addr value, addr type)
{
	enum LISPDECL decl;
	addr left;
	size_t size;

	/* type */
	GetArrayType(type, 0, &left);
	GetLispDecl(left, &decl);
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_BIT)
		return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (type_asterisk_p(left))
		return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1)
			return 0;
		GetArrayA4(left, 0, &left);
		if (type_asterisk_p(left))
			return 1;
		bitmemory_length(value, &size);
		return equal_fixnum_index(left, size);
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array_result(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return typep_array_array(value, type);

		case LISPTYPE_VECTOR:
			return typep_array_vector(value, type);

		case LISPTYPE_STRING:
			return typep_array_string(value, type);

		case LISPTYPE_BITVECTOR:
			return typep_array_bitvector(value, type);

		default:
			break;
	}

	return 0;
}

static int typep_array_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = typep_array_result(value, type);
	return 0;
}

static int typep_simple_array_result(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return array_simple_p(value) && typep_array_array(value, type);

		case LISPTYPE_VECTOR:
			return typep_array_vector(value, type);

		case LISPTYPE_STRING:
			return typep_array_string(value, type);

		case LISPTYPE_BITVECTOR:
			return typep_array_bitvector(value, type);

		default:
			break;
	}

	return 0;
}

static int typep_simple_array_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = typep_simple_array_result(value, type);
	return 0;
}

static int typep_character_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_CHARACTER);
	return 0;
}

static int typep_base_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = base_char_p(value);
	return 0;
}

static int typep_standard_char_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = standard_char_p(value);
	return 0;
}

static int typep_number_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = numberp(value);
	return 0;
}

static int typep_ratio_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RATIO);
	return 0;
}

static int typep_complex_(Execute ptr, addr value, addr type, int *ret)
{
	int result;
	addr check;

	if (GetType(value) != LISPTYPE_COMPLEX)
		return Result(ret, 0);
	GetRealComplex(value, &check);
	GetArrayType(type, 0, &type);
	Return(typep_call_(ptr, check, type, 1, &result));
	if (! result)
		return Result(ret, 0);
	GetImagComplex(value, &check);
	return typep_call_(ptr, check, type, 1, ret);
}


/*
 *  range
 */
static int less_mode_nolocal_(addr mode, addr left, addr right, int *ret,
		int (*call_less_)(addr, addr, int *),
		int (*call_less_equal_)(addr, addr, int *))
{
	if (type_asterisk_p(mode))
		return Result(ret, 1);
	if (mode == Nil)
		return (*call_less_equal_)(left, right, ret);
	else
		return (*call_less_)(left, right, ret);
}

static int typep_range_nolocal_(addr value, addr type, int *ret,
		int (*typecheck)(addr),
		int (*call_less_)(addr, addr, int *),
		int (*call_less_equal_)(addr, addr, int *))
{
	int check;
	addr mode, pos;

	/* type */
	if (! (*typecheck)(value))
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &pos);
	Return(less_mode_nolocal_(mode, pos, value, &check,
				call_less_, call_less_equal_));
	if (! check)
		return Result(ret, 0);

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &pos);
	return less_mode_nolocal_(mode, value, pos, ret,
			call_less_, call_less_equal_);
}

static int less_mode_local_(LocalRoot local,
		addr mode, addr left, addr right, int *ret,
		int (*call_less_)(LocalRoot, addr, addr, int *),
		int (*call_less_equal_)(LocalRoot, addr, addr, int *))
{
	if (type_asterisk_p(mode))
		return Result(ret, 1);
	if (mode == Nil)
		return (*call_less_equal_)(local, left, right, ret);
	else
		return (*call_less_)(local, left, right, ret);
}

static int typep_range_local_(LocalRoot local, addr value, addr type, int *ret,
		int (*typecheck)(addr),
		int (*call_less_)(LocalRoot, addr, addr, int *),
		int (*call_less_equal_)(LocalRoot, addr, addr, int *))
{
	int check;
	addr mode, pos;

	/* type */
	if (! (*typecheck)(value))
		return Result(ret, 0);

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &pos);
	Return(less_mode_local_(local, mode, pos, value, &check,
				call_less_, call_less_equal_));
	if (! check)
		return Result(ret, 0);

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &pos);
	return less_mode_local_(local, mode, value, pos, ret,
			call_less_, call_less_equal_);
}

static int typep_integer_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			integerp,
			less_integer_,
			less_equal_integer_);
}

static int typep_rational_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_local_(ptr->local, value, type, ret,
			rationalp,
			less_rational_,
			less_equal_rational_);
}

static int typep_real_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_local_(ptr->local, value, type, ret,
			realp,
			less_real_,
			less_equal_real_);
}

static int typep_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			floatp,
			less_float_clang_,
			less_equal_float_clang_);
}

static int single_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_SINGLE_FLOAT);
}
static int typep_single_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			single_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int double_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_DOUBLE_FLOAT);
}
static int typep_double_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			double_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int long_float_p_clang(addr value)
{
	return (GetType(value) == LISPTYPE_LONG_FLOAT);
}
static int typep_long_float_(Execute ptr, addr value, addr type, int *ret)
{
	return typep_range_nolocal_(value, type, ret,
			long_float_p_clang,
			less_float_clang_,
			less_equal_float_clang_);
}

static int typep_restart_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_RESTART);
	return 0;
}

static int typep_environment_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_ENVIRONMENT);
	return 0;
}

static int typep_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = streamp(value);
	return 0;
}

static int typep_broadcast_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = broadcast_stream_p(value);
	return 0;
}

static int typep_concatenated_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = concatenated_stream_p(value);
	return 0;
}

static int typep_echo_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = echo_stream_p(value);
	return 0;
}

static int typep_file_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = file_stream_p(value);
	return 0;
}

static int typep_string_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = string_stream_p(value);
	return 0;
}

static int typep_synonym_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = synonym_stream_p(value);
	return 0;
}

static int typep_two_way_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = twoway_stream_p(value);
	return 0;
}

static int typep_prompt_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = prompt_stream_p(value);
	return 0;
}

static int typep_pretty_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pretty_stream_p(value);
	return 0;
}

static int typep_memory_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = memory_stream_p(value);
	return 0;
}

static int typep_pipe_stream_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = pipe_stream_p(value);
	return 0;
}

static int typep_byte_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_BYTESPEC);
	return 0;
}

static int typep_print_dispatch_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PRINT_DISPATCH);
	return 0;
}

static int typep_paper_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_PAPER);
	return 0;
}

static int typep_eval_(Execute ptr, addr value, addr type, int *ret)
{
	*ret = (GetType(value) == LISPTYPE_EVAL);
	return 0;
}


/*
 *  typep-clang
 */
int typep_table_(Execute ptr, addr value, addr type, int *ret)
{
	call_type_typep call;

	CheckType(type, LISPTYPE_TYPE);
	call = TypeTypep[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");
	return (*call)(ptr, value, type, ret);
}

void init_type_typep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeTypep[i] = typep_invalid_;

	TypeTypep[LISPDECL_DELAY] = typep_delay_;
	TypeTypep[LISPDECL_TYPE] = typep_type_;
	TypeTypep[LISPDECL_CLOS] = typep_clos_;
	TypeTypep[LISPDECL_ASTERISK] = typep_asterisk_;
	TypeTypep[LISPDECL_OPTIMIZED] = typep_optimized_;
	TypeTypep[LISPDECL_SUBTYPEP] = typep_optimized_;
	/* Compound-type */
	TypeTypep[LISPDECL_AND] = typep_and_;
	TypeTypep[LISPDECL_OR] = typep_or_;
	TypeTypep[LISPDECL_EQL] = typep_eql_;
	TypeTypep[LISPDECL_MEMBER] = typep_member_;
	TypeTypep[LISPDECL_MOD] = typep_mod_;
	TypeTypep[LISPDECL_NOT] = typep_not_;
	TypeTypep[LISPDECL_SATISFIES] = typep_satisfies_;
	TypeTypep[LISPDECL_VALUES] = typep_values_;
	/* Extract-type */
	TypeTypep[LISPDECL_ATOM] = typep_atom_;
	TypeTypep[LISPDECL_LIST] = typep_list_;
	TypeTypep[LISPDECL_BOOLEAN] = typep_boolean_;
	TypeTypep[LISPDECL_VECTOR] = typep_vector_;
	TypeTypep[LISPDECL_SIMPLE_VECTOR] = typep_simple_vector_;
	TypeTypep[LISPDECL_BIT_VECTOR] = typep_bit_vector_;
	TypeTypep[LISPDECL_SIMPLE_BIT_VECTOR] = typep_simple_bit_vector_;
	TypeTypep[LISPDECL_EXTENDED_CHAR] = typep_extended_char_;
	TypeTypep[LISPDECL_STRING] = typep_string_;
	TypeTypep[LISPDECL_BASE_STRING] = typep_base_string_;
	TypeTypep[LISPDECL_SIMPLE_STRING] = typep_simple_string_;
	TypeTypep[LISPDECL_SIMPLE_BASE_STRING] = typep_simple_base_string_;
	TypeTypep[LISPDECL_SIGNED_BYTE] = typep_signed_byte_;
	TypeTypep[LISPDECL_UNSIGNED_BYTE] = typep_unsigned_byte_;
	TypeTypep[LISPDECL_BIT] = typep_bit_;
	TypeTypep[LISPDECL_FIXNUM] = typep_fixnum_;
	TypeTypep[LISPDECL_BIGNUM] = typep_bignum_;
	/* Atomic-type */
	TypeTypep[LISPDECL_NIL] = typep_nil_;
	TypeTypep[LISPDECL_T] = typep_t_;
	TypeTypep[LISPDECL_NULL] = typep_null_;
	TypeTypep[LISPDECL_CONS] = typep_cons_;
	TypeTypep[LISPDECL_HASH_TABLE] = typep_hash_table_;
	TypeTypep[LISPDECL_SYMBOL] = typep_symbol_;
	TypeTypep[LISPDECL_KEYWORD] = typep_keyword_;
	TypeTypep[LISPDECL_PACKAGE] = typep_package_;
	TypeTypep[LISPDECL_RANDOM_STATE] = typep_random_state_;
	TypeTypep[LISPDECL_READTABLE] = typep_readtable_;
	TypeTypep[LISPDECL_FUNCTION] = typep_function_;
	TypeTypep[LISPDECL_COMPILED_FUNCTION] = typep_compiled_function_;
	TypeTypep[LISPDECL_PATHNAME] = typep_pathname_;
	TypeTypep[LISPDECL_LOGICAL_PATHNAME] = typep_logical_pathname_;
	TypeTypep[LISPDECL_SEQUENCE] = typep_sequence_;
	TypeTypep[LISPDECL_ARRAY] = typep_array_;
	TypeTypep[LISPDECL_SIMPLE_ARRAY] = typep_simple_array_;
	TypeTypep[LISPDECL_CHARACTER] = typep_character_;
	TypeTypep[LISPDECL_BASE_CHAR] = typep_base_char_;
	TypeTypep[LISPDECL_STANDARD_CHAR] = typep_standard_char_;
	TypeTypep[LISPDECL_NUMBER] = typep_number_;
	TypeTypep[LISPDECL_REAL] = typep_real_;
	TypeTypep[LISPDECL_RATIO] = typep_ratio_;
	TypeTypep[LISPDECL_INTEGER] = typep_integer_;
	TypeTypep[LISPDECL_RATIONAL] = typep_rational_;
	TypeTypep[LISPDECL_COMPLEX] = typep_complex_;
	TypeTypep[LISPDECL_FLOAT] = typep_float_;
	TypeTypep[LISPDECL_SHORT_FLOAT] = typep_single_float_;
	TypeTypep[LISPDECL_SINGLE_FLOAT] = typep_single_float_;
	TypeTypep[LISPDECL_DOUBLE_FLOAT] = typep_double_float_;
	TypeTypep[LISPDECL_LONG_FLOAT] = typep_long_float_;
	TypeTypep[LISPDECL_RESTART] = typep_restart_;
	TypeTypep[LISPDECL_ENVIRONMENT] = typep_environment_;
	TypeTypep[LISPDECL_STREAM] = typep_stream_;
	TypeTypep[LISPDECL_BROADCAST_STREAM] = typep_broadcast_stream_;
	TypeTypep[LISPDECL_CONCATENATED_STREAM] = typep_concatenated_stream_;
	TypeTypep[LISPDECL_ECHO_STREAM] = typep_echo_stream_;
	TypeTypep[LISPDECL_FILE_STREAM] = typep_file_stream_;
	TypeTypep[LISPDECL_STRING_STREAM] = typep_string_stream_;
	TypeTypep[LISPDECL_SYNONYM_STREAM] = typep_synonym_stream_;
	TypeTypep[LISPDECL_TWO_WAY_STREAM] = typep_two_way_stream_;
	TypeTypep[LISPDECL_PROMPT_STREAM] = typep_prompt_stream_;
	TypeTypep[LISPDECL_PRETTY_STREAM] = typep_pretty_stream_;
	TypeTypep[LISPDECL_MEMORY_STREAM] = typep_memory_stream_;
	TypeTypep[LISPDECL_PIPE_STREAM] = typep_pipe_stream_;
	TypeTypep[LISPDECL_BYTESPEC] = typep_byte_;
	TypeTypep[LISPDECL_PRINT_DISPATCH] = typep_print_dispatch_;
	TypeTypep[LISPDECL_PAPER] = typep_paper_;
	TypeTypep[LISPDECL_EVAL] = typep_eval_;
}

static int typep_call_(Execute ptr, addr value, addr type, int asterisk, int *ret)
{
	int result;
	LocalHold hold;

	if ((! asterisk) && type_asterisk_p(type))
		return fmte_("typep don't allow to be asterisk *.", NULL);
	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, value, type, NULL);
	Return(typep_table_(ptr, value, type, &result));
	*ret = RefNotDecl(type)? (! result): result;
	localhold_end(hold);

	return 0;
}

int typep_clang_(Execute ptr, addr value, addr type, int *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return typep_call_(ptr, value, type, 0, ret);
}

int typep_asterisk_clang_(Execute ptr, addr value, addr type, int *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	return typep_call_(ptr, value, type, 1, ret);
}

