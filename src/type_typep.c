#include "array.h"
#include "bignum.h"
#include "character.h"
#include "clos_standard.h"
#include "cmpl.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "equal.h"
#include "function.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "rational.h"
#include "real_float.h"
#include "unicode.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_parse.h"
#include "type_typep.h"

static int typep_call(addr value, addr type, int asterisk);
typedef int (*call_type_typep)(addr value, addr type);
static call_type_typep TypeTypep[LISPDECL_SIZE];

static int typep_error(addr value, addr type)
{
	fmte("Invalid type ~S.", type, NULL);
	return 0;
}

static int typep_type(addr value, addr type)
{
	return GetType(value) == LISPTYPE_TYPE;
}

static int typep_clos(addr value, addr type)
{
	if (GetType(value) != LISPTYPE_CLOS) return 0;
	GetArrayType(type, 0, &type);
	return std_subtype_p(value, type);
}

static int typep_asterisk(addr value, addr type)
{
	return 1;
}

static int typep_optimized(addr value, addr type)
{
	GetArrayType(type, 0, &type);
	return typep_call(value, type, 1);
}


/*
 *  Compound-type
 */
static int typep_and(addr value, addr type)
{
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (! typep_call(value, check, 1)) return 0;
	}

	return 1;
}

static int typep_or(addr value, addr type)
{
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (typep_call(value, check, 1)) return 1;
	}

	return 0;
}

static int typep_eql(addr value, addr type)
{
	GetArrayType(type, 0, &type);
	return eql_function(value, type);
}

static int typep_member(addr value, addr type)
{
	addr check;
	size_t i, size;

	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(type, i, &check);
		if (eql_function(value, check)) return 1;
	}

	return 0;
}

static int typep_not(addr value, addr type)
{
	GetArrayType(type, 0, &type);
	return ! typep_call(value, type, 1);
}

static int typep_mod(addr value, addr type)
{
	if (! integerp(value)) return 0;
	if (minusp_integer(value)) return 0;
	GetArrayType(type, 0, &type);

	return less_integer(value, type);
}

static int typep_satisfies(addr value, addr type)
{
	GetArrayType(type, 0, &type);
	if (callclang_funcall(Execute_Thread, &type, type, value, NULL))
		fmte("Cannot jump a external procedure.", NULL);

	return type != Nil;
}

static int typep_values(addr value, addr type)
{
	fmte("The values type don't use in typep context.", NULL);
	return 0;
}


/*
 *  Extract-type
 */
static int typep_atom(addr value, addr type)
{
	return ! IsCons(value);
}

static int typep_list(addr value, addr type)
{
	return IsList(value);
}

static int typep_boolean(addr value, addr type)
{
	return value == Nil || value == T;
}

static int typep_vector_vector(addr value, addr type)
{
	enum LISPDECL left;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &left);
	if ((left != LISPDECL_ASTERISK) && (left != LISPDECL_T)) return 0;
	GetArrayType(type, 1, &check);
	if (asterisk_p(check)) return 1;
	lenarray(value, &size1);
	GetFixnum(check, &size2);

	return size1 == (size_t)size2;
}

static int character_decl_p(enum LISPDECL decl)
{
	return
		decl == LISPDECL_CHARACTER ||
		decl == LISPDECL_BASE_CHAR ||
		decl == LISPDECL_STANDARD_CHAR ||
		decl == LISPDECL_EXTENDED_CHAR;
}

static int typep_vector_string(addr value, addr type)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (! character_decl_p(decl))) return 0;
	GetArrayType(type, 1, &check);
	if (asterisk_p(check)) return 1;
	string_length(value, &size1);
	GetFixnum(check, &size2);

	return size1 == (size_t)size2;
}

static int typep_vector_bitvector(addr value, addr type)
{
	enum LISPDECL decl;
	addr check;
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &check);
	GetLispDecl(check, &decl);
	if ((decl != LISPDECL_ASTERISK) && (decl != LISPDECL_BIT)) return 0;
	GetArrayType(type, 1, &check);
	if (asterisk_p(check)) return 1;
	string_length(value, &size1);
	GetFixnum(check, &size2);

	return size1 == (size_t)size2;
}

static int typep_vector_dimension(addr value, addr type)
{
	/* asterisk */
	if (asterisk_p(type)) {
		return array_vector_p(value);
	}

	/* fixnum */
	if (GetType(type) == LISPTYPE_FIXNUM) {
		return array_size_vector_p(value, (size_t)RefFixnum(type));
	}

	/* error */
	Abort("type error");
	return 0;
}

static int typep_vector_array(addr value, addr type)
{
	addr left, right;

	GetArrayType(type, 0, &left);
	GetArrayInfo(value, ARRAY_INFO_TYPE, &right);
	if ((! asterisk_p(left)) && (! equal_array_type(left, right))) return 0;
	GetArrayType(type, 1, &type);

	return typep_vector_dimension(value, type);
}

static int typep_vector(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_vector_vector(value, type);

		case LISPTYPE_STRING:
			return typep_vector_string(value, type);

		case LISPTYPE_ARRAY:
			if (strarrayp(value))
				return typep_vector_string(value, type);
			return typep_vector_array(value, type);

		case LISPTYPE_BITVECTOR:
			return typep_vector_bitvector(value, type);

		default:
			break;
	}

	return 0;
}

static int typep_simple_vector_vector(addr value, addr type)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (asterisk_p(type)) return 1;
	lenarray(value, &size1);
	GetFixnum(type, &size2);

	return size1 == (size_t)size2;
}

static int typep_type_vector_array(addr value, addr type, enum LISPDECL decl)
{
	addr check;

	GetArrayInfo(value, ARRAY_INFO_TYPE, &check);
	if ((! asterisk_p(check)) && RefLispDecl(check) != decl) return 0;
	GetArrayType(type, 0, &check);

	return typep_vector_dimension(value, check);
}

static int typep_simple_vector(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_VECTOR:
			return typep_simple_vector_vector(value, type);

		case LISPTYPE_ARRAY:
			if (! array_simple_p(value)) return 0;
			return typep_type_vector_array(value, type, LISPDECL_T);

		default:
			break;
	}

	return 0;
}

static int typep_bit_vector(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return typep_type_vector_array(value, type, LISPDECL_BIT);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}

static int typep_simple_bit_vector(addr value, addr type)
{
	switch (GetType(value)) {
		case LISPTYPE_ARRAY:
			return array_simple_p(value) &&
				typep_type_vector_array(value, type, LISPDECL_BIT);

		case LISPTYPE_BITVECTOR:
			return 1;

		default:
			return 0;
	}
}

static int typep_extended_char(addr value, addr type)
{
	return extended_char_p(value);
}

static int typep_string_size(addr value, addr type)
{
	fixnum size2;
	size_t size1;

	GetArrayType(type, 0, &type);
	if (asterisk_p(type)) return 1;
	string_length(value, &size1);
	GetFixnum(type, &size2);

	return size1 == (size_t)size2;
}

static int typep_string(addr value, addr type)
{
	if (! stringp(value)) return 0;
	return typep_string_size(value, type);
}

static int typep_base_string_size(addr value, addr type)
{
	enum CHARACTER_TYPE chartype;
	GetCharacterType(value, &chartype);
	return (chartype == CHARACTER_TYPE_EMPTY ||
			chartype == CHARACTER_TYPE_STANDARD ||
			chartype == CHARACTER_TYPE_BASE) &&
		typep_string_size(value, type);
}

static int typep_base_string(addr value, addr type)
{
	if (! stringp(value)) return 0;
	return typep_base_string_size(value, type);
}

static int typep_simple_string(addr value, addr type)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING) {
		return typep_string_size(value, type);
	}
	if (check == LISPTYPE_ARRAY) {
		return array_simple_p(value) &&
			strarrayp(value) &&
			typep_string_size(value, type);
	}

	return 0;
}

static int typep_simple_base_string(addr value, addr type)
{
	enum LISPTYPE check;

	check = GetType(value);
	if (check == LISPTYPE_STRING) {
		return typep_base_string_size(value, type);
	}
	if (check == LISPTYPE_ARRAY) {
		return array_simple_p(value) &&
			strarrayp(value) &&
			typep_base_string_size(value, type);
	}

	return 0;
}

static int typep_signed_byte(addr value, addr type)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (asterisk_p(check)) return 1;
		return fixnum_signed_byte_p(value, RefFixnum(check));
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (asterisk_p(check)) return 1;
		return bignum_signed_byte_p(value, RefFixnum(check));
	}

	return 0;
}

static int typep_unsigned_byte(addr value, addr type)
{
	addr check;
	enum LISPTYPE listtype;

	listtype = GetType(value);
	if (listtype == LISPTYPE_FIXNUM) {
		GetArrayType(type, 0, &check);
		if (asterisk_p(check))
			return 0 <= RefFixnum(value);
		return fixnum_unsigned_byte_p(value, RefFixnum(check));
	}

	if (listtype == LISPTYPE_BIGNUM) {
		GetArrayType(type, 0, &check);
		if (asterisk_p(check))
			return zerop_or_plusp_bignum(value);
		return bignum_unsigned_byte_p(value, RefFixnum(check));
	}

	return 0;
}

static int typep_bit(addr value, addr type)
{
	enum LISPTYPE lisptype;
	fixnum check;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM) {
		GetFixnum(value, &check);
		return check == 0 || check == 1;
	}
	if (lisptype == LISPTYPE_BIGNUM) {
		return zerop_bignum(value) ||
			equal_value_bignum(value, signplus_bignum, 1);
	}

	return 0;
}

static int fbf_bignum(fixnum left, addr value, fixnum right)
{
	return compare_value_bignum(left, value) <= 0 &&
		compare_bignum_value(value, right) <= 0;
}

static int typep_fixnum(addr value, addr type)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM) {
		return 1;
	}
	if (lisptype == LISPTYPE_BIGNUM) {
		return fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX);
	}

	return 0;
}

static int typep_bignum(addr value, addr type)
{
	enum LISPTYPE lisptype;

	lisptype = GetType(value);
	if (lisptype == LISPTYPE_FIXNUM) {
		return 0;
	}
	if (lisptype == LISPTYPE_BIGNUM) {
		return ! fbf_bignum(FIXNUM_MIN, value, FIXNUM_MAX);
	}

	return 0;
}


/*
 *  Atomic-type
 */
static int typep_nil(addr value, addr type)
{
	return 0;
}

static int typep_t(addr value, addr type)
{
	return 1;
}

static int typep_null(addr value, addr type)
{
	return value == Nil;
}

static int typep_cons(addr value, addr type)
{
	addr left, check;

	if (! IsCons(value)) return 0;
	GetCons(value, &left, &value);
	GetArrayType(type, 0, &check);
	if (! typep_call(left, check, 1)) return 0;
	GetArrayType(type, 1, &check);

	return typep_call(value, check, 1);
}

static int typep_hash_table(addr value, addr type)
{
	return GetType(value) == LISPTYPE_HASHTABLE;
}

static int typep_symbol(addr value, addr type)
{
	return symbolp(value);
}

static int typep_keyword(addr value, addr type)
{
	addr keyword;

	if (GetType(value) != LISPTYPE_SYMBOL) return 0;
	GetConst(PACKAGE_KEYWORD, &keyword);
	Check(GetType(keyword) != LISPTYPE_PACKAGE, "type error");
	GetPackageSymbol(value, &value);

	return value == keyword;
}

static int typep_package(addr value, addr type)
{
	return GetType(value) == LISPTYPE_PACKAGE;
}

static int typep_random_state(addr value, addr type)
{
	return GetType(value) == LISPTYPE_RANDOM_STATE;
}

static int typep_readtable(addr value, addr type)
{
	return GetType(value) == LISPTYPE_READTABLE;
}

static int typep_function_check(addr value, addr right)
{
	int validp;
	addr left;

	gettype_function(value, &left);
	if (left == Nil) {
		if (compiled_function_p(value))
			type_compiled_function_asterisk(NULL, &left);
		else
			type_function_asterisk(NULL, &left);
	}
	return subtypep_clang(left, right, &validp);
}

static int typep_function(addr value, addr type)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (type == Nil)
		fmte("The cons type (FUNCTION ? ?) don't accept.", NULL);
	if (! functionp(value)) return 0;
	return typep_function_check(value, type);
}

static int typep_compiled_function(addr value, addr type)
{
	addr check;

	GetArrayType(type, 2, &check);
	if (type == Nil)
		fmte("The cons type (COMPILED-FUNCTION ? ?) don't accept.", NULL);
	if (! compiled_function_p(value)) return 0;
	return typep_function_check(value, type);
}

static int typep_pathname(addr value, addr type)
{
	return pathnamep(value);
}

static int typep_logical_pathname(addr value, addr type)
{
	return pathname_logical_p(value);
}

static int typep_sequence(addr value, addr type)
{
	return sequencep(value);
}

static int equal_array_dimension(addr value, addr right)
{
	addr left, check;
	size_t i, size, *psize;

	/* size check */
	GetArrayInfo(value, ARRAY_INFO_DIMENSION, &left);
	size = ArrayInfoStruct(value)->dimension;
	LenArrayA4(right, &i);
	if (size != i) return 0;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		if (size != 1) return 0;
		GetArrayA4(right, 0, &check);
		return asterisk_p(check) || equal_ff_real(check, left);
	}

	/* system */
	if (GetType(left) == LISPSYSTEM_ARRAY_DIMENSION) {
		psize = PtrArrayDimension(left);
		for (i = 0; i < size; i++) {
			GetArrayA4(right, i, &check);
			if (asterisk_p(check)) continue;
			if (psize[i] != (size_t)RefFixnum(check)) return 0;
		}
		return 1;
	}

	/* nil */
	if (left == Nil) {
		return 1;
	}

	/* error */
	Abort("type error");
	return 1;
}

static int typep_array_dimension(addr value, addr type)
{
	/* asterisk */
	if (asterisk_p(type)) {
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
	GetArrayInfo(value, ARRAY_INFO_TYPE, &right);
	if ((! asterisk_p(left)) && (! equal_array_type(left, right))) return 0;
	GetArrayType(type, 1, &type);

	return typep_array_dimension(value, type);
}

static int equal_fixnum_index(addr left, size_t right)
{
	fixnum value;

	GetFixnum(left, &value);
	if (value < 0) return 0;
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
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_T) return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (asterisk_p(left)) return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1) return 0;
		GetArrayA4(left, 0, &left);
		if (asterisk_p(left)) return 1;
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
	if ((decl != LISPDECL_ASTERISK) && (! character_decl_p(decl))) return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (asterisk_p(left)) return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1) return 0;
		GetArrayA4(left, 0, &left);
		if (asterisk_p(left)) return 1;
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
	if (decl != LISPDECL_ASTERISK && decl != LISPDECL_BIT) return 0;

	/* dimension */
	GetArrayType(type, 1, &left);
	if (asterisk_p(left)) return 1;

	/* fixnum */
	if (GetType(left) == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}

	/* vector */
	if (GetType(left) == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		if (size != 1) return 0;
		GetArrayA4(left, 0, &left);
		if (asterisk_p(left)) return 1;
		string_length(value, &size);
		return equal_fixnum_index(left, size);
	}

	/* error */
	Abort("Invalid array type.");
	return 0;
}

static int typep_array(addr value, addr type)
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

static int typep_simple_array(addr value, addr type)
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

static int typep_character(addr value, addr type)
{
	return GetType(value) == LISPTYPE_CHARACTER;
}

static int typep_base_char(addr value, addr type)
{
	return base_char_p(value);
}

static int typep_standard_char(addr value, addr type)
{
	return standard_char_p(value);
}

static int typep_number(addr value, addr type)
{
	return numberp(value);
}

static int typep_ratio(addr value, addr type)
{
	return GetType(value) == LISPTYPE_RATIO;
}

static int typep_complex(addr value, addr type)
{
	addr check;

	if (GetType(value) != LISPTYPE_COMPLEX) return 0;
	GetRealComplex(value, &check);
	GetArrayType(type, 0, &type);
	if (! typep_call(check, type, 1)) return 0;
	GetImagComplex(value, &check);

	return typep_call(check, type, 1);
}


/*
 *  range
 */
static int less_mode_nolocal(addr mode, addr left, addr right,
		int (*less)(addr, addr),
		int (*less_equal)(addr, addr))
{
	if (asterisk_p(mode))
		return 1;
	if (mode == Nil)
		return less_equal(left, right);
	else
		return less(left, right);
}

static int typep_range_nolocal(addr value, addr type,
		int (*typecheck)(addr),
		int (*less)(addr, addr),
		int (*less_equal)(addr, addr))
{
	addr mode, check;

	/* type */
	if (! typecheck(value)) return 0;

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &check);
	if (! less_mode_nolocal(mode, check, value, less, less_equal)) {
		return 0;
	}

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &check);
	if (! less_mode_nolocal(mode, value, check, less, less_equal)) {
		return 0;
	}

	return 1;
}

static int less_mode_local(LocalRoot local, addr mode, addr left, addr right,
		int (*less)(LocalRoot, addr, addr),
		int (*less_equal)(LocalRoot, addr, addr))
{
	if (asterisk_p(mode))
		return 1;
	if (mode == Nil)
		return less_equal(local, left, right);
	else
		return less(local, left, right);
}

static int typep_range_local(LocalRoot local, addr value, addr type,
		int (*typecheck)(addr),
		int (*less)(LocalRoot, addr, addr),
		int (*less_equal)(LocalRoot, addr, addr))
{
	addr mode, check;

	/* type */
	if (! typecheck(value)) return 0;

	/* left */
	GetArrayType(type, 0, &mode);
	GetArrayType(type, 1, &check);
	if (! less_mode_local(local, mode, check, value, less, less_equal)) {
		return 0;
	}

	/* right */
	GetArrayType(type, 2, &mode);
	GetArrayType(type, 3, &check);
	if (! less_mode_local(local, mode, value, check, less, less_equal)) {
		return 0;
	}

	return 1;
}

static int typep_integer(addr value, addr type)
{
	return typep_range_nolocal(value, type,
			integerp,
			less_integer_clang,
			less_equal_integer_clang);
}

static int typep_rational(addr value, addr type)
{
	return typep_range_local(Local_Thread, value, type,
			rationalp,
			less_rational_clang,
			less_equal_rational_clang);
}

static int typep_real(addr value, addr type)
{
	return typep_range_local(Local_Thread, value, type,
			realp,
			less_real_clang,
			less_equal_real_clang);
}

static int typep_float(addr value, addr type)
{
	return typep_range_nolocal(value, type,
			floatp,
			less_float_clang,
			less_equal_float_clang);
}

static int single_float_p_clang(addr value)
{
	return GetType(value) == LISPTYPE_SINGLE_FLOAT;
}
static int typep_single_float(addr value, addr type)
{
	return typep_range_nolocal(value, type,
			single_float_p_clang,
			less_ss_clang,
			less_equal_ss_clang);
}

static int double_float_p_clang(addr value)
{
	return GetType(value) == LISPTYPE_DOUBLE_FLOAT;
}
static int typep_double_float(addr value, addr type)
{
	return typep_range_nolocal(value, type,
			double_float_p_clang,
			less_dd_clang,
			less_equal_dd_clang);
}

static int long_float_p_clang(addr value)
{
	return GetType(value) == LISPTYPE_LONG_FLOAT;
}
static int typep_long_float(addr value, addr type)
{
	return typep_range_nolocal(value, type,
			long_float_p_clang,
			less_ll_clang,
			less_equal_ll_clang);
}

static int typep_restart(addr value, addr type)
{
	return GetType(value) == LISPTYPE_RESTART;
}

static int typep_environment(addr value, addr type)
{
	return GetType(value) == LISPTYPE_ENVIRONMENT;
}

static int typep_stream(addr value, addr type)
{
	return streamp(value);
}

static int typep_broadcast_stream(addr value, addr type)
{
	return broadcast_stream_p(value);
}

static int typep_concatenated_stream(addr value, addr type)
{
	return concatenated_stream_p(value);
}

static int typep_echo_stream(addr value, addr type)
{
	return echo_stream_p(value);
}

static int typep_file_stream(addr value, addr type)
{
	return file_stream_p(value);
}

static int typep_string_stream(addr value, addr type)
{
	return string_stream_p(value);
}

static int typep_synonym_stream(addr value, addr type)
{
	return synonym_stream_p(value);
}

static int typep_two_way_stream(addr value, addr type)
{
	return twoway_stream_p(value);
}


/*
 *  typep-clang
 */
int typep_table(addr value, addr type)
{
	call_type_typep call;

	Check(GetType(type) != LISPTYPE_TYPE, "type error");
	call = TypeTypep[(int)RefLispDecl(type)];
	Check(call == NULL, "build error");

	return call(value, type);
}

void init_type_typep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeTypep[i] = typep_error;

	TypeTypep[LISPDECL_TYPE] = typep_type;
	TypeTypep[LISPDECL_CLOS] = typep_clos;
	TypeTypep[LISPDECL_ASTERISK] = typep_asterisk;
	TypeTypep[LISPDECL_OPTIMIZED] = typep_optimized;
	TypeTypep[LISPDECL_SUBTYPEP] = typep_optimized;
	/* Compound-type */
	TypeTypep[LISPDECL_AND] = typep_and;
	TypeTypep[LISPDECL_OR] = typep_or;
	TypeTypep[LISPDECL_EQL] = typep_eql;
	TypeTypep[LISPDECL_MEMBER] = typep_member;
	TypeTypep[LISPDECL_MOD] = typep_mod;
	TypeTypep[LISPDECL_NOT] = typep_not;
	TypeTypep[LISPDECL_SATISFIES] = typep_satisfies;
	TypeTypep[LISPDECL_VALUES] = typep_values;
	/* Extract-type */
	TypeTypep[LISPDECL_ATOM] = typep_atom;
	TypeTypep[LISPDECL_LIST] = typep_list;
	TypeTypep[LISPDECL_BOOLEAN] = typep_boolean;
	TypeTypep[LISPDECL_VECTOR] = typep_vector;
	TypeTypep[LISPDECL_SIMPLE_VECTOR] = typep_simple_vector;
	TypeTypep[LISPDECL_BIT_VECTOR] = typep_bit_vector;
	TypeTypep[LISPDECL_SIMPLE_BIT_VECTOR] = typep_simple_bit_vector;
	TypeTypep[LISPDECL_EXTENDED_CHAR] = typep_extended_char;
	TypeTypep[LISPDECL_STRING] = typep_string;
	TypeTypep[LISPDECL_BASE_STRING] = typep_base_string;
	TypeTypep[LISPDECL_SIMPLE_STRING] = typep_simple_string;
	TypeTypep[LISPDECL_SIMPLE_BASE_STRING] = typep_simple_base_string;
	TypeTypep[LISPDECL_SIGNED_BYTE] = typep_signed_byte;
	TypeTypep[LISPDECL_UNSIGNED_BYTE] = typep_unsigned_byte;
	TypeTypep[LISPDECL_BIT] = typep_bit;
	TypeTypep[LISPDECL_FIXNUM] = typep_fixnum;
	TypeTypep[LISPDECL_BIGNUM] = typep_bignum;
	/* Atomic-type */
	TypeTypep[LISPDECL_NIL] = typep_nil;
	TypeTypep[LISPDECL_T] = typep_t;
	TypeTypep[LISPDECL_NULL] = typep_null;
	TypeTypep[LISPDECL_CONS] = typep_cons;
	TypeTypep[LISPDECL_HASH_TABLE] = typep_hash_table;
	TypeTypep[LISPDECL_SYMBOL] = typep_symbol;
	TypeTypep[LISPDECL_KEYWORD] = typep_keyword;
	TypeTypep[LISPDECL_PACKAGE] = typep_package;
	TypeTypep[LISPDECL_RANDOM_STATE] = typep_random_state;
	TypeTypep[LISPDECL_READTABLE] = typep_readtable;
	TypeTypep[LISPDECL_FUNCTION] = typep_function;
	TypeTypep[LISPDECL_COMPILED_FUNCTION] = typep_compiled_function;
	TypeTypep[LISPDECL_PATHNAME] = typep_pathname;
	TypeTypep[LISPDECL_LOGICAL_PATHNAME] = typep_logical_pathname;
	TypeTypep[LISPDECL_SEQUENCE] = typep_sequence;
	TypeTypep[LISPDECL_ARRAY] = typep_array;
	TypeTypep[LISPDECL_SIMPLE_ARRAY] = typep_simple_array;
	TypeTypep[LISPDECL_CHARACTER] = typep_character;
	TypeTypep[LISPDECL_BASE_CHAR] = typep_base_char;
	TypeTypep[LISPDECL_STANDARD_CHAR] = typep_standard_char;
	TypeTypep[LISPDECL_NUMBER] = typep_number;
	TypeTypep[LISPDECL_REAL] = typep_real;
	TypeTypep[LISPDECL_RATIO] = typep_ratio;
	TypeTypep[LISPDECL_INTEGER] = typep_integer;
	TypeTypep[LISPDECL_RATIONAL] = typep_rational;
	TypeTypep[LISPDECL_COMPLEX] = typep_complex;
	TypeTypep[LISPDECL_FLOAT] = typep_float;
	TypeTypep[LISPDECL_SHORT_FLOAT] = typep_single_float;
	TypeTypep[LISPDECL_SINGLE_FLOAT] = typep_single_float;
	TypeTypep[LISPDECL_DOUBLE_FLOAT] = typep_double_float;
	TypeTypep[LISPDECL_LONG_FLOAT] = typep_long_float;
	TypeTypep[LISPDECL_RESTART] = typep_restart;
	TypeTypep[LISPDECL_ENVIRONMENT] = typep_environment;
	TypeTypep[LISPDECL_STREAM] = typep_stream;
	TypeTypep[LISPDECL_BROADCAST_STREAM] = typep_broadcast_stream;
	TypeTypep[LISPDECL_CONCATENATED_STREAM] = typep_concatenated_stream;
	TypeTypep[LISPDECL_ECHO_STREAM] = typep_echo_stream;
	TypeTypep[LISPDECL_FILE_STREAM] = typep_file_stream;
	TypeTypep[LISPDECL_STRING_STREAM] = typep_string_stream;
	TypeTypep[LISPDECL_SYNONYM_STREAM] = typep_synonym_stream;
	TypeTypep[LISPDECL_TWO_WAY_STREAM] = typep_two_way_stream;
}

static int typep_call(addr value, addr type, int asterisk)
{
	int result;
	if ((! asterisk) && asterisk_p(type))
		fmte("typep don't allow to be asterisk *.", NULL);
	result = typep_table(value, type);
	return RefNotDecl(type)? (! result): result;
}

int typep_clang(addr value, addr type)
{
	Check(GetType(type) != LISPTYPE_TYPE, "type error");
	return typep_call(value, type, 0);
}

int typep_asterisk_clang(addr value, addr type)
{
	Check(GetType(type) != LISPTYPE_TYPE, "type error");
	return typep_call(value, type, 1);
}

int typep_throw(LocalRoot local, addr value, addr type)
{
	int result;
	LocalStack stack;

	push_local(local, &stack);
	type_throw_local(local, &type, type);
	result = typep_clang(value, type);
	rollback_local(local, stack);

	return result;
}

int typep_asterisk_throw(LocalRoot local, addr value, addr type)
{
	int result;
	LocalStack stack;

	push_local(local, &stack);
	type_throw_local(local, &type, type);
	result = typep_asterisk_clang(value, type);
	rollback_local(local, stack);

	return result;
}

