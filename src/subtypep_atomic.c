#include "clos_class.h"
#include "condition.h"
#include "integer.h"
#include "subtypep_atomic.h"
#include "subtypep_range.h"
#include "type.h"
#include "type_upgraded.h"
#include "typedef.h"

int ReturnReverse(SubtypepResult *ret, SubtypepResult check)
{
	switch (check) {
		case SUBTYPEP_INCLUDE:
			return ReturnFalse(ret);

		default:
			return Result(ret, check);
	}
}


/*
 *  type
 */
int subtypep_call_invalid_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInvalid(ret);
}


/*
 *  clos
 */
int subtypep_call_clos_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	if (RefLispDecl(x) != LISPDECL_CLOS)
		return ReturnExclude(ret);
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	Return(clos_subclass_p_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	return ReturnFalse(ret);
}


/*
 *  asterisk
 */
int subtypep_call_asterisk_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_call_t_(ptr, x, y, ret);
}


/*
 *  nil
 */
int subtypep_call_nil_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  t
 */
int subtypep_call_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return ReturnInclude(ret);
}


/*
 *  null
 */
int subtypep_call_null_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  eqltype
 */
int subtypep_call_eqltype_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == RefLispDecl(y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  symbol
 */
int subtypep_call_symbol_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NULL:
		case LISPDECL_SYMBOL:
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  keyword
 */
int subtypep_call_keyword_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		case LISPDECL_SYMBOL:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  pathname
 */
int subtypep_call_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_PATHNAME:
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  logical-pathname
 */
int subtypep_call_logical_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		case LISPDECL_PATHNAME:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  array
 */
static int subtypep_call_aa_integer_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	Return(getindex_integer_(x, &size1));
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_vector_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size1, size2;

	LenArrayA4(x, &size1);
	Return(getindex_integer_(y, &size2));

	return ReturnIncludeExclude(ret, (size1 == size2));
}

static int subtypep_call_aa_integer_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check;
	size_t size1, size2, i;

	Return(getindex_integer_(x, &size1));
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		GetArrayA4(y, i, &check);
		if (! type_asterisk_p(check))
			return ReturnFalse(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_vector_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check1, check2;
	size_t size1, size2, i, dim1, dim2;

	LenArrayA4(x, &size1);
	LenArrayA4(y, &size2);
	if (size1 != size2)
		return ReturnExclude(ret);

	for (i = 0; i < size1; i++) {
		/* asterisk */
		GetArrayA4(y, i, &check2);
		if (type_asterisk_p(check2))
			continue;
		GetArrayA4(x, i, &check1);
		if (type_asterisk_p(check1))
			return ReturnFalse(ret);

		/* dimension */
		Return(getindex_integer_(check1, &dim1));
		Return(getindex_integer_(check2, &dim2));
		if (dim1 != dim2)
			return ReturnExclude(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_aa_integer_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_integer_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_aa_vector_(addr x, addr y, SubtypepResult *ret)
{
	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_vector_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_dimension_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	switch (GetType(y)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return subtypep_call_aa_integer_(x, y, ret);

		case LISPTYPE_VECTOR:
			return subtypep_call_aa_vector_(x, y, ret);

		default:
			return ReturnInvalid(ret);
	}
}

static int subtypep_call_array_array_equal_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	if (upgraded_array0_equal(x, y))
		return ReturnInclude(ret);

	return ReturnExclude(ret);
}

static int subtypep_call_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr type1, type2, dim1, dim2;

	GetArrayType(x, 0, &type1);
	GetArrayType(y, 0, &type2);
	GetArrayType(x, 1, &dim1);
	GetArrayType(y, 1, &dim2);

	/* subtypep */
	Return(subtypep_call_array_array_equal_(type1, type2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_call_array_array_dimension_(dim1, dim2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_equal_(type2, type1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_call_array_array_dimension_(dim2, dim1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

int subtypep_call_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  simple-array
 */
static int subtypep_call_simple_array_array_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_call_array_array_(y, x, &check));
	return ReturnReverse(ret, check);
}

int subtypep_call_simple_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		case LISPDECL_ARRAY:
			return subtypep_call_simple_array_array_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  character
 */
int subtypep_call_character_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  base-char
 */
int subtypep_call_base_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  standard-char
 */
int subtypep_call_standard_char_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_BASE_CHAR:
		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real range
 */
static int subtypep_real_less_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);

	return range_right_right_less_equal_(x, y, ret);
}

static int subtypep_real_greater_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);

	return range_left_left_greater_equal_(x, y, ret);
}

static int subtypep_real_range_(addr x, addr y, int *ret)
{
	if (! range_between_p(x))
		return Result(ret, 0);

	return range_in_between_(x, y, ret);
}

static int subtypep_realcheck_(addr x, addr y, int *ret)
{
	addr check1, check2;

	if (range_asterisk_p(y))
		return Result(ret, 1);
	if (range_asterisk_p(x))
		return Result(ret, 0);

	GetArrayType(y, 0, &check1);
	if (type_asterisk_p(check1))
		return subtypep_real_less_(x, y, ret);

	GetArrayType(y, 2, &check2);
	if (type_asterisk_p(check2))
		return subtypep_real_greater_(x, y, ret);

	return subtypep_real_range_(x, y, ret);
}

static int realexclude_left_(addr x, addr y, int *ret)
{
	if (! range_any_right_p(x))
		return Result(ret, 0);
	if (! range_left_any_p(y))
		return Result(ret, 0);

	return range_right_left_less_(x, y, ret);
}

static int realexclude_right_(addr x, addr y, int *ret)
{
	if (! range_left_any_p(x))
		return Result(ret, 0);
	if (! range_any_right_p(y))
		return Result(ret, 0);

	return range_left_right_greater_(x, y, ret);
}

static int subtypep_realexlucde_(addr x, addr y, int *ret)
{
	int check;

	Return(realexclude_left_(x, y, &check));
	if (check)
		return Result(ret, 1);

	return realexclude_right_(x, y, ret);
}

static int subtypep_realparameter_(addr x, addr y, SubtypepResult *ret)
{
	int check;

	Return(subtypep_realcheck_(x, y, &check));
	if (check)
		return ReturnInclude(ret);

	Return(subtypep_realexlucde_(x, y, &check));
	if (check)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_realexclude_(addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_realparameter_(y, x, &check));
	return ReturnReverse(ret, check);
}


/*
 *  integer
 */
int subtypep_call_integer_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  rational
 */
static int subtypep_call_rational_ratio_(addr y, SubtypepResult *ret)
{
	if (range_asterisk_p(y))
		return ReturnInclude(ret);
	else
		return ReturnFalse(ret);
}

int subtypep_call_rational_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_RATIONAL:
		case LISPDECL_INTEGER:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  real
 */
int subtypep_call_real_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return subtypep_call_rational_ratio_(y, ret);

		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float
 */
int subtypep_call_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
			return subtypep_realexclude_(x, y, ret);

		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return subtypep_realparameter_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  float range
 */
static int subtypep_float_type_(addr x, addr y,
		SubtypepResult *ret, enum LISPDECL check)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	if (type == check)
		return subtypep_realparameter_(x, y, ret);

	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
			return subtypep_realexclude_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  short-float
 */
int subtypep_call_short_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SHORT_FLOAT);
}


/*
 *  single-float
 */
int subtypep_call_single_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_SINGLE_FLOAT);
}


/*
 *  double-float
 */
int subtypep_call_double_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_DOUBLE_FLOAT);
}


/*
 *  long-float
 */
int subtypep_call_long_float_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_float_type_(x, y, ret, LISPDECL_LONG_FLOAT);
}


/*
 *  number
 */
int subtypep_call_number_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(x, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_COMPLEX:
		case LISPDECL_RATIO:
		case LISPDECL_INTEGER:
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  ratio
 */
int subtypep_call_ratio_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
		case LISPDECL_RATIONAL:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream
 */
int subtypep_call_stream_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_STREAM:
		case LISPDECL_BROADCAST_STREAM:
		case LISPDECL_CONCATENATED_STREAM:
		case LISPDECL_ECHO_STREAM:
		case LISPDECL_FILE_STREAM:
		case LISPDECL_STRING_STREAM:
		case LISPDECL_SYNONYM_STREAM:
		case LISPDECL_TWO_WAY_STREAM:
		case LISPDECL_PROMPT_STREAM:
		case LISPDECL_PRETTY_STREAM:
		case LISPDECL_MEMORY_STREAM:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  stream object
 */
int subtypep_call_stream_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;

	GetLispDecl(x, &type1);
	GetLispDecl(y, &type2);
	if (type1 == type2)
		return ReturnInclude(ret);
	if (type1 == LISPDECL_STREAM)
		return ReturnFalse(ret);

	return ReturnExclude(ret);
}

