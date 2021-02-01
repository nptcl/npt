#include "clos_class.h"
#include "type.h"
#include "type_compare.h"
#include "type_range.h"
#include "type_upgraded.h"
#include "typedef.h"

/*
 *  type
 */
int subtypep_call_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) != LISPDECL_TYPE)
		return Result(ret, SUBTYPEP_EXCLUDE);
	else
		return ReturnBool(ret, RefLispDecl(x) == RefLispDecl(y));
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
	return ReturnBool(ret, check);
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

		case LISPDECL_CONS:
		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

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
 *  sequence
 */
static int subtypep_call_sequence_array(addr x)
{
	enum LISPTYPE type;
	size_t size;

	GetArrayType(x, 1, &x);
	if (type_asterisk_p(x)) {
		return 0;
	}
	type = GetType(x);
	if (type == LISPTYPE_FIXNUM) {
		return RefFixnum(x) == 1;
	}
	if (type == LISPTYPE_VECTOR) {
		LenArrayA4(x, &size);
		return size == 1;
	}

	return 0;
}

int subtypep_call_sequence_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_SEQUENCE:
		case LISPDECL_NULL:
		case LISPDECL_CONS:
			return ReturnInclude(ret);

		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return ReturnBool(ret, subtypep_call_sequence_array(x));

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  array
 */
static int subtypep_call_array_array_integer_(addr x, addr y, SubtypepResult *ret)
{
	size_t size;

	switch (GetType(x)) {
		case LISPTYPE_FIXNUM:
			return ReturnBool(ret, fixnumequal(x, y));

		case LISPTYPE_VECTOR:
			Check(GetStatusSize(x) != LISPSIZE_ARRAY4, "size left error");
			LenArrayA4(x, &size);
			return ReturnBool(ret, ((fixnum)size) == RefFixnum(y));

		default:
			return ReturnFalse(ret);
	}
}

static int subtypep_call_array_array_vector_(addr x, addr y, SubtypepResult *ret)
{
	addr check1, check2;
	size_t i, size;

	if (GetType(x) != LISPTYPE_VECTOR)
		return ReturnFalse(ret);
	Check(GetStatusSize(y) != LISPSIZE_ARRAY4, "size right error");
	Check(GetStatusSize(x) != LISPSIZE_ARRAY4, "size left error");
	LenArrayA4(y, &i);
	LenArrayA4(x, &size);
	if (size != i)
		return ReturnFalse(ret);
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &check2);
		if (type_asterisk_p(check2))
			continue;
		GetArrayA4(x, i, &check1);
		if (type_asterisk_p(check1))
			return ReturnFalse(ret);
		Check(GetType(check2) != LISPTYPE_FIXNUM, "fixnum right error");
		Check(GetType(check1) != LISPTYPE_FIXNUM, "fixnum left error");
		if (! fixnumequal(check1, check2))
			return ReturnFalse(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_call_array_array_dimension_(addr x, addr y, SubtypepResult *ret)
{
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);
	if (GetType(y) == LISPTYPE_FIXNUM)
		return subtypep_call_array_array_integer_(x, y, ret);
	if (GetType(y) == LISPTYPE_VECTOR)
		return subtypep_call_array_array_vector_(x, y, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_call_array_array_equal(addr x, addr y)
{
	if (type_asterisk_p(y))
		return 1;
	if (type_asterisk_p(x))
		return 0;
	return upgraded_array0_equal(x, y);
}

static int subtypep_call_array_array_(addr x, addr y, SubtypepResult *ret)
{
	addr check1, check2;

	/* type */
	GetArrayType(x, 0, &check1);
	GetArrayType(y, 0, &check2);
	if (! subtypep_call_array_array_equal(check1, check2))
		return ReturnFalse(ret);

	/* dimension */
	GetArrayType(x, 1, &x);
	GetArrayType(y, 1, &y);
	return subtypep_call_array_array_dimension_(x, y, ret);
}

int subtypep_call_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  simple-array
 */
int subtypep_call_simple_array_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_call_array_array_(x, y, ret);

		case LISPDECL_ARRAY:
		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

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
	int check;

	Return(subtypep_realexlucde_(x, y, &check));
	if (check)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
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
int subtypep_call_rational_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnBool(ret, range_asterisk_p(y));

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
			return ReturnBool(ret, range_asterisk_p(y));

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

