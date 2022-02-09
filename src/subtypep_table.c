#include "condition.h"
#include "subtypep_atomic.h"
#include "subtypep_compound.h"
#include "subtypep_table.h"
#include "type.h"
#include "type_function.h"

static call_type_subtypep TypeSubtypep[LISPDECL_SIZE];

/*
 *  call error
 */
static int subtypep_call_error_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	infoprint(x);
	infoprint(y);
	*ret = SUBTYPEP_INVALID;
	return fmte_("Invalid subtypep argument.", NULL);
}


/*
 *  call cons
 */
static int subtypep_call_cons_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (type_astert_p(y))
		return ReturnInclude(ret);
	if (type_astert_p(x))
		return ReturnFalse(ret);

	return subtypep_compound_(ptr, x, y, ret);
}

static int subtypep_call_cons_value_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr car1, car2, cdr1, cdr2;

	GetArrayType(x, 0, &car1);
	GetArrayType(y, 0, &car2);
	GetArrayType(x, 1, &cdr1);
	GetArrayType(y, 1, &cdr2);

	/* subtypep */
	Return(subtypep_call_cons_t_(ptr, car1, car2, &value1));
	Return(subtypep_call_cons_t_(ptr, cdr1, cdr2, &value2));

	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);
	if (value1 == SUBTYPEP_INVALID || value2 == SUBTYPEP_INVALID)
		return ReturnInvalid(ret);
	if (value1 == SUBTYPEP_EXCLUDE || value2 == SUBTYPEP_EXCLUDE)
		return ReturnExclude(ret);

	return ReturnFalse(ret);
}

static int subtypep_call_cons_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_CONS:
			return subtypep_call_cons_value_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  call complex
 */
static int subtypep_complex_value_(addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;

	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (type_astert_p(y))
		return ReturnInclude(ret);
	if (type_astert_p(x))
		return ReturnFalse(ret);
	check1 = (RefLispDecl(x) == RefLispDecl(y));
	check2 = (RefNotDecl(x) == RefNotDecl(y));

	return ReturnIncludeExclude(ret, check1 && check2);
}

static int subtypep_call_complex_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_COMPLEX:
			return subtypep_complex_value_(x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  call function
 */
static int subtypep_ordinary_subtypep_(Execute ptr,
		const ordargs *ptr1, const ordtype *type1,
		const ordargs *ptr2, const ordtype *type2,
		SubtypepResult *ret)
{
	SubtypepResult value;
	addr x, y;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &x, ptr1, type1);
	merge_ordargs(local, &y, ptr2, type2);
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);
	Return(subtypep_compound_(ptr, x, y, &value));
	rollback_local(local, stack);

	return Result(ret, value);
}

static int subtypep_ordinary_size_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2,
		size_t size, SubtypepResult *ret)
{
	SubtypepResult value;
	size_t i;
	ordtype type1, type2;

	for (i = 0; i < size; i++) {
		Return(gettype_ordargs_(ptr1, i, &type1));
		Return(gettype_ordargs_(ptr2, i, &type2));
		if (type1.nil)
			break;
		if (type2.nil)
			return ReturnExclude(ret);
		Return(subtypep_ordinary_subtypep_(ptr, ptr1, &type1, ptr2, &type2, &value));
		if (value != SUBTYPEP_INCLUDE)
			return Result(ret, value);
	}

	return ReturnInclude(ret);
}

static int subtypep_ordinary_simple_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	if (ptr1->size > ptr2->size)
		return ReturnExclude(ret);
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_simple_left_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_check_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, SubtypepResult *ret)
{
	/* long size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size),
			ret);
}

static int subtypep_function_ordinary_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	/* list */
	make_ordargs(&ptr1, x);
	make_ordargs(&ptr2, y);
	if (ptr1.size_var < ptr2.size_var)
		return ReturnExclude(ret);

	check1 = simple_p_ordargs(&ptr1);
	check2 = simple_p_ordargs(&ptr2);
	if (check1 && check2)
		return subtypep_ordinary_simple_(ptr, &ptr1, &ptr2, ret);
	if (check1)
		return subtypep_ordinary_simple_left_(ptr, &ptr1, &ptr2, ret);
	if (check2)
		return ReturnExclude(ret);
	else
		return subtypep_ordinary_check_(ptr, &ptr1, &ptr2, ret);
}

static int subtypep_function_values_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	/* asterisk */
	if (type_asterisk_p(y))
		return ReturnInclude(ret);
	if (type_asterisk_p(x))
		return ReturnFalse(ret);

	/* type check */
	return subtypep_compound_(ptr, x, y, ret);
}

static int subtypep_function_check_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value1, value2;
	addr arg1, arg2, the1, the2;

	GetArrayType(x, 0, &arg1);
	GetArrayType(y, 0, &arg2);
	GetArrayType(x, 1, &the1);
	GetArrayType(y, 1, &the2);

	/* subtypep */
	Return(subtypep_function_ordinary_(ptr, arg1, arg2, &value1));
	ReturnSecondThrow(ret, value1);
	Return(subtypep_function_values_(ptr, the1, the2, &value2));
	ReturnSecondThrow(ret, value2);

	/* include */
	if (value1 == SUBTYPEP_INCLUDE && value2 == SUBTYPEP_INCLUDE)
		return ReturnInclude(ret);

	/* reverse */
	if (value1 == SUBTYPEP_INCLUDE) {
		Return(subtypep_function_ordinary_(ptr, arg2, arg1, &value1));
		return ReturnSecondValue(ret, value1);
	}
	if (value2 == SUBTYPEP_INCLUDE) {
		Return(subtypep_function_values_(ptr, the2, the1, &value2));
		return ReturnSecondValue(ret, value2);
	}

	return ReturnFalse(ret);
}

static int subtypep_call_function_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_call_compiled_function_function_(
		Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult check;
	Return(subtypep_function_check_(ptr, y, x, &check));
	return ReturnReverse(ret, check);
}

static int subtypep_call_compiled_function_(
		Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_FUNCTION:
			return subtypep_call_compiled_function_function_(ptr, x, y, ret);

		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, x, y, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  table
 */
int subtypep_table_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	call_type_subtypep call;

	call = TypeSubtypep[(int)RefLispDecl(y)];
	if (call)
		return (*call)(ptr, x, y, ret);
	*ret = SUBTYPEP_INVALID;
	return 0;
}

void init_subtypep_table(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeSubtypep[i] = subtypep_call_error_;

	TypeSubtypep[LISPDECL_INVALID] = subtypep_call_invalid_;
	TypeSubtypep[LISPDECL_TYPE] = subtypep_call_error_;
	TypeSubtypep[LISPDECL_CLOS] = subtypep_call_clos_;
	TypeSubtypep[LISPDECL_ASTERISK] = subtypep_call_asterisk_;
	TypeSubtypep[LISPDECL_NIL] = subtypep_call_nil_;
	TypeSubtypep[LISPDECL_T] = subtypep_call_t_;
	TypeSubtypep[LISPDECL_NULL] = subtypep_call_null_;
	TypeSubtypep[LISPDECL_CONS] = subtypep_call_cons_;
	TypeSubtypep[LISPDECL_HASH_TABLE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_SYMBOL] = subtypep_call_symbol_;
	TypeSubtypep[LISPDECL_KEYWORD] = subtypep_call_keyword_;
	TypeSubtypep[LISPDECL_PACKAGE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_RANDOM_STATE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_READTABLE] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_FUNCTION] = subtypep_call_function_;
	TypeSubtypep[LISPDECL_COMPILED_FUNCTION] = subtypep_call_compiled_function_;
	TypeSubtypep[LISPDECL_PATHNAME] = subtypep_call_pathname_;
	TypeSubtypep[LISPDECL_LOGICAL_PATHNAME] = subtypep_call_logical_pathname_;
	TypeSubtypep[LISPDECL_ARRAY] = subtypep_call_array_;
	TypeSubtypep[LISPDECL_SIMPLE_ARRAY] = subtypep_call_simple_array_;
	TypeSubtypep[LISPDECL_CHARACTER] = subtypep_call_character_;
	TypeSubtypep[LISPDECL_BASE_CHAR] = subtypep_call_base_char_;
	TypeSubtypep[LISPDECL_STANDARD_CHAR] = subtypep_call_standard_char_;
	TypeSubtypep[LISPDECL_INTEGER] = subtypep_call_integer_;
	TypeSubtypep[LISPDECL_RATIONAL] = subtypep_call_rational_;
	TypeSubtypep[LISPDECL_REAL] = subtypep_call_real_;
	TypeSubtypep[LISPDECL_NUMBER] = subtypep_call_number_;
	TypeSubtypep[LISPDECL_FLOAT] = subtypep_call_float_;
	TypeSubtypep[LISPDECL_SHORT_FLOAT] = subtypep_call_short_float_;
	TypeSubtypep[LISPDECL_SINGLE_FLOAT] = subtypep_call_single_float_;
	TypeSubtypep[LISPDECL_DOUBLE_FLOAT] = subtypep_call_double_float_;
	TypeSubtypep[LISPDECL_LONG_FLOAT] = subtypep_call_long_float_;
	TypeSubtypep[LISPDECL_RATIO] = subtypep_call_ratio_;
	TypeSubtypep[LISPDECL_COMPLEX] = subtypep_call_complex_;
	TypeSubtypep[LISPDECL_RESTART] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_ENVIRONMENT] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_STREAM] = subtypep_call_stream_;
	TypeSubtypep[LISPDECL_BROADCAST_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_CONCATENATED_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_ECHO_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_FILE_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_STRING_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_SYNONYM_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_TWO_WAY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_PROMPT_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_PRETTY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_MEMORY_STREAM] = subtypep_call_stream_type_;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PAPER] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_EVAL] = subtypep_call_eqltype_;
}

