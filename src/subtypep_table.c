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
	SubtypepResult value;
	addr car1, car2, cdr1, cdr2;

	GetArrayType(x, 0, &car1);
	GetArrayType(x, 1, &cdr1);
	GetArrayType(y, 0, &car2);
	GetArrayType(y, 1, &cdr2);

	Return(subtypep_call_cons_t_(ptr, car1, car2, &value));
	ReturnSwitchInclude(ret, value);
	Return(subtypep_call_cons_t_(ptr, cdr1, cdr2, &value));
	ReturnSwitchInclude(ret, value);

	return ReturnInclude(ret);
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

	return ReturnBool(ret, check1 && check2);
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
		int *ret)
{
	SubtypepResult value;
	addr x, y;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &x, ptr1, type1);
	merge_ordargs(local, &y, ptr2, type2);
	Return(subtypep_compound_(ptr, x, y, &value));
	rollback_local(local, stack);

	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_ordinary_size_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2,
		size_t size, int *ret)
{
	int check;
	size_t i;
	ordtype type1, type2;

	for (i = 0; i < size; i++) {
		Return(gettype_ordargs_(ptr1, i, &type1));
		Return(gettype_ordargs_(ptr2, i, &type2));
		if (type1.nil)
			break;
		if (type2.nil)
			return Result(ret, 0);
		Return(subtypep_ordinary_subtypep_(ptr, ptr1, &type1, ptr2, &type2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int subtypep_ordinary_simple_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	if (ptr1->size > ptr2->size)
		return Result(ret, 0);
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_simple_left_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* short size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int subtypep_ordinary_check_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* long size */
	return subtypep_ordinary_size_(ptr, ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size),
			ret);
}

static int subtypep_function_ordinary_(Execute ptr, addr x, addr y, int *ret)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(y))
		return Result(ret, 1);
	if (type_asterisk_p(x))
		return Result(ret, 0);

	/* list */
	make_ordargs(&ptr1, x);
	make_ordargs(&ptr2, y);
	if (ptr1.size_var < ptr2.size_var)
		return Result(ret, 0);

	check1 = simple_p_ordargs(&ptr1);
	check2 = simple_p_ordargs(&ptr2);
	if (check1 && check2)
		return subtypep_ordinary_simple_(ptr, &ptr1, &ptr2, ret);
	if (check1)
		return subtypep_ordinary_simple_left_(ptr, &ptr1, &ptr2, ret);
	if (check2)
		return Result(ret, 0);
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
	int check;
	SubtypepResult value;
	addr check1, check2;

	/* lambda-list */
	GetArrayType(x, 0, &check1);
	GetArrayType(y, 0, &check2);
	Return(subtypep_function_ordinary_(ptr, check1, check2, &check));
	if (! check)
		return ReturnFalse(ret);

	/* values */
	GetArrayType(x, 1, &check1);
	GetArrayType(y, 1, &check2);
	Return(subtypep_function_values_(ptr, check1, check2, &value));
	ReturnSwitchInclude(ret, value);

	return ReturnInclude(ret);
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

static int subtypep_call_compiled_function_(
		Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_FUNCTION:
			return ReturnFalse(ret);

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
	TypeSubtypep[LISPDECL_TYPE] = subtypep_call_type_;
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
	TypeSubtypep[LISPDECL_EVAL] = subtypep_call_eqltype_;
}

