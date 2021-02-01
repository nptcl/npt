#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "equal.h"
#include "format.h"
#include "hold.h"
#include "integer.h"
#include "object.h"
#include "sequence.h"
#include "rational.h"
#include "real.h"
#include "symbol.h"
#include "type.h"
#include "type_compare.h"
#include "type_copy.h"
#include "type_function.h"
#include "type_number.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_range.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_upgraded.h"

static call_type_subtypep TypeSubtypep[LISPDECL_SIZE];
static int subtypep_call_(Execute ptr, addr x, addr y, int aster, SubtypepResult *ret);
static int subtypep_static_(Execute ptr, addr x, addr y, int aster, SubtypepResult *ret);

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
static int subtypep_call_cons_p(addr pos)
{
	enum LISPDECL decl;
	GetLispDecl(pos, &decl);
	return decl == LISPDECL_ASTERISK || decl == LISPDECL_T;
}

static int subtypep_call_cons_t_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	if (subtypep_call_cons_p(y))
		return ReturnInclude(ret);
	else if (subtypep_call_cons_p(x))
		return ReturnFalse(ret);
	else
		return subtypep_static_(ptr, x, y, 0, ret);
}

static int subtypep_call_cons_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	enum LISPDECL decl;
	SubtypepResult value;
	addr car1, car2, cdr1, cdr2;

	GetLispDecl(x, &decl);
	if (decl == LISPDECL_SEQUENCE)
		return ReturnFalse(ret);
	if (decl != LISPDECL_CONS)
		return ReturnExclude(ret);

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


/*
 *  call complex
 */
static int subtypep_complex_value_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	check = RefLispDecl(left) == RefLispDecl(right)
		&& RefNotDecl(left) == RefNotDecl(right);
	return ReturnBool(ret, check);
}

static int subtypep_complex_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_COMPLEX:
			return subtypep_complex_value_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}


/*
 *  function
 */
static int ordinary_subtypep_(Execute ptr,
		const ordargs *ptr1, const ordtype *type1,
		const ordargs *ptr2, const ordtype *type2,
		int *ret)
{
	SubtypepResult value;
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &left, ptr1, type1);
	merge_ordargs(local, &right, ptr2, type2);
	Return(subtypep_static_(ptr, left, right, 1, &value));
	rollback_local(local, stack);

	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int ordinary_size_(Execute ptr,
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
		Return(ordinary_subtypep_(ptr, ptr1, &type1, ptr2, &type2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int ordinary_simple_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	if (ptr1->size > ptr2->size)
		return Result(ret, 0);
	/* short size */
	return ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int ordinary_simple_left_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* short size */
	return ordinary_size_(ptr, ptr1, ptr2, ptr1->size, ret);
}

static int ordinary_check_(Execute ptr,
		const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* long size */
	return ordinary_size_(ptr, ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size),
			ret);
}

static int subtypep_function_ordinary_(Execute ptr, addr left, addr right, int *ret)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(right))
		return Result(ret, 1);
	if (type_asterisk_p(left))
		return Result(ret, 0);

	/* list */
	make_ordargs(&ptr1, left);
	make_ordargs(&ptr2, right);
	if (ptr1.size_var < ptr2.size_var)
		return Result(ret, 0);

	check1 = simple_p_ordargs(&ptr1);
	check2 = simple_p_ordargs(&ptr2);
	if (check1 && check2)
		return ordinary_simple_(ptr, &ptr1, &ptr2, ret);
	if (check1)
		return ordinary_simple_left_(ptr, &ptr1, &ptr2, ret);
	if (check2)
		return Result(ret, 0);
	else
		return ordinary_check_(ptr, &ptr1, &ptr2, ret);
}

static int subtypep_function_check_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int check;
	SubtypepResult value;
	addr check1, check2;

	/* lambda-list */
	GetArrayType(left, 0, &check1);
	GetArrayType(right, 0, &check2);
	Return(subtypep_function_ordinary_(ptr, check1, check2, &check));
	if (! check)
		return ReturnFalse(ret);

	/* values */
	GetArrayType(left, 1, &left);
	GetArrayType(right, 1, &right);
	Return(subtypep_static_(ptr, left, right, 1, &value));
	ReturnSwitchInclude(ret, value);

	return ReturnInclude(ret);
}

static int subtypep_function_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_compiled_function_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_FUNCTION:
			return ReturnFalse(ret);

		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(ptr, left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_stream_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
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

static int subtypep_stream_child_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;

	GetLispDecl(left, &type1);
	GetLispDecl(right, &type2);
	if (type1 == type2)
		return ReturnInclude(ret);
	else if (type1 == LISPDECL_STREAM)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  subtypep_lisptype_
 */
static int subtypep_lisptype_normal_(Execute ptr,
		addr left, addr right, SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, left, right, &value));
	GetNotDecl(right, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_not_(Execute ptr,
		addr left, addr right, SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, right, left, &value));  /* reverse */
	GetNotDecl(right, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, SUBTYPEP_FALSE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_(Execute ptr,
		addr left, addr right, SubtypepResult *ret, call_type_subtypep call)
{
	if (RefNotDecl(left))
		return subtypep_lisptype_not_(ptr, left, right, ret, call);
	else
		return subtypep_lisptype_normal_(ptr, left, right, ret, call);
}


/*
 *  subtypep_eql
 */
static int subtypep_eql_eql_(addr left, addr right, SubtypepResult *ret)
{
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (eql_function(left, right))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_type_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(right, &right);
	if (RefLispDecl(right) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep '(eql x) right) */
	GetArrayType(left, 0, &left);
	Return(typep_table_(ptr, left, right, &check));
	if (check)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_type_eql_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(left, &left);
	if (RefLispDecl(left) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep left '(eql x)) */
	GetArrayType(right, 0, &right);
	Return(typep_table_(ptr, right, left, &check));
	if (check)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_call_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_EQL);
	check2 = (RefLispDecl(right) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql_(left, right, ret);
	if (check1)
		return subtypep_eql_type_(ptr, left, right, ret);
	if (check2)
		return subtypep_type_eql_(ptr, left, right, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_eql_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, left, right, ret, subtypep_eql_call_);
}


/*
 *  subtypep_values
 */
static size_t getsize_values(addr pos)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	/* opt */
	GetArrayType(pos, 1, &check);
	size += length_list_unsafe(check);

	return size;
}

static int gettype_values_(addr pos, size_t index, addr *ret)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);
	index -= size;

	/* opt */
	GetArrayType(pos, 1, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);

	/* rest */
	GetArrayType(pos, 2, ret);

	return 0;
}

static int subtypep_boolean_(Execute ptr, addr left, addr right, int *ret)
{
	SubtypepResult value;
	Return(subtypep_static_(ptr, left, right, 1, &value));
	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_values_values_(Execute ptr, addr left, addr right, int *ret)
{
	int check;
	addr check1, check2;
	size_t size1, size2, size, i;

	Check(RefLispDecl(left) != LISPDECL_VALUES, "decl left error");
	Check(RefLispDecl(right) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(left), "left not error");
	Check(RefNotDecl(right), "right not error");

	/* size */
	size1 = getsize_values(left);
	size2 = getsize_values(right);
	size = (size1 > size2)? size1: size2;
	size++; /* &rest check */

	/* check */
	for (i = 0; i < size; i++) {
		Return(gettype_values_(left, i, &check1));
		Return(gettype_values_(right, i, &check2));
		Return(subtypep_boolean_(ptr, check1, check2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static void subtypep_values_local(addr pos, addr *ret)
{
	LocalRoot local;
	addr rest;

	local = Local_Thread;
	conscar_local(local, &pos, pos);
	GetTypeTable(&rest, Null);
	type_values_local(local, pos, Nil, rest, Nil, ret);
}

static int subtypep_values_type_(Execute ptr, addr left, addr right, int *ret)
{
	Check(RefLispDecl(left) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(left), "left not error");
	subtypep_values_local(right, &right);
	return subtypep_values_values_(ptr, left, right, ret);
}

static int subtypep_type_values_(Execute ptr, addr left, addr right, int *ret)
{
	Check(RefLispDecl(right) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(right), "right not error");
	subtypep_values_local(left, &left);
	return subtypep_values_values_(ptr, left, right, ret);
}

static int subtypep_values_call_(Execute ptr, addr left, addr right, int *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_VALUES);
	check2 = (RefLispDecl(right) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values_(ptr, left, right, ret);
	if (check1)
		return subtypep_values_type_(ptr, left, right, ret);
	if (check2)
		return subtypep_type_values_(ptr, left, right, ret);
	Abort("type error");
	return Result(ret, 0);
}

static int subtypep_values_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int value;
	LocalRoot local;
	LocalStack stack;

	/*  typespec values cannot recognize subtypep-exclude.
	 *  result is include or false.
	 */
	local = Local_Thread;
	push_local(local, &stack);
	Return(subtypep_values_call_(ptr, left, right, &value));
	rollback_local(local, stack);
	return Result(ret, value? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE);
}


/*
 *  subtypep_call
 */
int subtypep_array_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	call_type_subtypep call;

	call = TypeSubtypep[(int)RefLispDecl(right)];
	if (call)
		return (*call)(ptr, left, right, ret);
	*ret = SUBTYPEP_INVALID;
	return 0;
}

static int subtypep_leftright_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, left, right, ret, subtypep_array_);
}

static int subtypep_clos_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int not1;

	GetNotDecl(right, &not1);
	if (not1)
		return Result(ret, SUBTYPEP_INVALID);
	else
		return subtypep_leftright_(ptr, left, right, ret);
}


/* and/or reduce */
static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret);

static int subtypep_reduce_vector_(Execute ptr, addr pos, addr *value, int *ret)
{
	int result, check;
	addr src, dst, reduce;
	size_t size, i;

	GetArrayType(pos, 0, &src);
	LenArrayA4(src, &size);
	vector4_local(ptr->local, &dst, size);
	result = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &reduce);
		Return(subtypep_reduce_(ptr, reduce, &reduce, &check));
		result |= check;
		SetArrayA4(dst, i, reduce);
	}
	*value = result? dst: src;
	return Result(ret, result);
}

/* (and) -> t */
static size_t subtypep_reduce_size(addr vector)
{
	size_t size;
	LenArrayA4(vector, &size);
	return size;
}

static int subtypep_reduce_and1(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 0)
		return 0;
	GetTypeTable(value, T);
	return 1;
}

/* (and type) -> type */
static int subtypep_reduce_and2(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 1)
		return 0;
	GetArrayA4(vector, 0, value);
	return 1;
}

/* (and ... nil ...) -> nil */
static int subtypep_reduce_and3(addr vector, addr *value)
{
	addr check;
	size_t size, i;

	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == LISPDECL_NIL) {
			GetTypeTable(value, Nil);
			return 1;
		}
	}

	return 0;
}

/* (and ... t ...) -> (and ...)  remove t */
static int subtypep_reduce_remove(LocalRoot local, addr vector, addr *value,
		enum LISPDECL equal, enum LISPDECL make)
{
	int exist;
	addr check, dst;
	size_t size, count, i;

	/* length */
	LenArrayA4(vector, &size);
	count = 0;
	exist = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == equal)
			exist = 1;
		else
			count++;
	}
	if (exist == 0)
		return 0;

	/* replace */
	vector4_local(local, &dst, count);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == equal)
			continue;
		SetArrayA4(dst, count++, check);
	}
	type1_local(local, make, dst, value);

	return 1;
}

static int subtypep_reduce_and4(LocalRoot local, addr vector, addr *value)
{
	return subtypep_reduce_remove(local, vector, value, LISPDECL_T, LISPDECL_AND);
}

/* (and [exclude]) -> nil */
static int subtypep_reduce_and5_(Execute ptr, addr vector, addr *value, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	LenArrayA4(vector, &size);
	for (x = 0; x < size; x++) {
		GetArrayA4(vector, x, &left);
		for (y = x + 1; y < size; y++) {
			GetArrayA4(vector, y, &right);
			Return(subtypep_call_(ptr, left, right, 0, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				GetTypeTable(value, Nil);
				return Result(ret, 1);
			}
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and_(Execute ptr, addr pos, addr *value, int *ret)
{
	int result, check;
	addr vector;
	LocalRoot local;

	local = ptr->local;
	result = 0;
first:
	Return(subtypep_reduce_vector_(ptr, pos, &vector, &check));
	result |= check;
	if (subtypep_reduce_and1(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and2(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and3(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and4(local, vector, &pos)) {
		result = 1;
		goto first;
	}
	Return(subtypep_reduce_and5_(ptr, vector, value, &check));
	if (check)
		return Result(ret, 1);
	if (result) {
		type1_local(local, LISPDECL_AND, vector, value);
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

/* (or) -> nil */
static int subtypep_reduce_or1(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 0)
		return 0;
	GetTypeTable(value, Nil);
	return 1;
}

/* (or type) -> type */
static int subtypep_reduce_or2(addr vector, addr *value)
{
	return subtypep_reduce_and2(vector, value);
}

/* (or ... t ...) -> t */
static int subtypep_reduce_or3(addr vector, addr *value)
{
	addr check;
	size_t size, i;

	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == LISPDECL_T) {
			GetTypeTable(value, T);
			return 1;
		}
	}

	return 0;
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int subtypep_reduce_or4(LocalRoot local, addr vector, addr *value)
{
	return subtypep_reduce_remove(local, vector, value, LISPDECL_NIL, LISPDECL_OR);
}

static int subtypep_reduce_or_(Execute ptr, addr pos, addr *value, int *ret)
{
	int result, check;
	addr vector;
	LocalRoot local;

	local = ptr->local;
	result = 0;
first:
	Return(subtypep_reduce_vector_(ptr, pos, &vector, &check));
	result |= check;
	if (subtypep_reduce_or1(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or2(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or3(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or4(local, vector, &pos)) {
		result = 1;
		goto first;
	}
	if (result) {
		type1_local(local, LISPDECL_OR, vector, value);
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret)
{
	switch (RefLispDecl(pos)) {
		case LISPDECL_AND:
			return subtypep_reduce_and_(ptr, pos, value, ret);

		case LISPDECL_OR:
			return subtypep_reduce_or_(ptr, pos, value, ret);

		default:
			*value = pos;
			return Result(ret, 0);
	}
}


/* and/or */
static int subtypep_and_right_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, right, &right, &include));
	if (include)
		return subtypep_call_(ptr, left, right, 0, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	include = 1;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		Return(subtypep_call_(ptr, left, check, 0, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	if (include)
		return ReturnInclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_or_right_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, right, &right, &include));
	if (include)
		return subtypep_call_(ptr, left, right, 0, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	include = 0;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		Return(subtypep_call_(ptr, left, check, 0, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	if (exclude)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_and_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, left, &left, &include));
	if (include)
		return subtypep_call_(ptr, left, right, 0, ret);
	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	include = 0;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		Return(subtypep_call_(ptr, check, right, 0, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_or_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, left, &left, &include));
	if (include)
		return subtypep_call_(ptr, left, right, 0, ret);
	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	include = 1;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		Return(subtypep_call_(ptr, check, right, 0, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

#define LISP_DEBUG_SUBTYPEP
#undef LISP_DEBUG_SUBTYPEP

#ifdef LISP_DEBUG_SUBTYPEP
static int infotype_(addr pos)
{
	Return(type_object_(&pos, pos));
	infoprint(pos);
}
static void infosubtypep(SubtypepResult value)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			info("subtypep: include");
			break;

		case SUBTYPEP_EXCLUDE:
			info("subtypep: exclude");
			break;

		case SUBTYPEP_FALSE:
			info("subtypep: false");
			break;

		case SUBTYPEP_INVALID:
			info("subtypep: invalid");
			break;

		default:
			info("subtypep: error");
			break;
	}
}
static int subtypep_andargs_right_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_right_(ptr, left, right, &value));
	info("[and-right]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_orargs_right_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_right_(ptr, left, right, &value));
	info("[or-right]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_andargs_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_left_(ptr, left, right, &value));
	info("[and-left]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_orargs_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_left_(ptr, left, right, &value));
	info("[or-left]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
#else
#define subtypep_orargs_left_ subtypep_or_left_
#define subtypep_andargs_left_ subtypep_and_left_
#define subtypep_orargs_right_ subtypep_or_right_
#define subtypep_andargs_right_ subtypep_and_right_
#endif

static int subtypep_or_right_switch_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, left, right, ret);

		default:
			return subtypep_orargs_right_(ptr, left, right, ret);
	}
}

static int subtypep_and_right_switch_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, left, right, ret);

		default:
			return subtypep_andargs_right_(ptr, left, right, ret);
	}
}


/* left */
static int subtypep_satisfies_left_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(right) == LISPDECL_T)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_left_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	Check(GetType(left) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, left, right, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(ptr, left, right, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, left, right, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left_(left, right, ret);

		case LISPDECL_NIL:
			return ReturnInclude(ret);

		case LISPDECL_T:
			return ReturnFalse(ret);

		case LISPDECL_CLOS:
			return subtypep_clos_left_(ptr, left, right, ret);

		default:
			return subtypep_leftright_(ptr, left, right, ret);
	}
}

/* right */
static int subtypep_satisfies_right_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(left) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_nil_right_(addr left, SubtypepResult *ret)
{
	if (RefLispDecl(left) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_right_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	Check(GetType(right) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(right)) {
		case LISPDECL_AND:
			return subtypep_and_right_switch_(ptr, left, right, ret);

		case LISPDECL_OR:
			return subtypep_or_right_switch_(ptr, left, right, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(ptr, left, right, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, left, right, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right_(left, right, ret);

		case LISPDECL_NIL:
			return subtypep_nil_right_(left, ret);

		case LISPDECL_T:
			return ReturnInclude(ret);

		default:
			return subtypep_left_(ptr, left, right, ret);
	}
}


/*
 *  subtypep_clang
 */
void init_type_subtypep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeSubtypep[i] = subtypep_call_error_;

	TypeSubtypep[LISPDECL_TYPE] = subtypep_call_type_;
	TypeSubtypep[LISPDECL_CLOS] = subtypep_call_clos_;
	TypeSubtypep[LISPDECL_ASTERISK] = subtypep_call_error_;
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
	TypeSubtypep[LISPDECL_FUNCTION] = subtypep_function_;
	TypeSubtypep[LISPDECL_COMPILED_FUNCTION] = subtypep_compiled_function_;
	TypeSubtypep[LISPDECL_PATHNAME] = subtypep_call_pathname_;
	TypeSubtypep[LISPDECL_LOGICAL_PATHNAME] = subtypep_call_logical_pathname_;
	TypeSubtypep[LISPDECL_SEQUENCE] = subtypep_call_sequence_;
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
	TypeSubtypep[LISPDECL_COMPLEX] = subtypep_complex_;
	TypeSubtypep[LISPDECL_RESTART] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_ENVIRONMENT] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_STREAM] = subtypep_stream_;
	TypeSubtypep[LISPDECL_BROADCAST_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_CONCATENATED_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_ECHO_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_FILE_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_STRING_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_SYNONYM_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_TWO_WAY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_PROMPT_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_PRETTY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_MEMORY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_call_eqltype_;
	TypeSubtypep[LISPDECL_EVAL] = subtypep_call_eqltype_;
}

static int subtypep_call_asterisk_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	if (type_asterisk_p(right)) {
		if (RefNotDecl(right)) {
			*ret = SUBTYPEP_INVALID;
			return fmte_("Don't allow to use (not *).", NULL);
		}
		else {
			return ReturnInclude(ret);
		}
	}
	if (type_asterisk_p(left)) {
		if (RefNotDecl(left)) {
			*ret = SUBTYPEP_INVALID;
			return fmte_("Don't allow to use (not *).", NULL);
		}
		else {
			return ReturnFalse(ret);
		}
	}
	return subtypep_right_(ptr, left, right, ret);
}

static int subtypep_call_normal_(Execute ptr, addr left, addr right, SubtypepResult *ret)
{
	if (type_asterisk_p(left) || type_asterisk_p(right)) {
		*ret = SUBTYPEP_INVALID;
		return fmte_("Don't allow to use asterisk.", NULL);
	}
	else {
		return subtypep_right_(ptr, left, right, ret);
	}
}

static int subtypep_call_(Execute ptr, addr left, addr right,
		int aster, SubtypepResult *ret)
{
	if (aster)
		return subtypep_call_asterisk_(ptr, left, right, ret);
	else
		return subtypep_call_normal_(ptr, left, right, ret);
}

static int real_extract_subtypep_(LocalRoot local, addr *ret, addr type)
{
	type_copy_local(local, &type, type);
	Return(real_extract_local_(local, &type, type));
	get_type_subtypep(ret, type);

	return 0;
}

static int subtypep_static_(Execute ptr,
		addr left, addr right, int aster, SubtypepResult *ret)
{
	SubtypepResult result;
	LocalRoot local;
	LocalStack stack;

	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	local = Local_Thread;
	push_local(local, &stack);
	Return(real_extract_subtypep_(local, &left, left));
	Return(real_extract_subtypep_(local, &right, right));
	Return(subtypep_call_(ptr, left, right, aster, &result));
	rollback_local(local, stack);

	return Result(ret, result);
}


/*
 *  interface
 */
int subtypep_value_(Execute ptr, addr x, addr y, addr env, int as, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	Return(subtypep_static_(ptr, x, y, as, ret));
	localhold_end(hold);

	return 0;
}

static int subtypep_check_asterisk_(Execute ptr, addr x, addr y, addr env,
		int aster, int *ret, int *validp)
{
	SubtypepResult value;

	Return(subtypep_value_(ptr, x, y, env, aster, &value));
	switch (value) {
		case SUBTYPEP_INCLUDE:
			if (validp)
				*validp = 1;
			return Result(ret, 1);

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			if (validp)
				*validp = 1;
			return Result(ret, 0);

		case SUBTYPEP_INVALID:
		default:
			break;
	}
	if (validp)
		*validp = 0;
	return Result(ret, 0);
}

int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp)
{
	return subtypep_check_asterisk_(ptr, x, y, env, 1, ret, validp);
}

int subtypep_check_common_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp)
{
	return subtypep_check_asterisk_(ptr, x, y, env, 0, ret, validp);
}

static void subtypep_value_keyword(SubtypepResult value, addr *ret)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			GetConst(SYSTEM_INCLUDE, ret);
			break;

		case SUBTYPEP_EXCLUDE:
			GetConst(SYSTEM_EXCLUDE, ret);
			break;

		case SUBTYPEP_FALSE:
			GetConst(SYSTEM_FALSE, ret);
			break;

		case SUBTYPEP_INVALID:
		default:
			GetConst(SYSTEM_INVALID, ret);
			break;
	}
}

int subtypep_syscall_(Execute ptr, addr x, addr y, addr env, addr *ret)
{
	SubtypepResult value;
	Return(subtypep_value_(ptr, x, y, env, 1, &value));
	subtypep_value_keyword(value, ret);

	return 0;
}

int subtypep_table_(Execute ptr, addr x, addr y, addr env, addr *ret)
{
	SubtypepResult value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	Return(subtypep_array_(ptr, x, y, &value));
	subtypep_value_keyword(value, ret);
	localhold_end(hold);

	return 0;
}

