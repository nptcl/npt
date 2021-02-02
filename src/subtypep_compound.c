#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "equal.h"
#include "subtypep_andor.h"
#include "subtypep_compound.h"
#include "subtypep_number.h"
#include "subtypep_table.h"
#include "type.h"
#include "type_copy.h"
#include "type_table.h"
#include "type_typep.h"
#include "typedef.h"

/*
 *  subtypep_lisptype_
 */
static int subtypep_lisptype_normal_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, x, y, &value));
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_not_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(ptr, y, x, &value));  /* reverse */
	GetNotDecl(y, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, SUBTYPEP_FALSE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_(Execute ptr, addr x, addr y,
		SubtypepResult *ret, call_type_subtypep call)
{
	if (RefNotDecl(x))
		return subtypep_lisptype_not_(ptr, x, y, ret, call);
	else
		return subtypep_lisptype_normal_(ptr, x, y, ret, call);
}


/*
 *  subtypep_eql
 */
static int subtypep_eql_eql_(addr x, addr y, SubtypepResult *ret)
{
	GetArrayType(x, 0, &x);
	GetArrayType(y, 0, &y);
	if (eql_function(x, y))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_type_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(y, &y);
	if (RefLispDecl(y) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep '(eql x) y) */
	GetArrayType(x, 0, &x);
	Return(typep_table_(ptr, x, y, &check));
	if (check)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_type_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(x, &x);
	if (RefLispDecl(x) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep x '(eql x)) */
	GetArrayType(y, 0, &y);
	Return(typep_table_(ptr, y, x, &check));
	if (check)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_call_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_EQL);
	check2 = (RefLispDecl(y) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql_(x, y, ret);
	if (check1)
		return subtypep_eql_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_eql_(ptr, x, y, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_eql_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_eql_call_);
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

static int subtypep_boolean_(Execute ptr, addr x, addr y, int *ret)
{
	SubtypepResult value;
	Return(subtypep_compound_(ptr, x, y, &value));
	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_values_values_(Execute ptr, addr x, addr y, int *ret)
{
	int check;
	addr check1, check2;
	size_t size1, size2, size, i;

	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(x), "left not error");
	Check(RefNotDecl(y), "right not error");

	/* size */
	size1 = getsize_values(x);
	size2 = getsize_values(y);
	size = (size1 > size2)? size1: size2;
	size++; /* &rest check */

	/* check */
	for (i = 0; i < size; i++) {
		Return(gettype_values_(x, i, &check1));
		Return(gettype_values_(y, i, &check2));
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

static int subtypep_values_type_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(x) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(x), "left not error");
	subtypep_values_local(y, &y);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_type_values_(Execute ptr, addr x, addr y, int *ret)
{
	Check(RefLispDecl(y) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(y), "right not error");
	subtypep_values_local(x, &x);
	return subtypep_values_values_(ptr, x, y, ret);
}

static int subtypep_values_call_(Execute ptr, addr x, addr y, int *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(x) == LISPDECL_VALUES);
	check2 = (RefLispDecl(y) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values_(ptr, x, y, ret);
	if (check1)
		return subtypep_values_type_(ptr, x, y, ret);
	if (check2)
		return subtypep_type_values_(ptr, x, y, ret);
	Abort("type error");
	return Result(ret, 0);
}

static int subtypep_values_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int value;
	LocalRoot local;
	LocalStack stack;

	/*  typespec values cannot recognize subtypep-exclude.
	 *  result is include or false.
	 */
	local = Local_Thread;
	push_local(local, &stack);
	Return(subtypep_values_call_(ptr, x, y, &value));
	rollback_local(local, stack);
	return Result(ret, value? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE);
}


/*
 *  subtypep_call
 */
int subtypep_atomic_not_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_lisptype_(ptr, x, y, ret, subtypep_table_);
}

static int subtypep_clos_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int not1;

	GetNotDecl(y, &not1);
	if (not1)
		return Result(ret, SUBTYPEP_INVALID);
	else
		return subtypep_atomic_not_(ptr, x, y, ret);
}


/* left */
static int subtypep_satisfies_left_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(y) == LISPDECL_T)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(x) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(x)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, x, y, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, x, y, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left_(x, y, ret);

		case LISPDECL_NIL:
			return ReturnInclude(ret);

		case LISPDECL_T:
			return ReturnFalse(ret);

		case LISPDECL_CLOS:
			return subtypep_clos_left_(ptr, x, y, ret);

		default:
			return subtypep_atomic_not_(ptr, x, y, ret);
	}
}

/* right */
static int subtypep_satisfies_right_(addr x, addr y, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_nil_right_(addr x, SubtypepResult *ret)
{
	if (RefLispDecl(x) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	Check(GetType(y) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(y)) {
		case LISPDECL_AND:
			return subtypep_and_right_switch_(ptr, x, y, ret);

		case LISPDECL_OR:
			return subtypep_or_right_switch_(ptr, x, y, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(ptr, x, y, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(ptr, x, y, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right_(x, y, ret);

		case LISPDECL_NIL:
			return subtypep_nil_right_(x, ret);

		case LISPDECL_T:
			return ReturnInclude(ret);

		default:
			return subtypep_left_(ptr, x, y, ret);
	}
}


/*
 *  subtypep_clang
 */
int subtypep_compound_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_right_(ptr, x, y, ret);
}

static int real_extract_subtypep_(LocalRoot local, addr *ret, addr type)
{
	type_copy_local(local, &type, type);
	Return(real_extract_local_(local, &type, type));
	get_type_subtypep(ret, type);

	return 0;
}

int subtypep_force_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult result;
	LocalRoot local;
	LocalStack stack;

	CheckType(x, LISPTYPE_TYPE);
	CheckType(y, LISPTYPE_TYPE);
	local = Local_Thread;
	push_local(local, &stack);
	Return(real_extract_subtypep_(local, &x, x));
	Return(real_extract_subtypep_(local, &y, y));
	Return(subtypep_compound_(ptr, x, y, &result));
	rollback_local(local, stack);

	return Result(ret, result);
}

int subtypep_normal_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	return subtypep_force_(ptr, x, y, ret);
}

