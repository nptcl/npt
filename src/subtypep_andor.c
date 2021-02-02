#include "subtypep_andor.h"
#include "subtypep_compound.h"
#include "type.h"
#include "type_table.h"
#include "typedef.h"

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
			Return(subtypep_compound_(ptr, left, right, &check));
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
int subtypep_and_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, y, &y, &include));
	if (include)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);
	LenArrayA4(y, &size);
	include = 1;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &check);
		Return(subtypep_compound_(ptr, x, check, &result));
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

int subtypep_or_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, y, &y, &include));
	if (include)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(y, 0, &y);
	LenArrayA4(y, &size);
	include = 0;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(y, i, &check);
		Return(subtypep_compound_(ptr, x, check, &result));
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

int subtypep_and_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, x, &x, &include));
	if (include)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);
	LenArrayA4(x, &size);
	include = 0;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &check);
		Return(subtypep_compound_(ptr, check, y, &result));
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

int subtypep_or_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(ptr, x, &x, &include));
	if (include)
		return subtypep_compound_(ptr, x, y, ret);
	GetArrayType(x, 0, &x);
	LenArrayA4(x, &size);
	include = 1;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(x, i, &check);
		Return(subtypep_compound_(ptr, check, y, &result));
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

#ifdef LISP_DEBUG_SUBTYPEP
static int subtypep_debug_infotype_(addr pos)
{
	Return(type_object_(&pos, pos));
	infoprint(pos);
}
static void subtypep_debug_infosubtypep(SubtypepResult value)
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

int subtypep_andargs_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_right_(ptr, x, y, &value));
	info("[and-right]");
	Return(subtypep_debug_infotype_(x));
	Return(subtypep_debug_infotype_(y));
	subtypep_debug_infosubtypep(value);
	return Result(ret, value);
}

int subtypep_orargs_right_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_right_(ptr, x, y, &value));
	info("[or-right]");
	Return(subtypep_debug_infotype_(x));
	Return(subtypep_debug_infotype_(y));
	subtypep_debug_infosubtypep(value);
	return Result(ret, value);
}

int subtypep_andargs_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_left_(ptr, x, y, &value));
	info("[and-left]");
	Return(subtypep_debug_infotype_(x));
	Return(subtypep_debug_infotype_(y));
	subtypep_debug_infosubtypep(value);
	return Result(ret, value);
}

int subtypep_orargs_left_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_left_(ptr, x, y, &value));
	info("[or-left]");
	Return(subtypep_debug_infotype_(x));
	Return(subtypep_debug_infotype_(y));
	subtypep_debug_infosubtypep(value);
	return Result(ret, value);
}
#endif

int subtypep_or_right_switch_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, x, y, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, x, y, ret);

		default:
			return subtypep_orargs_right_(ptr, x, y, ret);
	}
}

int subtypep_and_right_switch_(Execute ptr, addr x, addr y, SubtypepResult *ret)
{
	switch (RefLispDecl(x)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(ptr, x, y, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(ptr, x, y, ret);

		default:
			return subtypep_andargs_right_(ptr, x, y, ret);
	}
}

