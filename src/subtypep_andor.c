#include "subtypep_andor.h"
#include "subtypep_compound.h"
#include "type.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  reduce
 */
#define Return_subtypep_reduce_p(ptr, pos, ret, call) { \
	int __check; \
	Return(call(ptr, pos, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

#define Return_subtypep_reduce(ptr, pos, value, ret, call) { \
	int __check; \
	Return(call(ptr, pos, value, &__check)); \
	if (__check) { \
		return Result(ret, 1); \
	} \
}

static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret);
static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret);


/* (and ...) */
static int subtypep_reduce_vector_all_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_p_(ptr, vect, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_vector_all_(Execute ptr,
		addr pos, addr *value, int *ret, enum LISPDECL make)
{
	int check;
	addr dst, vect;
	size_t size, i;
	LocalRoot local;

	local = ptr->local;
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);

	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		Return(subtypep_reduce_(ptr, vect, &vect, &check));
		SetArrayA4(dst, i, vect);
	}

	/* make */
	type1_local(local, make, dst, value);
	return Result(ret, 1);
}

static int subtypep_reduce_and_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_AND);
}


/* (and) -> t */
static int subtypep_reduce_and1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_and1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (and type) -> type */
static int subtypep_reduce_and2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_and2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (and ... nil ...) -> nil */
static int subtypep_reduce_find_p_(Execute ptr, addr pos,
		enum LISPDECL type, enum LISPDECL find, int *ret)
{
	addr check;
	size_t size, i;

	if (RefLispDecl(pos) != type)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == find)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_NIL, ret);
}

static int subtypep_reduce_and3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (and ... t ...) -> (and ...)  remove t */
static int subtypep_reduce_and4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_AND, LISPDECL_T, ret);
}

static int subtypep_reduce_remove_(Execute ptr, addr pos, addr *value, int *ret,
		enum LISPDECL equal, enum LISPDECL make)
{
	int exist;
	addr check, dst;
	size_t size, count, i;
	LocalRoot local;

	/* length */
	LenArrayA4(pos, &size);
	count = 0;
	exist = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			exist = 1;
		else
			count++;
	}
	if (exist == 0)
		return Result(ret, 0);

	/* replace */
	local = ptr->local;
	vector4_local(local, &dst, count);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &check);
		if (RefLispDecl(check) == equal)
			continue;
		SetArrayA4(dst, count++, check);
	}
	type1_local(local, make, dst, value);

	return Result(ret, 1);
}

static int subtypep_reduce_and4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_T, LISPDECL_AND);
}


/* (and [exclude]) -> nil */
static int subtypep_reduce_and5_p_(Execute ptr, addr pos, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (x = 0; x < size; x++) {
		GetArrayA4(pos, x, &left);
		for (y = x + 1; y < size; y++) {
			GetArrayA4(pos, y, &right);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and5_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and5_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/*  (and ... (or ...)) */
static int subtypep_reduce_andor_p_(Execute ptr, addr pos, size_t index, int *ret)
{
	SubtypepResult check;
	addr vector, left, right;
	size_t size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and6_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr vect;
	size_t size, i;

	if (RefLispDecl(pos) != LISPDECL_AND)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &vect);
		if (RefLispDecl(vect) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_p_(ptr, pos, i, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_andor_size_(Execute ptr,
		addr pos, size_t index, size_t *rsize, int *ret)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove)
			count++;
	}

	*rsize = count;
	return Result(ret, size1 != count);
}

static int subtypep_reduce_andor_set_(Execute ptr,
		addr pos, size_t index, addr dst, size_t size)
{
	int remove;
	SubtypepResult check;
	addr vector, left, right;
	size_t count, size1, size2, x, y;

	GetArrayA4(pos, index, &vector);
	GetArrayType(vector, 0, &vector);
	LenArrayA4(vector, &size1);
	LenArrayA4(pos, &size2);

	count = 0;
	for (x = 0; x < size1; x++) {
		GetArrayA4(vector, x, &right);
		remove = 0;
		for (y = 0; y < size2; y++) {
			if (y == index)
				continue;
			GetArrayA4(pos, y, &left);
			Return(subtypep_compound_(ptr, left, right, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				remove = 1;
				break;
			}
		}
		if (! remove) {
			SetArrayA4(dst, count, right);
			count++;
		}
	}

	return 0;
}

static int subtypep_reduce_andor_or_(Execute ptr,
		addr pos, size_t index, addr *value, int *ret)
{
	int check;
	addr dst;
	size_t size;
	LocalRoot local;

	Return(subtypep_reduce_andor_size_(ptr, pos, index, &size, &check));
	if (! check)
		return Result(ret, 0);

	/* value */
	local = ptr->local;
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_OR, dst, value);
	Return(subtypep_reduce_andor_set_(ptr, pos, index, dst, size));

	return Result(ret, 1);
}

static int subtypep_reduce_andor_(Execute ptr, addr pos, addr *value)
{
	int check;
	addr dst, x, y;
	size_t size, i;
	LocalRoot local;

	/* value */
	local = ptr->local;
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	vector4_local(local, &dst, size);
	type1_local(local, LISPDECL_AND, dst, value);

	/* remove */
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &x);
		if (RefLispDecl(x) == LISPDECL_OR) {
			Return(subtypep_reduce_andor_or_(ptr, pos, i, &y, &check));
			if (check)
				x = y;
		}
		SetArrayA4(dst, i, x);
	}

	return 0;
}

static int subtypep_reduce_and6_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_and6_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	*ret = 1;
	return subtypep_reduce_andor_(ptr, pos, value);
}


/* and */
static int subtypep_reduce_and_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and4_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and5_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and6_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_and_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and4_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and5_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_and6_);

	return Result(ret, 0);
}


/* (or ...) */
static int subtypep_reduce_or_all_p_(Execute ptr, addr pos, int *ret)
{
	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_p_(ptr, pos, ret);
}

static int subtypep_reduce_or_all_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or_all_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_vector_all_(ptr, pos, value, ret, LISPDECL_OR);
}

/* (or) -> nil */
static int subtypep_reduce_or1_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 0);
}

static int subtypep_reduce_or1_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or1_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, Nil);
	return Result(ret, 1);
}


/* (or type) -> type */
static int subtypep_reduce_or2_p_(Execute ptr, addr pos, int *ret)
{
	size_t size;

	if (RefLispDecl(pos) != LISPDECL_OR)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);
	return Result(ret, size == 1);
}

static int subtypep_reduce_or2_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or2_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	GetArrayA4(pos, 0, value);
	return Result(ret, 1);
}


/* (or ... t ...) -> t */
static int subtypep_reduce_or3_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_T, ret);
}

static int subtypep_reduce_or3_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or3_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetTypeTable(value, T);
	return Result(ret, 1);
}


/* (or ... nil ...) -> (or ...)  remove nil */
static int subtypep_reduce_or4_p_(Execute ptr, addr pos, int *ret)
{
	return subtypep_reduce_find_p_(ptr, pos, LISPDECL_OR, LISPDECL_NIL, ret);
}

static int subtypep_reduce_or4_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;

	Return(subtypep_reduce_or4_p_(ptr, pos, &check));
	if (! check)
		return Result(ret, 0);

	GetArrayType(pos, 0, &pos);
	return subtypep_reduce_remove_(ptr, pos, value, ret, LISPDECL_NIL, LISPDECL_OR);
}


/* or */
static int subtypep_reduce_or_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_all_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or1_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or2_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or3_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or4_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_or_(Execute ptr, addr pos, addr *value, int *ret)
{
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or_all_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or1_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or2_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or3_);
	Return_subtypep_reduce(ptr, pos, value, ret, subtypep_reduce_or4_);

	return Result(ret, 0);
}


/* reduce interface */
static int subtypep_reduce_p_(Execute ptr, addr pos, int *ret)
{
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_and_p_);
	Return_subtypep_reduce_p(ptr, pos, ret, subtypep_reduce_or_p_);

	return Result(ret, 0);
}

static int subtypep_reduce_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check, loop, update;
	addr x;

	update = 0;
start:
	loop = 0;

	/* and */
	Return(subtypep_reduce_and_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* or */
	Return(subtypep_reduce_or_(ptr, pos, &x, &check));
	if (check) {
		loop = 1;
		pos = x;
	}

	/* result */
	if (loop) {
		update = 1;
		goto start;
	}
	*value = pos;
	return Result(ret, update);
}


/*
 *  and/or
 */
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
#include "type_object.h"

static int subtypep_debug_infotype_(addr pos)
{
	Return(type_object_(&pos, pos));
	infoprint(pos);
	return 0;
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

