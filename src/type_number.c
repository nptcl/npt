#include "cons.h"
#include "sequence.h"
#include "type.h"
#include "type_copy.h"
#include "type_number.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_range.h"
#include "type_table.h"

/*
 *  real_filter
 */
static void type_range_left(LocalRoot local,
		addr *ret, enum LISPDECL type, addr left1, addr left2)
{
	addr aster;
	GetTypeTable(&aster, Asterisk);
	type4_local(local, type, left1, left2, aster, aster, ret);
}

static void type_range_right(LocalRoot local,
		addr *ret, enum LISPDECL type, addr right1, addr right2)
{
	addr aster;
	GetTypeTable(&aster, Asterisk);
	type4_local(local, type, aster, aster, right1, right2, ret);
}

static void type_range_not(LocalRoot local, addr *ret, enum LISPDECL type,
		addr left1, addr left2, addr right1, addr right2)
{
	addr pos;

	vector4_local(local, &pos, 2);
	type_range_left(local, &right1, type, (right1 == Nil)? T: Nil, right2);
	type_range_right(local, &left1, type, (left1 == Nil)? T: Nil, left2);
	SetArrayA4(pos, 0, right1);
	SetArrayA4(pos, 1, left1);
	type1_local(local, LISPDECL_OR, pos, ret);
}

static void real_filter_not_range(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	int aster1, aster2;
	addr left1, left2, right1, right2;

	GetArrayType(type, 0, &left1);
	GetArrayType(type, 2, &right1);
	aster1 = type_asterisk_p(left1);
	aster2 = type_asterisk_p(right1);
	if (aster1 && aster2) {
		*ret = Nil;
		return;
	}

	GetArrayType(type, 3, &right2);
	if (aster1) {
		type_range_left(local, ret, decl, (right1 == Nil)? T: Nil, right2);
		return;
	}

	GetArrayType(type, 1, &left2);
	if (aster2) {
		type_range_right(local, ret, decl, (left1 == Nil)? T: Nil, left2);
		return;
	}

	type_range_not(local, ret, decl, left1, left2, right1, right2);
}

static void real_filter_not(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER) {
		*ret = Nil;
		return;
	}
	if (decl_subtypep_real(decl, right)) {
		real_filter_not_range(local, ret, type, decl);
		return;
	}
	type4aster_local(local, decl, ret);
}

static void real_filter_normal(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;
	addr left1, left2, right1, right2;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER) {
		type4aster_local(local, decl, ret);
		return;
	}
	if (decl_subtypep_real(decl, right)) {
		GetArrayType(type, 0, &left1);
		GetArrayType(type, 1, &left2);
		GetArrayType(type, 2, &right1);
		GetArrayType(type, 3, &right2);
		type4_local(local, decl, left1, left2, right1, right2, ret);
		return;
	}
	*ret = Nil;
}

static void real_filter_type(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	if (RefNotDecl(type))
		real_filter_not(local, ret, type, decl);
	else
		real_filter_normal(local, ret, type, decl);
}

static void vector4_andor(LocalRoot local,
		addr *ret, addr src, size_t size, enum LISPDECL decl)
{
	addr dst, pos;
	size_t i;

	if (size == 0) {
		*ret = Nil;
		return;
	}
	vector4_local(local, &dst, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &pos);
		SetArrayA4(dst, i, pos);
	}
	type1_local(local, decl, dst, ret);
}

static void real_filter(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static void real_filter_and(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		real_filter(local, &pos, pos, decl);
		if (pos == Nil) break;
		SetArrayA4(temp, count++, pos);
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_AND);
}

static void real_filter_or(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		real_filter(local, &pos, pos, decl);
		if (pos != Nil) {
			SetArrayA4(temp, count++, pos);
		}
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_OR);
}

static void real_filter(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			real_filter_and(local, ret, type, decl);
			break;

		case LISPDECL_OR:
			real_filter_or(local, ret, type, decl);
			break;

		default:
			real_filter_type(local, ret, type, decl);
			break;
	}
}


/*
 *  merge_range
 */
static void merge_range_cons(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Check(RefLispDecl(type) != decl, "type error");
	Check(RefNotDecl(type), "not error");
	conscar_local(local, ret, type);
}

/* merge-range-and */
static void make_range_left_right(LocalRoot local, addr *ret, addr left, addr right)
{
	addr left1, left2, right1, right2;

	range_left_value(left, &left1, &left2);
	range_right_value(right, &right1, &right2);
	type4_local(local, RefLispDecl(left), left1, left2, right1, right2, ret);
}

static void make_range_left_aster(LocalRoot local, addr *ret, addr left)
{
	addr left1, left2;

	range_left_value(left, &left1, &left2);
	type_range_left(local, ret, RefLispDecl(left), left1, left2);
}

static void make_range_aster_right(LocalRoot local, addr *ret, addr right)
{
	addr right1, right2;

	range_right_value(right, &right1, &right2);
	type_range_right(local, ret, RefLispDecl(right), right1, right2);
}

/* (10 *) (20 *) */
static void range_and_left_left(addr *ret, addr left, addr right)
{
	*ret = range_left_left_less(left, right)? right: left;
}

/* (* 20) (10 *) */
static void range_and_right_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_right_left_greater_equal(left, right))
		make_range_left_right(local, ret, right, left);
	else
		*ret = Nil;
}

/* (10 20) (15 *) */
static void range_and_between_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_left_left_less_equal(right, left)) {
		*ret = left;
		return;
	}
	if (range_left_right_less_equal(right, left)) {
		make_range_left_right(local, ret, right, left);
		return;
	}
	*ret = Nil;
}

static void range_and_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left)) {
		*ret = right;
		return;
	}
	if (range_left_p(left)) {
		range_and_left_left(ret, left, right);
		return;
	}
	if (range_right_p(left)) {
		range_and_right_left(local, ret, left, right);
		return;
	}
	if (range_between_p(left)) {
		range_and_between_left(local, ret, left, right);
		return;
	}
	Abort("type error");
}

/* (10 *) (* 20) */
static void range_and_left_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_left_right_less_equal(left, right))
		make_range_left_right(local, ret, left, right);
	else
		*ret = Nil;
}

/* (* 10) (* 20) */
static void range_and_right_right(addr *ret, addr left, addr right)
{
	*ret = range_right_right_less(left, right)? left: right;
}

/* (10 30) (* 20) */
static void range_and_between_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_right_right_less_equal(left, right)) {
		*ret = left;
		return;
	}
	if (range_left_right_less_equal(left, right)) {
		make_range_left_right(local, ret, left, right);
		return;
	}
	*ret = Nil;
}

static void range_and_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left)) {
		*ret = right;
		return;
	}
	if (range_left_p(left)) {
		range_and_left_right(local, ret, left, right);
		return;
	}
	if (range_right_p(left)) {
		range_and_right_right(ret, left, right);
		return;
	}
	if (range_between_p(left)) {
		range_and_between_right(local, ret, left, right);
		return;
	}
	Abort("type error");
}

/* (10 40) (20 30) */
static void range_and_between_between(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_in(left, right)) {
		*ret = right;
		return;
	}
	if (range_between_in(right, left)) {
		*ret = left;
		return;
	}
	if (range_between_left(left, right)) {
		make_range_left_right(local, ret, right, left);
		return;
	}
	if (range_between_right(left, right)) {
		make_range_left_right(local, ret, left, right);
		return;
	}
	*ret = Nil;
}

static void range_and_between(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left)) {
		*ret = right;
		return;
	}
	if (range_left_p(left)) {
		range_and_between_left(local, ret, right, left);
		return;
	}
	if (range_right_p(left)) {
		range_and_between_right(local, ret, right, left);
		return;
	}
	if (range_between_p(left)) {
		range_and_between_between(local, ret, left, right);
		return;
	}
	Abort("type error");
}

static void range_and(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(right)) {
		*ret = left;
		return;
	}
	if (range_left_p(right)) {
		range_and_left(local, ret, left, right);
		return;
	}
	if (range_right_p(right)) {
		range_and_right(local, ret, left, right);
		return;
	}
	if (range_between_p(right)) {
		range_and_between(local, ret, left, right);
		return;
	}
	Abort("type error");
}

static void map_range_and(LocalRoot local, addr *ret, addr list, addr right)
{
	addr result, left;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &left, &list);
		range_and(local, &left, left, right);
		if (left != Nil)
			cons_local(local, &result, left, result);
	}
	nreverse_list_unsafe(ret, result);
}

static void merge_range_andplus(LocalRoot local, addr *ret, addr left, addr right)
{
	addr type;

	while (right != Nil) {
		GetCons(right, &type, &right);
		map_range_and(local, &left, left, type);
		if (left == Nil) break;
	}
	*ret = left;
}

static void merge_range_type(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static void range_and_otherwise(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t i, size;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		merge_range_type(local, &right, right, decl);
		if (right == Nil) {
			left = Nil;
			break;
		}
		else if (right == T) {
			continue;
		}
		else if (left == Nil) {
			left = right;
		}
		else {
			merge_range_andplus(local, &left, left, right);
			if (left == Nil) break;
		}
	}
	*ret = left;
}

static void merge_range_and(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0) {
		*ret = T;
		return;
	}
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return;
	}
	range_and_otherwise(local, ret, type, decl);
}

/* merge-range-or */
typedef int (*extpairtype)(LocalRoot, addr *, addr, addr);
static int extpaircall_right(LocalRoot local,
		extpairtype call, addr *ret, addr left, addr cons)
{
	int check;
	addr right;

	while (cons != Nil) {
		GetCons(cons, &right, &cons);
		if (left != right) {
			check = call(local, &right, left, right);
			if (check) {
				*ret = right;
				return check;
			}
		}
	}

	return 0;
}

static void pushlist(LocalRoot local, addr *ret, addr list, addr result)
{
	addr one;

	while (list != Nil) {
		GetCons(list, &one, &list);
		cons_local(local, &result, one, result);
	}
	*ret = result;
}

static int extpaircall_left(LocalRoot local, extpairtype call, addr *ret, addr right)
{
	int update, check;
	addr left, result, value, cons;

	result = Nil;
	update = 0;
	for (cons = right; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		if (update) {
			cons_local(local, &result, left, result);
		}
		else {
			check = extpaircall_right(local, call, &value, left, right);
			if (check < 0)
				pushlist(local, &result, value, result);
			else
				cons_local(local, &result, check? value: left, result);
			if (check)
				update = 1;
		}
	}
	if (update)
		nreverse_list_unsafe(ret, result);
	return update;
}

static void extpaircall(LocalRoot local, extpairtype call, addr *cons, int *update)
{
	int check;
	addr value;

	value = *cons;
	check = 0;
	while (extpaircall_left(local, call, &value, value)) {
		check = 1;
	}
	if (check) {
		*cons = value;
		*update = 1;
	}
}

/* check only */
static int range_or_check(LocalRoot local, addr *ret, addr left, addr right)
{
	Check(RefLispDecl(left) != RefLispDecl(right), "type error");
	Check(RefNotDecl(left) || RefNotDecl(right), "not error");
	return 0;
}

/* (? ?) (* *) -> delete */
static int range_or_aster(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(right)) {
		*ret = Nil;
		return -1;  /* list */
	}
	return 0;
}

/* (20 ?) (10 *) -> delete */
static int range_or_left_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_left_any_p(left) &&
			range_left_p(right) &&
			range_left_left_less_equal(right, left)) {
		*ret = Nil;
		return -1;
	}
	return 0;
}

/* (? 10) (* 20) -> delete */
static int range_or_right_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_any_right_p(left) &&
			range_right_p(right) &&
			range_right_right_less_equal(left, right)) {
		*ret = Nil;
		return -1;
	}
	return 0;
}

/* (10 *) (* 20) -> (10 20) */
static int range_or_left_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_left_p(left) &&
			range_right_p(right) &&
			range_connect_right_left(right, left)) {
		type4aster_local(local, RefLispDecl(left), ret);
		return 1;
	}
	return 0;
}

/* (10 30) (20 *) -> (10 *) */
static int range_or_range_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_p(left) &&
			range_left_p(right) &&
			range_connect_between_left(left, right)) {
		make_range_left_aster(local, ret, left);
		return 1;
	}
	return 0;
}

/* (10 30) (* 20) -> (* 30) */
static int range_or_range_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_p(left) &&
			range_right_p(right) &&
			range_connect_between_right(left, right)) {
		make_range_aster_right(local, ret, left);
		return 1;
	}
	return 0;
}

/* (21 22) (10 30) -> delete */
static int range_or_range_range_in(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_p(left) &&
			range_between_p(right) &&
			range_between_in(right, left)) {
		*ret = Nil;
		return -1;
	}
	return 0;
}

/* (10 30) (20 40) -> (10 30) */
static int range_or_range_range_left(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_p(left) &&
			range_between_p(right) &&
			range_between_left(left, right) &&
			(! range_between_right(left, right))) {
		make_range_left_right(local, ret, left, right);
		return 1;
	}
	return 0;
}

/* (20 40) (10 30) -> (10 40) */
static int range_or_range_range_right(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_between_p(left) &&
			range_between_p(right) &&
			(! range_between_left(left, right)) &&
			range_between_right(left, right)) {
		make_range_left_right(local, ret, right, left);
		return 1;
	}
	return 0;
}

static void merge_range_orplus(LocalRoot local, addr *ret, addr left, addr right)
{
	int update, result;

	append_cons_local_unsafe(local, &left, left, right);
	for (result = 0; ; result |= update) {
		update = 0;
		extpaircall(local, range_or_check, &left, &update);
		extpaircall(local, range_or_aster, &left, &update);
		extpaircall(local, range_or_left_left, &left, &update);
		extpaircall(local, range_or_right_right, &left, &update);
		extpaircall(local, range_or_left_right, &left, &update);
		extpaircall(local, range_or_range_left, &left, &update);
		extpaircall(local, range_or_range_right, &left, &update);
		extpaircall(local, range_or_range_range_in, &left, &update);
		extpaircall(local, range_or_range_range_left, &left, &update);
		extpaircall(local, range_or_range_range_right, &left, &update);
		if (update == 0) break;                                                               }
	*ret = left;
}

static void range_or_otherwise(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t size, i;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		merge_range_type(local, &right, right, decl);
		if (right == Nil) {
			continue;
		}
		if (right == T) {
			left = T;
			break;
		}
		if (left == Nil) {
			left = right;
		}
		else {
			merge_range_orplus(local, &left, left, right);
			if (left == T) break;
		}
	}
	*ret = left;
}

static void merge_range_or(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0) {
		*ret = Nil;
		return;
	}
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return;
	}
	range_or_otherwise(local, ret, type, decl);
}

static void merge_range_type(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			merge_range_and(local, ret, type, decl);
			break;

		case LISPDECL_OR:
			merge_range_or(local, ret, type, decl);
			break;

		default:
			merge_range_cons(local, ret, type, decl);
			break;
	}
}

static void type_or_cons(LocalRoot local, addr *ret, addr cons)
{
	addr array, pos;
	size_t i, size;

	array = cons;
	for (size = 0; array != Nil; size++) {
		GetCdr(array, &array);
	}
	vector4_local(local, &array, size);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static void make_merge_range(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	merge_range_type(local, &type, type, decl);
	if (type == Nil) {
		*ret = Nil;
		return;
	}
	if (type == T) {
		type4aster_local(local, decl, ret);
		return;
	}
	if (singlep(type)) {
		GetCar(type, ret);
		return;
	}
	if (GetType(type) == LISPTYPE_CONS) {
		type_or_cons(local, ret, type);
		return;
	}
	Abort("type error");
}

static void merge_range(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	real_filter(local, &type, type, decl);
	if (type != Nil) {
		type_optimize_local(local, &type, type);
		get_type_optimized(&type, type);
	}
	if ((type == Nil) || (RefLispDecl(type) == LISPDECL_NIL))
		*ret = Nil;
	else
		make_merge_range(local, ret, type, decl);
}


/*
 *  real_extract
 */
static const enum LISPDECL RealFilterDecls[] = {
	LISPDECL_INTEGER,
	LISPDECL_RATIONAL,
	LISPDECL_REAL,
	LISPDECL_FLOAT,
	LISPDECL_SHORT_FLOAT,
	LISPDECL_SINGLE_FLOAT,
	LISPDECL_DOUBLE_FLOAT,
	LISPDECL_LONG_FLOAT,
	LISPDECL_EMPTY
};

static size_t real_filter_range_list(LocalRoot local, addr *ret, addr type)
{
	addr cons, check;
	size_t i, size;
	enum LISPDECL decl;

	cons = Nil;
	for (i = size = 0; ; i++) {
		decl = RealFilterDecls[i];
		if (decl == LISPDECL_EMPTY) break;
		merge_range(local, &check, type, decl);
		if (check != Nil) {
			cons_local(local, &cons, check, cons);
			size++;
		}
	}
	nreverse_list_unsafe(ret, cons);

	return size;
}

static void real_reject(LocalRoot local, addr *ret, addr type)
{
	addr pos;

	/* (and (not real) [type]) */
	vector4_local(local, &pos, 2);
	SetArrayA4(pos, 1, type);
	type4aster_local(local, LISPDECL_REAL, &type);
	SetNotDecl(type, 1);
	SetArrayA4(pos, 0, type);
	type1_local(local, LISPDECL_AND, pos, ret);
}

static void copy_cons_to_vector4_local(LocalRoot local,
		addr *ret, addr cons, size_t size)
{
	addr array, pos;
	size_t i;

	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}

static void make_real_filter(LocalRoot local, addr *ret, addr type)
{
	addr pos;
	size_t size;

	size = real_filter_range_list(local, &pos, type);
	if (size == 0) {
		*ret = type;
	}
	else {
		real_reject(local, &type, type);
		cons_local(local, &pos, type, pos);
		copy_cons_to_vector4_local(local, &pos, pos, size + 1UL);
		type1_local(local, LISPDECL_OR, pos, ret);
	}
}

static void real_extract(LocalRoot local, addr *ret, addr type)
{
	type_optimize_local(local, &type, type);
	get_type_optimized(&type, type);
	make_real_filter(local, &type, type);
	type_optimize_local(local, &type, type);
	get_type_optimized(ret, type);
}

_g void real_extract_local(LocalRoot local, addr *ret, addr type)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_SUBTYPEP) {
		*ret = type;
	}
	else {
		real_extract(local, &type, type);
		type1_local(local, LISPDECL_SUBTYPEP, type, ret);
	}
}

_g void real_extract_heap(LocalRoot local, addr *ret, addr type)
{
	LocalStack stack;

	CheckLocal(local);
	push_local(local, &stack);
	real_extract_local(local, &type, type);
	type_copy_heap(ret, type);
	rollback_local(local, stack);
}

_g int type_subtypep_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_SUBTYPEP;
}

_g int type_optimized_or_subtypep(addr type)
{
	return type_optimized_p(type) || type_subtypep_p(type);
}

_g void get_type_subtypep(addr *ret, addr type)
{
	if (type_subtypep_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

