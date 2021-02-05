#include "bignum.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "float_object.h"
#include "format.h"
#include "sequence.h"
#include "subtypep_number.h"
#include "subtypep_range.h"
#include "subtypep_optimize.h"
#include "ratio.h"
#include "real_truncate.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_table.h"

/*
 *  coerce
 */
static int real_extract_integer_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			return Result(ret, pos);

		case LISPTYPE_RATIO:
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_rational_local_(LocalRoot local, addr pos, addr *ret)
{
	addr rem;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
		case LISPTYPE_RATIO:
			return Result(ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return truncate1_common_(local, ret, &rem, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_single_local_(LocalRoot local, addr pos, addr *ret)
{
	single_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_ds_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ls_value_(pos, &value));
			single_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_double_local_(LocalRoot local, addr pos, addr *ret)
{
	double_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			double_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return double_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return double_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sd_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			return Result(ret, pos);

		case LISPTYPE_LONG_FLOAT:
			Return(cast_ld_value_(pos, &value));
			double_float_local(local, ret, value);
			return 0;

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_long_local_(LocalRoot local, addr pos, addr *ret)
{
	long_float value;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			long_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return long_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return long_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
			Return(cast_sl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_DOUBLE_FLOAT:
			Return(cast_dl_value_(pos, &value));
			long_float_local(local, ret, value);
			return 0;

		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_float_local_(LocalRoot local, addr pos, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			single_float_fixnum_local(local, ret, pos);
			return 0;

		case LISPTYPE_BIGNUM:
			return single_float_bignum_local_(local, ret, pos);

		case LISPTYPE_RATIO:
			return single_float_ratio_local_(local, ret, pos);

		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type ~S.", pos, NULL);
	}
}

static int real_extract_lispdecl_local_(LocalRoot local,
		enum LISPDECL type, addr pos, addr *ret)
{
	if (type_asterisk_p(pos))
		return Result(ret, pos);

	switch (type) {
		case LISPDECL_INTEGER:
			return real_extract_integer_local_(local, pos, ret);

		case LISPDECL_RATIONAL:
			return real_extract_rational_local_(local, pos, ret);

		case LISPDECL_SHORT_FLOAT:
		case LISPDECL_SINGLE_FLOAT:
			return real_extract_single_local_(local, pos, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return real_extract_double_local_(local, pos, ret);

		case LISPDECL_LONG_FLOAT:
			return real_extract_long_local_(local, pos, ret);

		case LISPDECL_FLOAT:
			return real_extract_float_local_(local, pos, ret);

		case LISPDECL_REAL:
			return Result(ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid real type.", NULL);
	}
}


/*
 *  real_filter
 */
static int type_range_left_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr left1, addr left2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, left2, &left2));
	type4_local(local, type, left1, left2, aster, aster, ret);

	return 0;
}

static int type_range_right_(LocalRoot local,
		addr *ret, enum LISPDECL type, addr right1, addr right2)
{
	addr aster;

	GetTypeTable(&aster, Asterisk);
	Return(real_extract_lispdecl_local_(local, type, right2, &right2));
	type4_local(local, type, aster, aster, right1, right2, ret);

	return 0;
}

static int type_range_not_(LocalRoot local, addr *ret, enum LISPDECL type,
		addr left1, addr left2, addr right1, addr right2)
{
	addr pos;

	vector4_local(local, &pos, 2);
	Return(type_range_left_(local, &right1, type, (right1 == Nil)? T: Nil, right2));
	Return(type_range_right_(local, &left1, type, (left1 == Nil)? T: Nil, left2));
	SetArrayA4(pos, 0, right1);
	SetArrayA4(pos, 1, left1);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_filter_not_range_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	int aster1, aster2;
	addr left1, left2, right1, right2;

	GetArrayType(type, 0, &left1);
	GetArrayType(type, 2, &right1);
	aster1 = type_asterisk_p(left1);
	aster2 = type_asterisk_p(right1);
	if (aster1 && aster2)
		return Result(ret, Nil);

	GetArrayType(type, 3, &right2);
	if (aster1)
		return type_range_left_(local, ret, decl, (right1 == Nil)? T: Nil, right2);

	GetArrayType(type, 1, &left2);
	if (aster2)
		return type_range_right_(local, ret, decl, (left1 == Nil)? T: Nil, left2);

	return type_range_not_(local, ret, decl, left1, left2, right1, right2);
}

static int real_filter_not_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER)
		return Result(ret, Nil);
	if (decl_subtypep_real(decl, right))
		return real_filter_not_range_(local, ret, type, decl);
	type4aster_local(local, decl, ret);

	return 0;
}

static int real_filter_normal_(LocalRoot local,
		addr *ret, addr type, enum LISPDECL decl)
{
	enum LISPDECL right;
	addr left1, left2, right1, right2;

	GetLispDecl(type, &right);
	if (right == LISPDECL_NUMBER) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (decl_subtypep_real(decl, right)) {
		GetArrayType(type, 0, &left1);
		GetArrayType(type, 1, &left2);
		GetArrayType(type, 2, &right1);
		GetArrayType(type, 3, &right2);
		Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
		Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
		type4_local(local, decl, left1, left2, right1, right2, ret);
		return 0;
	}

	return Result(ret, Nil);
}

static int real_filter_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	if (RefNotDecl(type))
		return real_filter_not_(local, ret, type, decl);
	else
		return real_filter_normal_(local, ret, type, decl);
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

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int real_filter_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos == Nil)
			return Result(ret, Nil);
		SetArrayA4(temp, count++, pos);
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_AND);
	return 0;
}

static int real_filter_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	addr temp, pos;
	size_t i, size, count;

	/* copy temporary */
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	vector4_local(local, &temp, size);
	for (count = i = 0; i < size; i++) {
		GetArrayA4(type, i, &pos);
		Return(real_filter_(local, &pos, pos, decl));
		if (pos != Nil) {
			SetArrayA4(temp, count++, pos);
		}
	}

	/* make type-or */
	vector4_andor(local, ret, temp, count, LISPDECL_OR);
	return 0;
}

static int real_filter_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return real_filter_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return real_filter_or_(local, ret, type, decl);

		default:
			return real_filter_type_(local, ret, type, decl);
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
static int make_range_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	enum LISPDECL decl;
	addr left1, left2, right1, right2;

	range_left_value(left, &left1, &left2);
	range_right_value(right, &right1, &right2);
	GetLispDecl(left, &decl);
	Return(real_extract_lispdecl_local_(local, decl, left2, &left2));
	Return(real_extract_lispdecl_local_(local, decl, right2, &right2));
	type4_local(local, decl, left1, left2, right1, right2, ret);

	return 0;
}

static int make_range_left_aster_(LocalRoot local, addr *ret, addr left)
{
	addr left1, left2;

	range_left_value(left, &left1, &left2);
	return type_range_left_(local, ret, RefLispDecl(left), left1, left2);
}

static int make_range_aster_right_(LocalRoot local, addr *ret, addr right)
{
	addr right1, right2;

	range_right_value(right, &right1, &right2);
	return type_range_right_(local, ret, RefLispDecl(right), right1, right2);
}

/* (10 *) (20 *) */
static int range_and_left_left_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_left_left_less_(left, right, &check));
	return Result(ret, check? right: left);
}

/* (* 20) (10 *) */
static int range_and_right_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_left_greater_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

/* (10 20) (15 *) */
static int range_and_between_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_left_less_equal_(right, left, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(right, left, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);

	return Result(ret, Nil);
}

static int range_and_left_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_left_(ret, left, right);
	if (range_right_p(left))
		return range_and_right_left_(local, ret, left, right);
	if (range_between_p(left))
		return range_and_between_left_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 *) (* 20) */
static int range_and_left_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

/* (* 10) (* 20) */
static int range_and_right_right_(addr *ret, addr left, addr right)
{
	int check;
	Return(range_right_right_less_(left, right, &check));
	return Result(ret, check? left: right);
}

/* (10 30) (* 20) */
static int range_and_between_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_right_right_less_equal_(left, right, &check));
	if (check)
		return Result(ret, left);

	Return(range_left_right_less_equal_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_right_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_left_right_(local, ret, left, right);
	if (range_right_p(left))
		return range_and_right_right_(ret, left, right);
	if (range_between_p(left))
		return range_and_between_right_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

/* (10 40) (20 30) */
static int range_and_between_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	int check;

	Return(range_between_in_(left, right, &check));
	if (check)
		return Result(ret, right);
	Return(range_between_in_(right, left, &check));
	if (check)
		return Result(ret, left);
	Return(range_between_left_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, right, left);
	Return(range_between_right_(left, right, &check));
	if (check)
		return make_range_left_right_(local, ret, left, right);

	return Result(ret, Nil);
}

static int range_and_between_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(left))
		return Result(ret, right);
	if (range_left_p(left))
		return range_and_between_left_(local, ret, right, left);
	if (range_right_p(left))
		return range_and_between_right_(local, ret, right, left);
	if (range_between_p(left))
		return range_and_between_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int range_and_(LocalRoot local, addr *ret, addr left, addr right)
{
	if (range_asterisk_p(right))
		return Result(ret, left);
	if (range_left_p(right))
		return range_and_left_(local, ret, left, right);
	if (range_right_p(right))
		return range_and_right_(local, ret, left, right);
	if (range_between_p(right))
		return range_and_between_(local, ret, left, right);

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int map_range_and_(LocalRoot local, addr *ret, addr list, addr right)
{
	addr result, left;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &left, &list);
		Return(range_and_(local, &left, left, right));
		if (left != Nil)
			cons_local(local, &result, left, result);
	}
	nreverse(ret, result);

	return 0;
}

static int merge_range_andplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	addr type;

	while (right != Nil) {
		GetCons(right, &type, &right);
		Return(map_range_and_(local, &left, left, type));
		if (left == Nil)
			break;
	}

	return Result(ret, left);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl);
static int range_and_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t i, size;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
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
			Return(merge_range_andplus_(local, &left, left, right));
			if (left == Nil)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_and_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, T);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_and_otherwise_(local, ret, type, decl);
}

/* merge-range-or */
typedef int (*extpairtype)(LocalRoot, addr x, addr y, addr *value, int *ret);
static int extpaircall_right_(LocalRoot local,
		extpairtype call, addr left, addr cons, addr *value, int *ret)
{
	int check;
	addr right;

	while (cons != Nil) {
		GetCons(cons, &right, &cons);
		if (left != right) {
			Return((*call)(local, left, right, &right, &check));
			if (check) {
				*value = right;
				return Result(ret, check);
			}
		}
	}

	return Result(ret, 0);
}

static void extpaircall_pushlist(LocalRoot local, addr *ret, addr list, addr result)
{
	addr one;

	while (list != Nil) {
		GetCons(list, &one, &list);
		cons_local(local, &result, one, result);
	}
	*ret = result;
}

static int extpaircall_left_(
		LocalRoot local, extpairtype call, addr right, addr *value, int *ret)
{
	int update, check;
	addr left, result, pos, cons;

	result = Nil;
	update = 0;
	for (cons = right; cons != Nil; ) {
		GetCons(cons, &left, &cons);
		if (update) {
			cons_local(local, &result, left, result);
		}
		else {
			Return(extpaircall_right_(local, call, left, right, &pos, &check));
			if (check < 0)
				extpaircall_pushlist(local, &result, pos, result);
			else
				cons_local(local, &result, check? pos: left, result);
			if (check)
				update = 1;
		}
	}
	if (update)
		nreverse(value, result);
	return Result(ret, update);
}

static int extpaircall_(LocalRoot local, extpairtype call, addr *cons, int *update)
{
	int loop, check;
	addr pos;

	pos = *cons;
	loop = 0;
	for (;;) {
		Return(extpaircall_left_(local, call, pos, &pos, &check));
		if (! check)
			break;
		loop = 1;
	}
	if (loop) {
		*cons = pos;
		*update = 1;
	}

	return 0;
}

/* check only */
static int range_or_check_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	Check(RefLispDecl(left) != RefLispDecl(right), "type error");
	Check(RefNotDecl(left) || RefNotDecl(right), "not error");
	return Result(ret, 0);
}

/* (? ?) (* *) -> delete */
static int range_or_aster_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	if (range_asterisk_p(right)) {
		*value = Nil;
		return Result(ret, -1); /* list */
	}

	return Result(ret, 0);
}

/* (20 ?) (10 *) -> delete */
static int range_or_left_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_any_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_left_left_less_equal_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (? 10) (* 20) -> delete */
static int range_or_right_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_any_right_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_right_right_less_equal_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 *) (* 20) -> (10 20) */
static int range_or_left_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_left_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_right_left_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	type4aster_local(local, RefLispDecl(left), value);
	return Result(ret, 1);
}

/* (10 30) (20 *) -> (10 *) */
static int range_or_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_left_p(right))
		return Result(ret, 0);
	Return(range_connect_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_aster_(local, value, left));
	return Result(ret, 1);
}

/* (10 30) (* 20) -> (* 30) */
static int range_or_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_right_p(right))
		return Result(ret, 0);
	Return(range_connect_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_aster_right_(local, value, left));
	return Result(ret, 1);
}

/* (21 22) (10 30) -> delete */
static int range_or_range_range_in_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_in_(right, left, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	*value = Nil;
	return Result(ret, -1);
}

/* (10 30) (20 40) -> (10 30) */
static int range_or_range_range_left_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (! check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, left, right));
	return Result(ret, 1);
}

/* (20 40) (10 30) -> (10 40) */
static int range_or_range_range_right_(
		LocalRoot local, addr left, addr right, addr *value, int *ret)
{
	int check;

	if (! range_between_p(left))
		return Result(ret, 0);
	if (! range_between_p(right))
		return Result(ret, 0);
	Return(range_between_left_(left, right, &check));
	if (check)
		return Result(ret, 0);
	Return(range_between_right_(left, right, &check));
	if (! check)
		return Result(ret, 0);

	/* true */
	Return(make_range_left_right_(local, value, right, left));
	return Result(ret, 1);
}

#define Return_extpaircall(a,b,c,d) Return(extpaircall_((a),(b),(c),(d)))
static int merge_range_orplus_(LocalRoot local, addr *ret, addr left, addr right)
{
	int update, result;

	append2_local_unsafe(local, left, right, &left);
	for (result = 0; ; result |= update) {
		update = 0;
		Return_extpaircall(local, range_or_check_, &left, &update);
		Return_extpaircall(local, range_or_aster_, &left, &update);
		Return_extpaircall(local, range_or_left_left_, &left, &update);
		Return_extpaircall(local, range_or_right_right_, &left, &update);
		Return_extpaircall(local, range_or_left_right_, &left, &update);
		Return_extpaircall(local, range_or_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_right_, &left, &update);
		Return_extpaircall(local, range_or_range_range_in_, &left, &update);
		Return_extpaircall(local, range_or_range_range_left_, &left, &update);
		Return_extpaircall(local, range_or_range_range_right_, &left, &update);
		if (update == 0)
			break;
	}

	return Result(ret, left);
}

static int range_or_otherwise_(LocalRoot local,
		addr *ret, addr array, enum LISPDECL decl)
{
	addr left, right;
	size_t size, i;

	LenArrayA4(array, &size);
	left = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &right);
		Return(merge_range_type_(local, &right, right, decl));
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
			Return(merge_range_orplus_(local, &left, left, right));
			if (left == T)
				break;
		}
	}

	return Result(ret, left);
}

static int merge_range_or_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	size_t size;

	Check(RefNotDecl(type), "not error");
	GetArrayType(type, 0, &type);
	LenArrayA4(type, &size);
	if (size == 0)
		return Result(ret, Nil);
	if (size == 1) {
		GetArrayA4(type, 0, &type);
		conscar_local(local, ret, type);
		return 0;
	}

	return range_or_otherwise_(local, ret, type, decl);
}

static int merge_range_type_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	switch (RefLispDecl(type)) {
		case LISPDECL_AND:
			return merge_range_and_(local, ret, type, decl);

		case LISPDECL_OR:
			return merge_range_or_(local, ret, type, decl);

		default:
			merge_range_cons(local, ret, type, decl);
			return 0;
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

static int make_merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	Return(merge_range_type_(local, &type, type, decl));
	if (type == Nil)
		return Result(ret, Nil);
	if (type == T) {
		type4aster_local(local, decl, ret);
		return 0;
	}
	if (singlep(type)) {
		GetCar(type, ret);
		return 0;
	}
	if (GetType(type) == LISPTYPE_CONS) {
		type_or_cons(local, ret, type);
		return 0;
	}

	*ret = Nil;
	return fmte_("type error", NULL);
}

static int merge_range_(LocalRoot local, addr *ret, addr type, enum LISPDECL decl)
{
	int ignore;

	Return(real_filter_(local, &type, type, decl));
	if (type != Nil) {
		Return(type_optimize_local_(local, type, &type, &ignore));
		get_type_optimized(&type, type);
	}
	if ((type == Nil) || (RefLispDecl(type) == LISPDECL_NIL))
		return Result(ret, Nil);

	return make_merge_range_(local, ret, type, decl);
}


/*
 *  real_extract
 */
static const enum LISPDECL RealFilterDecls[] = {
	LISPDECL_INTEGER,
	LISPDECL_RATIONAL,
	LISPDECL_SHORT_FLOAT,
	LISPDECL_SINGLE_FLOAT,
	LISPDECL_DOUBLE_FLOAT,
	LISPDECL_LONG_FLOAT,
	LISPDECL_EMPTY
};

static int real_filter_range_list_(LocalRoot local, addr type,
		addr *value, size_t *ret)
{
	addr cons, check;
	size_t i, size;
	enum LISPDECL decl;

	cons = Nil;
	for (i = size = 0; ; i++) {
		decl = RealFilterDecls[i];
		if (decl == LISPDECL_EMPTY)
			break;
		Return(merge_range_(local, &check, type, decl));
		if (check != Nil) {
			cons_local(local, &cons, check, cons);
			size++;
		}
	}
	*value = cons;

	return Result(ret, size);
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

static int make_real_filter_(LocalRoot local, addr *ret, addr type)
{
	addr pos;
	size_t size;

	Return(real_filter_range_list_(local, type, &pos, &size));
	if (size == 0)
		return Result(ret, type);
	real_reject(local, &type, type);
	cons_local(local, &pos, type, pos);
	nreverse(&pos, pos);
	copy_cons_to_vector4_local(local, &pos, pos, size + 1UL);
	type1_local(local, LISPDECL_OR, pos, ret);

	return 0;
}

static int real_extract_(LocalRoot local, addr *ret, addr type)
{
	int ignore;

	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(make_real_filter_(local, &type, type));
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(ret, type);

	return 0;
}

int real_extract_local_(LocalRoot local, addr *ret, addr type)
{
	CheckLocal(local);
	CheckType(type, LISPTYPE_TYPE);
	if (RefLispDecl(type) == LISPDECL_SUBTYPEP) {
		*ret = type;
	}
	else {
		Return(real_extract_(local, &type, type));
		type1_local(local, LISPDECL_SUBTYPEP, type, ret);
	}

	return 0;
}

int real_extract_heap_(LocalRoot local, addr *ret, addr type)
{
	LocalStack stack;

	CheckLocal(local);
	push_local(local, &stack);
	Return(real_extract_local_(local, &type, type));
	type_copy_heap(ret, type);
	rollback_local(local, stack);

	return 0;
}

int type_subtypep_p(addr type)
{
	CheckType(type, LISPTYPE_TYPE);
	return RefLispDecl(type) == LISPDECL_SUBTYPEP;
}

int type_optimized_or_subtypep(addr type)
{
	return type_optimized_p(type) || type_subtypep_p(type);
}

void get_type_subtypep(addr *ret, addr type)
{
	if (type_subtypep_p(type)) {
		GetArrayType(type, 0, ret);
	}
	else {
		*ret = type;
	}
}

int type_subtypep_throw_heap_(LocalRoot local, addr type, addr *ret)
{
	Return(real_extract_heap_(local, &type, type));
	get_type_subtypep(ret, type);
	return 0;
}

