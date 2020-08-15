#include "type.h"
#include "type_parse.h"
#include "type_range.h"
#include "real_equal.h"

/* (integetr * *) */
_g int range_asterisk_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return type_asterisk_p(check);
}

/* (integer 10 *) */
_g int range_left_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return type_asterisk_p(check);
}

/* (integer 10 ?) */
_g int range_left_any_p(addr type)
{
	addr check;
	GetArrayType(type, 0, &check);
	return ! type_asterisk_p(check);
}

/* (integer * 10) */
_g int range_right_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return ! type_asterisk_p(check);
}

/* (integer ? 10) */
_g int range_any_right_p(addr type)
{
	addr check;
	GetArrayType(type, 2, &check);
	return ! type_asterisk_p(check);
}

/* (integer 10 20) */
_g int range_between_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (type_asterisk_p(check))
		return 0;
	GetArrayType(type, 2, &check);

	return ! type_asterisk_p(check);
}

_g void range_left_value(addr value, addr *left1, addr *left2)
{
	GetArrayType(value, 0, left1);
	GetArrayType(value, 1, left2);
}

_g void range_right_value(addr value, addr *right1, addr *right2)
{
	GetArrayType(value, 2, right1);
	GetArrayType(value, 3, right2);
}

/*
 * ( 10  *) ( 10  *) -> nil
 * ((10) *) ( 10  *) -> nil
 * ( 10  *) ((10) *) -> t
 * ((10) *) ((10) *) -> nil
 */
_g int range_left_left_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

/*
 * ( 10  *) ( 10  *) -> t
 * ((10) *) ( 10  *) -> nil
 * ( 10  *) ((10) *) -> t
 * ((10) *) ((10) *) -> t
 */
_g int range_left_left_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

_g int range_left_left_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_left_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

_g int range_left_left_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_left_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * ( 10  *) (*  10 ) -> nil
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
_g int range_left_right_less_(addr left, addr right, int *ret)
{
	/* range_left_value(left, &left1, &left); */
	GetArrayType(left, 1, &left);
	/* range_right_value(right, &right1, &right); */
	GetArrayType(right, 3, &right);
	return less_real_(Local_Thread, left, right, ret);
}

/*
 * ( 10  *) (*  10 ) -> t
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
_g int range_left_right_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

_g int range_left_right_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_right_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

_g int range_left_right_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_left_right_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * (*  10 ) ( 10  *) -> nil
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
_g int range_right_left_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

/*
 * (*  10 ) ( 10  *) -> t
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
_g int range_right_left_less_equal_(addr left, addr right, int *ret)
{
	/* range_right_value(left, &left1, &left); */
	GetArrayType(left, 3, &left);
	/* range_left_value(right, &right1, &right); */
	GetArrayType(right, 1, &right);
	return less_equal_real_(Local_Thread, left, right, ret);
}

_g int range_right_left_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_left_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

_g int range_right_left_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_left_less_(left, right, &check));
	return Result(ret, ! check);
}

/*
 * (*  10 ) (*  10 ) -> nil
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> nil
 */
_g int range_right_right_less_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_equal_real_(Local_Thread, left, right, ret);
	else
		return less_real_(Local_Thread, left, right, ret);
}

/*
 * (*  10 ) (*  10 ) -> t
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> t
 */
_g int range_right_right_less_equal_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_real_(Local_Thread, left, right, ret);
	else
		return less_equal_real_(Local_Thread, left, right, ret);
}

_g int range_right_right_greater_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_right_less_equal_(left, right, &check));
	return Result(ret, ! check);
}

_g int range_right_right_greater_equal_(addr left, addr right, int *ret)
{
	int check;
	Return(range_right_right_less_(left, right, &check));
	return Result(ret, ! check);
}

/* (10 30) (20 *) -> t */
	static int range_and2_call_(
			int (*call1)(addr, addr, int *), addr a, addr b,
			int (*call2)(addr, addr, int *), addr c, addr d,
			int *ret)
{
	int check;

	Return((*call1)(a, b, &check));
	if (! check)
		return Result(ret, 0);
	return (*call2)(c, d, ret);
}

_g int range_between_left_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, left, right,
			range_left_right_less_equal_, right, left, ret);
}

/* (20 *) (10 30) -> t */
_g int range_left_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, right, left,
			range_left_right_less_equal_, left, right, ret);
}

/* (10 30) (* 20) -> t */
_g int range_between_right_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_right_less_equal_, left, right,
			range_right_right_less_equal_, right, left, ret);
}

/* (* 20) (10 30) -> t */
_g int range_right_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_right_less_equal_, right, left,
			range_right_right_less_equal_, left, right, ret);
}

/* (10 30) (11 12) -> t */
_g int range_between_in_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_between_left_, left, right,
			range_between_right_, left, right, ret);
}

/* (11 12) (10 30) -> t */
_g int range_in_between_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_between_, left, right,
			range_right_between_, left, right, ret);
}

/*
 * (?  10 ) ( 10  ?) -> t
 * (? (10)) ( 10  ?) -> t
 * (?  10 ) ((10) ?) -> t
 * (? (10)) ((10) ?) -> nil
 */
_g int range_connect_right_left_(addr left, addr right, int *ret)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 != Nil)
		return less_real_(Local_Thread, right, left, ret);
	else
		return less_equal_real_(Local_Thread, right, left, ret);
}

/* (10 20) (20 *) */
_g int range_connect_between_left_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_left_left_less_equal_, left, right,
			range_connect_right_left_, left, right, ret);
}

/* (10 20) (* 10) */
_g int range_connect_between_right_(addr left, addr right, int *ret)
{
	return range_and2_call_(
			range_right_right_less_equal_, right, left,
			range_connect_right_left_, right, left, ret);
}

