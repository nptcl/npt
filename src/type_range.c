#include "type.h"
#include "type_parse.h"
#include "type_range.h"
#include "real.h"

/* (integetr * *) */
int range_asterisk_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! asterisk_p(check)) return 0;
	GetArrayType(type, 2, &check);

	return asterisk_p(check);
}

/* (integer 10 *) */
int range_left_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (asterisk_p(check)) return 0;
	GetArrayType(type, 2, &check);

	return asterisk_p(check);
}

/* (integer 10 ?) */
int range_left_any_p(addr type)
{
	addr check;
	GetArrayType(type, 0, &check);
	return ! asterisk_p(check);
}

/* (integer * 10) */
int range_right_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (! asterisk_p(check)) return 0;
	GetArrayType(type, 2, &check);

	return ! asterisk_p(check);
}

/* (integer ? 10) */
int range_any_right_p(addr type)
{
	addr check;
	GetArrayType(type, 2, &check);
	return ! asterisk_p(check);
}

/* (integer 10 20) */
int range_between_p(addr type)
{
	addr check;

	GetArrayType(type, 0, &check);
	if (asterisk_p(check)) return 0;
	GetArrayType(type, 2, &check);

	return ! asterisk_p(check);
}

void range_left_value(addr value, addr *left1, addr *left2)
{
	GetArrayType(value, 0, left1);
	GetArrayType(value, 1, left2);
}

void range_right_value(addr value, addr *right1, addr *right2)
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
int range_left_left_less(addr left, addr right)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_equal_real(Local_Thread, left, right);
	else
		return less_real(Local_Thread, left, right);
}

/*
 * ( 10  *) ( 10  *) -> t
 * ((10) *) ( 10  *) -> nil
 * ( 10  *) ((10) *) -> t
 * ((10) *) ((10) *) -> t
 */
int range_left_left_less_equal(addr left, addr right)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_real(Local_Thread, left, right);
	else
		return less_equal_real(Local_Thread, left, right);
}

int range_left_left_greater(addr left, addr right)
{
	return ! range_left_left_less_equal(left, right);
}

int range_left_left_greater_equal(addr left, addr right)
{
	return ! range_left_left_less(left, right);
}

/*
 * ( 10  *) (*  10 ) -> nil
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
int range_left_right_less(addr left, addr right)
{
	/* range_left_value(left, &left1, &left); */
	GetArrayType(left, 1, &left);
	/* range_right_value(right, &right1, &right); */
	GetArrayType(right, 3, &right);
	return less_real(Local_Thread, left, right);
}

/*
 * ( 10  *) (*  10 ) -> t
 * ((10) *) (*  10 ) -> nil
 * ( 10  *) (* (10)) -> nil
 * ((10) *) (* (10)) -> nil
 */
int range_left_right_less_equal(addr left, addr right)
{
	addr left1, right1;

	range_left_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_equal_real(Local_Thread, left, right);
	else
		return less_real(Local_Thread, left, right);
}

int range_left_right_greater(addr left, addr right)
{
	return ! range_left_right_less_equal(left, right);
}

int range_left_right_greater_equal(addr left, addr right)
{
	return ! range_left_right_less(left, right);
}

/*
 * (*  10 ) ( 10  *) -> nil
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
int range_right_left_less(addr left, addr right)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 == Nil && right1 == Nil)
		return less_real(Local_Thread, left, right);
	else
		return less_equal_real(Local_Thread, left, right);
}

/*
 * (*  10 ) ( 10  *) -> t
 * (* (10)) ( 10  *) -> t
 * (*  10 ) ((10) *) -> t
 * (* (10)) ((10) *) -> t
 */
int range_right_left_less_equal(addr left, addr right)
{
	/* range_right_value(left, &left1, &left); */
	GetArrayType(left, 3, &left);
	/* range_left_value(right, &right1, &right); */
	GetArrayType(right, 1, &right);
	return less_equal_real(Local_Thread, left, right);
}

int range_right_left_greater(addr left, addr right)
{
	return ! range_right_left_less_equal(left, right);
}

int range_right_left_greater_equal(addr left, addr right)
{
	return ! range_right_left_less(left, right);
}

/*
 * (*  10 ) (*  10 ) -> nil
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> nil
 */
int range_right_right_less(addr left, addr right)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 != Nil && right1 == Nil)
		return less_equal_real(Local_Thread, left, right);
	else
		return less_real(Local_Thread, left, right);
}

/*
 * (*  10 ) (*  10 ) -> t
 * (* (10)) (*  10 ) -> t
 * (*  10 ) (* (10)) -> nil
 * (* (10)) (* (10)) -> t
 */
int range_right_right_less_equal(addr left, addr right)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_right_value(right, &right1, &right);
	if (left1 == Nil && right1 != Nil)
		return less_real(Local_Thread, left, right);
	else
		return less_equal_real(Local_Thread, left, right);
}

int range_right_right_greater(addr left, addr right)
{
	return ! range_right_right_less_equal(left, right);
}

int range_right_right_greater_equal(addr left, addr right)
{
	return ! range_right_right_less(left, right);
}

/* (10 30) (20 *) -> t */
int range_between_left(addr left, addr right)
{
	return range_left_left_less_equal(left, right) &&
		range_left_right_less_equal(right, left);
}

/* (20 *) (10 30) -> t */
int range_left_between(addr left, addr right)
{
	return range_left_left_less_equal(right, left) &&
		range_left_right_less_equal(left, right);
}

/* (10 30) (* 20) -> t */
int range_between_right(addr left, addr right)
{
	return range_left_right_less_equal(left, right)
		&& range_right_right_less_equal(right, left);
}

/* (* 20) (10 30) -> t */
int range_right_between(addr left, addr right)
{
	return range_left_right_less_equal(right, left) &&
		range_right_right_less_equal(left, right);
}

/* (10 30) (11 12) -> t */
int range_between_in(addr left, addr right)
{
	return range_between_left(left, right) &&
		range_between_right(left, right);
}

/* (11 12) (10 30) -> t */
int range_in_between(addr left, addr right)
{
	return range_left_between(left, right) &&
		range_right_between(left, right);
}

/*
 * (?  10 ) ( 10  ?) -> t
 * (? (10)) ( 10  ?) -> t
 * (?  10 ) ((10) ?) -> t
 * (? (10)) ((10) ?) -> nil
 */
int range_connect_right_left(addr left, addr right)
{
	addr left1, right1;

	range_right_value(left, &left1, &left);
	range_left_value(right, &right1, &right);
	if (left1 != Nil && right1 != Nil)
		return less_real(Local_Thread, right, left);
	else
		return less_equal_real(Local_Thread, right, left);
}

/* (10 20) (20 *) */
int range_connect_between_left(addr left, addr right)
{
	return range_left_left_less_equal(left, right) &&
		range_connect_right_left(left, right);
}

/* (10 20) (* 10) */
int range_connect_between_right(addr left, addr right)
{
	return range_right_right_less_equal(right, left) &&
		range_connect_right_left(right, left);
}

