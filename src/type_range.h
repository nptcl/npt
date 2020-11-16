#ifndef __TYPE_RANGE_HEADER__
#define __TYPE_RANGE_HEADER__

#include "typedef.h"

#define range_asterisk_p _n(range_asterisk_p)
#define range_left_p _n(range_left_p)
#define range_left_any_p _n(range_left_any_p)
#define range_right_p _n(range_right_p)
#define range_any_right_p _n(range_any_right_p)
#define range_between_p _n(range_between_p)
#define range_left_value _n(range_left_value)
#define range_right_value _n(range_right_value)
#define range_left_left_less_ _n(range_left_left_less_)
#define range_left_left_less_equal_ _n(range_left_left_less_equal_)
#define range_left_left_greater_ _n(range_left_left_greater_)
#define range_left_left_greater_equal_ _n(range_left_left_greater_equal_)
#define range_left_right_less_ _n(range_left_right_less_)
#define range_left_right_less_equal_ _n(range_left_right_less_equal_)
#define range_left_right_greater_ _n(range_left_right_greater_)
#define range_left_right_greater_equal_ _n(range_left_right_greater_equal_)
#define range_right_left_less_ _n(range_right_left_less_)
#define range_right_left_less_equal_ _n(range_right_left_less_equal_)
#define range_right_left_greater_ _n(range_right_left_greater_)
#define range_right_left_greater_equal_ _n(range_right_left_greater_equal_)
#define range_right_right_less_ _n(range_right_right_less_)
#define range_right_right_less_equal_ _n(range_right_right_less_equal_)
#define range_right_right_greater_ _n(range_right_right_greater_)
#define range_right_right_greater_equal_ _n(range_right_right_greater_equal_)
#define range_between_left_ _n(range_between_left_)
#define range_left_between_ _n(range_left_between_)
#define range_between_right_ _n(range_between_right_)
#define range_right_between_ _n(range_right_between_)
#define range_between_in_ _n(range_between_in_)
#define range_in_between_ _n(range_in_between_)
#define range_connect_right_left_ _n(range_connect_right_left_)
#define range_connect_between_left_ _n(range_connect_between_left_)
#define range_connect_between_right_ _n(range_connect_between_right_)

int range_asterisk_p(addr type);
int range_left_p(addr type);
int range_left_any_p(addr type);
int range_right_p(addr type);
int range_any_right_p(addr type);
int range_between_p(addr type);

void range_left_value(addr value, addr *left1, addr *left2);
void range_right_value(addr value, addr *right1, addr *right2);

int range_left_left_less_(addr left, addr right, int *ret);
int range_left_left_less_equal_(addr left, addr right, int *ret);
int range_left_left_greater_(addr left, addr right, int *ret);
int range_left_left_greater_equal_(addr left, addr right, int *ret);
int range_left_right_less_(addr left, addr right, int *ret);
int range_left_right_less_equal_(addr left, addr right, int *ret);
int range_left_right_greater_(addr left, addr right, int *ret);
int range_left_right_greater_equal_(addr left, addr right, int *ret);
int range_right_left_less_(addr left, addr right, int *ret);
int range_right_left_less_equal_(addr left, addr right, int *ret);
int range_right_left_greater_(addr left, addr right, int *ret);
int range_right_left_greater_equal_(addr left, addr right, int *ret);
int range_right_right_less_(addr left, addr right, int *ret);
int range_right_right_less_equal_(addr left, addr right, int *ret);
int range_right_right_greater_(addr left, addr right, int *ret);
int range_right_right_greater_equal_(addr left, addr right, int *ret);

int range_between_left_(addr left, addr right, int *ret);
int range_left_between_(addr left, addr right, int *ret);
int range_between_right_(addr left, addr right, int *ret);
int range_right_between_(addr left, addr right, int *ret);
int range_between_in_(addr left, addr right, int *ret);
int range_in_between_(addr left, addr right, int *ret);

int range_connect_right_left_(addr left, addr right, int *ret);
int range_connect_between_left_(addr left, addr right, int *ret);
int range_connect_between_right_(addr left, addr right, int *ret);

#endif

