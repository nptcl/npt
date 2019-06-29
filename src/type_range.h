#ifndef __TYPE_RANGE_HEADER__
#define __TYPE_RANGE_HEADER__

#include "typedef.h"

_g int range_asterisk_p(addr type);
_g int range_left_p(addr type);
_g int range_left_any_p(addr type);
_g int range_right_p(addr type);
_g int range_any_right_p(addr type);
_g int range_between_p(addr type);

_g void range_left_value(addr value, addr *left1, addr *left2);
_g void range_right_value(addr value, addr *right1, addr *right2);

_g int range_left_left_less(addr left, addr right);
_g int range_left_left_less_equal(addr left, addr right);
_g int range_left_left_greater(addr left, addr right);
_g int range_left_left_greater_equal(addr left, addr right);
_g int range_left_right_less(addr left, addr right);
_g int range_left_right_less_equal(addr left, addr right);
_g int range_left_right_greater(addr left, addr right);
_g int range_left_right_greater_equal(addr left, addr right);
_g int range_right_left_less(addr left, addr right);
_g int range_right_left_less_equal(addr left, addr right);
_g int range_right_left_greater(addr left, addr right);
_g int range_right_left_greater_equal(addr left, addr right);
_g int range_right_right_less(addr left, addr right);
_g int range_right_right_less_equal(addr left, addr right);
_g int range_right_right_greater(addr left, addr right);
_g int range_right_right_greater_equal(addr left, addr right);

_g int range_between_left(addr left, addr right);
_g int range_left_between(addr left, addr right);
_g int range_between_right(addr left, addr right);
_g int range_right_between(addr left, addr right);
_g int range_between_in(addr left, addr right);
_g int range_in_between(addr left, addr right);

_g int range_connect_right_left(addr left, addr right);
_g int range_connect_between_left(addr left, addr right);
_g int range_connect_between_right(addr left, addr right);

#endif

