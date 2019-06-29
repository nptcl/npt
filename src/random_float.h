#ifndef __RANDOM_FLOAT_HEADER__
#define __RANDOM_FLOAT_HEADER__

#include <stddef.h>
#include <stdint.h>
#include "random_state.h"

/* float (0 <= return < 1.0) */
_g float float_random_32bit(struct random_state *state);
_g float float_random_64bit(struct random_state *state);
_g double double_random_32bit(struct random_state *state);
_g double double_random_64bit(struct random_state *state);
_g long double long_random_32bit(struct random_state *state);
_g long double long_random_64bit(struct random_state *state);

#endif

