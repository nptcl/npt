#ifndef __RANDOM_FLOAT_HEADER__
#define __RANDOM_FLOAT_HEADER__

#include <stddef.h>
#include <stdint.h>
#include "random_state.h"

#define float_random_32bit _n(float_random_32bit)
#define float_random_64bit _n(float_random_64bit)
#define double_random_32bit _n(double_random_32bit)
#define double_random_64bit _n(double_random_64bit)
#define long_random_32bit _n(long_random_32bit)
#define long_random_64bit _n(long_random_64bit)

/* float (0 <= return < 1.0) */
float float_random_32bit(struct random_state *state);
float float_random_64bit(struct random_state *state);
double double_random_32bit(struct random_state *state);
double double_random_64bit(struct random_state *state);
long double long_random_32bit(struct random_state *state);
long double long_random_64bit(struct random_state *state);

#endif

