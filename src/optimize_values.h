#ifndef __OPTIMIZE_VALUES_HEADER__
#define __OPTIMIZE_VALUES_HEADER__

#include "optimize.h"
#include "typedef.h"

#define checkparse_values_ _n(checkparse_values_)
#define checkparse_multiple_value_bind_ _n(checkparse_multiple_value_bind_)
#define checkparse_multiple_value_call_ _n(checkparse_multiple_value_call_)
#define checkparse_multiple_value_prog1_ _n(checkparse_multiple_value_prog1_)
#define checkparse_nth_value_ _n(checkparse_nth_value_)

#define optparse_values_ _n(optparse_values_)
#define optparse_multiple_value_bind_ _n(optparse_multiple_value_bind_)
#define optparse_multiple_value_call_ _n(optparse_multiple_value_call_)
#define optparse_multiple_value_prog1_ _n(optparse_multiple_value_prog1_)
#define optparse_nth_value_ _n(optparse_nth_value_)

int checkparse_values_(OptimizeInfo *str, int *ret);
int checkparse_multiple_value_bind_(OptimizeInfo *str, int *ret);
int checkparse_multiple_value_call_(OptimizeInfo *str, int *ret);
int checkparse_multiple_value_prog1_(OptimizeInfo *str, int *ret);
int checkparse_nth_value_(OptimizeInfo *str, int *ret);

int optparse_values_(OptimizeInfo *str, int *ret);
int optparse_multiple_value_bind_(OptimizeInfo *str, int *ret);
int optparse_multiple_value_call_(OptimizeInfo *str, int *ret);
int optparse_multiple_value_prog1_(OptimizeInfo *str, int *ret);
int optparse_nth_value_(OptimizeInfo *str, int *ret);

#endif

