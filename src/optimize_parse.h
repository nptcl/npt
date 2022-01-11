#ifndef __OPTIMIZE_PARSE_HEADER__
#define __OPTIMIZE_PARSE_HEADER__

#include "define.h"
#include "execute.h"
#include "optimize.h"
#include "typedef.h"

#define checkparse_inplace_ _n(checkparse_inplace_)
#define optparse_inplace_ _n(optparse_inplace_)
#define checkparse_implicit_declare_ _n(checkparse_implicit_declare_)
#define optparse_implicit_declare_ _n(optparse_implicit_declare_)
#define optimize_value_and_function _n(optimize_value_and_function)
#define optimize_value_only _n(optimize_value_only)
#define checkparse_implicit_all_ _n(checkparse_implicit_all_)
#define optparse_implicit_all_ _n(optparse_implicit_all_)
#define optparse_run_ _n(optparse_run_)
#define optimize_parse_ _n(optimize_parse_)

#define Return_or_optparse(call, str, ret) { \
	Return(call(str, ret)); \
	if (*ret) { \
		return 0;\
	} \
}
#define Return_check_optparse(call, str, ret) { \
	Return(call(str, ret)); \
	if (*ret == 0) { \
		return 0; \
	} \
};

int checkparse_inplace_(OptimizeInfo *str, addr pos, int *ret);
int optparse_inplace_(OptimizeInfo *str, addr pos, addr *value, int *ret);
int checkparse_implicit_declare_(OptimizeInfo *str,
		addr decl, addr cons, int *ret);
int optparse_implicit_declare_(OptimizeInfo *str,
		addr decl, addr cons, addr *value, int *ret);
int optimize_value_and_function(addr pos);
int optimize_value_only(addr pos);
int checkparse_implicit_all_(OptimizeInfo *str, addr list, int *ret);
int optparse_implicit_all_(OptimizeInfo *str, addr list, addr *value, int *ret);
int optparse_run_(OptimizeInfo *str, int *ret, int (*call)(OptimizeInfo *));
int optimize_parse_(Execute ptr, addr pos, addr *value, int *ret);

#endif

