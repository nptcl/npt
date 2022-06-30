#ifndef __CLOSGET_METHOD_HEADER__
#define __CLOSGET_METHOD_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_method_function_ _n(stdget_method_function_)
#define stdget_method_generic_function_ _n(stdget_method_generic_function_)
#define stdget_method_lambda_list_ _n(stdget_method_lambda_list_)
#define stdget_method_qualifiers_ _n(stdget_method_qualifiers_)
#define stdget_method_specializers_ _n(stdget_method_specializers_)

#define stdset_method_function_ _n(stdset_method_function_)
#define stdset_method_generic_function_ _n(stdset_method_generic_function_)
#define stdset_method_lambda_list_ _n(stdset_method_lambda_list_)
#define stdset_method_qualifiers_ _n(stdset_method_qualifiers_)
#define stdset_method_specializers_ _n(stdset_method_specializers_)

#define stdboundp_method_function_ _n(stdboundp_method_function_)
#define stdboundp_method_generic_function_ _n(stdboundp_method_generic_function_)
#define stdboundp_method_lambda_list_ _n(stdboundp_method_lambda_list_)
#define stdboundp_method_qualifiers_ _n(stdboundp_method_qualifiers_)
#define stdboundp_method_specializers_ _n(stdboundp_method_specializers_)

int stdget_method_function_(Execute ptr, addr pos, addr *ret);
int stdget_method_generic_function_(Execute ptr, addr pos, addr *ret);
int stdget_method_lambda_list_(Execute ptr, addr pos, addr *ret);
int stdget_method_qualifiers_(Execute ptr, addr pos, addr *ret);
int stdget_method_specializers_(Execute ptr, addr pos, addr *ret);

int stdset_method_function_(Execute ptr, addr pos, addr value);
int stdset_method_generic_function_(Execute ptr, addr pos, addr value);
int stdset_method_lambda_list_(Execute ptr, addr pos, addr value);
int stdset_method_qualifiers_(Execute ptr, addr pos, addr value);
int stdset_method_specializers_(Execute ptr, addr pos, addr value);

int stdboundp_method_function_(Execute ptr, addr pos, int *ret);
int stdboundp_method_generic_function_(Execute ptr, addr pos, int *ret);
int stdboundp_method_lambda_list_(Execute ptr, addr pos, int *ret);
int stdboundp_method_qualifiers_(Execute ptr, addr pos, int *ret);
int stdboundp_method_specializers_(Execute ptr, addr pos, int *ret);

#endif

