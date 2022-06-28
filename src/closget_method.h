#ifndef __CLOSGET_METHOD_HEADER__
#define __CLOSGET_METHOD_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_method_function_ _n(stdget_method_function_)
#define stdset_method_function_ _n(stdset_method_function_)
#define stdget_method_generic_function_ _n(stdget_method_generic_function_)
#define stdset_method_generic_function_ _n(stdset_method_generic_function_)
#define stdget_method_lambda_list_ _n(stdget_method_lambda_list_)
#define stdset_method_lambda_list_ _n(stdset_method_lambda_list_)
#define stdget_method_qualifiers_ _n(stdget_method_qualifiers_)
#define stdset_method_qualifiers_ _n(stdset_method_qualifiers_)
#define stdget_method_specializers_ _n(stdget_method_specializers_)
#define stdset_method_specializers_ _n(stdset_method_specializers_)

int stdget_method_function_(addr pos, addr *ret);
int stdset_method_function_(addr pos, addr value);
int stdget_method_generic_function_(addr pos, addr *ret);
int stdset_method_generic_function_(addr pos, addr value);
int stdget_method_lambda_list_(addr pos, addr *ret);
int stdset_method_lambda_list_(addr pos, addr value);
int stdget_method_qualifiers_(addr pos, addr *ret);
int stdset_method_qualifiers_(addr pos, addr value);
int stdget_method_specializers_(addr pos, addr *ret);
int stdset_method_specializers_(addr pos, addr value);

#endif

