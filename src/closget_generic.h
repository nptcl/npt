#ifndef __CLOSGET_GENERIC_HEADER__
#define __CLOSGET_GENERIC_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_generic_name_ _n(stdget_generic_name_)
#define stdset_generic_name_ _n(stdset_generic_name_)
#define stdget_generic_methods_ _n(stdget_generic_methods_)
#define stdset_generic_methods_ _n(stdset_generic_methods_)
#define stdget_generic_lambda_list_ _n(stdget_generic_lambda_list_)
#define stdset_generic_lambda_list_ _n(stdset_generic_lambda_list_)
#define stdget_generic_argument_precedence_order_ _n(stdget_generic_argument_precedence_order_)
#define stdset_generic_argument_precedence_order_ _n(stdset_generic_argument_precedence_order_)
#define stdget_generic_declarations_ _n(stdget_generic_declarations_)
#define stdset_generic_declarations_ _n(stdset_generic_declarations_)
#define stdget_generic_method_class_ _n(stdget_generic_method_class_)
#define stdset_generic_method_class_ _n(stdset_generic_method_class_)
#define stdget_generic_method_combination_ _n(stdget_generic_method_combination_)
#define stdset_generic_method_combination_ _n(stdset_generic_method_combination_)

#define stdget_generic_vector_ _n(stdget_generic_vector_)
#define stdset_generic_vector_ _n(stdset_generic_vector_)
#define stdget_generic_remove_ _n(stdget_generic_remove_)
#define stdset_generic_remove_ _n(stdset_generic_remove_)
#define stdget_generic_argument_ _n(stdget_generic_argument_)
#define stdset_generic_argument_ _n(stdset_generic_argument_)
#define stdget_generic_documentation_ _n(stdget_generic_documentation_)
#define stdset_generic_documentation_ _n(stdset_generic_documentation_)
#define stdget_generic_eqlcheck_ _n(stdget_generic_eqlcheck_)
#define stdset_generic_eqlcheck_ _n(stdset_generic_eqlcheck_)
#define stdget_generic_cache_ _n(stdget_generic_cache_)
#define stdset_generic_cache_ _n(stdset_generic_cache_)
#define stdget_generic_call_ _n(stdget_generic_call_)
#define stdset_generic_call_ _n(stdset_generic_call_)
#define stdget_generic_precedence_index_ _n(stdget_generic_precedence_index_)
#define stdset_generic_precedence_index_ _n(stdset_generic_precedence_index_)
#define stdboundp_generic_argument_precedence_order_ _n(stdboundp_generic_argument_precedence_order_)
#define stdboundp_generic_eqlcheck_ _n(stdboundp_generic_eqlcheck_)

int stdget_generic_name_(addr pos, addr *ret);
int stdset_generic_name_(addr pos, addr value);
int stdget_generic_methods_(addr pos, addr *ret);
int stdset_generic_methods_(addr pos, addr value);
int stdget_generic_lambda_list_(addr pos, addr *ret);
int stdset_generic_lambda_list_(addr pos, addr value);
int stdget_generic_argument_precedence_order_(addr pos, addr *ret);
int stdset_generic_argument_precedence_order_(addr pos, addr value);
int stdget_generic_declarations_(addr pos, addr *ret);
int stdset_generic_declarations_(addr pos, addr value);
int stdget_generic_method_class_(addr pos, addr *ret);
int stdset_generic_method_class_(addr pos, addr value);
int stdget_generic_method_combination_(addr pos, addr *ret);
int stdset_generic_method_combination_(addr pos, addr value);
int stdget_generic_vector_(addr pos, addr *ret);
int stdset_generic_vector_(addr pos, addr value);
int stdget_generic_remove_(addr pos, addr *ret);
int stdset_generic_remove_(addr pos, addr value);
int stdget_generic_argument_(addr pos, addr *ret);
int stdset_generic_argument_(addr pos, addr value);
int stdget_generic_documentation_(addr pos, addr *ret);
int stdset_generic_documentation_(addr pos, addr value);
int stdget_generic_eqlcheck_(addr pos, addr *ret);
int stdset_generic_eqlcheck_(addr pos, addr value);
int stdget_generic_cache_(addr pos, addr *ret);
int stdset_generic_cache_(addr pos, addr value);
int stdget_generic_call_(addr pos, addr *ret);
int stdset_generic_call_(addr pos, addr value);
int stdget_generic_precedence_index_(addr pos, addr *ret);
int stdset_generic_precedence_index_(addr pos, addr value);
int stdboundp_generic_argument_precedence_order_(addr pos, int *ret);
int stdboundp_generic_eqlcheck_(addr pos, int *ret);

#endif

