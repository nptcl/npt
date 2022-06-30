#ifndef __CLOSGET_COMBINATION_HEADER__
#define __CLOSGET_COMBINATION_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_longcomb_name_ _n(stdget_longcomb_name_)
#define stdget_longcomb_documentation_ _n(stdget_longcomb_documentation_)
#define stdget_longcomb_lambda_list_ _n(stdget_longcomb_lambda_list_)
#define stdget_longcomb_binding_ _n(stdget_longcomb_binding_)
#define stdget_longcomb_qualifiers_ _n(stdget_longcomb_qualifiers_)
#define stdget_longcomb_arguments_ _n(stdget_longcomb_arguments_)
#define stdget_longcomb_generic_ _n(stdget_longcomb_generic_)
#define stdget_longcomb_form_ _n(stdget_longcomb_form_)
#define stdget_shortcomb_name_ _n(stdget_shortcomb_name_)
#define stdget_shortcomb_documentation_ _n(stdget_shortcomb_documentation_)
#define stdget_shortcomb_identity_ _n(stdget_shortcomb_identity_)
#define stdget_shortcomb_operator_ _n(stdget_shortcomb_operator_)
#define stdget_shortcomb_order_ _n(stdget_shortcomb_order_)
#define stdget_longdef_name_ _n(stdget_longdef_name_)
#define stdget_longdef_documentation_ _n(stdget_longdef_documentation_)
#define stdget_longdef_lambda_list_ _n(stdget_longdef_lambda_list_)
#define stdget_longdef_qualifiers_ _n(stdget_longdef_qualifiers_)
#define stdget_longdef_arguments_ _n(stdget_longdef_arguments_)
#define stdget_longdef_generic_ _n(stdget_longdef_generic_)
#define stdget_longdef_form_ _n(stdget_longdef_form_)
#define stdget_shortdef_name_ _n(stdget_shortdef_name_)
#define stdget_shortdef_documentation_ _n(stdget_shortdef_documentation_)
#define stdget_shortdef_identity_ _n(stdget_shortdef_identity_)
#define stdget_shortdef_operator_ _n(stdget_shortdef_operator_)

#define stdset_longcomb_name_ _n(stdset_longcomb_name_)
#define stdset_longcomb_documentation_ _n(stdset_longcomb_documentation_)
#define stdset_longcomb_lambda_list_ _n(stdset_longcomb_lambda_list_)
#define stdset_longcomb_binding_ _n(stdset_longcomb_binding_)
#define stdset_longcomb_qualifiers_ _n(stdset_longcomb_qualifiers_)
#define stdset_longcomb_arguments_ _n(stdset_longcomb_arguments_)
#define stdset_longcomb_generic_ _n(stdset_longcomb_generic_)
#define stdset_longcomb_form_ _n(stdset_longcomb_form_)
#define stdset_shortcomb_name_ _n(stdset_shortcomb_name_)
#define stdset_shortcomb_documentation_ _n(stdset_shortcomb_documentation_)
#define stdset_shortcomb_identity_ _n(stdset_shortcomb_identity_)
#define stdset_shortcomb_operator_ _n(stdset_shortcomb_operator_)
#define stdset_shortcomb_order_ _n(stdset_shortcomb_order_)
#define stdset_longdef_name_ _n(stdset_longdef_name_)
#define stdset_longdef_documentation_ _n(stdset_longdef_documentation_)
#define stdset_longdef_lambda_list_ _n(stdset_longdef_lambda_list_)
#define stdset_longdef_qualifiers_ _n(stdset_longdef_qualifiers_)
#define stdset_longdef_arguments_ _n(stdset_longdef_arguments_)
#define stdset_longdef_generic_ _n(stdset_longdef_generic_)
#define stdset_longdef_form_ _n(stdset_longdef_form_)
#define stdset_shortdef_name_ _n(stdset_shortdef_name_)
#define stdset_shortdef_documentation_ _n(stdset_shortdef_documentation_)
#define stdset_shortdef_identity_ _n(stdset_shortdef_identity_)
#define stdset_shortdef_operator_ _n(stdset_shortdef_operator_)

#define stdboundp_longcomb_name_ _n(stdboundp_longcomb_name_)
#define stdboundp_longcomb_documentation_ _n(stdboundp_longcomb_documentation_)
#define stdboundp_longcomb_lambda_list_ _n(stdboundp_longcomb_lambda_list_)
#define stdboundp_longcomb_binding_ _n(stdboundp_longcomb_binding_)
#define stdboundp_longcomb_qualifiers_ _n(stdboundp_longcomb_qualifiers_)
#define stdboundp_longcomb_arguments_ _n(stdboundp_longcomb_arguments_)
#define stdboundp_longcomb_generic_ _n(stdboundp_longcomb_generic_)
#define stdboundp_longcomb_form_ _n(stdboundp_longcomb_form_)
#define stdboundp_shortcomb_name_ _n(stdboundp_shortcomb_name_)
#define stdboundp_shortcomb_documentation_ _n(stdboundp_shortcomb_documentation_)
#define stdboundp_shortcomb_identity_ _n(stdboundp_shortcomb_identity_)
#define stdboundp_shortcomb_operator_ _n(stdboundp_shortcomb_operator_)
#define stdboundp_shortcomb_order_ _n(stdboundp_shortcomb_order_)
#define stdboundp_longdef_name_ _n(stdboundp_longdef_name_)
#define stdboundp_longdef_documentation_ _n(stdboundp_longdef_documentation_)
#define stdboundp_longdef_lambda_list_ _n(stdboundp_longdef_lambda_list_)
#define stdboundp_longdef_qualifiers_ _n(stdboundp_longdef_qualifiers_)
#define stdboundp_longdef_arguments_ _n(stdboundp_longdef_arguments_)
#define stdboundp_longdef_generic_ _n(stdboundp_longdef_generic_)
#define stdboundp_longdef_form_ _n(stdboundp_longdef_form_)
#define stdboundp_shortdef_name_ _n(stdboundp_shortdef_name_)
#define stdboundp_shortdef_documentation_ _n(stdboundp_shortdef_documentation_)
#define stdboundp_shortdef_identity_ _n(stdboundp_shortdef_identity_)
#define stdboundp_shortdef_operator_ _n(stdboundp_shortdef_operator_)

int stdget_longcomb_name_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_documentation_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_lambda_list_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_binding_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_qualifiers_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_arguments_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_generic_(Execute ptr, addr pos, addr *ret);
int stdget_longcomb_form_(Execute ptr, addr pos, addr *ret);
int stdget_shortcomb_name_(Execute ptr, addr pos, addr *ret);
int stdget_shortcomb_documentation_(Execute ptr, addr pos, addr *ret);
int stdget_shortcomb_identity_(Execute ptr, addr pos, addr *ret);
int stdget_shortcomb_operator_(Execute ptr, addr pos, addr *ret);
int stdget_shortcomb_order_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_name_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_documentation_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_lambda_list_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_qualifiers_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_arguments_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_generic_(Execute ptr, addr pos, addr *ret);
int stdget_longdef_form_(Execute ptr, addr pos, addr *ret);
int stdget_shortdef_name_(Execute ptr, addr pos, addr *ret);
int stdget_shortdef_documentation_(Execute ptr, addr pos, addr *ret);
int stdget_shortdef_identity_(Execute ptr, addr pos, addr *ret);
int stdget_shortdef_operator_(Execute ptr, addr pos, addr *ret);

int stdset_longcomb_name_(Execute ptr, addr pos, addr value);
int stdset_longcomb_documentation_(Execute ptr, addr pos, addr value);
int stdset_longcomb_lambda_list_(Execute ptr, addr pos, addr value);
int stdset_longcomb_binding_(Execute ptr, addr pos, addr value);
int stdset_longcomb_qualifiers_(Execute ptr, addr pos, addr value);
int stdset_longcomb_arguments_(Execute ptr, addr pos, addr value);
int stdset_longcomb_generic_(Execute ptr, addr pos, addr value);
int stdset_longcomb_form_(Execute ptr, addr pos, addr value);
int stdset_shortcomb_name_(Execute ptr, addr pos, addr value);
int stdset_shortcomb_documentation_(Execute ptr, addr pos, addr value);
int stdset_shortcomb_identity_(Execute ptr, addr pos, addr value);
int stdset_shortcomb_operator_(Execute ptr, addr pos, addr value);
int stdset_shortcomb_order_(Execute ptr, addr pos, addr value);
int stdset_longdef_name_(Execute ptr, addr pos, addr value);
int stdset_longdef_documentation_(Execute ptr, addr pos, addr value);
int stdset_longdef_lambda_list_(Execute ptr, addr pos, addr value);
int stdset_longdef_qualifiers_(Execute ptr, addr pos, addr value);
int stdset_longdef_arguments_(Execute ptr, addr pos, addr value);
int stdset_longdef_generic_(Execute ptr, addr pos, addr value);
int stdset_longdef_form_(Execute ptr, addr pos, addr value);
int stdset_shortdef_name_(Execute ptr, addr pos, addr value);
int stdset_shortdef_documentation_(Execute ptr, addr pos, addr value);
int stdset_shortdef_identity_(Execute ptr, addr pos, addr value);
int stdset_shortdef_operator_(Execute ptr, addr pos, addr value);

int stdboundp_longcomb_name_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_documentation_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_lambda_list_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_binding_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_qualifiers_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_arguments_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_generic_(Execute ptr, addr pos, int *ret);
int stdboundp_longcomb_form_(Execute ptr, addr pos, int *ret);
int stdboundp_shortcomb_name_(Execute ptr, addr pos, int *ret);
int stdboundp_shortcomb_documentation_(Execute ptr, addr pos, int *ret);
int stdboundp_shortcomb_identity_(Execute ptr, addr pos, int *ret);
int stdboundp_shortcomb_operator_(Execute ptr, addr pos, int *ret);
int stdboundp_shortcomb_order_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_name_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_documentation_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_lambda_list_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_qualifiers_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_arguments_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_generic_(Execute ptr, addr pos, int *ret);
int stdboundp_longdef_form_(Execute ptr, addr pos, int *ret);
int stdboundp_shortdef_name_(Execute ptr, addr pos, int *ret);
int stdboundp_shortdef_documentation_(Execute ptr, addr pos, int *ret);
int stdboundp_shortdef_identity_(Execute ptr, addr pos, int *ret);
int stdboundp_shortdef_operator_(Execute ptr, addr pos, int *ret);

#endif

