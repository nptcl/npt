#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"

#define stdget_longcomb_name_ _n(stdget_longcomb_name_)
#define stdset_longcomb_name_ _n(stdset_longcomb_name_)
#define stdget_longcomb_document_ _n(stdget_longcomb_document_)
#define stdset_longcomb_document_ _n(stdset_longcomb_document_)
#define stdget_longcomb_lambda_list_ _n(stdget_longcomb_lambda_list_)
#define stdset_longcomb_lambda_list_ _n(stdset_longcomb_lambda_list_)
#define stdget_longcomb_qualifiers_ _n(stdget_longcomb_qualifiers_)
#define stdset_longcomb_qualifiers_ _n(stdset_longcomb_qualifiers_)
#define stdget_longcomb_arguments_ _n(stdget_longcomb_arguments_)
#define stdset_longcomb_arguments_ _n(stdset_longcomb_arguments_)
#define stdget_longcomb_generic_ _n(stdget_longcomb_generic_)
#define stdset_longcomb_generic_ _n(stdset_longcomb_generic_)
#define stdget_longcomb_form_ _n(stdget_longcomb_form_)
#define stdset_longcomb_form_ _n(stdset_longcomb_form_)
#define stdget_longcomb_function_ _n(stdget_longcomb_function_)
#define stdset_longcomb_function_ _n(stdset_longcomb_function_)
#define stdget_longcomb_binding_ _n(stdget_longcomb_binding_)
#define stdset_longcomb_binding_ _n(stdset_longcomb_binding_)
#define stdget_shortcomb_name_ _n(stdget_shortcomb_name_)
#define stdset_shortcomb_name_ _n(stdset_shortcomb_name_)
#define stdget_shortcomb_document_ _n(stdget_shortcomb_document_)
#define stdset_shortcomb_document_ _n(stdset_shortcomb_document_)
#define stdget_shortcomb_identity_ _n(stdget_shortcomb_identity_)
#define stdset_shortcomb_identity_ _n(stdset_shortcomb_identity_)
#define stdget_shortcomb_operator_ _n(stdget_shortcomb_operator_)
#define stdset_shortcomb_operator_ _n(stdset_shortcomb_operator_)
#define stdget_shortcomb_order_ _n(stdget_shortcomb_order_)
#define stdset_shortcomb_order_ _n(stdset_shortcomb_order_)
#define stdget_longdef_name_ _n(stdget_longdef_name_)
#define stdset_longdef_name_ _n(stdset_longdef_name_)
#define stdget_longdef_document_ _n(stdget_longdef_document_)
#define stdset_longdef_document_ _n(stdset_longdef_document_)
#define stdget_longdef_lambda_list_ _n(stdget_longdef_lambda_list_)
#define stdset_longdef_lambda_list_ _n(stdset_longdef_lambda_list_)
#define stdget_longdef_qualifiers_ _n(stdget_longdef_qualifiers_)
#define stdset_longdef_qualifiers_ _n(stdset_longdef_qualifiers_)
#define stdget_longdef_arguments_ _n(stdget_longdef_arguments_)
#define stdset_longdef_arguments_ _n(stdset_longdef_arguments_)
#define stdget_longdef_generic_ _n(stdget_longdef_generic_)
#define stdset_longdef_generic_ _n(stdset_longdef_generic_)
#define stdget_longdef_form_ _n(stdget_longdef_form_)
#define stdset_longdef_form_ _n(stdset_longdef_form_)
#define stdget_shortdef_name_ _n(stdget_shortdef_name_)
#define stdset_shortdef_name_ _n(stdset_shortdef_name_)
#define stdget_shortdef_document_ _n(stdget_shortdef_document_)
#define stdset_shortdef_document_ _n(stdset_shortdef_document_)
#define stdget_shortdef_identity_ _n(stdget_shortdef_identity_)
#define stdset_shortdef_identity_ _n(stdset_shortdef_identity_)
#define stdget_shortdef_operator_ _n(stdget_shortdef_operator_)
#define stdset_shortdef_operator_ _n(stdset_shortdef_operator_)
#define check_qualifiers_equal_ _n(check_qualifiers_equal_)
#define method_combination_qualifiers_count_ _n(method_combination_qualifiers_count_)
#define qualifiers_position_nil_ _n(qualifiers_position_nil_)
#define qualifiers_position_ _n(qualifiers_position_)
#define build_clos_combination _n(build_clos_combination)
#define clos_find_method_combination_ _n(clos_find_method_combination_)
#define ensure_define_combination_short_common_ _n(ensure_define_combination_short_common_)
#define ensure_define_combination_long_common_ _n(ensure_define_combination_long_common_)
#define comb_longmacro_ _n(comb_longmacro_)
#define comb_longform_ _n(comb_longform_)
#define comb_shortform_ _n(comb_shortform_)

/* access */
_g int stdget_longcomb_name_(addr pos, addr *ret);
_g int stdset_longcomb_name_(addr pos, addr value);
_g int stdget_longcomb_document_(addr pos, addr *ret);
_g int stdset_longcomb_document_(addr pos, addr value);
_g int stdget_longcomb_lambda_list_(addr pos, addr *ret);
_g int stdset_longcomb_lambda_list_(addr pos, addr value);
_g int stdget_longcomb_qualifiers_(addr pos, addr *ret);
_g int stdset_longcomb_qualifiers_(addr pos, addr value);
_g int stdget_longcomb_arguments_(addr pos, addr *ret);
_g int stdset_longcomb_arguments_(addr pos, addr value);
_g int stdget_longcomb_generic_(addr pos, addr *ret);
_g int stdset_longcomb_generic_(addr pos, addr value);
_g int stdget_longcomb_form_(addr pos, addr *ret);
_g int stdset_longcomb_form_(addr pos, addr value);
_g int stdget_longcomb_function_(addr pos, addr *ret);
_g int stdset_longcomb_function_(addr pos, addr value);
_g int stdget_longcomb_binding_(addr pos, addr *ret);
_g int stdset_longcomb_binding_(addr pos, addr value);

_g int stdget_shortcomb_name_(addr pos, addr *ret);
_g int stdset_shortcomb_name_(addr pos, addr value);
_g int stdget_shortcomb_document_(addr pos, addr *ret);
_g int stdset_shortcomb_document_(addr pos, addr value);
_g int stdget_shortcomb_identity_(addr pos, addr *ret);
_g int stdset_shortcomb_identity_(addr pos, addr value);
_g int stdget_shortcomb_operator_(addr pos, addr *ret);
_g int stdset_shortcomb_operator_(addr pos, addr value);
_g int stdget_shortcomb_order_(addr pos, addr *ret);
_g int stdset_shortcomb_order_(addr pos, addr value);

_g int stdget_longdef_name_(addr pos, addr *ret);
_g int stdset_longdef_name_(addr pos, addr value);
_g int stdget_longdef_document_(addr pos, addr *ret);
_g int stdset_longdef_document_(addr pos, addr value);
_g int stdget_longdef_lambda_list_(addr pos, addr *ret);
_g int stdset_longdef_lambda_list_(addr pos, addr value);
_g int stdget_longdef_qualifiers_(addr pos, addr *ret);
_g int stdset_longdef_qualifiers_(addr pos, addr value);
_g int stdget_longdef_arguments_(addr pos, addr *ret);
_g int stdset_longdef_arguments_(addr pos, addr value);
_g int stdget_longdef_generic_(addr pos, addr *ret);
_g int stdset_longdef_generic_(addr pos, addr value);
_g int stdget_longdef_form_(addr pos, addr *ret);
_g int stdset_longdef_form_(addr pos, addr value);

_g int stdget_shortdef_name_(addr pos, addr *ret);
_g int stdset_shortdef_name_(addr pos, addr value);
_g int stdget_shortdef_document_(addr pos, addr *ret);
_g int stdset_shortdef_document_(addr pos, addr value);
_g int stdget_shortdef_identity_(addr pos, addr *ret);
_g int stdset_shortdef_identity_(addr pos, addr value);
_g int stdget_shortdef_operator_(addr pos, addr *ret);
_g int stdset_shortdef_operator_(addr pos, addr value);

/* control */
_g int check_qualifiers_equal_(Execute ptr, addr comb, addr qua, int *ret);
_g int method_combination_qualifiers_count_(addr comb, size_t *ret);
_g int qualifiers_position_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret);
_g int qualifiers_position_(Execute ptr, addr qua, addr comb, size_t *rsize, int *ret);
_g void build_clos_combination(void);

/* generic-function */
_g int clos_find_method_combination_(addr gen, addr list, addr *ret);
_g int ensure_define_combination_short_common_(
		addr name, addr doc, addr ident, addr oper);
_g int ensure_define_combination_long_common_(addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form, addr decl);

/* long form */
_g int comb_longmacro_(addr *ret,
		addr lambda, addr spec, addr args, addr gen, addr decl, addr form);
_g int comb_longform_(Execute ptr, addr *ret, addr gen, addr comb, addr data);
_g int comb_shortform_(Execute ptr, addr *ret, addr gen, addr comb, addr data);

#endif

