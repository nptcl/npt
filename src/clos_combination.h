#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"

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

