#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"

/* access */
_g void stdget_longcomb_name(addr pos, addr *ret);
_g void stdset_longcomb_name(addr pos, addr value);
_g void stdget_longcomb_document(addr pos, addr *ret);
_g void stdset_longcomb_document(addr pos, addr value);
_g void stdget_longcomb_lambda_list(addr pos, addr *ret);
_g void stdset_longcomb_lambda_list(addr pos, addr value);
_g void stdget_longcomb_qualifiers(addr pos, addr *ret);
_g void stdset_longcomb_qualifiers(addr pos, addr value);
_g void stdget_longcomb_arguments(addr pos, addr *ret);
_g void stdset_longcomb_arguments(addr pos, addr value);
_g void stdget_longcomb_generic(addr pos, addr *ret);
_g void stdset_longcomb_generic(addr pos, addr value);
_g void stdget_longcomb_form(addr pos, addr *ret);
_g void stdset_longcomb_form(addr pos, addr value);
_g void stdget_longcomb_function(addr pos, addr *ret);
_g void stdset_longcomb_function(addr pos, addr value);
_g void stdget_longcomb_binding(addr pos, addr *ret);
_g void stdset_longcomb_binding(addr pos, addr value);

_g void stdget_shortcomb_name(addr pos, addr *ret);
_g void stdset_shortcomb_name(addr pos, addr value);
_g void stdget_shortcomb_document(addr pos, addr *ret);
_g void stdset_shortcomb_document(addr pos, addr value);
_g void stdget_shortcomb_identity(addr pos, addr *ret);
_g void stdset_shortcomb_identity(addr pos, addr value);
_g void stdget_shortcomb_operator(addr pos, addr *ret);
_g void stdset_shortcomb_operator(addr pos, addr value);
_g void stdget_shortcomb_order(addr pos, addr *ret);
_g void stdset_shortcomb_order(addr pos, addr value);

_g void stdget_longdef_name(addr pos, addr *ret);
_g void stdset_longdef_name(addr pos, addr value);
_g void stdget_longdef_document(addr pos, addr *ret);
_g void stdset_longdef_document(addr pos, addr value);
_g void stdget_longdef_lambda_list(addr pos, addr *ret);
_g void stdset_longdef_lambda_list(addr pos, addr value);
_g void stdget_longdef_qualifiers(addr pos, addr *ret);
_g void stdset_longdef_qualifiers(addr pos, addr value);
_g void stdget_longdef_arguments(addr pos, addr *ret);
_g void stdset_longdef_arguments(addr pos, addr value);
_g void stdget_longdef_generic(addr pos, addr *ret);
_g void stdset_longdef_generic(addr pos, addr value);
_g void stdget_longdef_form(addr pos, addr *ret);
_g void stdset_longdef_form(addr pos, addr value);

_g void stdget_shortdef_name(addr pos, addr *ret);
_g void stdset_shortdef_name(addr pos, addr value);
_g void stdget_shortdef_document(addr pos, addr *ret);
_g void stdset_shortdef_document(addr pos, addr value);
_g void stdget_shortdef_identity(addr pos, addr *ret);
_g void stdset_shortdef_identity(addr pos, addr value);
_g void stdget_shortdef_operator(addr pos, addr *ret);
_g void stdset_shortdef_operator(addr pos, addr value);

/* control */
_g int check_qualifiers_equal(Execute ptr, addr combination, addr qualifiers);
_g void method_combination_qualifiers_count(addr combination, size_t *ret);
_g int qualifiers_position_nil(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
_g void qualifiers_position(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
_g void build_clos_combination(void);

/* generic-function */
_g void clos_find_method_combination(addr gen, addr list, addr *ret);
_g void ensure_define_combination_short_common(addr name,
		addr doc, addr ident, addr oper);
_g void ensure_define_combination_long_common(addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form, addr decl);

/* long form */
_g void comb_longmacro(addr *ret,
		addr lambda, addr spec, addr args, addr gen, addr decl, addr form);
_g int comb_longform(Execute ptr, addr *ret, addr gen, addr comb, addr data);
_g int comb_shortform(Execute ptr, addr *ret, addr gen, addr comb, addr data);

#endif

