#ifndef __CLOS_COMBINATION_HEADER__
#define __CLOS_COMBINATION_HEADER__

#include "execute.h"

/* access */
void stdget_combination_name(addr pos, addr *ret);
void stdset_combination_name(addr pos, addr value);
void stdget_combination_long_p(addr pos, addr *ret);
void stdset_combination_long_p(addr pos, addr value);
void stdget_combination_document(addr pos, addr *ret);
void stdset_combination_document(addr pos, addr value);
void stdget_combination_identity(addr pos, addr *ret);
void stdset_combination_identity(addr pos, addr value);
void stdget_combination_operator(addr pos, addr *ret);
void stdset_combination_operator(addr pos, addr value);
void stdget_combination_lambda_list(addr pos, addr *ret);
void stdset_combination_lambda_list(addr pos, addr value);
void stdget_combination_qualifiers(addr pos, addr *ret);
void stdset_combination_qualifiers(addr pos, addr value);
void stdget_combination_arguments(addr pos, addr *ret);
void stdset_combination_arguments(addr pos, addr value);
void stdget_combination_generic(addr pos, addr *ret);
void stdset_combination_generic(addr pos, addr value);
void stdget_combination_form(addr pos, addr *ret);
void stdset_combination_form(addr pos, addr value);
void stdget_combination_function(addr pos, addr *ret);
void stdset_combination_function(addr pos, addr value);

/* control */
int check_qualifiers_equal(Execute ptr, addr combination, addr qualifiers);
void method_combination_qualifiers_count(addr combination, size_t *ret);
int qualifiers_position_nil(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
void qualifiers_position(Execute ptr,
		addr qualifiers, addr combination, size_t *ret);
void build_clos_combination(void);

#endif

