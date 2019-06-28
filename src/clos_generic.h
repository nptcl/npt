#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"
#include "local.h"

struct generic_argument {
	Execute ptr;
	addr env, name, lambda, generic, method, combination, order, declare, doc;
};

/* access */
void stdget_generic_name(addr pos, addr *ret);
void stdset_generic_name(addr pos, addr value);
void stdget_generic_lambda_list(addr pos, addr *ret);
void stdset_generic_lambda_list(addr pos, addr value);
void stdget_generic_methods(addr pos, addr *ret);
void stdset_generic_methods(addr pos, addr value);
void stdget_generic_method_class(addr pos, addr *ret);
void stdset_generic_method_class(addr pos, addr value);
void stdget_generic_argument_precedence_order(addr pos, addr *ret);
void stdset_generic_argument_precedence_order(addr pos, addr value);
void stdget_generic_declarations(addr pos, addr *ret);
void stdset_generic_declarations(addr pos, addr value);
void stdget_generic_method_combination(addr pos, addr *ret);
void stdset_generic_method_combination(addr pos, addr value);
void stdget_generic_eqlcheck(addr pos, addr *ret);
void stdset_generic_eqlcheck(addr pos, addr value);
void stdget_generic_cache(addr pos, addr *ret);
void stdset_generic_cache(addr pos, addr value);
void stdget_generic_call(addr pos, addr *ret);
void stdset_generic_call(addr pos, addr value);
int stdboundp_generic_argument_precedence_order(addr pos);
int stdboundp_generic_eqlcheck(addr pos);

/* eql-specializer */
void stdget_specializer_object(addr pos, addr *ret);
void stdset_specializer_object(addr pos, addr value);
void stdget_specializer_type(addr pos, addr *ret);
void stdset_specializer_type(addr pos, addr value);

/* generic-function */
int generic_eql_specializer(addr left, addr right, int check);
void generic_finalize(addr gen);
void closrun_execute(Execute ptr, addr pos, addr args);

void generic_common_instance(addr *ret, addr name, addr args);
int ensure_generic_function_common(Execute ptr, addr name, addr rest, addr *ret);
void generic_empty(addr name, addr lambda, addr *ret);
int generic_add(struct generic_argument *ptr, addr *ret);
int generic_change(struct generic_argument *ptr, addr *ret);

/* common */
void generic_compute_applicable_methods(LocalRoot local,
		addr gen, addr args, addr *ret);
void generic_find_method(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret);

/* initialize */
void init_clos_generic(void);

#endif

