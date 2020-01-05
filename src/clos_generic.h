#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"
#include "local.h"

struct generic_argument {
	Execute ptr;
	addr env, name, lambda, generic, method, combination, order, declare, doc;
};

/* access */
_g void stdget_generic_name(addr pos, addr *ret);
_g void stdset_generic_name(addr pos, addr value);
_g void stdget_generic_lambda_list(addr pos, addr *ret);
_g void stdset_generic_lambda_list(addr pos, addr value);
_g void stdget_generic_methods(addr pos, addr *ret);
_g void stdset_generic_methods(addr pos, addr value);
_g void stdget_generic_method_class(addr pos, addr *ret);
_g void stdset_generic_method_class(addr pos, addr value);
_g void stdget_generic_argument_precedence_order(addr pos, addr *ret);
_g void stdset_generic_argument_precedence_order(addr pos, addr value);
_g void stdget_generic_declarations(addr pos, addr *ret);
_g void stdset_generic_declarations(addr pos, addr value);
_g void stdget_generic_method_combination(addr pos, addr *ret);
_g void stdset_generic_method_combination(addr pos, addr value);
_g void stdget_generic_eqlcheck(addr pos, addr *ret);
_g void stdset_generic_eqlcheck(addr pos, addr value);
_g void stdget_generic_cache(addr pos, addr *ret);
_g void stdset_generic_cache(addr pos, addr value);
_g void stdget_generic_call(addr pos, addr *ret);
_g void stdset_generic_call(addr pos, addr value);
_g void stdget_generic_precedence_index(addr pos, addr *ret);
_g void stdset_generic_precedence_index(addr pos, addr value);
_g int stdboundp_generic_argument_precedence_order(addr pos);
_g int stdboundp_generic_eqlcheck(addr pos);

/* eql-specializer */
_g void stdget_specializer_object(addr pos, addr *ret);
_g void stdset_specializer_object(addr pos, addr value);
_g void stdget_specializer_type(addr pos, addr *ret);
_g void stdset_specializer_type(addr pos, addr value);

/* generic-function */
_g int generic_eql_specializer(addr left, addr right, int check);
_g void generic_finalize(addr gen);
_g void closrun_execute(Execute ptr, addr pos, addr args);

_g void generic_common_instance(addr *ret, addr name, addr args);
_g void generic_common_order(addr gen, addr order, addr list);
_g int ensure_generic_function_common(Execute ptr, addr name, addr rest, addr *ret);
_g void generic_empty(addr name, addr lambda, addr *ret);
_g int generic_add(struct generic_argument *ptr, addr *ret);
_g int generic_change(struct generic_argument *ptr, addr *ret);

/* common */
_g void generic_compute_applicable_methods(LocalRoot local,
		addr gen, addr args, addr *ret);
_g void generic_find_method(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret);

/* initialize */
_g void init_clos_generic(void);

#endif

