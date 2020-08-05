#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"
#include "local.h"

struct generic_argument {
	Execute ptr;
	addr env, name, lambda, generic, method, combination, order, declare, doc;
};

/* access */
_g int stdget_generic_name_(addr pos, addr *ret);
_g int stdset_generic_name_(addr pos, addr value);
_g int stdget_generic_lambda_list_(addr pos, addr *ret);
_g int stdset_generic_lambda_list_(addr pos, addr value);
_g int stdget_generic_methods_(addr pos, addr *ret);
_g int stdset_generic_methods_(addr pos, addr value);
_g int stdget_generic_method_class_(addr pos, addr *ret);
_g int stdset_generic_method_class_(addr pos, addr value);
_g int stdget_generic_argument_precedence_order_(addr pos, addr *ret);
_g int stdset_generic_argument_precedence_order_(addr pos, addr value);
_g int stdget_generic_declarations_(addr pos, addr *ret);
_g int stdset_generic_declarations_(addr pos, addr value);
_g int stdget_generic_method_combination_(addr pos, addr *ret);
_g int stdset_generic_method_combination_(addr pos, addr value);
_g int stdget_generic_eqlcheck_(addr pos, addr *ret);
_g int stdset_generic_eqlcheck_(addr pos, addr value);
_g int stdget_generic_cache_(addr pos, addr *ret);
_g int stdset_generic_cache_(addr pos, addr value);
_g int stdget_generic_call_(addr pos, addr *ret);
_g int stdset_generic_call_(addr pos, addr value);
_g int stdget_generic_precedence_index_(addr pos, addr *ret);
_g int stdset_generic_precedence_index_(addr pos, addr value);
_g int stdboundp_generic_argument_precedence_order_(addr pos, int *ret);
_g int stdboundp_generic_eqlcheck_(addr pos, int *ret);

/* eql-specializer */
_g int stdget_specializer_object_(addr pos, addr *ret);
_g int stdset_specializer_object_(addr pos, addr value);
_g int stdget_specializer_type_(addr pos, addr *ret);
_g int stdset_specializer_type_(addr pos, addr value);

/* generic-function */
_g int generic_eql_specializer_(addr left, addr right, int check, int *ret);
_g int generic_finalize_(addr gen);
_g int closrun_execute_(Execute ptr, addr clos, addr args);

_g int generic_common_instance_(addr *ret, addr name, addr args);
_g int generic_common_order_(addr gen, addr order, addr list);
_g int ensure_generic_function_common_(Execute ptr, addr name, addr rest, addr *ret);
_g int generic_empty_(addr name, addr lambda, addr *ret);
_g int generic_add_(struct generic_argument *ptr, addr *ret);
_g int generic_change_(struct generic_argument *str, addr *ret);

/* common */
_g int generic_compute_applicable_methods_(LocalRoot local,
		addr gen, addr args, addr *ret);
_g int generic_find_method_(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret);

/* initialize */
_g void init_clos_generic(void);

#endif

