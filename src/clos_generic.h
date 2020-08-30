#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"
#include "local.h"

#define stdget_generic_name_ _n(stdget_generic_name_)
#define stdset_generic_name_ _n(stdset_generic_name_)
#define stdget_generic_lambda_list_ _n(stdget_generic_lambda_list_)
#define stdset_generic_lambda_list_ _n(stdset_generic_lambda_list_)
#define stdget_generic_methods_ _n(stdget_generic_methods_)
#define stdset_generic_methods_ _n(stdset_generic_methods_)
#define stdget_generic_method_class_ _n(stdget_generic_method_class_)
#define stdset_generic_method_class_ _n(stdset_generic_method_class_)
#define stdget_generic_argument_precedence_order_ _n(stdget_generic_argument_precedence_order_)
#define stdset_generic_argument_precedence_order_ _n(stdset_generic_argument_precedence_order_)
#define stdget_generic_declarations_ _n(stdget_generic_declarations_)
#define stdset_generic_declarations_ _n(stdset_generic_declarations_)
#define stdget_generic_method_combination_ _n(stdget_generic_method_combination_)
#define stdset_generic_method_combination_ _n(stdset_generic_method_combination_)
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
#define stdget_specializer_object_ _n(stdget_specializer_object_)
#define stdset_specializer_object_ _n(stdset_specializer_object_)
#define stdget_specializer_type_ _n(stdget_specializer_type_)
#define stdset_specializer_type_ _n(stdset_specializer_type_)
#define generic_eql_specializer_ _n(generic_eql_specializer_)
#define generic_finalize_ _n(generic_finalize_)
#define closrun_execute_ _n(closrun_execute_)
#define generic_common_instance_ _n(generic_common_instance_)
#define generic_common_order_ _n(generic_common_order_)
#define ensure_generic_function_common_ _n(ensure_generic_function_common_)
#define generic_empty_ _n(generic_empty_)
#define generic_add_ _n(generic_add_)
#define generic_change_ _n(generic_change_)
#define generic_compute_applicable_methods_ _n(generic_compute_applicable_methods_)
#define generic_find_method_ _n(generic_find_method_)
#define init_clos_generic _n(init_clos_generic)

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

