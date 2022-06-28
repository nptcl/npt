#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define generic_eql_specializer_ _n(generic_eql_specializer_)
#define generic_finalize_ _n(generic_finalize_)
#define closrun_execute_ _n(closrun_execute_)
#define generic_order_ _n(generic_order_)
#define generic_compute_applicable_methods_ _n(generic_compute_applicable_methods_)
#define generic_find_method_ _n(generic_find_method_)
#define get_documentation_function_object_ _n(get_documentation_function_object_)
#define set_documentation_function_object_ _n(set_documentation_function_object_)
#define init_clos_generic _n(init_clos_generic)

/* generic-function */
int generic_eql_specializer_(addr left, addr right, int check, int *ret);
int generic_finalize_(addr gen);
int closrun_execute_(Execute ptr, addr clos, addr args);
int generic_order_(addr gen, addr order, addr list);

/* common */
int generic_compute_applicable_methods_(LocalRoot local,
		addr gen, addr args, addr *ret);
int generic_find_method_(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret);

/* documentation */
int get_documentation_function_object_(addr pos, addr *ret);
int set_documentation_function_object_(addr pos, addr value);

/* initialize */
void init_clos_generic(void);

#endif

