#ifndef __CLOS_DEFGENERIC_HEADER__
#define __CLOS_DEFGENERIC_HEADER__

#include "execute.h"
#include "clos_generic.h"
#include "typedef.h"

#define generic_cache_heap _n(generic_cache_heap)
#define generic_make_ _n(generic_make_)
#define generic_make_empty_ _n(generic_make_empty_)
#define ensure_generic_function_common_ _n(ensure_generic_function_common_)
#define mop_generic_new_ _n(mop_generic_new_)
#define mop_generic_change_ _n(mop_generic_change_)
#define system_generic_define_ _n(system_generic_define_)
#define system_generic_method_ _n(system_generic_method_)

void generic_cache_heap(addr *ret);
int generic_make_(addr *ret, addr call, addr args);
int generic_make_empty_(addr call, addr lambda, addr *ret);
int ensure_generic_function_common_(Execute ptr, addr name, addr rest, addr *ret);
int mop_generic_new_(Execute ptr, addr name, addr rest, addr *ret);
int mop_generic_change_(Execute ptr, addr clos, addr name, addr rest);
int system_generic_define_(Execute ptr, addr name, addr args, addr *ret);
int system_generic_method_(addr gen, addr method);

#endif

