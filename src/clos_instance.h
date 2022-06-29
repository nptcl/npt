#ifndef __CLOS_INSTANCE_HEADER__
#define __CLOS_INSTANCE_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define clos_subclass_p_ _n(clos_subclass_p_)
#define clos_subtype_p_ _n(clos_subtype_p_)
#define clos_class_p_ _n(clos_class_p_)
#define clos_funcallable_p_ _n(clos_funcallable_p_)
#define clos_generic_p_ _n(clos_generic_p_)
#define clos_method_p_ _n(clos_method_p_)
#define clos_define_combination_p_ _n(clos_define_combination_p_)
#define clos_define_long_combination_p_ _n(clos_define_long_combination_p_)
#define clos_define_short_combination_p_ _n(clos_define_short_combination_p_)
#define clos_combination_p_ _n(clos_combination_p_)
#define clos_long_combination_p_ _n(clos_long_combination_p_)
#define clos_short_combination_p_ _n(clos_short_combination_p_)
#define clos_specializer_p_ _n(clos_specializer_p_)
#define clos_referenced_p_ _n(clos_referenced_p_)
#define clos_built_p_ _n(clos_built_p_)
#define funcallp_ _n(funcallp_)
#define clos_instance_alloc_ _n(clos_instance_alloc_)
#define clos_instance_local_ _n(clos_instance_local_)
#define clos_instance_heap_ _n(clos_instance_heap_)
#define clos_find_slotname _n(clos_find_slotname)
#define clos_precedence_list_redefine_ _n(clos_precedence_list_redefine_)
#define clos_precedence_list_ _n(clos_precedence_list_)
#define clos_compute_slots_ _n(clos_compute_slots_)
#define slotvector_set_location _n(slotvector_set_location)
#define clos_stdclass_direct_slots_ _n(clos_stdclass_direct_slots_)
#define clos_stdclass_prototype_ _n(clos_stdclass_prototype_)
#define clos_stdclass_supers_ _n(clos_stdclass_supers_)
#define build_clos_class _n(build_clos_class)
#define clos_subclass_p_debug _n(clos_subclass_p_debug)
#define clos_subtype_p_debug _n(clos_subtype_p_debug)
#define clos_generic_p_debug _n(clos_generic_p_debug)
#define clos_method_p_debug _n(clos_method_p_debug)
#define clos_define_combination_p_debug _n(clos_define_combination_p_debug)

/* check */
int clos_subclass_p_(Execute ptr, addr clos, addr super, int *ret);
int clos_subtype_p_(Execute ptr, addr clos, addr super, int *ret);
int clos_class_p_(Execute ptr, addr clos, int *ret);
int clos_funcallable_p_(Execute ptr, addr clos, int *ret);
int clos_generic_p_(Execute ptr, addr clos, int *ret);
int clos_method_p_(Execute ptr, addr clos, int *ret);

int clos_define_combination_p_(Execute ptr, addr clos, int *ret);
int clos_define_long_combination_p_(Execute ptr, addr pos, int *ret);
int clos_define_short_combination_p_(Execute ptr, addr pos, int *ret);
int clos_combination_p_(Execute ptr, addr pos, int *ret);
int clos_long_combination_p_(Execute ptr, addr pos, int *ret);
int clos_short_combination_p_(Execute ptr, addr pos, int *ret);

int clos_specializer_p_(Execute ptr, addr clos, int *ret);
int clos_referenced_p_(addr clos, int *ret);
int clos_built_p_(Execute ptr, addr clos, int *ret);
int funcallp_(Execute ptr, addr pos, int *ret);

/* make-instance */
int clos_instance_alloc_(Execute ptr, LocalRoot local, addr clos, addr *ret);
int clos_instance_local_(Execute ptr, addr clos, addr *ret);
int clos_instance_heap_(Execute ptr, addr clos, addr *ret);

/* interface */
int clos_find_slotname(addr slots, size_t size, addr name);
int clos_precedence_list_redefine_(
		Execute ptr, addr pos, addr *ret, addr x, addr list);
int clos_precedence_list_(Execute ptr, addr pos, addr *ret);
int clos_compute_slots_(Execute ptr, addr clos, addr *ret);
void slotvector_set_location(addr slots);
int clos_stdclass_direct_slots_(Execute ptr, addr instance, addr slots);
int clos_stdclass_prototype_(Execute ptr, addr clos);
int clos_stdclass_supers_(Execute ptr,
		addr *ret, addr metaclass, addr name, addr slots, addr supers);

/* build */
void build_clos_class(Execute ptr);

/* debug */
int clos_subclass_p_debug(addr clos, addr super);
int clos_subtype_p_debug(addr clos, addr super);
int clos_generic_p_debug(addr clos);
int clos_method_p_debug(addr clos);
int clos_define_combination_p_debug(addr clos);

#endif

