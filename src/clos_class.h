#ifndef __CLOS_CLASS_HEADER__
#define __CLOS_CLASS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

/* access */
_g int stdget_class_name_(addr pos, addr *ret);
_g int stdset_class_name_(addr pos, addr value);
_g int stdget_class_direct_slots_(addr pos, addr *ret);
_g int stdset_class_direct_slots_(addr pos, addr value);
_g int stdget_class_direct_subclasses_(addr pos, addr *ret);
_g int stdset_class_direct_subclasses_(addr pos, addr value);
_g int stdget_class_direct_superclasses_(addr pos, addr *ret);
_g int stdset_class_direct_superclasses_(addr pos, addr value);
_g int stdget_class_precedence_list_(addr pos, addr *ret);
_g int stdset_class_precedence_list_(addr pos, addr value);
_g int stdget_class_slots_(addr pos, addr *ret);
_g int stdset_class_slots_(addr pos, addr value);
_g int stdget_class_finalized_p_(addr pos, addr *ret);
_g int stdset_class_finalized_p_(addr pos, addr value);
_g int stdget_class_prototype_(addr pos, addr *ret);
_g int stdset_class_prototype_(addr pos, addr value);
_g int stdget_class_direct_methods_(addr pos, addr *ret);
_g int stdset_class_direct_methods_(addr pos, addr value);
_g int stdget_class_default_initargs_(addr pos, addr *ret);
_g int stdset_class_default_initargs_(addr pos, addr value);
_g int stdget_class_direct_default_initargs_(addr pos, addr *ret);
_g int stdset_class_direct_default_initargs_(addr pos, addr value);
_g int stdget_class_version_(addr pos, addr *ret);
_g int stdset_class_version_(addr pos, addr value);
_g int stdget_class_document_(addr pos, addr *ret);
_g int stdset_class_document_(addr pos, addr value);
_g int stdget_class_redefined_class_(addr pos, addr *ret);
_g int stdset_class_redefined_class_(addr pos, addr value);

/* check */
_g int clos_subclass_p_(addr clos, addr super, int *ret);
_g int clos_subtype_p_(addr clos, addr super, int *ret);
_g int clos_class_p_(addr clos, int *ret);
_g int clos_funcallable_p_(addr clos, int *ret);
_g int clos_generic_p_(addr clos, int *ret);
_g int clos_method_p_(addr clos, int *ret);

_g int clos_define_combination_p_(addr clos, int *ret);
_g int clos_define_long_combination_p_(addr pos, int *ret);
_g int clos_define_short_combination_p_(addr pos, int *ret);
_g int clos_combination_p_(addr pos, int *ret);
_g int clos_long_combination_p_(addr pos, int *ret);
_g int clos_short_combination_p_(addr pos, int *ret);

_g int clos_specializer_p_(addr clos, int *ret);
_g int clos_referenced_p_(addr clos, int *ret);
_g int clos_built_p_(addr clos, int *ret);
_g int funcallp_(addr pos, int *ret);

/* make-instance */
_g int clos_instance_alloc_(LocalRoot local, addr clos, addr *ret);
_g int clos_instance_local_(LocalRoot local, addr clos, addr *ret);
_g int clos_instance_heap_(addr clos, addr *ret);

/* interface */
_g int clos_find_slotname(addr slots, size_t size, addr name);
_g int clos_precedence_list_redefine_(
		LocalRoot local, addr pos, addr *ret, addr x, addr list);
_g int clos_precedence_list_(LocalRoot local, addr pos, addr *ret);
_g int clos_compute_slots_(LocalRoot local, addr clos, addr *ret);
_g void slotvector_set_location(addr slots);
_g int clos_stdclass_direct_slots_(addr instance, addr slots);
_g int clos_stdclass_prototype_(addr clos);

/* build */
_g void build_clos_class(LocalRoot local);

/* debug */
_g int clos_subclass_p_debug(addr clos, addr super);
_g int clos_subtype_p_debug(addr clos, addr super);
_g int clos_generic_p_debug(addr clos);
_g int clos_method_p_debug(addr clos);
_g int clos_define_combination_p_debug(addr clos);

#endif

