#ifndef __CLOS_CLASS_HEADER__
#define __CLOS_CLASS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

/* access */
_g void stdget_class_name(addr pos, addr *ret);
_g void stdset_class_name(addr pos, addr value);
_g void stdget_class_direct_slots(addr pos, addr *ret);
_g void stdset_class_direct_slots(addr pos, addr value);
_g void stdget_class_direct_subclasses(addr pos, addr *ret);
_g void stdset_class_direct_subclasses(addr pos, addr value);
_g void stdget_class_direct_superclasses(addr pos, addr *ret);
_g void stdset_class_direct_superclasses(addr pos, addr value);
_g void stdget_class_precedence_list(addr pos, addr *ret);
_g void stdset_class_precedence_list(addr pos, addr value);
_g void stdget_class_slots(addr pos, addr *ret);
_g void stdset_class_slots(addr pos, addr value);
_g void stdget_class_finalized_p(addr pos, addr *ret);
_g void stdset_class_finalized_p(addr pos, addr value);
_g void stdget_class_prototype(addr pos, addr *ret);
_g void stdset_class_prototype(addr pos, addr value);
_g void stdget_class_direct_methods(addr pos, addr *ret);
_g void stdset_class_direct_methods(addr pos, addr value);
_g void stdget_class_default_initargs(addr pos, addr *ret);
_g void stdset_class_default_initargs(addr pos, addr value);
_g void stdget_class_direct_default_initargs(addr pos, addr *ret);
_g void stdset_class_direct_default_initargs(addr pos, addr value);
_g void stdget_class_version(addr pos, addr *ret);
_g void stdset_class_version(addr pos, addr value);
_g void stdget_class_document(addr pos, addr *ret);
_g void stdset_class_document(addr pos, addr value);
_g void stdget_class_redefined_class(addr pos, addr *ret);
_g void stdset_class_redefined_class(addr pos, addr value);

/* check */
_g int clos_subclass_p(addr clos, addr super);
_g int clos_subtype_p(addr clos, addr super);
_g int clos_class_p(addr clos);
_g int clos_funcallable_p(addr clos);
_g int clos_generic_p(addr clos);
_g int clos_method_p(addr clos);

_g int clos_define_combination_p(addr pos);
_g int clos_define_long_combination_p(addr pos);
_g int clos_define_short_combination_p(addr pos);
_g int clos_combination_p(addr pos);
_g int clos_long_combination_p(addr pos);
_g int clos_short_combination_p(addr pos);

_g int clos_specializer_p(addr clos);
_g int clos_referenced_p(addr clos);
_g int clos_built_p(addr clos);
_g int funcallp(addr pos);

/* make-instance */
_g void clos_instance_alloc(LocalRoot local, addr clos, addr *ret);
_g void clos_instance_local(LocalRoot local, addr clos, addr *ret);
_g void clos_instance_heap(addr clos, addr *ret);

/* interface */
_g int clos_find_slotname(addr slots, size_t size, addr name);
_g void clos_precedence_list_redefine(
		LocalRoot local, addr pos, addr *ret, addr x, addr list);
_g void clos_precedence_list(LocalRoot local, addr clos, addr *ret);
_g void clos_compute_slots(LocalRoot local, addr clos, addr *ret);
_g void slotvector_set_location(addr slots);
_g void clos_stdclass_direct_slots(addr instance, addr slots);
_g void clos_stdclass_prototype(addr clos);

/* build */
_g void build_clos_class(LocalRoot local);

#endif

