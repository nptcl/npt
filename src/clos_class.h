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
_g void slotvector_set_location(addr slots);
_g int clos_ensure_class(Execute ptr, addr name, addr args, addr *ret);
_g int clos_ensure_class_redefine(Execute ptr, addr clos, addr name, addr rest);
_g void allocate_instance_stdclass(Execute ptr, addr clos, addr *ret);
_g int initialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
_g int reinitialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
_g int shared_initialize_stdobject(Execute ptr, addr pos, addr name, addr rest);
_g int make_instance_stdclass(Execute ptr, addr rest, addr *ret);
_g int clos_version_diff_p(addr pos);
_g int clos_version_check(Execute ptr, addr pos, addr clos);
_g int clos_change_class(Execute ptr, addr pos, addr clos, addr rest, addr *ret);
_g int clos_slot_missing(Execute ptr,
		addr clos, addr pos, addr name, addr operation, addr value);
_g int clos_slot_unbound(Execute ptr, addr clos, addr pos, addr name);
_g int slot_boundp_using_class_common(Execute ptr,
		addr clos, addr pos, addr name, int *ret);
_g int slot_makunbound_using_class(Execute ptr, addr clos, addr pos, addr key);
_g int slot_value_using_class_common(Execute ptr,
		addr clos, addr pos, addr key, addr *ret);
_g int setf_slot_value_using_class_common(Execute ptr,
		addr clos, addr pos, addr key, addr value);

/* build */
_g void build_clos_class(LocalRoot local);

/* initialize */
_g void init_clos_class(void);

#endif

