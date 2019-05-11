#ifndef __CLOS_CLASS_HEADER__
#define __CLOS_CLASS_HEADER__

#include "local.h"
#include "typedef.h"

/* access */
void stdget_class_name(addr pos, addr *ret);
void stdset_class_name(addr pos, addr value);
void stdget_class_direct_slots(addr pos, addr *ret);
void stdset_class_direct_slots(addr pos, addr value);
void stdget_class_direct_subclasses(addr pos, addr *ret);
void stdset_class_direct_subclasses(addr pos, addr value);
void stdget_class_direct_superclasses(addr pos, addr *ret);
void stdset_class_direct_superclasses(addr pos, addr value);
void stdget_class_precedence_list(addr pos, addr *ret);
void stdset_class_precedence_list(addr pos, addr value);
void stdget_class_slots(addr pos, addr *ret);
void stdset_class_slots(addr pos, addr value);
void stdget_class_finalized_p(addr pos, addr *ret);
void stdset_class_finalized_p(addr pos, addr value);
void stdget_class_prototype(addr pos, addr *ret);
void stdset_class_prototype(addr pos, addr value);
void stdget_class_direct_methods(addr pos, addr *ret);
void stdset_class_direct_methods(addr pos, addr value);
void stdget_class_default_initargs(addr pos, addr *ret);
void stdset_class_default_initargs(addr pos, addr value);
void stdget_class_direct_default_initargs(addr pos, addr *ret);
void stdset_class_direct_default_initargs(addr pos, addr value);
void stdget_class_version(addr pos, addr *ret);
void stdset_class_version(addr pos, addr value);
void stdget_class_document(addr pos, addr *ret);
void stdset_class_document(addr pos, addr value);
void stdget_class_redefined_class(addr pos, addr *ret);
void stdset_class_redefined_class(addr pos, addr value);

/* check */
int clos_subclass_p(addr clos, addr super);
int clos_subtype_p(addr clos, addr super);
int clos_class_p(addr clos);
int clos_funcallable_p(addr clos);
int clos_generic_p(addr clos);
int clos_method_p(addr clos);

int clos_define_combination_p(addr pos);
int clos_define_long_combination_p(addr pos);
int clos_define_short_combination_p(addr pos);
int clos_combination_p(addr pos);
int clos_long_combination_p(addr pos);
int clos_short_combination_p(addr pos);

int clos_specializer_p(addr clos);
int clos_referenced_p(addr clos);
int clos_built_p(addr clos);
int funcallp(addr pos);

/* make-instance */
void clos_instance_alloc(LocalRoot local, addr clos, addr *ret);
void clos_instance_local(LocalRoot local, addr clos, addr *ret);
void clos_instance_heap(addr clos, addr *ret);

/* interface */
void slotvector_set_location(addr slots);
int clos_ensure_class(Execute ptr, addr name, addr args, addr *ret);
int clos_ensure_class_redefine(Execute ptr, addr clos, addr name, addr rest);
void allocate_instance_stdclass(Execute ptr, addr clos, addr *ret);
int initialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
int reinitialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
int shared_initialize_stdobject(Execute ptr, addr pos, addr name, addr rest);
int make_instance_stdclass(Execute ptr, addr rest, addr *ret);
int clos_version_diff_p(addr pos);
int clos_version_check(Execute ptr, addr pos, addr clos);
int clos_change_class(Execute ptr, addr pos, addr clos, addr rest, addr *ret);
int clos_slot_missing(Execute ptr,
		addr clos, addr pos, addr name, addr operation, addr value);
int clos_slot_unbound(Execute ptr, addr clos, addr pos, addr name);
int slot_boundp_using_class_common(Execute ptr,
		addr clos, addr pos, addr name, int *ret);
int slot_makunbound_using_class(Execute ptr, addr clos, addr pos, addr key);
int slot_value_using_class_common(Execute ptr,
		addr clos, addr pos, addr key, addr *ret);
int setf_slot_value_using_class_common(Execute ptr,
		addr clos, addr pos, addr key, addr value);

/* build */
void build_clos_class(LocalRoot local);

#endif

