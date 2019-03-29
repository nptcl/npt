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
void stdget_class_direct_shared(addr pos, addr *ret);
void stdset_class_direct_shared(addr pos, addr value);
void stdget_class_default_initargs(addr pos, addr *ret);
void stdset_class_default_initargs(addr pos, addr value);
void stdget_class_direct_default_initargs(addr pos, addr *ret);
void stdset_class_direct_default_initargs(addr pos, addr value);
void stdget_class_version(addr pos, addr *ret);
void stdset_class_version(addr pos, addr value);
void stdget_class_update_info(addr pos, addr *ret);
void stdset_class_update_info(addr pos, addr value);
void stdget_class_document(addr pos, addr *ret);
void stdset_class_document(addr pos, addr value);

/* check */
int clos_subclass_p(addr clos, addr super);
int clos_subtype_p(addr clos, addr super);
int clos_class_p(addr clos);
int clos_funcallable_p(addr clos);
int clos_generic_p(addr clos);
int clos_method_p(addr clos);
int clos_combination_p(addr clos);
int clos_specializer_p(addr clos);
int funcallp(addr pos);

/* make-instance */
void clos_instance_alloc(LocalRoot local, addr clos, addr *ret);
void clos_instance_local(LocalRoot local, addr clos, addr *ret);
void clos_instance_heap(addr clos, addr *ret);

/* build */
void build_clos_class(LocalRoot local);

#endif

