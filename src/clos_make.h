#ifndef __CLOS_MAKE_HEADER__
#define __CLOS_MAKE_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define clos_ensure_class_supers_ _n(clos_ensure_class_supers_)
#define clos_ensure_class_slots_ _n(clos_ensure_class_slots_)
#define clos_ensure_class_direct_default_initargs_ _n(clos_ensure_class_direct_default_initargs_)
#define clos_ensure_class_init_ _n(clos_ensure_class_init_)
#define clos_finalize_ _n(clos_finalize_)
#define clos_ensure_class_ _n(clos_ensure_class_)
#define allocate_instance_standard_ _n(allocate_instance_standard_)
#define initialize_instance_stdobject_ _n(initialize_instance_stdobject_)
#define reinitialize_instance_stdobject_ _n(reinitialize_instance_stdobject_)
#define shared_initialize_stdobject_ _n(shared_initialize_stdobject_)
#define make_instance_stdclass_ _n(make_instance_stdclass_)
#define slot_boundp_using_class_common_ _n(slot_boundp_using_class_common_)
#define slot_makunbound_using_class_ _n(slot_makunbound_using_class_)
#define slot_value_using_class_common_ _n(slot_value_using_class_common_)
#define setf_slot_value_using_class_common_ _n(setf_slot_value_using_class_common_)
#define init_clos_make _n(init_clos_make)

int clos_ensure_class_supers_(addr args, addr *ret, int *referp);
int clos_ensure_class_slots_(addr args, addr *ret);
int clos_ensure_class_direct_default_initargs_(LocalRoot local,
		addr pos, addr args, addr *ret);
int clos_ensure_class_init_(Execute ptr, addr pos, int pushp);

int clos_finalize_(Execute ptr, addr pos, int *ret);
int clos_ensure_class_(Execute ptr, addr name, addr args, addr *ret);
int allocate_instance_standard_(Execute ptr, addr clos, addr *ret);
int initialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret);
int reinitialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret);
int shared_initialize_stdobject_(Execute ptr, addr pos, addr name, addr rest);
int make_instance_stdclass_(Execute ptr, addr rest, addr *ret);
int slot_boundp_using_class_common_(Execute ptr,
		addr clos, addr pos, addr name, int *ret);
int slot_makunbound_using_class_(Execute ptr, addr clos, addr pos, addr key);
int slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret);
int setf_slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr value);

void init_clos_make(void);

#endif

