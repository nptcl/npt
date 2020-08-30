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
#define allocate_instance_stdclass_ _n(allocate_instance_stdclass_)
#define initialize_instance_stdobject_ _n(initialize_instance_stdobject_)
#define reinitialize_instance_stdobject_ _n(reinitialize_instance_stdobject_)
#define shared_initialize_stdobject_ _n(shared_initialize_stdobject_)
#define make_instance_stdclass_ _n(make_instance_stdclass_)
#define clos_slot_missing_ _n(clos_slot_missing_)
#define clos_slot_unbound_ _n(clos_slot_unbound_)
#define slot_boundp_using_class_common_ _n(slot_boundp_using_class_common_)
#define slot_makunbound_using_class_ _n(slot_makunbound_using_class_)
#define slot_value_using_class_common_ _n(slot_value_using_class_common_)
#define setf_slot_value_using_class_common_ _n(setf_slot_value_using_class_common_)
#define init_clos_make _n(init_clos_make)

_g int clos_ensure_class_supers_(addr args, addr *ret, int *referp);
_g int clos_ensure_class_slots_(addr args, addr *ret);
_g int clos_ensure_class_direct_default_initargs_(LocalRoot local,
		addr pos, addr args, addr *ret);
_g int clos_ensure_class_init_(LocalRoot local, addr pos, int pushp);

_g int clos_finalize_(Execute ptr, addr pos, int *ret);
_g int clos_ensure_class_(Execute ptr, addr name, addr args, addr *ret);
_g int allocate_instance_stdclass_(Execute ptr, addr clos, addr *ret);
_g int initialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret);
_g int reinitialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret);
_g int shared_initialize_stdobject_(Execute ptr, addr pos, addr name, addr rest);
_g int make_instance_stdclass_(Execute ptr, addr rest, addr *ret);
_g int clos_slot_missing_(Execute ptr,
		addr clos, addr pos, addr name, addr operation, addr value);
_g int clos_slot_unbound_(Execute ptr, addr clos, addr pos, addr name);
_g int slot_boundp_using_class_common_(Execute ptr,
		addr clos, addr pos, addr name, int *ret);
_g int slot_makunbound_using_class_(Execute ptr, addr clos, addr pos, addr key);
_g int slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret);
_g int setf_slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr value);

_g void init_clos_make(void);

#endif

