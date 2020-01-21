#ifndef __CLOS_MAKE_HEADER__
#define __CLOS_MAKE_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

_g void clos_ensure_class_supers(addr args, addr *ret, int *referp);
_g void clos_ensure_class_slots(addr args, addr *ret);
_g void clos_ensure_class_direct_default_initargs(LocalRoot local,
		addr pos, addr args, addr *ret);
_g void clos_ensure_class_init(LocalRoot local, addr pos, int pushp);

_g int clos_finalize(Execute ptr, addr pos);
_g int clos_ensure_class(Execute ptr, addr name, addr args, addr *ret);
_g void allocate_instance_stdclass(Execute ptr, addr clos, addr *ret);
_g int initialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
_g int reinitialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret);
_g int shared_initialize_stdobject(Execute ptr, addr pos, addr name, addr rest);
_g int make_instance_stdclass(Execute ptr, addr rest, addr *ret);
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

_g void init_clos_make(void);

#endif

