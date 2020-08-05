#ifndef __CALL_OBJECTS_HEADER__
#define __CALL_OBJECTS_HEADER__

#include "execute.h"
#include "typedef.h"

_g int defclass_common(Execute ptr, addr form, addr env, addr *ret);
_g int define_condition_common(Execute ptr, addr form, addr env, addr *ret);
_g int find_class_common_(addr pos, int errorp, addr env, addr *ret);
_g void setf_find_class_common(addr pos, addr name, addr env);
_g int with_accessors_common(Execute ptr, addr form, addr env, addr *ret);
_g int with_slots_common(Execute ptr, addr form, addr env, addr *ret);

_g int defgeneric_common(addr form, addr env, addr *ret);
_g int defmethod_common(Execute ptr, addr form, addr env, addr *ret);
_g int define_method_combination_common(
		LocalRoot local, addr form, addr env, addr *ret);
_g int make_load_form_saving_slots_common(Execute ptr,
		addr var, addr list, addr env, addr *ret1, addr *ret2);
_g int set_slots_syscall(addr var, addr slots, addr values);

#endif

