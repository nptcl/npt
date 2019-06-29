#ifndef __CLOS_COMMON_HEADER__
#define __CLOS_COMMON_HEADER__

#include "typedef.h"

_g int defclass_common(Execute ptr, addr form, addr env, addr *ret);
_g int define_condition_common(Execute ptr, addr form, addr env, addr *ret);
_g void find_class_common(addr pos, int errorp, addr env, addr *ret);
_g void setf_find_class_common(addr pos, addr name, addr env);
_g void with_accessors_common(Execute ptr, addr form, addr env, addr *ret);
_g void with_slots_common(Execute ptr, addr form, addr env, addr *ret);

_g void defgeneric_common(addr form, addr env, addr *ret);
_g int defmethod_common(Execute ptr, addr form, addr env, addr *ret);
_g void define_method_combination_common(Execute ptr, addr form, addr env, addr *ret);

#endif

