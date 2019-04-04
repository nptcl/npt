#ifndef __CLOS_COMMON_HEADER__
#define __CLOS_COMMON_HEADER__

#include "typedef.h"

int defclass_common(Execute ptr, addr form, addr env, addr *ret);
int define_condition_common(Execute ptr, addr form, addr env, addr *ret);
void find_class_common(addr pos, int errorp, addr env, addr *ret);
void setf_find_class_common(addr pos, addr name, addr env);
void with_accessors_common(Execute ptr, addr form, addr env, addr *ret);
void with_slots_common(Execute ptr, addr form, addr env, addr *ret);

#endif

