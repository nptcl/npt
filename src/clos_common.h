#ifndef __CLOS_COMMON_HEADER__
#define __CLOS_COMMON_HEADER__

#include "typedef.h"

void slot_boundp_common(addr pos, addr name, addr *ret);
int defclass_common(Execute ptr, addr form, addr env, addr *ret);
void ensure_class_common(Execute ptr, addr name, addr args, addr *ret);
void find_class_common(addr pos, int errorp, addr env, addr *ret);

#endif

