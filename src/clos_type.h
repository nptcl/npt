#ifndef __CLOS_TYPE_HEADER__
#define __CLOS_TYPE_HEADER__

#include "execute.h"

void class_of(addr object, addr *ret);
void init_class_of(void);
void build_clos_type(Execute ptr);

#endif

