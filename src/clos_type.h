#ifndef __CLOS_TYPE_HEADER__
#define __CLOS_TYPE_HEADER__

#include "typedef.h"

/* class-of */
void init_clos_class_of(void);
void clos_class_of(addr object, addr *ret);

/* specializer */
void clos_intern_specializer(addr object, addr *ret);

#endif

