#ifndef __CLOS_TYPE_HEADER__
#define __CLOS_TYPE_HEADER__

#include "typedef.h"

/* class-of */
_g void init_clos_type(void);
_g void clos_class_of(addr object, addr *ret);

/* specializer */
_g int clos_intern_specializer_(addr object, addr *ret);

#endif

