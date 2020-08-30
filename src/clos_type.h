#ifndef __CLOS_TYPE_HEADER__
#define __CLOS_TYPE_HEADER__

#include "typedef.h"

#define init_clos_type _n(init_clos_type)
#define clos_class_of_ _n(clos_class_of_)
#define clos_intern_specializer_ _n(clos_intern_specializer_)

/* class-of */
_g void init_clos_type(void);
_g int clos_class_of_(addr object, addr *ret);

/* specializer */
_g int clos_intern_specializer_(addr object, addr *ret);

#endif

