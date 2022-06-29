#ifndef __CLOS_TYPE_HEADER__
#define __CLOS_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_clos_type _n(init_clos_type)
#define clos_class_of_ _n(clos_class_of_)
#define clos_intern_specializer_ _n(clos_intern_specializer_)

/* class-of */
void init_clos_type(void);
int clos_class_of_(addr object, addr *ret);

/* specializer */
int clos_intern_specializer_(Execute ptr, addr object, addr *ret);

#endif

