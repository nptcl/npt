#ifndef __CLOS_BUILD_HEADER__
#define __CLOS_BUILD_HEADER__

#include "execute.h"
#include "typedef.h"

#define clos_stdclass_slots_ _n(clos_stdclass_slots_)
#define slotvector_set_location_ _n(slotvector_set_location_)
#define clos_stdclass_direct_slots_ _n(clos_stdclass_direct_slots_)
#define clos_stdclass_prototype_ _n(clos_stdclass_prototype_)
#define clos_stdclass_supers_ _n(clos_stdclass_supers_)
#define clos_stdclass_type_ _n(clos_stdclass_type_)
#define build_clos_class _n(build_clos_class)

int clos_stdclass_slots_(Execute ptr, addr *ret);
int slotvector_set_location_(Execute ptr, addr slots);
int clos_stdclass_direct_slots_(Execute ptr, addr instance, addr slots);
int clos_stdclass_prototype_(Execute ptr, addr clos);
int clos_stdclass_supers_(Execute ptr,
		addr *ret, addr metaclass, addr name, addr slots, addr supers);
int clos_stdclass_type_(Execute ptr, addr *ret, addr metaclass, addr name, addr supers);
void build_clos_class(Execute ptr);

#endif

