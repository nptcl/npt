#ifndef __PACKAGE_DESIGNATOR_HEADER__
#define __PACKAGE_DESIGNATOR_HEADER__

#include "typedef.h"

#define package_designator_p _n(package_designator_p)
#define package_designator_equal_ _n(package_designator_equal_)
#define package_designator_ _n(package_designator_)
#define package_designator_update_p_ _n(package_designator_update_p_)
#define init_package_designator _n(init_package_designator)

int package_designator_p(addr pos);
int package_designator_equal_(addr left, addr right, int *ret);
int package_designator_(addr pos, addr *ret);
int package_designator_update_p_(addr pos, addr *ret);
void init_package_designator(void);

#endif

