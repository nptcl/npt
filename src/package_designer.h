#ifndef __PACKAGE_DESIGNER_HEADER__
#define __PACKAGE_DESIGNER_HEADER__

#include "typedef.h"

#define package_designer_p _n(package_designer_p)
#define package_designer_equal_ _n(package_designer_equal_)
#define package_designer_ _n(package_designer_)
#define package_designer_update_p_ _n(package_designer_update_p_)
#define init_package_designer _n(init_package_designer)

int package_designer_p(addr pos);
int package_designer_equal_(addr left, addr right, int *ret);
int package_designer_(addr pos, addr *ret);
int package_designer_update_p_(addr pos, addr *ret);
void init_package_designer(void);

#endif

