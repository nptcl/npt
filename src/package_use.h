#ifndef __PACKAGE_USE_HEADER__
#define __PACKAGE_USE_HEADER__

#include "typedef.h"

#define use_package_ _n(use_package_)
#define unuse_package_ _n(unuse_package_)

int use_package_(addr package, addr pos);
int unuse_package_(addr package, addr pos);

#endif

