#ifndef __PACKAGE_EXPORT_HEADER__
#define __PACKAGE_EXPORT_HEADER__

#include "typedef.h"

#define export_package_ _n(export_package_)
#define unexport_package_ _n(unexport_package_)

int export_package_(addr package, addr pos);
int unexport_package_(addr package, addr pos);

#endif

