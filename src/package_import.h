#ifndef __PACKAGE_IMPORT_HEADER__
#define __PACKAGE_IMPORT_HEADER__

#include "typedef.h"

#define import_bitpackage_ _n(import_bitpackage_)
#define import_package_ _n(import_package_)

int import_bitpackage_(addr package, addr symbol, addr *value, int *ret);
int import_package_(addr package, addr pos);

#endif

