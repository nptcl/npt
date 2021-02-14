#ifndef __PACKAGE_EXPORT_HEADER__
#define __PACKAGE_EXPORT_HEADER__

#include "typedef.h"

#define symbol_export_package_ _n(symbol_export_package_)
#define export_package_ _n(export_package_)
#define unexport_package_ _n(unexport_package_)

int symbol_export_package_(addr package, addr symbol);
int export_package_(addr package, addr pos);
int unexport_package_(addr package, addr pos);

#endif

