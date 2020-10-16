#ifndef __PACKAGE_SHADOW_HEADER__
#define __PACKAGE_SHADOW_HEADER__

#include "typedef.h"

#define shadow_package_ _n(shadow_package_)
#define shadowing_import_package_ _n(shadowing_import_package_)

_g int shadow_package_(addr package, addr pos);
_g int shadowing_import_package_(addr package, addr pos);

#endif

