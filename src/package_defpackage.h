#ifndef __PACKAEG_DEFPACKAGE_HEADER__
#define __PACKAEG_DEFPACKAGE_HEADER__

#include "typedef.h"

#define defpackage_common _n(defpackage_common)
#define defpackage_execute _n(defpackage_execute)

int defpackage_common(addr form, addr env, addr *ret);
int defpackage_execute(Execute ptr, addr var, addr rest, addr *ret);

#endif

