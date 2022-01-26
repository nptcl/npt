#ifndef __PACKAEG_DEFPACKAGE_HEADER__
#define __PACKAEG_DEFPACKAGE_HEADER__

#include "typedef.h"

#define defpackage_common_ _n(defpackage_common_)
#define defpackage_execute_ _n(defpackage_execute_)

int defpackage_common_(addr form, addr env, addr *ret);
int defpackage_execute_(Execute ptr, addr var, addr rest, addr *ret);

#endif

