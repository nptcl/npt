#ifndef __REQUIRE_CLOS_HEADER__
#define __REQUIRE_CLOS_HEADER__

#include "execute.h"
#include "typedef.h"

#define require_clos_ _n(require_clos_)
#define unrequire_clos_ _n(unrequire_clos_)

int require_clos_(Execute ptr, addr var, int forcep, int *ret);
int unrequire_clos_(Execute ptr, addr var, int forcep, int *ret);

#endif

