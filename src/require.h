#ifndef __REQUIRE_HEADER__
#define __REQUIRE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g void provide_common(Execute ptr, addr var);
_g int require_common(Execute ptr, addr var, addr opt);
_g void build_require(void);

#endif

