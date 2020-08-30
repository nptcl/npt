#ifndef __REQUIRE_HEADER__
#define __REQUIRE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define provide_common_ _n(provide_common_)
#define require_common _n(require_common)
#define build_require _n(build_require)

_g int provide_common_(Execute ptr, addr var);
_g int require_common(Execute ptr, addr var, addr opt);
_g void build_require(void);

#endif

