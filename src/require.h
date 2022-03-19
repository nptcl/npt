#ifndef __REQUIRE_HEADER__
#define __REQUIRE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define provide_common_ _n(provide_common_)
#define require_common_ _n(require_common_)
#define build_require _n(build_require)
#define init_require _n(init_require)

int provide_common_(Execute ptr, addr var);
int require_common_(Execute ptr, addr var, addr opt);
void build_require(void);
void init_require(void);

#endif

