#ifndef __OPTIMIZE_COMMON_HEADER__
#define __OPTIMIZE_COMMON_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

#define optimize_common _n(optimize_common)
#define optimize_check_code _n(optimize_check_code)
#define init_optimize_common _n(init_optimize_common)
#define build_optimize_common _n(build_optimize_common)

_g int optimize_common(LocalRoot local, addr code, addr scope);
_g int optimize_check_code(LocalRoot local, addr code, addr scope);

_g void init_optimize_common(void);
_g void build_optimize_common(void);

#endif

