#ifndef __OPTIMIZE_COMMON_HEADER__
#define __OPTIMIZE_COMMON_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

_g void optcode_car0_set(Execute ptr, addr list);
_g void optcode_car0_push(Execute ptr, addr list);
_g void optcode_car1_set(Execute ptr, addr list);
_g void optcode_car1_push(Execute ptr, addr list);

_g int optimize_common(LocalRoot local, addr code, addr scope);
_g int optimize_check_code(LocalRoot local, addr code, addr scope);

_g void init_optimize_common(void);
_g void build_optimize_common(void);

#endif

