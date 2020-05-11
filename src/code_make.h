#ifndef __CODE_MAKE_HEADER__
#define __CODE_MAKE_HEADER__

#include "local.h"
#include "typedef.h"

_g void code_make_execute(LocalRoot local, addr code, addr scope);
_g void code_make(LocalRoot local, addr *ret, addr scope);
_g void init_code_make(void);

#endif

