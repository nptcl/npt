#ifndef __SCOPE_DECLARE_HEADER__
#define __SCOPE_DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int specialp_tablevalue(Execute ptr, addr stack, addr symbol);
_g int find_tablevalue(addr stack, addr symbol, addr *ret);
_g int find_tablefunction(addr stack, addr call, addr *ret);
_g int globalp_tablefunction(Execute ptr, addr stack, addr call);
_g void apply_declare(Execute ptr, addr stack, addr decl, addr *ret);

#endif

