#ifndef __SCOPE_DECLARE_HEADER__
#define __SCOPE_DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int specialp_tablevalue_(Execute ptr, addr stack, addr symbol, int *ret);
_g int find_tablevalue(addr stack, addr symbol, addr *ret);
_g int find_tablefunction(addr stack, addr call, addr *ret);
_g int globalp_tablefunction_(Execute ptr, addr stack, addr call, int *ret);
_g int apply_declare_(Execute ptr, addr stack, addr decl, addr *ret);

#endif

