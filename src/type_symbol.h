#ifndef __TYPE_SYMBOL_HEADER__
#define __TYPE_SYMBOL_HEADER__

#include "constant.h"
#include "execute.h"
#include "typedef.h"
#include "type.h"

_g void build_type_symbol(void);
_g void init_type_symbol(void);
_g int find_symbol_type(Execute ptr, addr *ret, addr symbol, addr env);
_g constindex getdeclname(enum LISPDECL type);
_g int type_symbol_p(addr symbol);

#endif

