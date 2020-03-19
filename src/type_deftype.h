#ifndef __TYPE_DEFTYPE_HEADER__
#define __TYPE_DEFTYPE_HEADER__

#include "execute.h"
#include "typedef.h"

_g void getdeftype(addr symbol, addr *ret);
_g void setdeftype(addr symbol, addr pos);
_g int symbol_deftypep(addr symbol);
_g int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env);
_g int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env);
_g int deftype_common(Execute ptr, addr form, addr env, addr *ret);

#endif

