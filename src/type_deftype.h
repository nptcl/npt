#ifndef __TYPE_DEFTYPE_HEADER__
#define __TYPE_DEFTYPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define getdeftype _n(getdeftype)
#define setdeftype_ _n(setdeftype_)
#define symbol_deftypep _n(symbol_deftypep)
#define execute_list_deftype _n(execute_list_deftype)
#define execute_symbol_deftype _n(execute_symbol_deftype)
#define deftype_common _n(deftype_common)

_g void getdeftype(addr symbol, addr *ret);
_g int setdeftype_(addr symbol, addr pos);
_g int symbol_deftypep(addr symbol);
_g int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env);
_g int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env);
_g int deftype_common(Execute ptr, addr form, addr env, addr *ret);

#endif

