#ifndef __CALL_SYMBOLS_HEADER__
#define __CALL_SYMBOLS_HEADER__

#include "execute.h"
#include "typedef.h"

_g void make_symbol_common(addr var, addr *ret);
_g void copy_symbol_common(addr var, addr opt, addr *ret);
_g int gensym_common(Execute ptr, addr opt, addr *ret);
_g int gentemp_common(Execute ptr, addr opt1, addr opt2, addr *ret);
_g int setf_symbol_function_common(addr value, addr symbol);
_g int setf_symbol_value_common(Execute ptr, addr value, addr symbol);
_g int setf_symbol_plist_common(addr value, addr symbol);
_g int get_common(addr var1, addr var2, addr opt, addr *ret);
_g int setf_get_common(addr value, addr symbol, addr key);
_g int remprop_common(addr symbol, addr key, addr *ret);
_g int makunbound_common(Execute ptr, addr symbol);
_g int set_common(Execute ptr, addr symbol, addr value);

#endif

