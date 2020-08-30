#ifndef __CALL_SYMBOLS_HEADER__
#define __CALL_SYMBOLS_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_symbol_common _n(make_symbol_common)
#define copy_symbol_common_ _n(copy_symbol_common_)
#define gensym_common _n(gensym_common)
#define gentemp_common _n(gentemp_common)
#define setf_symbol_function_common _n(setf_symbol_function_common)
#define setf_symbol_value_common _n(setf_symbol_value_common)
#define setf_symbol_plist_common _n(setf_symbol_plist_common)
#define get_common _n(get_common)
#define setf_get_common _n(setf_get_common)
#define remprop_common _n(remprop_common)
#define makunbound_common _n(makunbound_common)
#define set_common _n(set_common)

_g void make_symbol_common(addr var, addr *ret);
_g int copy_symbol_common_(addr var, addr opt, addr *ret);
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

