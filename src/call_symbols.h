#ifndef __CALL_SYMBOLS_HEADER__
#define __CALL_SYMBOLS_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_symbol_common _n(make_symbol_common)
#define copy_symbol_common_ _n(copy_symbol_common_)
#define gensym_common_ _n(gensym_common_)
#define gentemp_common_ _n(gentemp_common_)
#define setf_symbol_function_common_ _n(setf_symbol_function_common_)
#define setf_symbol_value_common_ _n(setf_symbol_value_common_)
#define setf_symbol_plist_common_ _n(setf_symbol_plist_common_)
#define get_common_ _n(get_common_)
#define setf_get_common_ _n(setf_get_common_)
#define remprop_common_ _n(remprop_common_)
#define makunbound_common_ _n(makunbound_common_)
#define set_common_ _n(set_common_)

void make_symbol_common(addr var, addr *ret);
int copy_symbol_common_(addr var, addr opt, addr *ret);
int gensym_common_(Execute ptr, addr opt, addr *ret);
int gentemp_common_(Execute ptr, addr opt1, addr opt2, addr *ret);
int setf_symbol_function_common_(addr value, addr symbol);
int setf_symbol_value_common_(Execute ptr, addr value, addr symbol);
int setf_symbol_plist_common_(addr value, addr symbol);
int get_common_(addr var1, addr var2, addr opt, addr *ret);
int setf_get_common_(addr value, addr symbol, addr key);
int remprop_common_(addr symbol, addr key, addr *ret);
int makunbound_common_(Execute ptr, addr symbol);
int set_common_(Execute ptr, addr symbol, addr value);

#endif

