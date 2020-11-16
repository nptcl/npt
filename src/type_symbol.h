#ifndef __TYPE_SYMBOL_HEADER__
#define __TYPE_SYMBOL_HEADER__

#include "constant.h"
#include "execute.h"
#include "typedef.h"
#include "type.h"

#define build_type_symbol _n(build_type_symbol)
#define init_type_symbol _n(init_type_symbol)
#define find_symbol_type _n(find_symbol_type)
#define getdeclname _n(getdeclname)
#define type_symbol_p _n(type_symbol_p)

void build_type_symbol(void);
void init_type_symbol(void);
int find_symbol_type(Execute ptr, addr *ret, addr symbol, addr env);
constindex getdeclname(enum LISPDECL type);
int type_symbol_p(addr symbol);

#endif

