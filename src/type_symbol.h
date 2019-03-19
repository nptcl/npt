#ifndef __TYPE_SYMBOL_HEADER__
#define __TYPE_SYMBOL_HEADER__

#include "constant.h"
#include "execute.h"
#include "typedef.h"
#include "type.h"

void build_type_symbol(void);
void init_type_symbol(void);
int find_symbol_type(Execute ptr, addr *ret, addr symbol, addr env);
constindex getdeclname(enum LISPDECL type);
int type_symbol_p(addr symbol);

#endif

