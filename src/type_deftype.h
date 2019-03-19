#ifndef __TYPE_DEFTYPE_HEADER__
#define __TYPE_DEFTYPE_HEADER__

#include "execute.h"
#include "typedef.h"

void getdeftype(addr symbol, addr *ret);
void setdeftype(addr symbol, addr pos);
int symbol_deftypep(addr symbol);
int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env);
int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env);

#endif

