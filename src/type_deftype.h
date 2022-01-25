#ifndef __TYPE_DEFTYPE_HEADER__
#define __TYPE_DEFTYPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define getdeftype _n(getdeftype)
#define setdeftype_ _n(setdeftype_)
#define symbol_deftypep _n(symbol_deftypep)
#define execute_list_deftype_ _n(execute_list_deftype_)
#define execute_symbol_deftype_ _n(execute_symbol_deftype_)
#define deftype_common_ _n(deftype_common_)

void getdeftype(addr symbol, addr *ret);
int setdeftype_(addr symbol, addr pos);
int symbol_deftypep(addr symbol);
int execute_list_deftype_(Execute ptr, addr *ret, addr list, addr env);
int execute_symbol_deftype_(Execute ptr, addr *ret, addr symbol, addr env);
int deftype_common_(Execute ptr, addr form, addr env, addr *ret);

#endif

