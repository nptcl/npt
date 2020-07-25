#ifndef __PARSE_MACRO_HEADER__
#define __PARSE_MACRO_HEADER__

#include "execute.h"
#include "typedef.h"

_g void environment_symbol(addr *ret);
_g void init_parse_environment(Execute ptr);
_g void snapshot_envstack(Execute ptr, addr *ret);
_g int rollback_envstack_(Execute ptr, addr pos);
_g void defmacro_envstack(Execute ptr, addr name, addr lambda);
_g void macrolet_envstack(Execute ptr, addr name, addr lambda);
_g void define_symbol_macro_envstack(Execute ptr, addr name, addr form);
_g void symbol_macrolet_envstack(Execute ptr, addr name, addr form);
_g int symbol_macrolet_envstack_p(Execute ptr, addr name, addr *ret);
_g void environment_heap(Execute ptr, addr *ret);
_g void copy_environment(addr *ret, addr pos);
_g void close_environment(addr pos);
_g int parse_cons_check_macro(Execute ptr, addr symbol, addr *ret);
_g int find_environment_(addr symbol, addr env, addr *ret);
_g int call_macroexpand_hook(Execute ptr, addr *ret, addr call, addr cons, addr env);
_g int macroexpand1(Execute ptr, addr *ret, addr form, addr env, int *result);
_g int macroexpand(Execute ptr, addr *ret, addr form, addr env, int *result);

#endif

