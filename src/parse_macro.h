#ifndef __PARSE_MACRO_HEADER__
#define __PARSE_MACRO_HEADER__

#include "execute.h"
#include "typedef.h"

_g void environment_symbol(addr *ret);
_g void init_parse_environment(Execute ptr);
_g int snapshot_envstack_(Execute ptr, addr *ret);
_g int rollback_envstack_(Execute ptr, addr pos);
_g int defmacro_envstack_(Execute ptr, addr name, addr lambda);
_g int macrolet_envstack_(Execute ptr, addr name, addr lambda);
_g int define_symbol_macro_envstack_(Execute ptr, addr name, addr form);
_g int symbol_macrolet_envstack_(Execute ptr, addr name, addr form);
_g int symbol_macrolet_envstack_p_(Execute ptr, addr name, addr *value, int *ret);
_g int environment_heap_(Execute ptr, addr *ret);
_g void copy_environment(addr *ret, addr pos);
_g void close_environment(addr pos);
_g int parse_cons_check_macro(Execute ptr, addr symbol, addr *ret);
_g int find_environment_(addr symbol, addr env, addr *ret);
_g int call_macroexpand_hook(Execute ptr, addr *ret, addr call, addr cons, addr env);
_g int macroexpand1(Execute ptr, addr *ret, addr form, addr env, int *result);
_g int macroexpand(Execute ptr, addr *ret, addr form, addr env, int *result);

#endif

