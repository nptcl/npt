#ifndef __PARSE_MACRO_HEADER__
#define __PARSE_MACRO_HEADER__

#include "execute.h"
#include "typedef.h"

#define environment_symbol _n(environment_symbol)
#define init_parse_environment _n(init_parse_environment)
#define snapshot_envstack_ _n(snapshot_envstack_)
#define rollback_envstack_ _n(rollback_envstack_)
#define defmacro_envstack_ _n(defmacro_envstack_)
#define macrolet_envstack_ _n(macrolet_envstack_)
#define define_symbol_macro_envstack_ _n(define_symbol_macro_envstack_)
#define symbol_macrolet_envstack_ _n(symbol_macrolet_envstack_)
#define symbol_macrolet_envstack_p_ _n(symbol_macrolet_envstack_p_)
#define environment_heap_ _n(environment_heap_)
#define copy_environment _n(copy_environment)
#define close_environment _n(close_environment)
#define parse_cons_check_macro _n(parse_cons_check_macro)
#define find_environment_ _n(find_environment_)
#define call_macroexpand_hook _n(call_macroexpand_hook)
#define macroexpand1 _n(macroexpand1)
#define macroexpand _n(macroexpand)

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

