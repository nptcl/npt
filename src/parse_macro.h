#ifndef __PARSE_MACRO_HEADER__
#define __PARSE_MACRO_HEADER__

#include "execute.h"
#include "typedef.h"

#define environment_symbol _n(environment_symbol)
#define init_parse_environment _n(init_parse_environment)
#define snapshot_envstack_ _n(snapshot_envstack_)
#define rollback_envstack_ _n(rollback_envstack_)
#define defvar_envstack_ _n(defvar_envstack_)
#define lexical_envstack_ _n(lexical_envstack_)
#define defun_envstack_ _n(defun_envstack_)
#define function_envstack_ _n(function_envstack_)
#define defmacro_envstack_ _n(defmacro_envstack_)
#define macrolet_envstack_ _n(macrolet_envstack_)
#define define_symbol_macro_envstack_ _n(define_symbol_macro_envstack_)
#define symbol_macrolet_envstack_ _n(symbol_macrolet_envstack_)
#define symbol_macrolet_envstack_p_ _n(symbol_macrolet_envstack_p_)
#define environment_heap_ _n(environment_heap_)
#define copy_environment _n(copy_environment)
#define close_environment _n(close_environment)
#define parse_cons_check_macro_ _n(parse_cons_check_macro_)
#define find_environment_ _n(find_environment_)
#define call_macroexpand_hook_ _n(call_macroexpand_hook_)
#define macroexpand1_ _n(macroexpand1_)
#define macroexpand_ _n(macroexpand_)

void environment_symbol(addr *ret);
void init_parse_environment(Execute ptr);
int snapshot_envstack_(Execute ptr, addr *ret);
int rollback_envstack_(Execute ptr, addr pos);
int defvar_envstack_(Execute ptr, addr name);
int lexical_envstack_(Execute ptr, addr name);
int defun_envstack_(Execute ptr, addr name);
int function_envstack_(Execute ptr, addr name);
int defmacro_envstack_(Execute ptr, addr name, addr lambda);
int macrolet_envstack_(Execute ptr, addr name, addr lambda);
int define_symbol_macro_envstack_(Execute ptr, addr name, addr form);
int symbol_macrolet_envstack_(Execute ptr, addr name, addr form);
int symbol_macrolet_envstack_p_(Execute ptr, addr name, addr *value, int *ret);

int environment_heap_(Execute ptr, addr *ret);
void copy_environment(addr *ret, addr pos);
void close_environment(addr pos);

int parse_cons_check_macro_(Execute ptr, addr symbol, addr *ret);
int find_environment_(addr symbol, addr env, addr *ret);
int call_macroexpand_hook_(Execute ptr, addr *ret, addr call, addr cons, addr env);
int macroexpand1_(Execute ptr, addr *ret, addr form, addr env, int *result);
int macroexpand_(Execute ptr, addr *ret, addr form, addr env, int *result);

#endif

