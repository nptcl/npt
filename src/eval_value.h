#ifndef __EVAL_VALUE_HEADER__
#define __EVAL_VALUE_HEADER__

#include "execute.h"
#include "typedef.h"

#define get_toplevel_eval_ _n(get_toplevel_eval_)
#define set_toplevel_eval _n(set_toplevel_eval)
#define push_toplevel_eval _n(push_toplevel_eval)
#define toplevelp_eval _n(toplevelp_eval)
#define get_compile_time_eval_ _n(get_compile_time_eval_)
#define set_compile_time_eval _n(set_compile_time_eval)
#define push_compile_time_eval _n(push_compile_time_eval)
#define compile_time_too_eval_ _n(compile_time_too_eval_)
#define get_compile_toplevel_eval_ _n(get_compile_toplevel_eval_)
#define set_compile_toplevel_eval _n(set_compile_toplevel_eval)
#define push_compile_toplevel_eval _n(push_compile_toplevel_eval)
#define compile_toplevel_p_eval_ _n(compile_toplevel_p_eval_)
#define get_load_toplevel_eval_ _n(get_load_toplevel_eval_)
#define set_load_toplevel_eval _n(set_load_toplevel_eval)
#define push_load_toplevel_eval _n(push_load_toplevel_eval)
#define load_toplevel_p_eval_ _n(load_toplevel_p_eval_)
#define get_execute_eval_ _n(get_execute_eval_)
#define set_execute_eval _n(set_execute_eval)
#define push_execute_eval _n(push_execute_eval)
#define executep_eval_ _n(executep_eval_)
#define push_parse_declare _n(push_parse_declare)
#define get_parse_declare_ _n(get_parse_declare_)
#define get_nocheck_parse_declare _n(get_nocheck_parse_declare)
#define set_parse_declare _n(set_parse_declare)
#define add_parse_declare_ _n(add_parse_declare_)
#define push_enable_compiler_macro _n(push_enable_compiler_macro)
#define enable_compiler_macro_p _n(enable_compiler_macro_p)

int get_toplevel_eval_(Execute ptr, addr *ret);
void set_toplevel_eval(Execute ptr, addr value);
void push_toplevel_eval(Execute ptr, addr value);
int toplevelp_eval(Execute ptr);

int get_compile_time_eval_(Execute ptr, addr *ret);
void set_compile_time_eval(Execute ptr, addr value);
void push_compile_time_eval(Execute ptr, addr value);
int compile_time_too_eval_(Execute ptr, int *ret);

int get_compile_toplevel_eval_(Execute ptr, addr *ret);
void set_compile_toplevel_eval(Execute ptr, addr value);
void push_compile_toplevel_eval(Execute ptr, addr value);
int compile_toplevel_p_eval_(Execute ptr, int *ret);

int get_load_toplevel_eval_(Execute ptr, addr *ret);
void set_load_toplevel_eval(Execute ptr, addr value);
void push_load_toplevel_eval(Execute ptr, addr value);
int load_toplevel_p_eval_(Execute ptr, int *ret);

int get_execute_eval_(Execute ptr, addr *ret);
void set_execute_eval(Execute ptr, addr value);
void push_execute_eval(Execute ptr, addr value);
int executep_eval_(Execute ptr, int *ret);

void push_parse_declare(Execute ptr, addr value);
int get_parse_declare_(Execute ptr, addr *ret);
void get_nocheck_parse_declare(Execute ptr, addr *ret);
void set_parse_declare(Execute ptr, addr value);
int add_parse_declare_(Execute ptr, addr value, addr *ret);

void push_enable_compiler_macro(Execute ptr, addr value);
int enable_compiler_macro_p(Execute ptr);

#endif

