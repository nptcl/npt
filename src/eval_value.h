#ifndef __EVAL_VALUE_HEADER__
#define __EVAL_VALUE_HEADER__

#include "execute.h"
#include "typedef.h"

#define gettoplevel_eval_ _n(gettoplevel_eval_)
#define settoplevel_eval _n(settoplevel_eval)
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

int gettoplevel_eval_(Execute ptr, addr *ret);
void settoplevel_eval(Execute ptr, addr value);
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

#endif

