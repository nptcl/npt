#ifndef __EVAL_EXECUTE_HEADER__
#define __EVAL_EXECUTE_HEADER__

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
#define eval_execute_partial _n(eval_execute_partial)
#define eval_result_partial _n(eval_result_partial)
#define eval_result_macro _n(eval_result_macro)
#define eval_stream_partial _n(eval_stream_partial)
#define eval_stream_toplevel _n(eval_stream_toplevel)
#define eval_object _n(eval_object)
#define eval_load _n(eval_load)
#define compile_load _n(compile_load)

_g int gettoplevel_eval_(Execute ptr, addr *ret);
_g void settoplevel_eval(Execute ptr, addr value);
_g void push_toplevel_eval(Execute ptr, addr value);
_g int toplevelp_eval(Execute ptr);

_g int get_compile_time_eval_(Execute ptr, addr *ret);
_g void set_compile_time_eval(Execute ptr, addr value);
_g void push_compile_time_eval(Execute ptr, addr value);
_g int compile_time_too_eval_(Execute ptr, int *ret);

_g int get_compile_toplevel_eval_(Execute ptr, addr *ret);
_g void set_compile_toplevel_eval(Execute ptr, addr value);
_g void push_compile_toplevel_eval(Execute ptr, addr value);
_g int compile_toplevel_p_eval_(Execute ptr, int *ret);

_g int get_load_toplevel_eval_(Execute ptr, addr *ret);
_g void set_load_toplevel_eval(Execute ptr, addr value);
_g void push_load_toplevel_eval(Execute ptr, addr value);
_g int load_toplevel_p_eval_(Execute ptr, int *ret);

_g int get_execute_eval_(Execute ptr, addr *ret);
_g void set_execute_eval(Execute ptr, addr value);
_g void push_execute_eval(Execute ptr, addr value);
_g int executep_eval_(Execute ptr, int *ret);

_g int eval_execute_partial(Execute ptr, addr pos);
_g int eval_result_partial(Execute ptr, addr pos, addr *ret);
_g int eval_result_macro(Execute ptr, addr pos, addr *ret);
_g int eval_stream_partial(Execute ptr, addr stream);
_g int eval_stream_toplevel(Execute ptr, addr stream);
_g int eval_object(Execute ptr, addr eval, addr *ret);
_g int eval_load(Execute ptr, int *result,
		addr file, addr verbose, addr print, int exist, addr external);
_g int compile_load(Execute ptr, addr file, addr verbose, addr print, addr external);

#endif

