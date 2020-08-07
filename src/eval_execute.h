#ifndef __EVAL_EXECUTE_HEADER__
#define __EVAL_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

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

