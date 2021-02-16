#ifndef __EVAL_EXECUTE_HEADER__
#define __EVAL_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define begin_eval_ _n(begin_eval_)
#define begin_compile_ _n(begin_compile_)

#define eval_execute_partial _n(eval_execute_partial)
#define eval_result_partial _n(eval_result_partial)
#define eval_result_partial_form_ _n(eval_result_partial_form_)
#define eval_result_macro _n(eval_result_macro)

#define eval_load_stream_ _n(eval_load_stream_)
#define compile_load_stream_ _n(compile_load_stream_)
#define eval_stream_partial _n(eval_stream_partial)
#define eval_stream_toplevel _n(eval_stream_toplevel)

int begin_eval_(Execute ptr, addr *ret, addr toplevel);
int begin_compile_(Execute ptr, addr *ret);

int eval_execute_partial(Execute ptr, addr pos);
int eval_result_partial(Execute ptr, addr pos, addr *ret);
int eval_result_partial_form_(Execute ptr, addr pos, addr *ret);
int eval_result_macro(Execute ptr, addr pos, addr *ret);

int eval_load_stream_(Execute ptr, addr stream, addr toplevel);
int compile_load_stream_(Execute ptr, addr stream);
int eval_stream_partial(Execute ptr, addr stream);
int eval_stream_toplevel(Execute ptr, addr stream);

#endif

