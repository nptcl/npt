#ifndef __EVAL_EXECUTE_HEADER__
#define __EVAL_EXECUTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_execute_partial_ _n(eval_execute_partial_)
#define eval_result_partial_ _n(eval_result_partial_)
#define eval_result_partial_form_ _n(eval_result_partial_form_)
#define eval_result_macro_ _n(eval_result_macro_)
#define eval_stream_toplevel_ _n(eval_stream_toplevel_)

int eval_execute_partial_(Execute ptr, addr pos);
int eval_result_partial_(Execute ptr, addr pos, addr *ret);
int eval_result_partial_form_(Execute ptr, addr pos, addr *ret);
int eval_result_macro_(Execute ptr, addr pos, addr *ret);
int eval_stream_toplevel_(Execute ptr, addr stream);

#endif

