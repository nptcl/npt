#ifndef __COMPILE_EVAL_HEADER__
#define __COMPILE_EVAL_HEADER__

#include "execute.h"
#include "typedef.h"

#define compile_load_stream_ _n(compile_load_stream_)
#define compile_output_end_ _n(compile_output_end_)
#define compile_partial_ _n(compile_partial_)
#define compile_instance_ _n(compile_instance_)

int compile_load_stream_(Execute ptr, addr stream);
int compile_output_end_(Execute ptr);
int compile_partial_(Execute ptr, addr pos, addr *ret, addr *rtype);
int compile_instance_(Execute ptr, addr pos, addr make, addr init);

#endif

