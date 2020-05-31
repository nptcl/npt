#ifndef __PARSE_LOAD_HEADER__
#define __PARSE_LOAD_HEADER__

#include "execute.h"
#include "typedef.h"

/* parse */
_g void init_parse_load_time_value(Execute ptr);
_g int eval_parse_load_time_value(Execute ptr, addr *ret, addr pos);
_g int parse_load_time_value(Execute ptr, addr *ret, addr form);

/* copy */
_g void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval);

/* scope */
_g void init_scope_load_time_value(Execute ptr);
_g int scope_load_time_value(Execute ptr, addr *ret, addr eval);

/* code */
_g void execute_load_time_value_alloc(Execute ptr, size_t size);
_g void execute_load_time_value_value(Execute ptr, addr list);
_g void execute_load_time_value_get(Execute ptr, size_t index, addr *ret);

#endif

