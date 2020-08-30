#ifndef __PARSE_LOAD_HEADER__
#define __PARSE_LOAD_HEADER__

#include "execute.h"
#include "typedef.h"

#define load_time_value_heap _n(load_time_value_heap)
#define get_load_time_value_heap _n(get_load_time_value_heap)
#define set_load_time_value_heap _n(set_load_time_value_heap)
#define set_load_time_value_symbol _n(set_load_time_value_symbol)
#define init_parse_load_time_value _n(init_parse_load_time_value)
#define eval_parse_load_time_value _n(eval_parse_load_time_value)
#define parse_load_time_value _n(parse_load_time_value)
#define copy_eval_load_time_value _n(copy_eval_load_time_value)
#define init_scope_load_time_value _n(init_scope_load_time_value)
#define scope_load_time_value _n(scope_load_time_value)
#define execute_load_time_value_bind _n(execute_load_time_value_bind)
#define execute_load_time_value_init _n(execute_load_time_value_init)
#define execute_load_time_value_get _n(execute_load_time_value_get)

_g void load_time_value_heap(addr *ret);
_g void get_load_time_value_heap(addr pos, addr *ret);
_g void set_load_time_value_heap(addr pos, addr value);
_g void set_load_time_value_symbol(Execute ptr, addr value);

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
_g void execute_load_time_value_bind(Execute ptr, addr pos);
_g void execute_load_time_value_init(Execute ptr, addr pos);
_g void execute_load_time_value_get(Execute ptr, addr pos, addr *ret);

#endif

