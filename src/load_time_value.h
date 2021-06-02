#ifndef __LOAD_TIME_VALUE_HEADER__
#define __LOAD_TIME_VALUE_HEADER__

#include "execute.h"
#include "typedef.h"

#define intern_load_table_ _n(intern_load_table_)
#define get_load_table_ _n(get_load_table_)
#define get_index_load_table_ _n(get_index_load_table_)
#define incf_load_size_ _n(incf_load_size_)
#define get_load_size_ _n(get_load_size_)

#define parse_load_time_value_ _n(parse_load_time_value_)
#define copy_eval_load_time_value _n(copy_eval_load_time_value)
#define scope_load_time_value_ _n(scope_load_time_value_)

#define init_load_time_value _n(init_load_time_value)
#define disable_load_time_value _n(disable_load_time_value)

/* special */
int intern_load_table_(Execute ptr, addr pos, addr value);
int get_load_table_(Execute ptr, addr pos, addr *ret);
int get_index_load_table_(Execute ptr, addr pos, size_t *ret);
int incf_load_size_(Execute ptr, addr *ret);
int get_load_size_(Execute ptr, addr *ret);

/* eval */
int parse_load_time_value_(Execute ptr, addr *ret, addr form);
void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval);
int scope_load_time_value_(Execute ptr, addr *ret, addr eval);

/* initialize */
void init_load_time_value(Execute ptr);
void disable_load_time_value(Execute ptr);

#endif

