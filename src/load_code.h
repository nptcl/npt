#ifndef __LOAD_CODE_HEADER__
#define __LOAD_CODE_HEADER__

#include "execute.h"
#include "typedef.h"

#define execute_load_alloc _n(execute_load_alloc)
#define execute_load_gensym_ _n(execute_load_gensym_)
#define execute_load_set_ _n(execute_load_set_)
#define execute_load_get_ _n(execute_load_get_)
#define fasl_load_time_value _n(fasl_load_time_value)

#define code_make_load_alloc _n(code_make_load_alloc)
#define code_make_load_gensym _n(code_make_load_gensym)
#define code_make_load_set _n(code_make_load_set)

void execute_load_alloc(Execute ptr, size_t size);
int execute_load_gensym_(Execute ptr, addr name, size_t index);
int execute_load_set_(Execute ptr, size_t index);
int execute_load_get_(Execute ptr, size_t index, addr *ret);
void fasl_load_time_value(Execute ptr);

void code_make_load_alloc(Execute ptr, addr *ret, addr index);
void code_make_load_gensym(Execute ptr, addr *ret, addr list);
void code_make_load_set(Execute ptr, addr *ret, addr index);

#endif

