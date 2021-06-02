#ifndef __LOAD_GENSYM_HEADER__
#define __LOAD_GENSYM_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_load_gensym _n(init_load_gensym)
#define disable_load_gensym _n(disable_load_gensym)
#define list_load_gensym_ _n(list_load_gensym_)
#define load_value_ _n(load_value_)
#define load_value_code_ _n(load_value_code_)

void init_load_gensym(Execute ptr);
void disable_load_gensym(Execute ptr);
int list_load_gensym_(Execute ptr, addr *ret);
int load_value_(Execute ptr, addr pos);
int load_value_code_(Execute ptr, addr code);

#endif

