#ifndef __CODE_MAKE_HEADER__
#define __CODE_MAKE_HEADER__

#include "local.h"
#include "typedef.h"

#define code_make_execute _n(code_make_execute)
#define code_make _n(code_make)
#define init_code_make _n(init_code_make)

void code_make_execute(LocalRoot local, addr code, addr scope);
void code_make(LocalRoot local, addr *ret, addr scope);
void init_code_make(void);

#if 0
#define code_make_load_time_value_alloc _n(code_make_load_time_value_alloc)
#define code_make_load_time_value_expr _n(code_make_load_time_value_expr)
#define code_make_load_time_value_init _n(code_make_load_time_value_init)
void code_make_load_time_value_alloc(LocalRoot local, addr *ret, addr index);
void code_make_load_time_value_expr(LocalRoot local, addr *ret, addr list);
void code_make_load_time_value_init(LocalRoot local, addr *ret, addr list);
#endif

#endif

