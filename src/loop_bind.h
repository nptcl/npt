#ifndef __LOOP_BIND_HEADER__
#define __LOOP_BIND_HEADER__

#include "execute.h"
#include "typedef.h"

#define loop_bind_initial_list_ _n(loop_bind_initial_list_)
#define loop_bind_common_ _n(loop_bind_common_)

int loop_bind_initial_list_(Execute ptr, addr var, addr type, addr *ret);
int loop_bind_common_(Execute ptr, addr pos, addr type, addr value, addr *ret);

#endif

