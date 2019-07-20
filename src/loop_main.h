#ifndef __LOOP_MAIN_HEADER__
#define __LOOP_MAIN_HEADER__

#include "define.h"
#include "typedef.h"

struct loop_main {
	addr form, init, named;
};

_g void make_loop_main(struct loop_main *ptr, addr form, addr init, addr named);
_g void loop_push_main(struct loop_main *ptr, addr list);
_g void loop_let_main(addr *form, addr list);

#endif

