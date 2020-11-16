#ifndef __LOOP_MAIN_HEADER__
#define __LOOP_MAIN_HEADER__

#include "define.h"
#include "typedef.h"

#define make_loop_main _n(make_loop_main)
#define loop_push_main_ _n(loop_push_main_)
#define loop_let_main_ _n(loop_let_main_)

struct loop_main {
	addr form, init, named;
};

void make_loop_main(struct loop_main *ptr, addr form, addr init, addr named);
int loop_push_main_(struct loop_main *ptr, addr list);
int loop_let_main_(addr *form, addr list);

#endif

