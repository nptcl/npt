#ifndef __STRUCTURE_DEFINE_HEADER__
#define __STRUCTURE_DEFINE_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_structure_list_ _n(make_structure_list_)
#define make_structure_vector_ _n(make_structure_vector_)
#define make_structure_clos_ _n(make_structure_clos_)
#define ensure_structure_common_ _n(ensure_structure_common_)
#define init_structure_define _n(init_structure_define)

int make_structure_list_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure_vector_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure_clos_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int ensure_structure_common_(Execute ptr, addr name, addr slots, addr rest);
void init_structure_define(void);

#endif

