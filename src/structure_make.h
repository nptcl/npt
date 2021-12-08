#ifndef __STRUCTURE_MAKE_HEADER__
#define __STRUCTURE_MAKE_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_structure1_ _n(make_structure1_)
#define make_structure2_ _n(make_structure2_)
#define make_structure3_ _n(make_structure3_)
#define make_structure1_common_ _n(make_structure1_common_)
#define make_structure2_common_ _n(make_structure2_common_)
#define make_structure3_common_ _n(make_structure3_common_)

int make_structure1_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure2_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure3_(Execute ptr, addr *ret, addr pos, addr args, int initp);

int make_structure1_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp);
int make_structure2_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp);
int make_structure3_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp);

#endif

