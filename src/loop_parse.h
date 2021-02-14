#ifndef __LOOP_PARSE_HEADER__
#define __LOOP_PARSE_HEADER__

#include "execute.h"
#include "typedef.h"

#define loop_parse_common_ _n(loop_parse_common_)

int loop_parse_common_(Execute ptr, addr *named, addr *vars, addr *main, addr *list);

#endif

