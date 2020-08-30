#ifndef __LOOP_PARSE_HEADER__
#define __LOOP_PARSE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define loop_parse_common _n(loop_parse_common)
_g int loop_parse_common(Execute ptr, addr *named, addr *vars, addr *main, addr *list);

#endif

