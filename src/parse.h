#ifndef __PARSE_HEADER__
#define __PARSE_HEADER__

#include "execute.h"
#include "local.h"
#include "parse_typedef.h"

_g int check_variable_(addr symbol);
_g int check_function_variable_(addr symbol);
_g int tagbody_tag_p(addr pos);
_g int parse_compile_toplevel_(Execute ptr, addr expr, addr list, addr *ret);
_g int eval_parse(Execute ptr, addr *ret, addr pos, addr toplevel);

#endif

