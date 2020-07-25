#ifndef __PARSE_FUNCTION_HEADER__
#define __PARSE_FUNCTION_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define localhold_parse_self_(h,p,x) localhold_parse_execute_((h),(p),&(x),(x))
#define parse_self_(p, x) parse_execute_((p), &(x), (x))

_g int localhold_parse_allcons_(LocalHold hold, Execute ptr, addr *ret, addr cons);
_g int localhold_parse_execute_(LocalHold hold, Execute ptr, addr *ret, addr pos);

_g int parse_allcons_(Execute ptr, addr *ret, addr cons);
_g int parse_execute_(Execute ptr, addr *ret, addr pos);
_g int parse_allcons_toplevel_(Execute ptr, addr *ret, addr cons);
_g int parse_execute_toplevel_(Execute ptr, addr *ret, addr pos);

_g int parse_ordinary_(Execute ptr, addr *ret, addr args);

#endif

