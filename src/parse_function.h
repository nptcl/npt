#ifndef __PARSE_FUNCTION_HEADER__
#define __PARSE_FUNCTION_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define localhold_parse_self(h,p,x) localhold_parse_execute((h),(p),&(x),(x))
#define parse_self(p, x) parse_execute((p), &(x), (x))

_g int localhold_parse_allcons(LocalHold hold, Execute ptr, addr *ret, addr cons);
_g int localhold_parse_execute(LocalHold hold, Execute ptr, addr *ret, addr pos);

_g int parse_allcons(Execute ptr, addr *ret, addr cons);
_g int parse_execute(Execute ptr, addr *ret, addr pos);
_g int parse_allcons_toplevel(Execute ptr, addr *ret, addr cons);
_g int parse_execute_toplevel(Execute ptr, addr *ret, addr pos);

_g int parse_ordinary(Execute ptr, addr *ret, addr args);

#endif

