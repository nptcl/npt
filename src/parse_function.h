#ifndef __PARSE_FUNCTION_HEADER__
#define __PARSE_FUNCTION_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define localhold_parse_allcons_ _n(localhold_parse_allcons_)
#define localhold_parse_execute_ _n(localhold_parse_execute_)
#define parse_allcons_ _n(parse_allcons_)
#define parse_execute_ _n(parse_execute_)
#define parse_allcons_toplevel_ _n(parse_allcons_toplevel_)
#define parse_execute_toplevel_ _n(parse_execute_toplevel_)
#define parse_ordinary_ _n(parse_ordinary_)

#define localhold_parse_self_(h,p,x) localhold_parse_execute_((h),(p),&(x),(x))
#define parse_self_(p, x) parse_execute_((p), &(x), (x))

int localhold_parse_allcons_(LocalHold hold, Execute ptr, addr *ret, addr cons);
int localhold_parse_execute_(LocalHold hold, Execute ptr, addr *ret, addr pos);

int parse_allcons_(Execute ptr, addr *ret, addr cons);
int parse_execute_(Execute ptr, addr *ret, addr pos);
int parse_allcons_toplevel_(Execute ptr, addr *ret, addr cons);
int parse_execute_toplevel_(Execute ptr, addr *ret, addr pos);

int parse_ordinary_(Execute ptr, addr *ret, addr args);

#endif

