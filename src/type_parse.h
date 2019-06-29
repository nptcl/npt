#ifndef __TYPE_PARSE_HEADER__
#define __TYPE_PARSE_HEADER__

#include "execute.h"
#include "typedef.h"

_g void init_type_parse(void);
_g void build_type_parse(void);
_g int parse_type(Execute ptr, addr *ret, addr pos, addr env);
_g int parse_type_not(Execute ptr, addr *ret, addr pos, addr env);
_g int parse_type_noaster(Execute ptr, addr *ret, addr pos, addr env);
_g void parse_type_unsafe(addr *ret, addr pos);

/* debug */
_g int parse_type_values(Execute ptr, addr *ret, addr type, addr env);

#endif

