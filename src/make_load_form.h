#ifndef __MAKE_LOAD_FORM_HEADER__
#define __MAKE_LOAD_FORM_HEADER__

#include "execute.h"
#include "typedef.h"

_g void push_make_load_form(Execute ptr);
_g int parse_clos(Execute ptr, addr *ret, addr pos);

#endif

