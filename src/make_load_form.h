#ifndef __MAKE_LOAD_FORM_HEADER__
#define __MAKE_LOAD_FORM_HEADER__

#include "execute.h"
#include "typedef.h"

_g void init_parse_make_load_form(Execute ptr);
_g int parse_clos(Execute ptr, addr *ret, addr pos);

/* compile-value */
_g void init_write_make_load_form(Execute ptr);
_g void init_read_make_load_form(Execute ptr);
_g int get_write_make_load_form_(Execute ptr, addr key, addr *ret);
_g int get_read_make_load_form_(Execute ptr, addr key, addr *ret);

#endif

