#ifndef __EQUAL_HEADER__
#define __EQUAL_HEADER__

#include "build.h"

_g int atom_function(addr pos);
_g int eq_function(addr left, addr right);
_g int eq_function_(addr left, addr right, int *ret);
_g int eql_function(addr left, addr right);
_g int eql_function_(addr left, addr right, int *ret);
_g int equal_function_(addr left, addr right, int *ret);
_g int equalp_function_(addr left, addr right, int *ret);
_g int equalrt_function_(addr left, addr right, int *ret);

_g int equal_debug(addr left, addr right);
_g int equalp_debug(addr left, addr right);
_g int equalrt_debug(addr left, addr right);

#endif

