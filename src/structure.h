#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int structure_instance_p(addr pos);
_g int equalp_structure(addr left, addr right);
_g int equalrt_structure(addr left, addr right);
_g void ensure_structure_common(Execute ptr, addr name, addr slots, addr rest);
_g void copy_structure_common(addr var, addr *ret);

#endif

