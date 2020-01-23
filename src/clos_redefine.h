#ifndef __CLOS_REDEFINE_HEADER__
#define __CLOS_REDEFINE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int clos_ensure_class_redefine(Execute ptr, addr clos, addr name, addr rest);
_g int clos_version_diff_p(addr pos);
_g int clos_version_check(Execute ptr, addr pos, addr clos);
_g void clos_redefine_method(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest);
_g int clos_change_class(Execute ptr, addr pos, addr clos, addr rest);
_g void clos_change_method(Execute ptr, addr prev, addr inst, addr rest);

#endif

