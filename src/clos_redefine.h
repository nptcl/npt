#ifndef __CLOS_REDEFINE_HEADER__
#define __CLOS_REDEFINE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define clos_ensure_class_redefine_ _n(clos_ensure_class_redefine_)
#define clos_version_diff_p_ _n(clos_version_diff_p_)
#define clos_version_check_ _n(clos_version_check_)
#define clos_redefine_method_ _n(clos_redefine_method_)
#define clos_change_class_ _n(clos_change_class_)
#define clos_change_method_ _n(clos_change_method_)

_g int clos_ensure_class_redefine_(Execute ptr, addr clos, addr name, addr rest);
_g int clos_version_diff_p_(addr pos, int *ret);
_g int clos_version_check_(Execute ptr, addr pos, addr clos);
_g int clos_redefine_method_(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest);
_g int clos_change_class_(Execute ptr, addr pos, addr clos, addr rest);
_g int clos_change_method_(Execute ptr, addr prev, addr inst, addr rest);

#endif

