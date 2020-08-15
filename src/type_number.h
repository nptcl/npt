#ifndef __TYPE_NUMBER_HEADER__
#define __TYPE_NUMBER_HEADER__

#include "define.h"
#include "type.h"

_g int real_extract_local_(LocalRoot local, addr *ret, addr type);
_g int real_extract_heap_(LocalRoot local, addr *ret, addr type);
_g int type_subtypep_p(addr type);
_g int type_optimized_or_subtypep(addr type);
_g void get_type_subtypep(addr *ret, addr type);

#endif

