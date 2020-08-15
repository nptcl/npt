#ifndef __TYPE_TYPEP_HEADER__
#define __TYPE_TYPEP_HEADER__

#include "execute.h"
#include "typedef.h"

_g int typep_table_(Execute ptr, addr value, addr type, int *ret);
_g int typep_clang_(Execute ptr, addr value, addr type, int *ret);
_g int typep_asterisk_clang_(Execute ptr, addr value, addr type, int *ret);
_g void init_type_typep(void);

#endif

