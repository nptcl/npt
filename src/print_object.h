#ifndef __PRINT_OBJECT__
#define __PRINT_OBJECT__

#include "define.h"
#include "execute.h"

#define print_structure _n(print_structure)
#define build_print_object_ _n(build_print_object_)
#define init_print_object _n(init_print_object)

_g int print_structure(Execute ptr, addr stream, addr pos);
_g int build_print_object_(Execute ptr);
_g void init_print_object(void);

#endif

