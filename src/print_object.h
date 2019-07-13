#ifndef __PRINT_OBJECT__
#define __PRINT_OBJECT__

#include "define.h"
#include "execute.h"

_g int print_structure(Execute ptr, addr stream, addr pos);
_g void build_print_object(Execute ptr);
_g void init_print_object(void);

#endif

