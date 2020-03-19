#ifndef __PRINT_FUNCTION_HEADER__
#define __PRINT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int pprint_fill_print(Execute ptr, addr stream, addr list, int colon);
_g int pprint_linear_print(Execute ptr, addr stream, addr list, int colon);
_g int pprint_tabular_print(Execute ptr,
		addr stream, addr list, int colon, fixnum tabsize);
_g void init_print_function(void);

#endif

