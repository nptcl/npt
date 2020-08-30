#ifndef __PRINT_FUNCTION_HEADER__
#define __PRINT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define pprint_fill_print _n(pprint_fill_print)
#define pprint_linear_print _n(pprint_linear_print)
#define pprint_tabular_print _n(pprint_tabular_print)
#define init_print_function _n(init_print_function)

_g int pprint_fill_print(Execute ptr, addr stream, addr list, int colon);
_g int pprint_linear_print(Execute ptr, addr stream, addr list, int colon);
_g int pprint_tabular_print(Execute ptr,
		addr stream, addr list, int colon, fixnum tabsize);
_g void init_print_function(void);

#endif

