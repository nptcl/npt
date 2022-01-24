#ifndef __PRINT_FUNCTION_HEADER__
#define __PRINT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define pprint_fill_print_ _n(pprint_fill_print_)
#define pprint_linear_print_ _n(pprint_linear_print_)
#define pprint_tabular_print_ _n(pprint_tabular_print_)
#define init_print_function _n(init_print_function)

int pprint_fill_print_(Execute ptr, addr stream, addr list, int colon);
int pprint_linear_print_(Execute ptr, addr stream, addr list, int colon);
int pprint_tabular_print_(Execute ptr,
		addr stream, addr list, int colon, fixnum tabsize);
void init_print_function(void);

#endif

