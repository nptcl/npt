#ifndef __PRINT_FUNCTION_HEADER__
#define __PRINT_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int pprint_fill_common(Execute ptr, addr stream, addr list, int colon);
_g int pprint_linear_common(Execute ptr, addr stream, addr list, int colon);
_g int pprint_tabular_common(Execute ptr,
		addr stream, addr list, int colon, fixnum size);

_g int cons_default_dispatch(Execute ptr, addr stream, addr pos);
_g int vector_default_dispatch(Execute ptr, addr stream, addr pos);
_g int defun_default_dispatch(Execute ptr, addr stream, addr pos);
_g int let_default_dispatch(Execute ptr, addr stream, addr pos);

_g void init_print_function(void);

#endif

