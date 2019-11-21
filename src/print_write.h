#ifndef __PRINT_TYPE_HEADER__
#define __PRINT_TYPE_HEADER__

#include "define.h"
#include "print.h"
#include "typedef.h"

_g int write_default_print(Execute ptr, addr stream, addr object);
_g int write_print(Execute ptr, addr stream, addr object);
_g int princ_print(Execute ptr, addr stream, addr object);
_g int prin1_print(Execute ptr, addr stream, addr object);
_g int print_print(Execute ptr, addr stream, addr object);
_g int pprint_print(Execute ptr, addr stream, addr object);
_g int princ_string_heap(Execute ptr, addr *ret, addr object);
_g int princ_string_local(Execute ptr, addr *ret, addr object);
_g int prin1_string_heap(Execute ptr, addr *ret, addr object);
_g int prin1_string_local(Execute ptr, addr *ret, addr object);
_g void init_print_write(void);

#endif

