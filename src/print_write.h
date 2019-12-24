#ifndef __PRINT_TYPE_HEADER__
#define __PRINT_TYPE_HEADER__

#include "define.h"
#include "print.h"
#include "typedef.h"

_g void push_write_object(Execute ptr);
_g void getdepth_print_write(Execute ptr, size_t *ret);
_g void setdepth_print_write(Execute ptr, size_t value);
_g void write_check_all_clear(Execute ptr);
_g void write_check_call(Execute ptr, addr pos);
_g int pprint_pop_circle(Execute ptr, addr stream, addr pos);
_g int pprint_check_circle(Execute ptr, addr pos, addr *ret);
_g int write_default_print(Execute ptr, addr stream, addr pos);
_g int write_print(Execute ptr, addr stream, addr pos);
_g int princ_print(Execute ptr, addr stream, addr pos);
_g int prin1_print(Execute ptr, addr stream, addr pos);
_g int print_print(Execute ptr, addr stream, addr pos);
_g int pprint_print(Execute ptr, addr stream, addr pos);
_g int write_string_heap(Execute ptr, addr *ret, addr pos);
_g int write_string_local(Execute ptr, addr *ret, addr pos);
_g int princ_string_heap(Execute ptr, addr *ret, addr pos);
_g int princ_string_local(Execute ptr, addr *ret, addr pos);
_g int prin1_string_heap(Execute ptr, addr *ret, addr pos);
_g int prin1_string_local(Execute ptr, addr *ret, addr pos);
_g void init_print_write(void);

#endif

