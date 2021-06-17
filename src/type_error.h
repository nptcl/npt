#ifndef __TYPE_ERROR_HEADER__
#define __TYPE_ERROR_HEADER__

#include "execute.h"
#include "typedef.h"

#define get_error_type_ _n(get_error_type_)
#define check_error_type_ _n(check_error_type_)
#define execute_error_type_ _n(execute_error_type_)
#define init_type_error _n(init_type_error)

int get_error_type_(Execute ptr, addr pos, addr *ret);
int check_error_type_(Execute ptr, addr pos, int *ret);
int execute_error_type_(Execute ptr, addr pos);
void init_type_error(void);

#endif

