#ifndef __TYPE_DELAY_HEADER__
#define __TYPE_DELAY_HEADER__

#include "execute.h"
#include "typedef.h"

#define get_delay_type_ _n(get_delay_type_)
#define check_delay_type_ _n(check_delay_type_)
#define execute_delay_type_ _n(execute_delay_type_)
#define init_type_delay _n(init_type_delay)

int get_delay_type_(Execute ptr, addr pos, addr *ret);
int check_delay_type_(Execute ptr, addr pos, int *ret);
int execute_delay_type_(Execute ptr, addr pos);
void init_type_delay(void);

#endif

