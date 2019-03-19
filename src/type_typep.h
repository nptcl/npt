#ifndef __TYPE_TYPEP_HEADER__
#define __TYPE_TYPEP_HEADER__

#include "typedef.h"

int typep_table(addr value, addr type, int *ret);
int typep_clang(addr value, addr type, int *ret);
int typep_asterisk_clang(addr value, addr type, int *ret);
void init_type_typep(void);

#endif

