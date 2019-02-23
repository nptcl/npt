#ifndef __HEADER_TYPE_TYPEP__
#define __HEADER_TYPE_TYPEP__

#include "typedef.h"

int typep_table(addr value, addr type);
int typep_clang(addr value, addr type);
int typep_asterisk_clang(addr value, addr type);

int typep_throw(LocalRoot local, addr value, addr type);
int typep_asterisk_throw(LocalRoot local, addr value, addr type);

void init_type_typep(void);

#endif

