#ifndef __TYPE_TYPEP_HEADER__
#define __TYPE_TYPEP_HEADER__

#include "execute.h"
#include "typedef.h"

#define typep_table_ _n(typep_table_)
#define typep_clang_ _n(typep_clang_)
#define typep_asterisk_clang_ _n(typep_asterisk_clang_)
#define init_type_typep _n(init_type_typep)

int typep_table_(Execute ptr, addr value, addr type, int *ret);
int typep_clang_(Execute ptr, addr value, addr type, int *ret);
int typep_asterisk_clang_(Execute ptr, addr value, addr type, int *ret);
void init_type_typep(void);

#endif

