#ifndef __SUBTYPEP_TABLE_HEADER__
#define __SUBTYPEP_TABLE_HEADER__

#include "execute.h"
#include "subtypep_typedef.h"
#include "typedef.h"

#define init_subtypep_table _n(init_subtypep_table)
#define subtypep_table_ _n(subtypep_table_)

void init_subtypep_table(void);
int subtypep_table_(Execute ptr, addr left, addr right, SubtypepResult *ret);

#endif

