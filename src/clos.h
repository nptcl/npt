#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "execute.h"

#define CLOS_TABLE_CLASS_SIZE            256
#define CLOS_TABLE_COMBINATION_SIZE      32
#define CLOS_TABLE_SPECIALIZER_SIZE      32

void build_clos(Execute ptr);
void build_clos_table(Execute ptr);  /* for debug */

#endif

