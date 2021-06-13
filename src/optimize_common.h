#ifndef __OPTIMIZE_COMMON_HEADER__
#define __OPTIMIZE_COMMON_HEADER__

#include "define.h"
#include "local.h"
#include "make_typedef.h"
#include "typedef.h"

#define optimize_common_ _n(optimize_common_)
#define optimize_check_code_ _n(optimize_check_code_)
#define init_optimize_common _n(init_optimize_common)
#define build_optimize_common _n(build_optimize_common)

int optimize_common_(CodeMake ptr, addr scope, int *ret);
int optimize_check_code_(CodeMake ptr, addr scope, int *ret);

void init_optimize_common(void);
void build_optimize_common(void);

#endif

