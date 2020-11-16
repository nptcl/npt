#ifndef __ENVIRONMENT_HEADER__
#define __ENVIRONMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define init_environment _n(init_environment)
#define build_environment _n(build_environment)

void init_environment(void);
void build_environment(Execute ptr);

#endif

