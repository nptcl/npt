#ifndef __STEP_PROMPT_HEADER__
#define __STEP_PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

#define execute_step_code _n(execute_step_code)

int execute_step_code(Execute ptr, addr expr);

#endif

