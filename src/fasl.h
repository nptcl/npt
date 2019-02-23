#ifndef __FASL_HEADER__
#define __FASL_HEADER__

#include "execute.h"
#include "typedef.h"

void init_fasl(void);
void build_fasl(void);
int fasl_stream(Execute ptr, addr stream);

#endif

