#ifndef __DOCUMENT_HEADER__
#define __DOCUMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define init_documentation _n(init_documentation)
#define build_documentation _n(build_documentation)

void init_documentation(void);
void build_documentation(Execute ptr);

#endif

