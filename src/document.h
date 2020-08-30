#ifndef __DOCUMENT_HEADER__
#define __DOCUMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define init_documentation _n(init_documentation)
#define build_documentation _n(build_documentation)

_g void init_documentation(void);
_g void build_documentation(Execute ptr);

#endif

