#ifndef __OPTIMIZE_PARSE_HEADER__
#define __OPTIMIZE_PARSE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define optimize_parse_ _n(optimize_parse_)

int optimize_parse_(Execute ptr, addr pos, addr *value, int *ret);

#endif

