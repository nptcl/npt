#ifndef __COMPILE_TYPE_HEADER__
#define __COMPILE_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslwrite_value_type _n(faslwrite_value_type)
#define faslread_value_type _n(faslread_value_type)

int faslwrite_value_type(Execute ptr, addr stream, addr pos);
int faslread_value_type(Execute ptr, addr stream, addr *ret);

#endif

