#ifndef __COMPILE_TYPE_HEADER__
#define __COMPILE_TYPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslwrite_value_type_ _n(faslwrite_value_type_)
#define faslread_value_type_ _n(faslread_value_type_)

int faslwrite_value_type_(Execute ptr, addr stream, addr pos);
int faslread_value_type_(Execute ptr, addr stream, addr *ret);

#endif

