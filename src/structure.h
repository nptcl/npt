#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "typedef.h"

int structure_instance_p(addr pos);
int equalp_structure(addr left, addr right);
int equalrt_structure(addr left, addr right);
void ensure_structure_common(Execute ptr, addr name, addr slots, addr rest);
void copy_structure_common(addr var, addr *ret);

#endif

