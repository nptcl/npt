#ifndef __STRUCTURE_PARSE_HEADER__
#define __STRUCTURE_PARSE_HEADER__

#include "execute.h"
#include "local.h"
#include "structure_defstruct.h"
#include "typedef.h"

#define ensure_structure_struct_ _n(ensure_structure_struct_)
#define structure_arguments_ _n(structure_arguments_)

int ensure_structure_struct_(struct defstruct *str,
		Execute ptr, addr name, addr slots, addr args);
int structure_arguments_(struct defstruct *str, LocalHold hold);

#endif

