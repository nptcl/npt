#ifndef __PROCESS_CALLTYPE_HEADER__
#define __PROCESS_CALLTYPE_HEADER__

#include "pointer_type.h"
#include "typedef.h"

#define process_calltype_ _n(process_calltype_)

int process_calltype_(addr pos, enum CallBind_index *ret);

#endif

