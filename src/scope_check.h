#ifndef __SCOPE_CHECK_HEADER__
#define __SCOPE_CHECK_HEADER__

#include "execute.h"
#include "typedef.h"

#define scope_call_call_ _n(scope_call_call_)

int scope_call_call_(Execute ptr, addr first, addr args, addr *ret);

#endif

