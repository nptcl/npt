#ifndef __EVAL_COPY_HEADER__
#define __EVAL_COPY_HEADER__

#include "local.h"
#include "typedef.h"

_g void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval);
_g void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval);
_g void copy_eval_parse_heap(addr *ret, addr eval);

#endif

