#ifndef __EVAL_COPY_HEADER__
#define __EVAL_COPY_HEADER__

#include "local.h"
#include "typedef.h"

#define copy_eval_parse_alloc _n(copy_eval_parse_alloc)
#define copy_eval_parse_local _n(copy_eval_parse_local)
#define copy_eval_parse_heap _n(copy_eval_parse_heap)
#define copy_eval_parse _n(copy_eval_parse)
#define init_eval_copy _n(init_eval_copy)

void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval);
void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval);
void copy_eval_parse_heap(addr *ret, addr eval);
void copy_eval_parse(LocalRoot local, addr *ret, addr pos);
void init_eval_copy(void);

#endif

