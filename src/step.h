#ifndef __STEP_HEADER__
#define __STEP_HEADER__

#include "execute.h"
#include "typedef.h"

#define step_common_ _n(step_common_)
#define parse_step_ _n(parse_step_)
#define copy_eval_step _n(copy_eval_step)
#define scope_step_ _n(scope_step_)
#define init_parse_step _n(init_parse_step)

int step_common_(Execute ptr, addr form, addr env, addr *ret);
int parse_step_(Execute ptr, addr *ret, addr form);
void copy_eval_step(LocalRoot local, addr *ret, addr eval);
int scope_step_(Execute ptr, addr *ret, addr eval);
void init_parse_step(Execute ptr);

#endif

