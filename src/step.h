#ifndef __STEP_HEADER__
#define __STEP_HEADER__

#include "execute.h"
#include "typedef.h"

#define step_common _n(step_common)
#define init_parse_step _n(init_parse_step)
#define parse_step _n(parse_step)
#define parse_step_object_ _n(parse_step_object_)
#define copy_eval_step _n(copy_eval_step)
#define scope_step _n(scope_step)

/* macro */
int step_common(Execute ptr, addr form, addr env, addr *ret);

/* parse */
void init_parse_step(Execute ptr);
int parse_step(Execute ptr, addr *ret, addr form);
int parse_step_object_(Execute ptr, addr *ret, addr value, addr expr);

/* copy-eval */
void copy_eval_step(LocalRoot local, addr *ret, addr eval);

/* scope */
int scope_step(Execute ptr, addr *ret, addr eval);

#endif

