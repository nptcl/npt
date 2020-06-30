#ifndef __STEP_HEADER__
#define __STEP_HEADER__

#include "execute.h"
#include "typedef.h"

/* macro */
_g void step_common(Execute ptr, addr form, addr env, addr *ret);

/* parse */
_g void init_parse_step(Execute ptr);
_g int parse_step(Execute ptr, addr *ret, addr form);
_g void parse_step_object(Execute ptr, addr *ret, addr value, addr expr);

/* copy-eval */
_g void copy_eval_step(LocalRoot local, addr *ret, addr eval);

/* scope */
_g int scope_step(Execute ptr, addr *ret, addr eval);

#endif

