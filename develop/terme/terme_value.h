#ifndef __TERME_VALUE_HEADER__
#define __TERME_VALUE_HEADER__

#include "execute.h"
#include "prompt.h"
#include "typedef.h"

#define terme_build _n(terme_build)
#define terme_value_data_ _n(terme_value_data_)
#define terme_value_width_ _n(terme_value_width_)
#define terme_value_history_ _n(terme_value_history_)
#define get_history_terme_ _n(get_history_terme_)
#define set_history_terme_ _n(set_history_terme_)
#define terme_set_prompt_ _n(terme_set_prompt_)
#define terme_get_prompt_ _n(terme_get_prompt_)

void terme_build(void);
int terme_value_data_(Execute ptr, addr *ret);
int terme_value_width_(Execute ptr, addr *ret);
int terme_value_history_(Execute ptr, addr *ret);
int get_history_index_terme_(Execute ptr, int *ret);
int set_history_index_terme_(Execute ptr, int value);
int terme_set_prompt_(Execute ptr, addr value, enum prompt_mode mode);
int terme_get_prompt_(Execute ptr, addr *value, enum prompt_mode *mode);

#endif

