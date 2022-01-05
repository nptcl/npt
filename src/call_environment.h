#ifndef __CALL_ENVIRONMENT_HEADER__
#define __CALL_ENVIRONMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define apropos_common_ _n(apropos_common_)
#define apropos_list_common_ _n(apropos_list_common_)
#define time_common_ _n(time_common_)
#define room_common_ _n(room_common_)
#define ed_common_ _n(ed_common_)
#define dribble_common_ _n(dribble_common_)

int apropos_common_(Execute ptr, addr var, addr package);
int apropos_list_common_(Execute ptr, addr var, addr package, addr *ret);
int time_common_(addr form, addr env, addr *ret);
int room_common_(Execute ptr, addr var);
int ed_common_(Execute ptr, addr var);
int dribble_common_(Execute ptr, addr file);

#endif

