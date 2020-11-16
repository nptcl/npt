#ifndef __CALL_ENVIRONMENT_HEADER__
#define __CALL_ENVIRONMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define apropos_common _n(apropos_common)
#define apropos_list_common _n(apropos_list_common)
#define time_common _n(time_common)
#define room_common _n(room_common)
#define ed_common _n(ed_common)
#define dribble_common _n(dribble_common)

int apropos_common(Execute ptr, addr var, addr package);
int apropos_list_common(Execute ptr, addr var, addr package, addr *ret);
int time_common(addr form, addr env, addr *ret);
int room_common(Execute ptr, addr var);
int ed_common(Execute ptr, addr var);
int dribble_common(Execute ptr, addr file);

#endif

