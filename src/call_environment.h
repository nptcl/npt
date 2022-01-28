#ifndef __CALL_ENVIRONMENT_HEADER__
#define __CALL_ENVIRONMENT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define decode_universal_time_common_ _n(decode_universal_time_common_)
#define encode_universal_time_common_ _n(encode_universal_time_common_)
#define get_decoded_time_common_ _n(get_decoded_time_common_)
#define apropos_common_ _n(apropos_common_)
#define apropos_list_common_ _n(apropos_list_common_)
#define time_common_ _n(time_common_)
#define room_common_ _n(room_common_)
#define ed_common_ _n(ed_common_)
#define dribble_common_ _n(dribble_common_)

int decode_universal_time_common_(LocalRoot local, addr pos, addr zone,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone);
int encode_universal_time_common_(LocalRoot local, addr rest, addr *ret);
int get_decoded_time_common_(LocalRoot local,
		addr *rsecond, addr *rminute, addr *rhour,
		addr *rdate, addr *rmonth, addr *ryear,
		addr *rweek, addr *rdaylight, addr *rzone);
int apropos_common_(Execute ptr, addr var, addr package);
int apropos_list_common_(Execute ptr, addr var, addr package, addr *ret);
int time_common_(addr form, addr env, addr *ret);
int room_common_(Execute ptr, addr var);
int ed_common_(Execute ptr, addr var);
int dribble_common_(Execute ptr, addr file);

#endif

