#ifndef __ENV_TIME_HEADER__
#define __ENV_TIME_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

#define decode_universal_time_call_ _n(decode_universal_time_call_)
#define encode_universal_time_call_ _n(encode_universal_time_call_)
#define get_universal_time_call_ _n(get_universal_time_call_)
#define get_decoded_time_call_ _n(get_decoded_time_call_)
#define get_internal_time_units_per_second _n(get_internal_time_units_per_second)
#define get_internal_real_time_common_ _n(get_internal_real_time_common_)
#define get_internal_run_time_common _n(get_internal_run_time_common)
#define sleep_common_ _n(sleep_common_)

struct universal_time_struct {
	addr second, minute, hour, date, month, year, week, daylight_p, zone;
};

int decode_universal_time_call_(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone);
int encode_universal_time_call_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		addr day, addr month, addr year, addr zone);
int get_universal_time_call_(LocalRoot local, addr *ret);
int get_decoded_time_call_(LocalRoot local, struct universal_time_struct *u);
void get_internal_time_units_per_second(fixnum *ret);
int get_internal_real_time_common_(LocalRoot local, addr *ret);
void get_internal_run_time_common(addr *ret);
int sleep_common_(Execute ptr, addr var);

#endif

