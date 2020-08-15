#ifndef __ENV_TIME_HEADER__
#define __ENV_TIME_HEADER__

#include "define.h"
#include "local.h"
#include "typedef.h"

struct universal_time_struct {
	addr second, minute, hour, date, month, year, week, daylight_p, zone;
};

_g int decode_universal_time_common_(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone);
_g int encode_universal_time_common_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		addr day, addr month, addr year, addr zone);
_g int get_universal_time_common_(LocalRoot local, addr *ret);
_g int get_decoded_time_common_(LocalRoot local, struct universal_time_struct *u);
_g void get_internal_time_units_per_second(fixnum *ret);
_g int get_internal_real_time_common_(LocalRoot local, addr *ret);
_g void get_internal_run_time_common(addr *ret);
_g int sleep_common_(Execute ptr, addr var);

_g void init_environemnt_time(void);

#endif

