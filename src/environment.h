#ifndef __ENVIRONMENT_HEADER__
#define __ENVIRONMENT_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

struct universal_time_struct {
	addr second, minute, hour, date, month, year, day, daylight_p, zone;
};

_g void decode_universal_time(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone);
_g int disassemble_common(Execute ptr, addr stream, addr pos);
_g void implementation_type_common(addr *ret);
_g void implementation_version_common(addr *ret);
_g void short_site_name_common(addr *ret);
_g void long_site_name_common(addr *ret);
_g void machine_instance_common(addr *ret);
_g void machine_type_common(addr *ret);
_g void machine_version_common(addr *ret);
_g void software_type_common(addr *ret);
_g void software_version_common(addr *ret);
_g void user_homedir_pathname_common(Execute ptr, addr *ret);

#endif

