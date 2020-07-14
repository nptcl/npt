#ifndef __ENV_VERSION_HEADER__
#define __ENV_VERSION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g void implementation_type_common(addr *ret);
_g void implementation_version_common(addr *ret);
_g int short_site_name_common(addr *ret);
_g int long_site_name_common(addr *ret);
_g int machine_instance_common(addr *ret);
_g int machine_type_common(addr *ret);
_g int machine_version_common(addr *ret);
_g int software_type_common(addr *ret);
_g int software_version_common(addr *ret);
_g void user_homedir_pathname_common(Execute ptr, addr *ret);

#endif

