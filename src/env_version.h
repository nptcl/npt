#ifndef __ENV_VERSION_HEADER__
#define __ENV_VERSION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

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

