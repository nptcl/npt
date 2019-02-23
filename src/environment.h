#ifndef __ENVIRONMENT_HEADER__
#define __ENVIRONMENT_HEADER__

#include "execute.h"
#include "typedef.h"

void implementation_type_common(addr *ret);
void implementation_version_common(addr *ret);
void short_site_name_common(addr *ret);
void long_site_name_common(addr *ret);
void machine_instance_common(addr *ret);
void machine_type_common(addr *ret);
void machine_version_common(addr *ret);
void software_type_common(addr *ret);
void software_version_common(addr *ret);
void user_homedir_pathname_common(Execute ptr, addr *ret);

#endif

