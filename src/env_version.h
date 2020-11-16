#ifndef __ENV_VERSION_HEADER__
#define __ENV_VERSION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define implementation_type_common _n(implementation_type_common)
#define implementation_version_common _n(implementation_version_common)
#define short_site_name_common _n(short_site_name_common)
#define long_site_name_common _n(long_site_name_common)
#define machine_instance_common _n(machine_instance_common)
#define machine_type_common _n(machine_type_common)
#define machine_version_common _n(machine_version_common)
#define software_type_common _n(software_type_common)
#define software_version_common _n(software_version_common)
#define user_homedir_pathname_common _n(user_homedir_pathname_common)

void implementation_type_common(addr *ret);
void implementation_version_common(addr *ret);
int short_site_name_common(addr *ret);
int long_site_name_common(addr *ret);
int machine_instance_common(addr *ret);
int machine_type_common(addr *ret);
int machine_version_common(addr *ret);
int software_type_common(addr *ret);
int software_version_common(addr *ret);
int user_homedir_pathname_common(Execute ptr, addr *ret);

#endif

