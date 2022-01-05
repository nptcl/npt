#ifndef __ENV_VERSION_HEADER__
#define __ENV_VERSION_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define implementation_type_common _n(implementation_type_common)
#define implementation_version_common _n(implementation_version_common)
#define short_site_name_common_ _n(short_site_name_common_)
#define long_site_name_common_ _n(long_site_name_common_)
#define machine_instance_common_ _n(machine_instance_common_)
#define machine_type_common_ _n(machine_type_common_)
#define machine_version_common_ _n(machine_version_common_)
#define software_type_common_ _n(software_type_common_)
#define software_version_common_ _n(software_version_common_)
#define user_homedir_pathname_common_ _n(user_homedir_pathname_common_)

void implementation_type_common(addr *ret);
void implementation_version_common(addr *ret);
int short_site_name_common_(addr *ret);
int long_site_name_common_(addr *ret);
int machine_instance_common_(addr *ret);
int machine_type_common_(addr *ret);
int machine_version_common_(addr *ret);
int software_type_common_(addr *ret);
int software_version_common_(addr *ret);
int user_homedir_pathname_common_(Execute ptr, addr *ret);

#endif

