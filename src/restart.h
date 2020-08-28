#ifndef __RESTART_HEADER__
#define __RESTART_HEADER__

#include "typedef.h"

_g int restartp(addr pos);
_g void restart_heap(addr *ret, addr name);
_g void getname_restart(addr pos, addr *ret);
_g void setname_restart(addr pos, addr value);
_g void getfunction_restart(addr pos, addr *ret);
_g void setfunction_restart(addr pos, addr value);
_g void getinteractive_restart(addr pos, addr *ret);
_g void setinteractive_restart(addr pos, addr value);
_g void getreport_restart(addr pos, addr *ret);
_g void setreport_restart(addr pos, addr value);
_g void gettest_restart(addr pos, addr *ret);
_g void settest_restart(addr pos, addr value);
_g void getcondition_restart(addr pos, addr *ret);
_g void setcondition_restart(addr pos, addr value);
_g void getreference_restart(addr pos, addr *ret);
_g void setreference_restart(addr pos, addr value);
_g void setescape_restart(addr pos, int value);
_g int getescape_restart(addr pos);
_g void setenable_restart(addr pos, int value);
_g int getenable_restart(addr pos);
_g void setredirect_restart(addr pos, int value);
_g int getredirect_restart(addr pos);

_g void init_restart(void);

#endif

