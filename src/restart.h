#ifndef __RESTART_HEADER__
#define __RESTART_HEADER__

#include "typedef.h"

#define restartp _n(restartp)
#define restart_heap _n(restart_heap)
#define getname_restart _n(getname_restart)
#define setname_restart _n(setname_restart)
#define getfunction_restart _n(getfunction_restart)
#define setfunction_restart _n(setfunction_restart)
#define getinteractive_restart _n(getinteractive_restart)
#define setinteractive_restart _n(setinteractive_restart)
#define getreport_restart _n(getreport_restart)
#define setreport_restart _n(setreport_restart)
#define gettest_restart _n(gettest_restart)
#define settest_restart _n(settest_restart)
#define getcondition_restart _n(getcondition_restart)
#define setcondition_restart _n(setcondition_restart)
#define getassociated_restart _n(getassociated_restart)
#define setassociated_restart _n(setassociated_restart)
#define setescape_restart _n(setescape_restart)
#define getescape_restart _n(getescape_restart)
#define setenable_restart _n(setenable_restart)
#define getenable_restart _n(getenable_restart)
#define init_restart _n(init_restart)

int restartp(addr pos);
void restart_heap(addr *ret, addr name);
void getname_restart(addr pos, addr *ret);
void setname_restart(addr pos, addr value);
void getfunction_restart(addr pos, addr *ret);
void setfunction_restart(addr pos, addr value);
void getinteractive_restart(addr pos, addr *ret);
void setinteractive_restart(addr pos, addr value);
void getreport_restart(addr pos, addr *ret);
void setreport_restart(addr pos, addr value);
void gettest_restart(addr pos, addr *ret);
void settest_restart(addr pos, addr value);
void getcondition_restart(addr pos, addr *ret);
void setcondition_restart(addr pos, addr value);
void getassociated_restart(addr pos, addr *ret);
void setassociated_restart(addr pos, addr value);
void setescape_restart(addr pos, int value);
int getescape_restart(addr pos);
void setenable_restart(addr pos, int value);
int getenable_restart(addr pos);
void init_restart(void);

#endif

