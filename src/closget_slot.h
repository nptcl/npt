#ifndef __CLOSGET_SLOT_HEADER__
#define __CLOSGET_SLOT_HEADER__

#include "execute.h"
#include "typedef.h"

#define getname_slot _n(getname_slot)
#define gettype_slot _n(gettype_slot)
#define getargs_slot _n(getargs_slot)
#define getform_slot _n(getform_slot)
#define getfunction_slot _n(getfunction_slot)
#define getreaders_slot _n(getreaders_slot)
#define getwriters_slot _n(getwriters_slot)
#define getdocument_slot _n(getdocument_slot)
#define getclass_slot _n(getclass_slot)
#define getreadonly_slot _n(getreadonly_slot)
#define getallocation_slot _n(getallocation_slot)
#define getlocation_slot _n(getlocation_slot)
#define getaccess_slot _n(getaccess_slot)

#define setname_slot _n(setname_slot)
#define settype_slot _n(settype_slot)
#define setargs_slot _n(setargs_slot)
#define setform_slot _n(setform_slot)
#define setfunction_slot _n(setfunction_slot)
#define setreaders_slot _n(setreaders_slot)
#define setwriters_slot _n(setwriters_slot)
#define setdocument_slot _n(setdocument_slot)
#define setclass_slot _n(setclass_slot)
#define setreadonly_slot _n(setreadonly_slot)
#define setallocation_slot _n(setallocation_slot)
#define setlocation_slot _n(setlocation_slot)
#define setaccess_slot _n(setaccess_slot)

void getname_slot(addr pos, addr *ret);
void gettype_slot(addr pos, addr *ret);
void getargs_slot(addr pos, addr *ret);
void getform_slot(addr pos, addr *ret);
void getfunction_slot(addr pos, addr *ret);
void getreaders_slot(addr pos, addr *ret);
void getwriters_slot(addr pos, addr *ret);
void getdocument_slot(addr pos, addr *ret);
void getclass_slot(addr pos, addr *ret);
void getreadonly_slot(addr pos, addr *ret);
void getallocation_slot(addr pos, int *ret);
void getlocation_slot(addr pos, size_t *ret);
void getaccess_slot(addr pos, size_t *ret);

void setname_slot(addr pos, addr value);
void settype_slot(addr pos, addr value);
void setargs_slot(addr pos, addr value);
void setform_slot(addr pos, addr value);
void setfunction_slot(addr pos, addr value);
void setreaders_slot(addr pos, addr value);
void setwriters_slot(addr pos, addr value);
void setdocument_slot(addr pos, addr value);
void setclass_slot(addr pos, addr value);
void setreadonly_slot(addr pos, addr value);
void setallocation_slot(addr pos, int value);
void setlocation_slot(addr pos, size_t value);
void setaccess_slot(addr pos, size_t value);

#endif

