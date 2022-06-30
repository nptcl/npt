#ifndef __CLOSGET_SLOT_HEADER__
#define __CLOSGET_SLOT_HEADER__

#include "execute.h"
#include "typedef.h"

#define getname_slot_ _n(getname_slot_)
#define gettype_slot_ _n(gettype_slot_)
#define getargs_slot_ _n(getargs_slot_)
#define getform_slot_ _n(getform_slot_)
#define getfunction_slot_ _n(getfunction_slot_)
#define getreaders_slot_ _n(getreaders_slot_)
#define getwriters_slot_ _n(getwriters_slot_)
#define getdocumentation_slot_ _n(getdocumentation_slot_)
#define getclass_slot_ _n(getclass_slot_)
#define getreadonly_slot_ _n(getreadonly_slot_)
#define getallocation_slot_ _n(getallocation_slot_)
#define getlocation_slot_ _n(getlocation_slot_)
#define getaccess_slot_ _n(getaccess_slot_)

#define setname_slot_ _n(setname_slot_)
#define settype_slot_ _n(settype_slot_)
#define setargs_slot_ _n(setargs_slot_)
#define setform_slot_ _n(setform_slot_)
#define setfunction_slot_ _n(setfunction_slot_)
#define setreaders_slot_ _n(setreaders_slot_)
#define setwriters_slot_ _n(setwriters_slot_)
#define setdocumentation_slot_ _n(setdocumentation_slot_)
#define setclass_slot_ _n(setclass_slot_)
#define setreadonly_slot_ _n(setreadonly_slot_)
#define setallocation_slot_ _n(setallocation_slot_)
#define setlocation_slot_ _n(setlocation_slot_)
#define setaccess_slot_ _n(setaccess_slot_)

#define getname_slot_unsafe _n(getname_slot_unsafe)

int getname_slot_(Execute ptr, addr pos, addr *ret);
int gettype_slot_(Execute ptr, addr pos, addr *ret);
int getargs_slot_(Execute ptr, addr pos, addr *ret);
int getform_slot_(Execute ptr, addr pos, addr *ret);
int getfunction_slot_(Execute ptr, addr pos, addr *ret);
int getreaders_slot_(Execute ptr, addr pos, addr *ret);
int getwriters_slot_(Execute ptr, addr pos, addr *ret);
int getdocumentation_slot_(Execute ptr, addr pos, addr *ret);
int getclass_slot_(Execute ptr, addr pos, addr *ret);
int getreadonly_slot_(Execute ptr, addr pos, addr *ret);
int getallocation_slot_(Execute ptr, addr pos, int *ret);
int getlocation_slot_(Execute ptr, addr pos, size_t *ret);
int getaccess_slot_(Execute ptr, addr pos, size_t *ret);

int setname_slot_(Execute ptr, addr pos, addr value);
int settype_slot_(Execute ptr, addr pos, addr value);
int setargs_slot_(Execute ptr, addr pos, addr value);
int setform_slot_(Execute ptr, addr pos, addr value);
int setfunction_slot_(Execute ptr, addr pos, addr value);
int setreaders_slot_(Execute ptr, addr pos, addr value);
int setwriters_slot_(Execute ptr, addr pos, addr value);
int setdocumentation_slot_(Execute ptr, addr pos, addr value);
int setclass_slot_(Execute ptr, addr pos, addr value);
int setreadonly_slot_(Execute ptr, addr pos, addr value);
int setallocation_slot_(Execute ptr, addr pos, int value);
int setlocation_slot_(Execute ptr, addr pos, size_t value);
int setaccess_slot_(Execute ptr, addr pos, size_t value);

void getname_slot_unsafe(addr pos, addr *ret);

#endif

