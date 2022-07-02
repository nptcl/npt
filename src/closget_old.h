#ifndef __CLOSGET_OLD_HEADER__
#define __CLOSGET_OLD_HEADER__

#include "constant.h"
#include "execute.h"
#include "typedef.h"

#define clos_errorp _n(clos_errorp)
#define clos_getp _n(clos_getp)
#define clos_setp _n(clos_setp)
#define clos_checkp_ _n(clos_checkp_)
#define clos_get_ _n(clos_get_)
#define clos_set_ _n(clos_set_)
#define clos_check_ _n(clos_check_)
#define clos_getelt _n(clos_getelt)
#define clos_setelt _n(clos_setelt)
#define clos_checkelt_ _n(clos_checkelt_)
#define clos_getconst_ _n(clos_getconst_)
#define clos_setconst_ _n(clos_setconst_)
#define clos_checkconst_ _n(clos_checkconst_)

#define clos_slot_exists_p _n(clos_slot_exists_p)
#define clos_slot_boundp_nil _n(clos_slot_boundp_nil)
#define clos_slot_boundp_ _n(clos_slot_boundp_)
#define clos_slot_makunbound_nil_ _n(clos_slot_makunbound_nil_)
#define clos_slot_makunbound_ _n(clos_slot_makunbound_)

#define ClosGetConst_(p,n,r) clos_getconst_((p),CONSTANT_##n,(r))
#define ClosSetConst_(p,n,v) clos_setconst_((p),CONSTANT_##n,(v))
#define ClosCheckConst_(p,n,r) clos_checkconst_((p),CONSTANT_##n,(r))

int clos_errorp(addr pos, size_t index, constindex name);
int clos_getp(addr pos, addr key, addr *ret);
int clos_setp(addr pos, addr key, addr value);
int clos_checkp_(addr pos, addr key, addr *value, int *ret);
int clos_get_(addr pos, addr key, addr *ret);
int clos_set_(addr pos, addr key, addr value);
int clos_check_(addr pos, addr key, addr *ret);
void clos_getelt(addr pos, size_t index, addr *ret);
void clos_setelt(addr pos, size_t index, addr value);
int clos_checkelt_(addr pos, size_t index, addr *ret);
int clos_getconst_(addr pos, constindex index, addr *ret);
int clos_setconst_(addr pos, constindex index, addr value);
int clos_checkconst_(addr pos, constindex index, addr *ret);

int clos_slot_exists_p(addr pos, addr name);
int clos_slot_boundp_nil(addr pos, addr name);
int clos_slot_boundp_(addr pos, addr name, int *ret);
int clos_slot_makunbound_nil_(addr pos, addr name, int *ret);
int clos_slot_makunbound_(addr pos, addr name);

#endif

