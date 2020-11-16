#ifndef __CONS_PLIST_HEADER__
#define __CONS_PLIST_HEADER__

#include "constant.h"
#include "local.h"
#include "typedef.h"

#define getplist _n(getplist)
#define getplist_safe _n(getplist_safe)
#define setplist_alloc _n(setplist_alloc)
#define setplist_local _n(setplist_local)
#define setplist_heap _n(setplist_heap)
#define setplist_alloc_safe _n(setplist_alloc_safe)
#define setplist_local_safe _n(setplist_local_safe)
#define setplist_heap_safe _n(setplist_heap_safe)
#define pushnewplist_alloc _n(pushnewplist_alloc)
#define pushnewplist_local _n(pushnewplist_local)
#define pushnewplist_heap _n(pushnewplist_heap)
#define remplist_safe_ _n(remplist_safe_)
#define remplist_check _n(remplist_check)
#define remplist _n(remplist)
#define remplist_local_ _n(remplist_local_)
#define remplist_heap_ _n(remplist_heap_)
#define getplist_constant _n(getplist_constant)
#define getplist_constant_safe _n(getplist_constant_safe)
#define setplist_constant_alloc _n(setplist_constant_alloc)
#define setplist_constant_local _n(setplist_constant_local)
#define setplist_constant_heap _n(setplist_constant_heap)
#define remplist_constant _n(remplist_constant)
#define getpplist _n(getpplist)
#define setpplist_alloc _n(setpplist_alloc)
#define setpplist_local _n(setpplist_local)
#define setpplist_heap _n(setpplist_heap)
#define getplist_callname _n(getplist_callname)
#define setplist_callname_alloc _n(setplist_callname_alloc)
#define setplist_callname_local _n(setplist_callname_local)
#define setplist_callname_heap _n(setplist_callname_heap)
#define pushnewplist_callname_alloc _n(pushnewplist_callname_alloc)
#define pushnewplist_callname_local _n(pushnewplist_callname_local)
#define pushnewplist_callname_heap _n(pushnewplist_callname_heap)
#define getpplist_callname _n(getpplist_callname)
#define setpplist_callname_alloc _n(setpplist_callname_alloc)
#define setpplist_callname_local _n(setpplist_callname_local)
#define setpplist_callname_heap _n(setpplist_callname_heap)

enum RemPlist {
	RemPlist_Delete,
	RemPlist_NotFound,
	RemPlist_Update,
	RemPlist_Size
};

/* 0:find-value, 1:not-found(Nil) */
int getplist(addr plist, addr key, addr *ret);
int getplist_safe(addr plist, addr key, addr *ret);
int setplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_heap(addr plist, addr key, addr value, addr *ret);
int setplist_alloc_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_local_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_heap_safe(addr plist, addr key, addr value, addr *ret);

int pushnewplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushnewplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushnewplist_heap(addr plist, addr key, addr value, addr *ret);
int remplist_safe_(addr plist, addr key, addr *value, enum RemPlist *ret);
enum RemPlist remplist_check(addr plist, addr key, addr *ret);
int remplist(addr plist, addr key, addr *ret);
int remplist_local_(LocalRoot local, addr list, addr key, addr *value, int *ret);
int remplist_heap_(addr list, addr key, addr *value, int *ret);

/* 0:find-value, 1:not-found(Nil) */
int getplist_constant(addr plist, constindex index, addr *ret);
int getplist_constant_safe(addr plist, constindex index, addr *ret);
#define GetPlistConst(x,y,z) getplist_constant((x),CONSTANT_##y,(z))
#define GetKeyArgs(a,b,c) getplist_constant_safe((a),CONSTANT_##b,(c))

/* 0:find-and-set, 1:make-new-cons */
int setplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
int setplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
int setplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret);
int remplist_constant(addr plist, constindex index, addr *ret);

int getpplist(addr plist, addr key1, addr key2, addr *ret);
int setpplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setpplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setpplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret);

/* callname */
int getplist_callname(addr plist, addr callname, addr *ret);
int setplist_callname_alloc(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret);
int setplist_callname_local(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret);
int setplist_callname_heap(addr plist, addr callname, addr value, addr *ret);

int pushnewplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret);
int pushnewplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret);
int pushnewplist_callname_heap(addr plist,
		addr key, addr callname, addr *ret);

int getpplist_callname(addr plist, addr key, addr callname, addr *ret);
int setpplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
int setpplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
int setpplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret);

#endif

