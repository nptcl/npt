#ifndef __CONS_PLIST_HEADER__
#define __CONS_PLIST_HEADER__

#include "constant.h"
#include "local.h"
#include "typedef.h"

enum RemPlist {
	RemPlist_Delete,
	RemPlist_NotFound,
	RemPlist_Update,
	RemPlist_Size
};

/* 0:find-value, 1:not-found(Nil) */
_g int getplist(addr plist, addr key, addr *ret);
_g int getplist_safe(addr plist, addr key, addr *ret);
_g int setplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_heap(addr plist, addr key, addr value, addr *ret);
_g int setplist_alloc_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_local_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_heap_safe(addr plist, addr key, addr value, addr *ret);

_g int pushplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int pushplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int pushplist_heap(addr plist, addr key, addr value, addr *ret);
_g int pushnewplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int pushnewplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int pushnewplist_heap(addr plist, addr key, addr value, addr *ret);
_g enum RemPlist remplist_check(addr plist, addr key, addr *ret);
_g enum RemPlist remplist_check_safe(addr plist, addr key, addr *ret);
_g int remplist(addr plist, addr key, addr *ret);
_g int remplist_alloc(LocalRoot local, addr plist, addr key, addr *ret);
_g int remplist_local(LocalRoot local, addr plist, addr key, addr *ret);
_g int remplist_heap(addr plist, addr key, addr *ret);

/* 0:find-value, 1:not-found(Nil) */
_g int getplist_constant(addr plist, constindex index, addr *ret);
#define GetplistConst(x,y,z) getplist_constant((x),CONSTANT_##y,(z))
#define GetplistSystem(x,y,z) getplist_constant((x),CONSTANT_SYSTEM_PLIST_##y,(z))
_g int getplist_constant_safe(addr plist, constindex index, addr *ret);
#define getkeyargs(a,b,c) getplist_constant_safe((a),CONSTANT_##b,(c))
/* 0:find-and-set, 1:make-new-cons */
_g int setplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int setplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int setplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret);
_g int pushplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int pushplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int pushplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret);
_g int pushnewplist_constant_alloc(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int pushnewplist_constant_local(LocalRoot local, addr plist,
		constindex index, addr value, addr *ret);
_g int pushnewplist_constant_heap(addr plist,
		constindex index, addr value, addr *ret);
_g enum RemPlist remplist_check_constant(addr plist, constindex index, addr *ret);
_g int remplist_constant(addr plist, constindex index, addr *ret);

_g int getplistplist(addr plist, addr key1, addr key2, addr *ret);
_g int setplistplist(addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_heap_force(addr plist, addr key1, addr key2, addr value, addr *ret);

/* eql */
_g int getplist_eql(addr plist, addr name, addr *ret);
_g int setplist_eql_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_eql_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
_g int setplist_eql_heap(addr plist, addr key, addr value, addr *ret);
_g int getplistplist_eql(addr plist, addr key1, addr key2, addr *ret);
_g int setplistplist_eql_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_eql_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
_g int setplistplist_eql_heap(addr plist, addr key1, addr key2, addr value, addr *ret);


/*
 *  callname
 */
_g int getplist_callname(addr plist, addr callname, addr *ret);
_g int setplist_callname_alloc(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret);
_g int setplist_callname_local(LocalRoot local,
		addr plist, addr callname, addr value, addr *ret);
_g int setplist_callname_heap(addr plist, addr callname, addr value, addr *ret);

_g int pushnewplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret);
_g int pushnewplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr *ret);
_g int pushnewplist_callname_heap(addr plist,
		addr key, addr callname, addr *ret);

_g int getplistplist_callname(addr plist, addr key, addr callname, addr *ret);
_g int setplistplist_callname(addr plist, addr key, addr callname, addr value);
_g int setplistplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
_g int setplistplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
_g int setplistplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret);
_g int setplistplist_callname_heap_force(addr plist,
		addr key, addr callname, addr value, addr *ret);

#endif

