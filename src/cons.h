#ifndef __CONS_HEADER__
#define __CONS_HEADER__

#include "constant.h"
#include "local.h"
#include "typedef.h"

enum RemPlist {
	RemPlist_Delete,
	RemPlist_NotFound,
	RemPlist_Update,
	RemPlist_Size
};


/*
 *  list
 */
_g void getcons(addr cons, addr *left, addr *right);
_g void getcar(addr cons, addr *left);
_g void getcdr(addr cons, addr *right);
_g void setcons(addr cons, addr left, addr right);
_g void setcar(addr cons, addr left);
_g void setcdr(addr cons, addr right);
_g void getnth(addr cons, size_t index, addr *ret);
_g void getnth_large(addr cons, addr index, addr *ret);
_g void getnth_unsafe(addr cons, size_t index, addr *ret);
_g void getnth_unbound_unsafe(addr cons, size_t index, addr *ret);
_g void getnthcdr(addr cons, size_t index, addr *ret);
_g void getnthcdr_large(addr cons, addr index, addr *ret);
_g void getnthcdr_unsafe(addr cons, size_t index, addr *ret);
_g void setnth(addr cons, size_t index, addr value);
_g size_t length_list_safe(addr cons);
_g size_t length_list_safe_dotted(addr cons);
_g size_t length_list_unsafe(addr cons);
_g int equal_length_list_unsafe(addr left, addr right);
_g void nconc_safe(addr args, addr *ret);
_g void nconc2_safe(addr left, addr right, addr *ret);
_g void append_safe(addr args, addr *ret);
_g void append2_safe(addr left, addr right, addr *ret);
_g void butlast_safe(addr *ret, addr list, size_t index);
_g void butlast_large(addr *ret, addr list, addr index);
_g void nbutlast_safe(addr *ret, addr list, size_t index);
_g void nbutlast_large(addr *ret, addr list, addr index);
_g void last_safe(addr *ret, addr list, size_t index);
_g void last_large(addr *ret, addr list, addr index);
_g void butandlast_safe(addr *but, addr *last, addr list, size_t index);

_g int find_list_eq_unsafe(addr key, addr cons);
_g int find_list_eq_safe(addr key, addr cons);
_g int find_list_eql_unsafe(addr key, addr cons);
_g int find_list_equal_unsafe(addr key, addr cons);
_g int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_local(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_heap(addr list, addr value, addr *ret);
_g int pushnew_equal_alloc(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_equal_local(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_equal_heap(addr list, addr value, addr *ret);

_g void lista_alloc_safe(LocalRoot local, addr *ret, addr first, addr cons);
_g void lista_local_safe(LocalRoot local, addr *ret, addr first, addr cons);
_g void lista_heap_safe(addr *ret, addr first, addr cons);

_g void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
_g addr lista_allocr(LocalRoot local, ...);
_g addr lista_localr(LocalRoot local, ...);
_g addr lista_heapr(addr pos, ...);
_g void lista_alloc(LocalRoot local, addr *ret, ...);
_g void lista_local(LocalRoot local, addr *ret, ...);
_g void lista_heap(addr *ret, ...);

_g void List_bind(addr list, ...);
_g void list_bind(addr list, ...);
_g void Lista_bind(addr list, ...);
_g void lista_bind(addr list, ...);

_g void nreverse_list_unsafe(addr *ret, addr pos);
_g addr nreverse_list_unsafe_inplace(addr pos);
_g void nreverse_list_unsafe_dotted(addr *ret, addr cons, addr dot);
#define nreconc_unsafe nreverse_list_unsafe_dotted
_g void revappend_safe(addr *ret, addr list, addr tail);
_g void nreconc_safe(addr *ret, addr list, addr tail);

_g void reverse_list_heap_unsafe(addr *ret, addr cons);
_g void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
_g void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);

_g void nreverse_list_safe(addr *ret, addr pos);
_g addr nreverse_list_safe_inplace(addr pos);
_g void reverse_list_heap_safe(addr *ret, addr cons);
_g void reverse_list_local_safe(LocalRoot local, addr *ret, addr cons);
_g void reverse_list_alloc_safe(LocalRoot local, addr *ret, addr cons);

_g void copy_tree_alloc(LocalRoot local, addr *ret, addr list);
_g void copy_tree_local(LocalRoot local, addr *ret, addr list);
_g void copy_tree_heap(addr *ret, addr list);

_g void pushva_heap(addr *list, ...);


/*
 *  plist
 */
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
_g int getplist_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);
#define GetplistConst(x,y,z) getplist_constant((x),CONSTANT_##y,(z))
#define GetplistSystem(x,y,z) getplist_constant((x),CONSTANT_SYSTEM_PLIST_##y,(z))
_g int getplist_constant_safe(addr plist, enum CONSTANT_INDEX index, addr *ret);
#define getkeyargs(a,b,c) getplist_constant_safe((a),CONSTANT_##b,(c))
/* 0:find-and-set, 1:make-new-cons */
_g int setplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int setplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int setplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushnewplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushnewplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g int pushnewplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
_g enum RemPlist remplist_check_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);
_g int remplist_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);

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

/* callname */
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

_g int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret);
_g int pushnewlist_callname_heap(addr list, addr callname, addr *ret);
_g int find_list_callname_unsafe(addr callname, addr list);


/*
 *  assoc
 */
_g int getassoc(addr key, addr list, addr *ret);
_g int getrassoc(addr key, addr list, addr *ret);

#endif

