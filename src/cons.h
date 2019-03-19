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
void getcons(addr cons, addr *left, addr *right);
void getcar(addr cons, addr *left);
void getcdr(addr cons, addr *right);
void setcons(addr cons, addr left, addr right);
void setcar(addr cons, addr left);
void setcdr(addr cons, addr right);
void getnth(addr cons, size_t index, addr *ret);
void getnth_large(addr cons, addr index, addr *ret);
void getnth_unsafe(addr cons, size_t index, addr *ret);
void getnth_unbound_unsafe(addr cons, size_t index, addr *ret);
void getnthcdr(addr cons, size_t index, addr *ret);
void getnthcdr_large(addr cons, addr index, addr *ret);
void getnthcdr_unsafe(addr cons, size_t index, addr *ret);
void setnth(addr cons, size_t index, addr value);
size_t length_list_safe(addr cons);
size_t length_list_safe_dotted(addr cons);
size_t length_list_unsafe(addr cons);
int equal_length_list_unsafe(addr left, addr right);
void nconc_safe(addr args, addr *ret);
void nconc2_safe(addr left, addr right, addr *ret);
void append_safe(addr args, addr *ret);
void append2_safe(addr left, addr right, addr *ret);
void butlast_safe(addr *ret, addr list, size_t index);
void butlast_large(addr *ret, addr list, addr index);
void nbutlast_safe(addr *ret, addr list, size_t index);
void nbutlast_large(addr *ret, addr list, addr index);
void last_safe(addr *ret, addr list, size_t index);
void last_large(addr *ret, addr list, addr index);
void lastcar_unsafe(addr *ret, addr list, size_t index);
void butandlast_safe(addr *but, addr *last, addr list, size_t index);

int find_list_eq_unsafe(addr key, addr cons);
int find_list_eq_safe(addr key, addr cons);
int find_list_eql_unsafe(addr key, addr cons);
int find_list_equal_unsafe(addr key, addr cons);
int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret);
int pushnew_local(LocalRoot local, addr list, addr value, addr *ret);
int pushnew_heap(addr list, addr value, addr *ret);

void lista_alloc_safe(LocalRoot local, addr *ret, addr first, addr cons);
void lista_local_safe(LocalRoot local, addr *ret, addr first, addr cons);
void lista_heap_safe(addr *ret, addr first, addr cons);

void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
addr lista_allocr(LocalRoot local, ...);
addr lista_localr(LocalRoot local, ...);
addr lista_heapr(addr pos, ...);
void lista_alloc(LocalRoot local, addr *ret, ...);
void lista_local(LocalRoot local, addr *ret, ...);
void lista_heap(addr *ret, ...);

void List_bind(addr list, ...);
void list_bind(addr list, ...);
void Lista_bind(addr list, ...);
void lista_bind(addr list, ...);

void nreverse_list_unsafe(addr *ret, addr pos);
addr nreverse_list_unsafe_inplace(addr pos);
void nreverse_list_unsafe_dotted(addr *ret, addr cons, addr dot);
#define nreconc_unsafe nreverse_list_unsafe_dotted
void revappend_safe(addr *ret, addr list, addr tail);
void nreconc_safe(addr *ret, addr list, addr tail);

void reverse_list_heap_unsafe(addr *ret, addr cons);
void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);

void nreverse_list_safe(addr *ret, addr pos);
addr nreverse_list_safe_inplace(addr pos);
void reverse_list_heap_safe(addr *ret, addr cons);
void reverse_list_local_safe(LocalRoot local, addr *ret, addr cons);
void reverse_list_alloc_safe(LocalRoot local, addr *ret, addr cons);

void copy_tree_alloc(LocalRoot local, addr *ret, addr list);
void copy_tree_local(LocalRoot local, addr *ret, addr list);
void copy_tree_heap(addr *ret, addr list);


/*
 *  plist
 */
/* 0:find-value, 1:not-found(Nil) */
int getplist(addr plist, addr key, addr *ret);
int getplist_safe(addr plist, addr key, addr *ret);
int setplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_heap(addr plist, addr key, addr value, addr *ret);
int setplist_alloc_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_local_safe(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_heap_safe(addr plist, addr key, addr value, addr *ret);
int pushplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushplist_heap(addr plist, addr key, addr value, addr *ret);
int pushnewplist_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushnewplist_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int pushnewplist_heap(addr plist, addr key, addr value, addr *ret);
enum RemPlist remplist_check(addr plist, addr key, addr *ret);
enum RemPlist remplist_check_safe(addr plist, addr key, addr *ret);
int remplist(addr plist, addr key, addr *ret);
int remplist_heap(addr plist, addr key, addr *ret);

/* 0:find-value, 1:not-found(Nil) */
int getplist_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);
int getplist_constant_safe(addr plist, enum CONSTANT_INDEX index, addr *ret);
#define getkeyargs(a,b,c) getplist_constant_safe((a),CONSTANT_##b,(c))
/* 0:find-and-set, 1:make-new-cons */
int setplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int setplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int setplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushnewplist_constant_alloc(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushnewplist_constant_local(LocalRoot local, addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
int pushnewplist_constant_heap(addr plist,
		enum CONSTANT_INDEX index, addr value, addr *ret);
enum RemPlist remplist_check_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);
int remplist_constant(addr plist, enum CONSTANT_INDEX index, addr *ret);

int getplistplist(addr plist, addr key1, addr key2, addr *ret);
int setplistplist(addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_heap(addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_heap_force(addr plist, addr key1, addr key2, addr value, addr *ret);

/* eql */
int getplist_eql(addr plist, addr name, addr *ret);
int setplist_eql_alloc(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_eql_local(LocalRoot local, addr plist, addr key, addr value, addr *ret);
int setplist_eql_heap(addr plist, addr key, addr value, addr *ret);
int getplistplist_eql(addr plist, addr key1, addr key2, addr *ret);
int setplistplist_eql(addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_eql_alloc(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_eql_local(LocalRoot local,
		addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_eql_heap(addr plist, addr key1, addr key2, addr value, addr *ret);
int setplistplist_eql_heap_force(addr plist, addr key1, addr key2, addr value, addr *ret);

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

int getplistplist_callname(addr plist, addr key, addr callname, addr *ret);
int setplistplist_callname(addr plist, addr key, addr callname, addr value);
int setplistplist_callname_alloc(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
int setplistplist_callname_local(LocalRoot local,
		addr plist, addr key, addr callname, addr value, addr *ret);
int setplistplist_callname_heap(addr plist,
		addr key, addr callname, addr value, addr *ret);
int setplistplist_callname_heap_force(addr plist,
		addr key, addr callname, addr value, addr *ret);

int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret);
int pushnewlist_callname_heap(addr list, addr callname, addr *ret);
int find_list_callname_unsafe(addr callname, addr list);


/*
 *  assoc
 */
int getassoc(addr key, addr list, addr *ret);
int getrassoc(addr key, addr list, addr *ret);

#endif

