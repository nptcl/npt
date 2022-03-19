#ifndef __CONS_LIST_HEADER__
#define __CONS_LIST_HEADER__

#include "local.h"
#include "typedef.h"

#define getnth_abort _n(getnth_abort)
#define getnth_ _n(getnth_)
#define getnth_large_ _n(getnth_large_)
#define getnth_unsafe _n(getnth_unsafe)
#define getnth_unbound_unsafe _n(getnth_unbound_unsafe)
#define getnthcdr_ _n(getnthcdr_)
#define getnthcdr_large_ _n(getnthcdr_large_)
#define getnthcdr_unsafe _n(getnthcdr_unsafe)
#define setnth_ _n(setnth_)
#define setnth_unsafe _n(setnth_unsafe)
#define length_list_unsafe _n(length_list_unsafe)
#define length_list_safe_ _n(length_list_safe_)
#define length_list_p _n(length_list_p)
#define nconc2_safe_ _n(nconc2_safe_)
#define nconc2_unsafe _n(nconc2_unsafe)
#define append2_safe_ _n(append2_safe_)
#define append2_heap_unsafe _n(append2_heap_unsafe)
#define append2_local_unsafe _n(append2_local_unsafe)
#define append2_alloc_unsafe _n(append2_alloc_unsafe)
#define butandlast_safe _n(butandlast_safe)
#define setlastcdr_safe_ _n(setlastcdr_safe_)
#define find_list_eq_unsafe _n(find_list_eq_unsafe)
#define find_list_eq_safe_ _n(find_list_eq_safe_)
#define find_list_eql_unsafe _n(find_list_eql_unsafe)
#define find_list_equal_safe_ _n(find_list_equal_safe_)
#define find_list_equal_safe _n(find_list_equal_safe)
#define position_list_eq_unsafe _n(position_list_eq_unsafe)
#define find_assoc_eq_unsafe _n(find_assoc_eq_unsafe)
#define pushnew_alloc _n(pushnew_alloc)
#define pushnew_local _n(pushnew_local)
#define pushnew_heap _n(pushnew_heap)
#define pushnew_equal_heap_ _n(pushnew_equal_heap_)
#define nreconc_unsafe _n(nreconc_unsafe)
#define nreconc_safe_ _n(nreconc_safe_)
#define nreverse_list_unsafe _n(nreverse_list_unsafe)
#define nreverse_list_safe_ _n(nreverse_list_safe_)
#define reverse_list_heap_unsafe _n(reverse_list_heap_unsafe)
#define reverse_list_local_unsafe _n(reverse_list_local_unsafe)
#define reverse_list_alloc_unsafe _n(reverse_list_alloc_unsafe)
#define reverse_list_heap_safe_ _n(reverse_list_heap_safe_)
#define pushnewlist_callname_alloc _n(pushnewlist_callname_alloc)
#define pushnewlist_callname_heap _n(pushnewlist_callname_heap)
#define find_list_callname_unsafe _n(find_list_callname_unsafe)
#define copy_list_heap_unsafe _n(copy_list_heap_unsafe)
#define copy_list_local_unsafe _n(copy_list_local_unsafe)
#define copy_list_alloc_unsafe _n(copy_list_alloc_unsafe)
#define copy_list_heap_safe _n(copy_list_heap_safe)
#define copy_list_local_safe _n(copy_list_local_safe)
#define copy_list_alloc_safe _n(copy_list_alloc_safe)
#define delete_list_eq_unsafe _n(delete_list_eq_unsafe)
#define delete_list_eq_safe _n(delete_list_eq_safe)
#define delete_list_equal_unsafe_ _n(delete_list_equal_unsafe_)
#define delete1_list_eq_unsafe _n(delete1_list_eq_unsafe)
#define remove_list_eq_unsafe_heap _n(remove_list_eq_unsafe_heap)
#define remove_list_eq_unsafe_local _n(remove_list_eq_unsafe_local)
#define remove_list_eq_unsafe_alloc _n(remove_list_eq_unsafe_alloc)
#define remove_list_equal_safe_heap_ _n(remove_list_equal_safe_heap_)

/* nth */
void getnth_abort(addr cons, size_t index, addr *ret);
int getnth_(addr cons, size_t index, addr *ret);
int getnth_large_(addr cons, addr index, addr *ret);
void getnth_unsafe(addr cons, size_t index, addr *ret);
void getnth_unbound_unsafe(addr cons, size_t index, addr *ret);
int getnthcdr_(addr cons, size_t index, addr *ret);
int getnthcdr_large_(addr cons, addr index, addr *ret);
void getnthcdr_unsafe(addr cons, size_t index, addr *ret);
int setnth_(addr cons, size_t index, addr value);
void setnth_unsafe(addr cons, size_t index, addr value);

/* length */
size_t length_list_unsafe(addr list);
int length_list_safe_(addr list, size_t *ret);
int length_list_p(addr list, size_t *ret);

/* list */
int nconc2_safe_(addr left, addr right, addr *ret);
void nconc2_unsafe(addr left, addr right, addr *ret);
int append2_safe_(addr left, addr right, addr *ret);
void append2_heap_unsafe(addr list1, addr list2, addr *ret);
void append2_local_unsafe(LocalRoot local, addr list1, addr list2, addr *ret);
void append2_alloc_unsafe(LocalRoot local, addr list1, addr list2, addr *ret);
void butandlast_safe(addr *but, addr *last, addr list, size_t index);
int setlastcdr_safe_(addr list, addr cdr);

/* find */
int find_list_eq_unsafe(addr key, addr cons);
int find_list_eq_safe_(addr key, addr cons, int *ret);
int find_list_eql_unsafe(addr key, addr cons);
int find_list_equal_safe_(addr key, addr cons, int *ret);
int position_list_eq_unsafe(addr key, addr cons, size_t *ret);
int find_assoc_eq_unsafe(addr key, addr list, addr *ret);

/* pushnew */
int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret);
int pushnew_local(LocalRoot local, addr list, addr value, addr *ret);
int pushnew_heap(addr list, addr value, addr *ret);
int pushnew_equal_heap_(addr list, addr value, addr *ret);

/* nreverse */
void nreconc_unsafe(addr *ret, addr cons, addr tail);
int nreconc_safe_(addr *ret, addr cons, addr tail);
void nreverse_list_unsafe(addr *ret, addr pos);
int nreverse_list_safe_(addr *ret, addr pos);
#define nreconc nreconc_unsafe
#define nreverse nreverse_list_unsafe

/* reverse */
void reverse_list_heap_unsafe(addr *ret, addr cons);
void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
int reverse_list_heap_safe_(addr *ret, addr cons);

/* callname */
int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret);
int pushnewlist_callname_heap(addr list, addr callname, addr *ret);
int find_list_callname_unsafe(addr callname, addr list);

/* copy-list */
void copy_list_heap_unsafe(addr *ret, addr cons);
void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
void copy_list_heap_safe(addr *ret, addr cons);
void copy_list_local_safe(LocalRoot local, addr *ret, addr cons);
void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons);

/* delete / remove */
int delete_list_eq_unsafe(addr key, addr cons, addr *ret);
int delete_list_eq_safe(addr key, addr cons, addr *ret);
int delete_list_equal_unsafe_(addr key, addr cons, addr *root, int *ret);
int delete1_list_eq_unsafe(addr key, addr cons, addr *ret);
void remove_list_eq_unsafe_heap(addr key, addr cons, addr *ret);
void remove_list_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret);
void remove_list_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret);
int remove_list_equal_safe_heap_(addr key, addr cons, addr *ret);

#endif

