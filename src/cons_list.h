#ifndef __CONS_LIST_HEADER__
#define __CONS_LIST_HEADER__

#include "local.h"
#include "typedef.h"

/* nth */
_g void getnth_abort(addr cons, size_t index, addr *ret);
_g void getnth(addr cons, size_t index, addr *ret);
_g int getnth_(addr cons, size_t index, addr *ret);
_g int getnth_large(addr cons, addr index, addr *ret);
_g void getnth_unsafe(addr cons, size_t index, addr *ret);
_g void getnth_unbound_unsafe(addr cons, size_t index, addr *ret);
_g int getnthcdr_(addr cons, size_t index, addr *ret);
_g int getnthcdr_large(addr cons, addr index, addr *ret);
_g void getnthcdr_unsafe(addr cons, size_t index, addr *ret);
_g void setnth(addr cons, size_t index, addr value);
_g int setnth_(addr cons, size_t index, addr value);
_g void setnth_unsafe(addr cons, size_t index, addr value);

/* length */
_g size_t length_list_safe(addr list);
_g size_t length_list_unsafe(addr list);
_g int length_list_p(addr list, size_t *ret);

/* list */
_g void nconc2_safe(addr left, addr right, addr *ret);
_g void nconc2_unsafe(addr left, addr right, addr *ret);
_g void append2_safe(addr left, addr right, addr *ret);
_g void append2_heap_unsafe(addr list1, addr list2, addr *ret);
_g void append2_local_unsafe(LocalRoot local, addr list1, addr list2, addr *ret);
_g void append2_alloc_unsafe(LocalRoot local, addr list1, addr list2, addr *ret);
_g void butandlast_safe(addr *but, addr *last, addr list, size_t index);
_g void setlastcdr_safe(addr list, addr cdr);

/* find */
_g int find_list_eq_unsafe(addr key, addr cons);
_g int find_list_eq_safe(addr key, addr cons);
_g int find_list_eql_unsafe(addr key, addr cons);
_g int find_list_eql_safe(addr key, addr cons);
_g int find_list_equal_unsafe(addr key, addr cons);
_g int find_list_equal_safe(addr key, addr cons);
_g int position_list_eq_unsafe(addr key, addr cons, size_t *ret);

/* pushnew */
_g int pushnew_alloc(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_local(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_heap(addr list, addr value, addr *ret);
_g int pushnew_equal_alloc(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_equal_local(LocalRoot local, addr list, addr value, addr *ret);
_g int pushnew_equal_heap(addr list, addr value, addr *ret);

/* nreverse */
_g void nreconc_unsafe(addr *ret, addr cons, addr tail);
_g void nreverse_list_unsafe(addr *ret, addr pos);
_g void nreverse_list_safe(addr *ret, addr pos);
#define nreconc nreconc_unsafe
#define nreverse nreverse_list_unsafe

/* reverse */
_g void reverse_list_heap_unsafe(addr *ret, addr cons);
_g void reverse_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
_g void reverse_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
_g void reverse_list_heap_safe(addr *ret, addr cons);
_g void reverse_list_local_safe(LocalRoot local, addr *ret, addr cons);
_g void reverse_list_alloc_safe(LocalRoot local, addr *ret, addr cons);

/* callname */
_g int pushnewlist_callname_alloc(LocalRoot local, addr list, addr callname, addr *ret);
_g int pushnewlist_callname_heap(addr list, addr callname, addr *ret);
_g int find_list_callname_unsafe(addr callname, addr list);

/* copy-list */
_g void copy_list_heap_unsafe(addr *ret, addr cons);
_g void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_heap_safe(addr *ret, addr cons);
_g void copy_list_local_safe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons);

/* delete / remove */
_g int delete_list_eq_unsafe(addr key, addr cons, addr *ret);
_g int delete_list_equal_unsafe(addr key, addr cons, addr *ret);
_g int delete1_list_eq_unsafe(addr key, addr cons, addr *ret);
_g void remove_list_eq_unsafe_heap(addr key, addr cons, addr *ret);
_g void remove_list_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret);
_g void remove_list_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret);

#endif

