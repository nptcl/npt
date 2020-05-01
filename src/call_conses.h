#ifndef __CALL_CONSES_HEADER__
#define __CALL_CONSES_HEADER__

#include <stddef.h>
#include "execute.h"
#include "typedef.h"

_g int sublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
_g int nsublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
_g int subst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
_g int nsubst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
_g int subst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
_g int nsubst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
_g int subst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
_g int nsubst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
_g int tree_equal_common(Execute ptr, addr tree1, addr tree2, addr key, int *ret);
_g void list_length_common(addr list, addr *ret);
_g void make_list_common(addr var, addr rest, addr *ret);
_g int push_common(Execute ptr, addr form, addr env, addr *ret);
_g int pop_common(Execute ptr, addr form, addr env, addr *ret);
_g void nth_common(addr index, addr list, addr *ret);
_g void setf_nth_common(addr value, addr index, addr list);
_g void nthcdr_common(addr index, addr list, addr *ret);
_g int member_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
_g int member_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g int member_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g int mapc_common(Execute ptr, addr call, addr rest, addr *ret);
_g int mapcar_common(Execute ptr, addr call, addr rest, addr *ret);
_g int mapcan_common(Execute ptr, addr call, addr rest, addr *ret);
_g int mapl_common(Execute ptr, addr call, addr rest, addr *ret);
_g int maplist_common(Execute ptr, addr call, addr rest, addr *ret);
_g int mapcon_common(Execute ptr, addr call, addr rest, addr *ret);
_g void nconc_common(addr args, addr *ret);
_g void append_common(addr args, addr *ret);
_g void revappend_common(addr list, addr tail, addr *ret);
_g void nreconc_common(addr list, addr tail, addr *ret);
_g void butlast_common(addr list, addr index, addr *ret);
_g void nbutlast_common(addr list, addr index, addr *ret);
_g void last_common(addr list, addr index, addr *ret);
_g void ldiff_common(addr list, addr object, addr *ret);
_g void tailp_common(addr object, addr list, int *ret);
_g int assoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
_g int assoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g int assoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g void copy_alist_common(addr list, addr *ret);
_g void pairlis_common(addr keys, addr data, addr list, addr *ret);
_g int rassoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
_g int rassoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g int rassoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
_g void get_properties_common(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist);
_g int remf_common(Execute ptr, addr form, addr env, addr *ret);
_g int intersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
_g int nintersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
_g int adjoin_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
_g int pushnew_common(Execute ptr, addr form, addr env, addr *ret);
_g int set_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
_g int nset_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
_g int set_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
_g int nset_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
_g int subsetp_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
_g int union_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
_g int nunion_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);

#endif

