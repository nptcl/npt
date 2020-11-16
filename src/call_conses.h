#ifndef __CALL_CONSES_HEADER__
#define __CALL_CONSES_HEADER__

#include <stddef.h>
#include "execute.h"
#include "typedef.h"

#define sublis_common _n(sublis_common)
#define nsublis_common _n(nsublis_common)
#define subst_common _n(subst_common)
#define nsubst_common _n(nsubst_common)
#define subst_if_common _n(subst_if_common)
#define nsubst_if_common _n(nsubst_if_common)
#define subst_if_not_common _n(subst_if_not_common)
#define nsubst_if_not_common _n(nsubst_if_not_common)
#define tree_equal_common _n(tree_equal_common)
#define list_length_common _n(list_length_common)
#define make_list_common _n(make_list_common)
#define push_common _n(push_common)
#define pop_common _n(pop_common)
#define nth_common _n(nth_common)
#define setf_nth_common _n(setf_nth_common)
#define nthcdr_common _n(nthcdr_common)
#define member_common _n(member_common)
#define member_if_common _n(member_if_common)
#define member_if_not_common _n(member_if_not_common)
#define mapc_common _n(mapc_common)
#define mapcar_common _n(mapcar_common)
#define mapcan_common _n(mapcan_common)
#define mapl_common _n(mapl_common)
#define maplist_common _n(maplist_common)
#define mapcon_common _n(mapcon_common)
#define nconc_common _n(nconc_common)
#define append_common _n(append_common)
#define revappend_common _n(revappend_common)
#define nreconc_common _n(nreconc_common)
#define butlast_common _n(butlast_common)
#define nbutlast_common _n(nbutlast_common)
#define last_common _n(last_common)
#define ldiff_common _n(ldiff_common)
#define tailp_common _n(tailp_common)
#define assoc_common _n(assoc_common)
#define assoc_if_common _n(assoc_if_common)
#define assoc_if_not_common _n(assoc_if_not_common)
#define copy_alist_common _n(copy_alist_common)
#define pairlis_common _n(pairlis_common)
#define rassoc_common _n(rassoc_common)
#define rassoc_if_common _n(rassoc_if_common)
#define rassoc_if_not_common _n(rassoc_if_not_common)
#define get_properties_common _n(get_properties_common)
#define getf_common _n(getf_common)
#define remf_common _n(remf_common)
#define intersection_common _n(intersection_common)
#define nintersection_common _n(nintersection_common)
#define adjoin_common _n(adjoin_common)
#define pushnew_common _n(pushnew_common)
#define set_difference_common _n(set_difference_common)
#define nset_difference_common _n(nset_difference_common)
#define set_exclusive_or_common _n(set_exclusive_or_common)
#define nset_exclusive_or_common _n(nset_exclusive_or_common)
#define subsetp_common _n(subsetp_common)
#define union_common _n(union_common)
#define nunion_common _n(nunion_common)

int sublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
int nsublis_common(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
int subst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
int nsubst_common(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
int subst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int nsubst_if_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int subst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int nsubst_if_not_common(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int tree_equal_common(Execute ptr, addr tree1, addr tree2, addr key, int *ret);
int list_length_common(addr list, addr *ret);
int make_list_common(addr var, addr rest, addr *ret);
int push_common(Execute ptr, addr form, addr env, addr *ret);
int pop_common(Execute ptr, addr form, addr env, addr *ret);
int nth_common(addr index, addr list, addr *ret);
int setf_nth_common(addr value, addr index, addr list);
int nthcdr_common(addr index, addr list, addr *ret);
int member_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
int member_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int member_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int mapc_common(Execute ptr, addr call, addr rest, addr *ret);
int mapcar_common(Execute ptr, addr call, addr rest, addr *ret);
int mapcan_common(Execute ptr, addr call, addr rest, addr *ret);
int mapl_common(Execute ptr, addr call, addr rest, addr *ret);
int maplist_common(Execute ptr, addr call, addr rest, addr *ret);
int mapcon_common(Execute ptr, addr call, addr rest, addr *ret);
int nconc_common(addr args, addr *ret);
int append_common(addr args, addr *ret);
int revappend_common(addr list, addr tail, addr *ret);
int nreconc_common(addr list, addr tail, addr *ret);
int butlast_common(addr list, addr index, addr *ret);
int nbutlast_common(addr list, addr index, addr *ret);
int last_common(addr list, addr index, addr *ret);
void ldiff_common(addr list, addr object, addr *ret);
void tailp_common(addr object, addr list, int *ret);
int assoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
int assoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int assoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int copy_alist_common(addr list, addr *ret);
int pairlis_common(addr keys, addr data, addr list, addr *ret);
int rassoc_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
int rassoc_if_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int rassoc_if_not_common(Execute ptr, addr call, addr list, addr rest, addr *ret);
int get_properties_common(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist);
int getf_common(addr list, addr key, addr value, addr *ret);
int remf_common(Execute ptr, addr form, addr env, addr *ret);
int intersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int nintersection_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int adjoin_common(Execute ptr, addr item, addr list, addr rest, addr *ret);
int pushnew_common(Execute ptr, addr form, addr env, addr *ret);
int set_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
int nset_difference_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
int set_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
int nset_exclusive_or_common(Execute ptr, addr a, addr b, addr rest, addr *ret);
int subsetp_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int union_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int nunion_common(Execute ptr, addr list1, addr list2, addr rest, addr *ret);

#endif

