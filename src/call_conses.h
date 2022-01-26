#ifndef __CALL_CONSES_HEADER__
#define __CALL_CONSES_HEADER__

#include <stddef.h>
#include "execute.h"
#include "typedef.h"

#define sublis_common_ _n(sublis_common_)
#define nsublis_common_ _n(nsublis_common_)
#define subst_common_ _n(subst_common_)
#define nsubst_common_ _n(nsubst_common_)
#define subst_if_common_ _n(subst_if_common_)
#define nsubst_if_common_ _n(nsubst_if_common_)
#define subst_if_not_common_ _n(subst_if_not_common_)
#define nsubst_if_not_common_ _n(nsubst_if_not_common_)
#define tree_equal_common_ _n(tree_equal_common_)
#define list_length_common_ _n(list_length_common_)
#define make_list_common_ _n(make_list_common_)
#define push_common_ _n(push_common_)
#define pop_common_ _n(pop_common_)
#define nth_common_ _n(nth_common_)
#define setf_nth_common_ _n(setf_nth_common_)
#define nthcdr_common_ _n(nthcdr_common_)
#define member_common_ _n(member_common_)
#define member_if_common_ _n(member_if_common_)
#define member_if_not_common_ _n(member_if_not_common_)
#define mapc_common_ _n(mapc_common_)
#define mapcar_common_ _n(mapcar_common_)
#define mapcan_common_ _n(mapcan_common_)
#define mapl_common_ _n(mapl_common_)
#define maplist_common_ _n(maplist_common_)
#define mapcon_common_ _n(mapcon_common_)
#define nconc_common_ _n(nconc_common_)
#define append_common_ _n(append_common_)
#define revappend_common_ _n(revappend_common_)
#define nreconc_common_ _n(nreconc_common_)
#define butlast_common_ _n(butlast_common_)
#define nbutlast_common_ _n(nbutlast_common_)
#define last_common_ _n(last_common_)
#define ldiff_common _n(ldiff_common)
#define tailp_common _n(tailp_common)
#define assoc_common_ _n(assoc_common_)
#define assoc_if_common_ _n(assoc_if_common_)
#define assoc_if_not_common_ _n(assoc_if_not_common_)
#define copy_alist_common_ _n(copy_alist_common_)
#define pairlis_common_ _n(pairlis_common_)
#define rassoc_common_ _n(rassoc_common_)
#define rassoc_if_common_ _n(rassoc_if_common_)
#define rassoc_if_not_common_ _n(rassoc_if_not_common_)
#define get_properties_common_ _n(get_properties_common_)
#define getf_common_ _n(getf_common_)
#define remf_common_ _n(remf_common_)
#define intersection_common_ _n(intersection_common_)
#define nintersection_common_ _n(nintersection_common_)
#define adjoin_common_ _n(adjoin_common_)
#define pushnew_common_ _n(pushnew_common_)
#define set_difference_common_ _n(set_difference_common_)
#define nset_difference_common_ _n(nset_difference_common_)
#define set_exclusive_or_common_ _n(set_exclusive_or_common_)
#define nset_exclusive_or_common_ _n(nset_exclusive_or_common_)
#define subsetp_common_ _n(subsetp_common_)
#define union_common_ _n(union_common_)
#define nunion_common_ _n(nunion_common_)

int sublis_common_(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
int nsublis_common_(Execute ptr, addr alist, addr tree, addr rest, addr *ret);
int subst_common_(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
int nsubst_common_(Execute ptr, addr one, addr old, addr tree, addr key, addr *ret);
int subst_if_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int nsubst_if_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int subst_if_not_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int nsubst_if_not_common_(Execute ptr,
		addr one, addr predicate, addr tree, addr key, addr *ret);
int tree_equal_common_(Execute ptr, addr tree1, addr tree2, addr key, int *ret);
int list_length_common_(addr list, addr *ret);
int make_list_common_(addr var, addr rest, addr *ret);
int push_common_(Execute ptr, addr form, addr env, addr *ret);
int pop_common_(Execute ptr, addr form, addr env, addr *ret);
int nth_common_(addr index, addr list, addr *ret);
int setf_nth_common_(addr value, addr index, addr list);
int nthcdr_common_(addr index, addr list, addr *ret);
int member_common_(Execute ptr, addr item, addr list, addr rest, addr *ret);
int member_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int member_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int mapc_common_(Execute ptr, addr call, addr rest, addr *ret);
int mapcar_common_(Execute ptr, addr call, addr rest, addr *ret);
int mapcan_common_(Execute ptr, addr call, addr rest, addr *ret);
int mapl_common_(Execute ptr, addr call, addr rest, addr *ret);
int maplist_common_(Execute ptr, addr call, addr rest, addr *ret);
int mapcon_common_(Execute ptr, addr call, addr rest, addr *ret);
int nconc_common_(addr args, addr *ret);
int append_common_(addr args, addr *ret);
int revappend_common_(addr list, addr tail, addr *ret);
int nreconc_common_(addr list, addr tail, addr *ret);
int butlast_common_(addr list, addr index, addr *ret);
int nbutlast_common_(addr list, addr index, addr *ret);
int last_common_(addr list, addr index, addr *ret);
void ldiff_common(addr list, addr object, addr *ret);
void tailp_common(addr object, addr list, int *ret);
int assoc_common_(Execute ptr, addr item, addr list, addr rest, addr *ret);
int assoc_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int assoc_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int copy_alist_common_(addr list, addr *ret);
int pairlis_common_(addr keys, addr data, addr list, addr *ret);
int rassoc_common_(Execute ptr, addr item, addr list, addr rest, addr *ret);
int rassoc_if_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int rassoc_if_not_common_(Execute ptr, addr call, addr list, addr rest, addr *ret);
int get_properties_common_(addr plist, addr indicator,
		addr *rkey, addr *rvalue, addr *rlist);
int getf_common_(addr list, addr key, addr value, addr *ret);
int remf_common_(Execute ptr, addr form, addr env, addr *ret);
int intersection_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int nintersection_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int adjoin_common_(Execute ptr, addr item, addr list, addr rest, addr *ret);
int pushnew_common_(Execute ptr, addr form, addr env, addr *ret);
int set_difference_common_(Execute ptr, addr a, addr b, addr rest, addr *ret);
int nset_difference_common_(Execute ptr, addr a, addr b, addr rest, addr *ret);
int set_exclusive_or_common_(Execute ptr, addr a, addr b, addr rest, addr *ret);
int nset_exclusive_or_common_(Execute ptr, addr a, addr b, addr rest, addr *ret);
int subsetp_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int union_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret);
int nunion_common_(Execute ptr, addr list1, addr list2, addr rest, addr *ret);

#endif

