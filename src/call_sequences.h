#ifndef __CALL_SEQUENCES_HEADER__
#define __CALL_SEQUENCES_HEADER__

#include "execute.h"
#include "typedef.h"

#define copy_seq_common_ _n(copy_seq_common_)
#define elt_common_ _n(elt_common_)
#define setf_elt_common_ _n(setf_elt_common_)
#define fill_common_ _n(fill_common_)
#define subseq_common_ _n(subseq_common_)
#define setf_subseq_common_ _n(setf_subseq_common_)
#define reduce_common_ _n(reduce_common_)
#define sort_common_ _n(sort_common_)
#define stable_sort_common_ _n(stable_sort_common_)
#define replace_common_ _n(replace_common_)
#define concatenate_common_ _n(concatenate_common_)

int copy_seq_common_(addr var, addr *ret);
int elt_common_(addr var, addr index, addr *ret);
int setf_elt_common_(addr value, addr pos, addr index);
int fill_common_(addr var, addr item, addr rest);
int subseq_common_(addr var, addr start, addr end, addr *ret);
int setf_subseq_common_(addr root, addr pos, addr start, addr end);
int reduce_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int sort_common_(Execute ptr, addr pos, addr call, addr rest);
int stable_sort_common_(Execute ptr, addr pos, addr call, addr rest);
int replace_common_(Execute ptr, addr pos1, addr pos2, addr rest);
int concatenate_common_(Execute ptr, addr *ret, addr type, addr right);

#endif

