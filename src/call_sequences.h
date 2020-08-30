#ifndef __CALL_SEQUENCES_HEADER__
#define __CALL_SEQUENCES_HEADER__

#include "execute.h"
#include "typedef.h"

#define copy_seq_common _n(copy_seq_common)
#define fill_common _n(fill_common)
#define make_sequence_common _n(make_sequence_common)
#define subseq_common _n(subseq_common)
#define setf_subseq_common_ _n(setf_subseq_common_)
#define map_common _n(map_common)
#define map_into_common _n(map_into_common)
#define reduce_common _n(reduce_common)
#define count_common _n(count_common)
#define count_if_common _n(count_if_common)
#define count_if_not_common _n(count_if_not_common)
#define merge_common _n(merge_common)
#define find_common _n(find_common)
#define find_if_common _n(find_if_common)
#define find_if_not_common _n(find_if_not_common)
#define position_common _n(position_common)
#define position_if_common _n(position_if_common)
#define position_if_not_common _n(position_if_not_common)
#define search_common _n(search_common)
#define mismatch_common _n(mismatch_common)
#define replace_common_ _n(replace_common_)
#define substitute_common _n(substitute_common)
#define substitute_if_common _n(substitute_if_common)
#define substitute_if_not_common _n(substitute_if_not_common)
#define nsubstitute_common _n(nsubstitute_common)
#define nsubstitute_if_common _n(nsubstitute_if_common)
#define nsubstitute_if_not_common _n(nsubstitute_if_not_common)
#define concatenate_common _n(concatenate_common)
#define remove_common _n(remove_common)
#define remove_if_common _n(remove_if_common)
#define remove_if_not_common _n(remove_if_not_common)
#define delete_common _n(delete_common)
#define delete_if_common _n(delete_if_common)
#define delete_if_not_common _n(delete_if_not_common)
#define remove_duplicates_common _n(remove_duplicates_common)
#define delete_duplicates_common _n(delete_duplicates_common)

_g int copy_seq_common(addr var, addr *ret);
_g int fill_common(addr var, addr item, addr start, addr end);
_g int make_sequence_common(Execute ptr, addr *ret, addr type, addr size, addr rest);
_g int subseq_common(addr var, addr start, addr end, addr *ret);
_g int setf_subseq_common_(addr root, addr pos, addr start, addr end);
_g int map_common(Execute ptr, addr *ret, addr type, addr call, addr rest);
_g int map_into_common(Execute ptr, addr var, addr call, addr rest);
_g int reduce_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int count_common(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int count_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int count_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int merge_common(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key);
_g int find_common(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int find_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int find_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int position_common(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int position_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int position_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int search_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
_g int mismatch_common(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
_g int replace_common_(Execute ptr, addr pos1, addr pos2, addr rest);
_g int substitute_common(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest);
_g int substitute_if_common(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
_g int substitute_if_not_common(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
_g int nsubstitute_common(Execute ptr,
		addr item1, addr item2, addr pos, addr rest);
_g int nsubstitute_if_common(Execute ptr,
		addr item, addr call, addr pos, addr rest);
_g int nsubstitute_if_not_common(Execute ptr,
		addr item, addr call, addr pos, addr rest);
_g int concatenate_common(Execute ptr, addr *ret, addr type, addr right);
_g int remove_common(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int remove_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int remove_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int delete_common(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int delete_if_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int delete_if_not_common(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int remove_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest);
_g int delete_duplicates_common(Execute ptr, addr *ret, addr pos, addr rest);

#endif

