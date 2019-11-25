#ifndef __SEQUENCE_COMMON_HEADER__
#define __SEQUENCE_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

_g void copy_seq_common(Execute ptr, addr var, addr *ret);
_g void fill_common(Execute ptr, addr var, addr item, addr start, addr end);
_g int make_sequence_common(Execute ptr, addr *ret, addr type, addr size, addr rest);
_g void subseq_common(Execute ptr, addr var, addr start, addr end, addr *ret);
_g void setf_subseq_common(addr root, addr pos, addr start, addr end);
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
_g void replace_common(Execute ptr, addr pos1, addr pos2, addr rest);
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

