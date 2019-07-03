#ifndef __SEQUENCE_HEADER__
#define __SEQUENCE_HEADER__

#include "array.h"
#include "object.h"
#include "local.h"

/*
 *  unsafe
 */
_g void copy_list_heap_unsafe(addr *ret, addr cons);
_g void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_heap_safe(addr *ret, addr cons);
_g void copy_list_local_safe(LocalRoot local, addr *ret, addr cons);
_g void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons);
_g int list_length_safe(addr list, size_t *ret);

_g void make_vector_from_list(addr *ret, addr cons);
_g void make_vector4_from_list(addr *ret, addr cons);

_g void nth_unsafe(addr *ret, size_t index, addr cons);

_g void append_cons_heap_unsafe(addr *ret, addr cons1, addr cons2);
_g void append_cons_local_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2);
_g void append_cons_alloc_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2);

_g int delete_cons_eq_unsafe(addr key, addr cons, addr *ret);
_g int delete1_cons_eq_unsafe(addr key, addr cons, addr *ret);
_g void remove_cons_eq_unsafe_heap(addr key, addr cons, addr *ret);
_g void remove_cons_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret);
_g void remove_cons_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret);


/*
 *  sequence
 */
_g void reverse_sequence_alloc(LocalRoot local, addr *ret, addr pos);
_g void reverse_sequence_local(LocalRoot local, addr *ret, addr pos);
_g void reverse_sequence_heap(addr *ret, addr pos);
_g void nreverse_sequence(addr *ret, addr pos);

_g void list_fill_safe(addr pos, addr item, addr start, addr end);
_g int sequence_start_end(addr start, addr end, size_t size, size_t *ret1, size_t *ret2);
_g void vector_fill(addr pos, addr item, addr start, addr end);
_g void list_subseq(addr *ret, addr pos, addr start, addr end);
_g void vector_subseq(addr *ret, addr pos, addr start, addr end);


/*
 *  sequence control
 */
_g int sequencep(addr pos);
_g size_t length_sequence(addr pos, int fill);
_g void getelt_inplace_sequence(addr pos, size_t index, struct array_value *str);
_g void setelt_inplace_sequence(LocalRoot local,
		addr pos, size_t index, const struct array_value *str);
_g void getelt_sequence(LocalRoot local, addr pos, size_t index, addr *ret);
_g void setelt_sequence(addr pos, size_t index, addr value);
_g int make_sequence_sequence(Execute ptr, addr *ret, addr type, addr size, addr rest);
_g int listp_sequence(addr pos);
_g void setf_subseq_sequence(addr root, addr pos, addr start, addr end);
_g int map_sequence(Execute ptr, addr *ret, addr type, addr call, addr rest);
_g int map_into_sequence(Execute ptr, addr var, addr call, addr rest);
_g int reduce_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int count_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int count_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int count_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int merge_sequence(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key);
_g int find_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int find_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int find_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int position_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int position_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int position_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int search_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
_g int mismatch_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
_g void replace_sequence(Execute ptr, addr pos1, addr pos2, addr rest);
_g int substitute_sequence(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest);
_g int substitute_if_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
_g int substitute_if_not_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
_g int nsubstitute_sequence(Execute ptr,
		addr item1, addr item2, addr pos, addr rest);
_g int nsubstitute_if_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest);
_g int nsubstitute_if_not_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest);
_g int concatenate_sequence(Execute ptr, addr *ret, addr type, addr right);
_g int remove_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int remove_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int remove_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int delete_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
_g int delete_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int delete_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
_g int remove_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest);
_g int delete_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest);

#endif

