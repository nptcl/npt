#ifndef __SEQUENCE_HEADER__
#define __SEQUENCE_HEADER__

#include "array.h"
#include "object.h"
#include "local.h"

/*
 *  unsafe
 */
void copy_list_heap_unsafe(addr *ret, addr cons);
void copy_list_local_unsafe(LocalRoot local, addr *ret, addr cons);
void copy_list_alloc_unsafe(LocalRoot local, addr *ret, addr cons);
void copy_list_heap_safe(addr *ret, addr cons);
void copy_list_local_safe(LocalRoot local, addr *ret, addr cons);
void copy_list_alloc_safe(LocalRoot local, addr *ret, addr cons);
int list_length_safe(addr list, size_t *ret);

void make_vector_from_list(addr *ret, addr cons);
void make_vector4_from_list(addr *ret, addr cons);
void simplesort_cons_unsafe(addr *ret, addr cons, int (*call)(addr left, addr right));
void simplesort_info_cons_unsafe(addr *ret, addr cons, addr info,
		int (*call)(addr info, addr left, addr right));

void nth_unsafe(addr *ret, size_t index, addr cons);

void append_cons_heap_unsafe(addr *ret, addr cons1, addr cons2);
void append_cons_local_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2);
void append_cons_alloc_unsafe(LocalRoot local,
		addr *ret, addr cons1, addr cons2);

int delete_cons_eq_unsafe(addr key, addr cons, addr *ret);
int delete1_cons_eq_unsafe(addr key, addr cons, addr *ret);
void remove_cons_eq_unsafe_heap(addr key, addr cons, addr *ret);
void remove_cons_eq_unsafe_local(LocalRoot local,
		addr key, addr cons, addr *ret);
void remove_cons_eq_unsafe_alloc(LocalRoot local,
		addr key, addr cons, addr *ret);


/*
 *  sequence
 */
void reverse_sequence_alloc(LocalRoot local, addr *ret, addr pos);
void reverse_sequence_local(LocalRoot local, addr *ret, addr pos);
void reverse_sequence_heap(addr *ret, addr pos);
void nreverse_sequence(addr *ret, addr pos);

void list_fill_safe(addr pos, addr item, addr start, addr end);
int sequence_start_end(addr start, addr end, size_t size, size_t *ret1, size_t *ret2);
void vector_fill(addr pos, addr item, addr start, addr end);
void list_subseq(addr *ret, addr pos, addr start, addr end);
void vector_subseq(addr *ret, addr pos, addr start, addr end);


/*
 *  sequence control
 */
int sequencep(addr pos);
size_t length_sequence(addr pos, int fill);
void getelt_inplace_sequence(addr pos, size_t index, struct array_value *str);
void setelt_inplace_sequence(LocalRoot local,
		addr pos, size_t index, const struct array_value *str);
void getelt_sequence(LocalRoot local, addr pos, size_t index, addr *ret);
void setelt_sequence(addr pos, size_t index, addr value);
int make_sequence_sequence(Execute ptr, addr *ret, addr type, addr size, addr rest);
void setf_subseq_sequence(addr root, addr pos, addr start, addr end);
int map_sequence(Execute ptr, addr *ret, addr type, addr call, addr rest);
int map_into_sequence(Execute ptr, addr var, addr call, addr rest);
int reduce_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int count_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int count_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int count_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int merge_sequence(Execute ptr, addr *ret,
		addr type, addr pos1, addr pos2, addr call, addr key);
int simple_sort_sequence(Execute ptr, addr pos, addr call, addr key);
int quick_sort_sequence(Execute ptr, addr pos, addr call, addr key);
int merge_sort_sequence(Execute ptr, addr pos, addr call, addr key);
int find_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int find_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int find_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int position_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int position_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int position_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int search_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
int mismatch_sequence(Execute ptr, addr *ret, addr pos1, addr pos2, addr rest);
void replace_sequence(Execute ptr, addr pos1, addr pos2, addr rest);
int substitute_sequence(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest);
int substitute_if_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
int substitute_if_not_sequence(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
int nsubstitute_sequence(Execute ptr,
		addr item1, addr item2, addr pos, addr rest);
int nsubstitute_if_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest);
int nsubstitute_if_not_sequence(Execute ptr,
		addr item, addr call, addr pos, addr rest);
int concatenate_sequence(Execute ptr, addr *ret, addr type, addr right);
int remove_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int remove_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int remove_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int delete_sequence(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int delete_if_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int delete_if_not_sequence(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int remove_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest);
int delete_duplicates_sequence(Execute ptr, addr *ret, addr pos, addr rest);

#endif

