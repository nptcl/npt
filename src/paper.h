#ifndef __PAPER_HEADER__
#define __PAPER_HEADER__

#include "local.h"
#include "typedef.h"

#define paper_array_alloc _n(paper_array_alloc)
#define paper_array_local _n(paper_array_local)
#define paper_array_heap _n(paper_array_heap)
#define paper_body_alloc _n(paper_body_alloc)
#define paper_body_local _n(paper_body_local)
#define paper_body_heap _n(paper_body_heap)
#define paper_arraybody_alloc_ _n(paper_arraybody_alloc_)
#define paper_arraybody_local_ _n(paper_arraybody_local_)
#define paper_arraybody_heap_ _n(paper_arraybody_heap_)
#define paperp _n(paperp)
#define paper_array_p _n(paper_array_p)
#define paper_body_p _n(paper_body_p)
#define paper_copy_body_alloc _n(paper_copy_body_alloc)
#define paper_get_type _n(paper_get_type)
#define paper_set_type _n(paper_set_type)
#define paper_len_array _n(paper_len_array)
#define paper_len_body _n(paper_len_body)
#define paper_get_array _n(paper_get_array)
#define paper_set_array _n(paper_set_array)
#define paper_get_body _n(paper_get_body)
#define paper_set_body _n(paper_set_body)
#define paper_ptr_body_unsafe _n(paper_ptr_body_unsafe)
#define paper_get_memory _n(paper_get_memory)
#define paper_set_memory _n(paper_set_memory)
#define paper_length_body_ _n(paper_length_body_)
#define paper_length_array_ _n(paper_length_array_)
#define paper_list_body_ _n(paper_list_body_)
#define paper_list_array_ _n(paper_list_array_)
#define paper_vector_body_ _n(paper_vector_body_)
#define paper_vector_array_ _n(paper_vector_array_)
#define paper_get_type_ _n(paper_get_type_)
#define paper_set_type_ _n(paper_set_type_)
#define paper_get_array_ _n(paper_get_array_)
#define paper_set_array_ _n(paper_set_array_)
#define paper_get_body_ _n(paper_get_body_)
#define paper_set_body_ _n(paper_set_body_)

void paper_array_alloc(LocalRoot local, addr *ret, size_t array);
void paper_array_local(LocalRoot local, addr *ret, size_t array);
void paper_array_heap(addr *ret, size_t array);
void paper_body_alloc(LocalRoot local, addr *ret, size_t body);
void paper_body_local(LocalRoot local, addr *ret, size_t body);
void paper_body_heap(addr *ret, size_t body);
int paper_arraybody_alloc_(LocalRoot local, addr *ret, size_t array, size_t body);
int paper_arraybody_local_(LocalRoot local, addr *ret, size_t array, size_t body);
int paper_arraybody_heap_(addr *ret, size_t array, size_t body);
int paperp(addr pos);
int paper_array_p(addr pos);
int paper_body_p(addr pos);
void paper_copy_body_alloc(LocalRoot local, addr *ret, addr pos);
void paper_get_type(addr pos, byte *ret);
void paper_set_type(addr pos, byte value);
void paper_len_array(addr pos, size_t *ret);
void paper_len_body(addr pos, size_t *ret);
void paper_get_array(addr pos, size_t index, addr *ret);
void paper_set_array(addr pos, size_t index, addr value);
void paper_get_body(addr pos, size_t index, byte *ret);
void paper_set_body(addr pos, size_t index, byte value);
void paper_ptr_body_unsafe(addr pos, void **ret);
void paper_get_memory(addr pos, size_t a, size_t b, void *ptr, size_t *ret);
void paper_set_memory(addr pos, size_t a, size_t b, const void *ptr, size_t *ret);

/* syscall */
int paper_length_body_(addr pos, addr *ret);
int paper_length_array_(addr pos, addr *ret);
int paper_list_body_(addr pos, addr *ret);
int paper_list_array_(addr pos, addr *ret);
int paper_vector_body_(addr pos, addr *ret);
int paper_vector_array_(addr pos, addr *ret);
int paper_get_type_(addr pos, addr *ret);
int paper_set_type_(addr pos, addr second);
int paper_get_array_(addr pos, addr index, addr *ret);
int paper_set_array_(addr pos, addr index, addr value);
int paper_get_body_(addr pos, addr index, addr *ret);
int paper_set_body_(addr pos, addr index, addr value);

#endif

