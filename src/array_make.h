#ifndef __ARRAY_OBJECT_HEADER__
#define __ARRAY_OBJECT_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"
#include "memory.h"

#define array_set_type _n(array_set_type)
#define array_set_element_size _n(array_set_element_size)
#define array_set_dimension_ _n(array_set_dimension_)
#define array_allocate_bit _n(array_allocate_bit)
#define array_allocate_size_ _n(array_allocate_size_)
#define array_allocate_ _n(array_allocate_)
#define array_set_displaced_ _n(array_set_displaced_)
#define array_set_simple _n(array_set_simple)
#define array_make_memory_ _n(array_make_memory_)
#define array_make_initial_ _n(array_make_initial_)
#define array_make_clear_ _n(array_make_clear_)
#define array_make_array_ _n(array_make_array_)
#define array_contents_heap_ _n(array_contents_heap_)
#define array_character_alloc_ _n(array_character_alloc_)
#define array_unsigned8_heap_ _n(array_unsigned8_heap_)
#define array_build_ _n(array_build_)

/* control */
void array_set_type(addr pos);
void array_set_element_size(addr pos);

/* array-allocate */
int array_set_dimension_(addr pos, addr value);
void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str);
int array_allocate_size_(LocalRoot local, addr pos, struct array_struct *str);
int array_allocate_(LocalRoot local, addr pos, struct array_struct *str);

/* make-array */
int array_set_displaced_(addr pos, addr displaced, addr offset);
void array_set_simple(addr pos);
int array_make_memory_(addr pos,
		addr adjust, addr fill, addr displaced, addr offset);
int array_make_initial_(addr pos, addr initial, addr contents);
int array_make_clear_(addr pos);
int array_make_array_(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset);

/* interface */
int array_contents_heap_(addr *ret, addr rank, addr contents);
int array_character_alloc_(LocalRoot local, addr pos);
int array_unsigned8_heap_(addr *ret, size_t size);
int array_build_(addr pos);

#endif

