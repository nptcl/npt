#ifndef __ARRAY_OBJECT_HEADER__
#define __ARRAY_OBJECT_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"
#include "memory.h"

/* control */
_g void array_set_type(addr pos);
_g void array_set_element_size(addr pos);

/* array-allocate */
_g void array_set_dimension(addr pos, addr value);
_g void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str);
_g void array_allocate_size(LocalRoot local, addr pos, struct array_struct *str);
_g void array_allocate(LocalRoot local, addr pos, struct array_struct *str);

/* make-array */
_g void array_set_displaced(addr pos, addr displaced, addr offset);
_g void array_set_simple(addr pos);
_g void array_make_memory(addr pos,
		addr adjustable, addr fill, addr displaced, addr offset);
_g void array_make_initial(addr pos, addr initial, addr contents);
_g void array_make_array(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset);

/* interface */
_g void array_contents_heap(addr *ret, addr rank, addr contents);
_g void array_character_alloc(LocalRoot local, addr pos);
_g void array_build(addr pos);

#endif

