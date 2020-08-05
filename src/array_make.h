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
_g int array_set_dimension_(addr pos, addr value);
_g void array_allocate_bit(LocalRoot local, addr pos, struct array_struct *str);
_g int array_allocate_size_(LocalRoot local, addr pos, struct array_struct *str);
_g int array_allocate_(LocalRoot local, addr pos, struct array_struct *str);

/* make-array */
_g int array_set_displaced_(addr pos, addr displaced, addr offset);
_g void array_set_simple(addr pos);
_g int array_make_memory_(addr pos,
		addr adjust, addr fill, addr displaced, addr offset);
_g int array_make_initial_(addr pos, addr initial, addr contents);
_g int array_make_array_(addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset);

/* interface */
_g int array_contents_heap_(addr *ret, addr rank, addr contents);
_g int array_character_alloc_(LocalRoot local, addr pos);
_g int array_build_(addr pos);

#endif

