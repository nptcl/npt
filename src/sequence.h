#ifndef __SEQUENCE_HEADER__
#define __SEQUENCE_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

_g int sequencep(addr pos);
_g int listp_sequence(addr pos);
_g int vectorp_sequence(addr pos);
_g void vector_check_sequence(addr type, size_t size);
_g void simple_vector_check_sequence(addr type, size_t size);
_g void array_check_sequence(addr type, size_t size);
_g void make_vector_from_list(addr *ret, addr cons);
_g void make_vector4_from_list(addr *ret, addr cons);
_g void list_start_end_sequence(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2);
_g int size_start_end_sequence(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2);

/* common */
_g size_t length_sequence(addr pos, int fill);
_g void getelt_inplace_sequence(addr pos, size_t index, struct array_value *str);
_g void setelt_inplace_sequence(LocalRoot local,
		addr pos, size_t index, const struct array_value *str);
_g void getelt_sequence(LocalRoot local, addr pos, size_t index, addr *ret);
_g void setelt_sequence(addr pos, size_t index, addr value);
_g void reverse_sequence_heap(addr *ret, addr pos);
_g void nreverse_sequence(addr *ret, addr pos);

#endif

