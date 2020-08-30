#ifndef __SEQUENCE_HEADER__
#define __SEQUENCE_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

#define sequencep _n(sequencep)
#define listp_sequence_ _n(listp_sequence_)
#define vectorp_sequence_ _n(vectorp_sequence_)
#define vectorp_sequence_debug _n(vectorp_sequence_debug)
#define vector_check_sequence_ _n(vector_check_sequence_)
#define simple_vector_check_sequence_ _n(simple_vector_check_sequence_)
#define array_check_sequence_ _n(array_check_sequence_)
#define make_vector_from_list_ _n(make_vector_from_list_)
#define make_vector4_from_list_ _n(make_vector4_from_list_)
#define list_start_end_sequence_ _n(list_start_end_sequence_)
#define size_start_end_sequence_ _n(size_start_end_sequence_)
#define length_sequence_ _n(length_sequence_)
#define getelt_inplace_sequence_ _n(getelt_inplace_sequence_)
#define setelt_inplace_sequence_ _n(setelt_inplace_sequence_)
#define getelt_sequence_ _n(getelt_sequence_)
#define setelt_sequence_ _n(setelt_sequence_)
#define reverse_sequence_heap_ _n(reverse_sequence_heap_)
#define nreverse_sequence_ _n(nreverse_sequence_)

_g int sequencep(addr pos);
_g int listp_sequence_(addr pos, int *ret);
_g int vectorp_sequence_(addr pos, int *ret);
_g int vectorp_sequence_debug(addr pos);
_g int vector_check_sequence_(addr type, size_t size);
_g int simple_vector_check_sequence_(addr type, size_t size);
_g int array_check_sequence_(addr type, size_t size);
_g int make_vector_from_list_(addr *ret, addr cons);
_g int make_vector4_from_list_(addr *ret, addr cons);
_g int list_start_end_sequence_(addr *list, addr *prev,
		addr start, addr end, size_t *ret1, size_t *ret2);
_g int size_start_end_sequence_(addr start, addr end,
		size_t size, size_t *ret1, size_t *ret2, int *ret);

/* common */
_g int length_sequence_(addr pos, int fill, size_t *ret);
_g int getelt_inplace_sequence_(addr pos, size_t index, struct array_value *str);
_g int setelt_inplace_sequence_(LocalRoot local,
		addr pos, size_t index, const struct array_value *str);
_g int getelt_sequence_(LocalRoot local, addr pos, size_t index, addr *ret);
_g int setelt_sequence_(addr pos, size_t index, addr value);
_g int reverse_sequence_heap_(addr *ret, addr pos);
_g int nreverse_sequence_(addr *ret, addr pos);

#endif

