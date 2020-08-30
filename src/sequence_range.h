#ifndef __SEQUENCE_RANGE_HEADER__
#define __SEQUENCE_RANGE_HEADER__

#include  <stddef.h>
#include "array_make.h"
#include "local.h"
#include "typedef.h"

#define save_sequence_range _n(save_sequence_range)
#define load_sequence_range _n(load_sequence_range)
#define build_sequence_range_ _n(build_sequence_range_)
#define make_sequence_range_ _n(make_sequence_range_)
#define build_sequence_range_endp_ _n(build_sequence_range_endp_)
#define make_sequence_range_endp_ _n(make_sequence_range_endp_)
#define build_sequence_range_vector2_ _n(build_sequence_range_vector2_)
#define build_sequence_range_vector_ _n(build_sequence_range_vector_)
#define make_sequence_range_vector_ _n(make_sequence_range_vector_)
#define get_sequence_range_ _n(get_sequence_range_)
#define getnext_sequence_range_ _n(getnext_sequence_range_)
#define next_sequence_range_ _n(next_sequence_range_)
#define endp_sequence_range _n(endp_sequence_range)
#define set_sequence_range_ _n(set_sequence_range_)
#define getinplace_sequence_range_ _n(getinplace_sequence_range_)
#define setinplace_sequence_range_ _n(setinplace_sequence_range_)
#define reverse_sequence_range _n(reverse_sequence_range)
#define endp_reverse_sequence_range _n(endp_reverse_sequence_range)
#define next_reverse_sequence_range_ _n(next_reverse_sequence_range_)
#define get_reverse_sequence_range_ _n(get_reverse_sequence_range_)
#define getnext_reverse_sequence_range_ _n(getnext_reverse_sequence_range_)
#define set_reverse_sequence_range_ _n(set_reverse_sequence_range_)
#define remove_sequence_range_ _n(remove_sequence_range_)

struct sequence_range {
	unsigned listp : 1;
	unsigned endp : 1;
	addr pos, list, prev;
	size_t start, end, size, index;
	addr save_pos, save_list, save_prev;
	size_t save_index, save_end, save_size;
};

/* save/load */
_g void save_sequence_range(struct sequence_range *ptr);
_g void load_sequence_range(struct sequence_range *ptr);


/* build */
_g int build_sequence_range_(struct sequence_range *ptr,
		addr pos, addr start, addr end);
_g int make_sequence_range_(LocalRoot local,
		addr pos, addr start, addr end, struct sequence_range **ret);
_g int build_sequence_range_endp_(struct sequence_range *ptr,
		addr list, addr start, addr end);
_g int make_sequence_range_endp_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret);
_g int build_sequence_range_vector2_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end,
		addr *root, addr *tail);
_g int build_sequence_range_vector_(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end);
_g int make_sequence_range_vector_(LocalRoot local,
		addr list, addr start, addr end, struct sequence_range **ret);

/* access */
_g int get_sequence_range_(struct sequence_range *ptr, addr *value, int *ret);
_g int getnext_sequence_range_(struct sequence_range *ptr, addr *value, int *ret);
_g int next_sequence_range_(struct sequence_range *ptr, int *ret);
_g int endp_sequence_range(struct sequence_range *ptr);
_g int set_sequence_range_(struct sequence_range *ptr, addr value);
_g int getinplace_sequence_range_(struct sequence_range *ptr, struct array_value *ret);
_g int setinplace_sequence_range_(LocalRoot local,
		struct sequence_range *ptr, const struct array_value *str);

/* reverse */
_g void reverse_sequence_range(struct sequence_range *ptr);
_g int endp_reverse_sequence_range(struct sequence_range *ptr);
_g int next_reverse_sequence_range_(struct sequence_range *ptr, int *ret);
_g int get_reverse_sequence_range_(struct sequence_range *ptr, addr *value, int *ret);
_g int getnext_reverse_sequence_range_(
		struct sequence_range *ptr, addr *value, int *ret);
_g int set_reverse_sequence_range_(struct sequence_range *ptr, addr value);

/* remove */
_g int remove_sequence_range_(struct sequence_range *ptr);

#endif

