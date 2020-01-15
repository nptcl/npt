#ifndef __SEQUENCE_RANGE_HEADER__
#define __SEQUENCE_RANGE_HEADER__

#include  <stddef.h>
#include "array_make.h"
#include "local.h"
#include "typedef.h"

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
_g void build_sequence_range(struct sequence_range *ptr,
		addr pos, addr start, addr end);
_g struct sequence_range *make_sequence_range(LocalRoot local,
		addr pos, addr start, addr end);
_g void build_sequence_range_endp(struct sequence_range *ptr,
		addr list, addr start, addr end);
_g struct sequence_range *make_sequence_range_endp(LocalRoot local,
		addr list, addr start, addr end);
_g void build_sequence_range_vector2(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end,
		addr *root, addr *tail);
_g void build_sequence_range_vector(LocalRoot local,
		struct sequence_range *ptr, addr list, addr start, addr end);
_g struct sequence_range *make_sequence_range_vector(LocalRoot local,
		addr list, addr start, addr end);

/* access */
_g int get_sequence_range(struct sequence_range *ptr, addr *ret);
_g int getnext_sequence_range(struct sequence_range *ptr, addr *ret);
_g int next_sequence_range(struct sequence_range *ptr);
_g int endp_sequence_range(struct sequence_range *ptr);
_g void set_sequence_range(struct sequence_range *ptr, addr value);
_g void getinplace_sequence_range(struct sequence_range *ptr, struct array_value *ret);
_g void setinplace_sequence_range(LocalRoot local,
		struct sequence_range *ptr, const struct array_value *str);

/* reverse */
_g void reverse_sequence_range(struct sequence_range *ptr);
_g int endp_reverse_sequence_range(struct sequence_range *ptr);
_g int next_reverse_sequence_range(struct sequence_range *ptr);
_g int get_reverse_sequence_range(struct sequence_range *ptr, addr *ret);
_g int getnext_reverse_sequence_range(struct sequence_range *ptr, addr *ret);
_g void set_reverse_sequence_range(struct sequence_range *ptr, addr value);

/* remove */
_g void remove_sequence_range(struct sequence_range *ptr);

#endif

