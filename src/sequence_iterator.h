#ifndef __SEQUENCE_ITERATOR_HEADER__
#define __SEQUENCE_ITERATOR_HEADER__

#include <stddef.h>
#include "local.h"
#include "typedef.h"

struct sequence_iterator {
	unsigned listp : 1;
	addr pos, root;
	size_t size, index;
};

struct sequence_group {
	struct sequence_iterator **data;
	size_t size, callsize;
	addr list;
};


/* sequence-iterator */
_g struct sequence_iterator *make_sequence_iterator_local(
		LocalRoot local, addr pos, int fill);
_g int end_sequence_iterator(struct sequence_iterator *ptr);
_g void length_sequence_iterator(struct sequence_iterator *ptr, size_t *ret);
_g int object_sequence_iterator(struct sequence_iterator *iter, addr *ret);
_g int set_sequence_iterator(struct sequence_iterator *iter, addr value);


/* sequence-group */
_g struct sequence_group *make_sequence_group_local(
		LocalRoot local, addr rest, int fill);
_g void list_sequence_group_local(LocalRoot local,
		addr *ret, struct sequence_group *group);
_g int set_sequence_group(struct sequence_group *group, addr list);
_g void clear_sequence_group(struct sequence_group *group);
_g void count_sequence_group(struct sequence_group *group, size_t *ret);

#endif

