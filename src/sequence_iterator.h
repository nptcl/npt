#ifndef __SEQUENCE_ITERATOR_HEADER__
#define __SEQUENCE_ITERATOR_HEADER__

#include <stddef.h>
#include "local.h"
#include "typedef.h"

#define make_sequence_iterator_local_ _n(make_sequence_iterator_local_)
#define end_sequence_iterator _n(end_sequence_iterator)
#define length_sequence_iterator_ _n(length_sequence_iterator_)
#define object_sequence_iterator_ _n(object_sequence_iterator_)
#define set_sequence_iterator_ _n(set_sequence_iterator_)
#define make_sequence_group_local_ _n(make_sequence_group_local_)
#define list_sequence_group_local _n(list_sequence_group_local)
#define set_sequence_group_ _n(set_sequence_group_)
#define clear_sequence_group _n(clear_sequence_group)
#define count_sequence_group_ _n(count_sequence_group_)

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
_g int make_sequence_iterator_local_(LocalRoot local,
		addr pos, int fill, struct sequence_iterator **ret);
_g int end_sequence_iterator(struct sequence_iterator *ptr);
_g int length_sequence_iterator_(struct sequence_iterator *ptr, size_t *ret);
_g int object_sequence_iterator_(struct sequence_iterator *iter, addr *value, int *ret);
_g int set_sequence_iterator_(struct sequence_iterator *iter, addr value, int *ret);


/* sequence-group */
_g int make_sequence_group_local_(
		LocalRoot local, addr rest, int fill, struct sequence_group **ret);
_g void list_sequence_group_local(LocalRoot local,
		addr *ret, struct sequence_group *group);
_g int set_sequence_group_(struct sequence_group *group, addr list, int *ret);
_g void clear_sequence_group(struct sequence_group *group);
_g int count_sequence_group_(struct sequence_group *group, size_t *ret);

#endif

