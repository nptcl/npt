#ifndef __SEQUENCE_WRITE_HEADER__
#define __SEQUENCE_WRITE_HEADER__

#include <stddef.h>
#include "typedef.h"
#include "sequence_range.h"

struct sequence_write {
	unsigned listp : 1;
	unsigned reverse : 1;
	addr pos;
	size_t index, size, revsize, revbase;
};

_g void build_sequence_write_list(struct sequence_write *ptr);
_g void build_sequence_write_result(struct sequence_write *ptr, addr pos);
_g void build_sequence_write(struct sequence_write *ptr, addr pos);
_g addr result_sequence_write(struct sequence_write *ptr);
_g void push_sequence_write(struct sequence_write *ptr, addr pos);
_g void before_sequence_write(struct sequence_write *ptr, struct sequence_range *range);
_g void after_sequence_write(struct sequence_write *ptr, struct sequence_range *range);
_g void reverse_sequence_write(struct sequence_write *ptr, size_t size);

#endif

