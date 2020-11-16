#ifndef __SEQUENCE_WRITE_HEADER__
#define __SEQUENCE_WRITE_HEADER__

#include <stddef.h>
#include "typedef.h"
#include "sequence_range.h"

#define build_sequence_write_list _n(build_sequence_write_list)
#define build_sequence_write_result _n(build_sequence_write_result)
#define build_sequence_write_ _n(build_sequence_write_)
#define result_sequence_write _n(result_sequence_write)
#define push_sequence_write_ _n(push_sequence_write_)
#define before_sequence_write_ _n(before_sequence_write_)
#define after_sequence_write_ _n(after_sequence_write_)
#define reverse_sequence_write _n(reverse_sequence_write)

struct sequence_write {
	unsigned listp : 1;
	unsigned reverse : 1;
	addr pos;
	size_t index, size, revsize, revbase;
};

void build_sequence_write_list(struct sequence_write *ptr);
void build_sequence_write_result(struct sequence_write *ptr, addr pos);
int build_sequence_write_(struct sequence_write *ptr, addr pos);
addr result_sequence_write(struct sequence_write *ptr);
int push_sequence_write_(struct sequence_write *ptr, addr pos);
int before_sequence_write_(struct sequence_write *ptr, struct sequence_range *range);
int after_sequence_write_(struct sequence_write *ptr, struct sequence_range *range);
void reverse_sequence_write(struct sequence_write *ptr, size_t size);

#endif

