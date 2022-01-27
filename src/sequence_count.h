#ifndef __SEQUENCE_COUNT_HEADER__
#define __SEQUENCE_COUNT_HEADER__

#include "execute.h"
#include "local.h"
#include "sequence_range.h"
#include "typedef.h"

#define boolean_count_sequence_ _n(boolean_count_sequence_)
#define count_common_ _n(count_common_)
#define count_if_common_ _n(count_if_common_)
#define count_if_not_common_ _n(count_if_not_common_)

struct count_struct {
	unsigned delp : 1;
	unsigned fromp : 1;
	unsigned notp : 1;
	unsigned single : 1;
	unsigned test : 2;
	Execute ptr;
	LocalRoot local;
	addr item, second, pos, from, start, end, key, test1, test2, count;
	size_t limit, start_value;
	struct sequence_range range;
};

int boolean_count_sequence_(struct count_struct *str, int *result, addr value);
int count_common_(Execute ptr, addr *ret, addr item, addr pos, addr rest);
int count_if_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);
int count_if_not_common_(Execute ptr, addr *ret, addr call, addr pos, addr rest);

#endif

