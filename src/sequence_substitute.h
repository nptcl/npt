#ifndef __SEQUENCE_SUBSTITUTE_HEADER__
#define __SEQUENCE_SUBSTITUTE_HEADER__

#include "execute.h"
#include "sequence_count.h"
#include "typedef.h"

#define boolean_substitute_sequence_ _n(boolean_substitute_sequence_)
#define substitute_common_ _n(substitute_common_)
#define substitute_if_common_ _n(substitute_if_common_)
#define substitute_if_not_common_ _n(substitute_if_not_common_)
#define nsubstitute_common_ _n(nsubstitute_common_)
#define nsubstitute_if_common_ _n(nsubstitute_if_common_)
#define nsubstitute_if_not_common_ _n(nsubstitute_if_not_common_)

int boolean_substitute_sequence_(struct count_struct *str, int *ret, addr pos);
int substitute_common_(Execute ptr,
		addr *ret, addr item1, addr item2, addr pos, addr rest);
int substitute_if_common_(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
int substitute_if_not_common_(Execute ptr,
		addr *ret, addr item, addr call, addr pos, addr rest);
int nsubstitute_common_(Execute ptr,
		addr item1, addr item2, addr pos, addr rest);
int nsubstitute_if_common_(Execute ptr,
		addr item, addr call, addr pos, addr rest);
int nsubstitute_if_not_common_(Execute ptr,
		addr item, addr call, addr pos, addr rest);

#endif

