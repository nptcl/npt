#ifndef __SORT_HEADER__
#define __SORT_HEADER__

#include "execute.h"
#include "typedef.h"

#define simplesort_cons_unsafe_ _n(simplesort_cons_unsafe_)
#define simplesort_info_cons_unsafe_ _n(simplesort_info_cons_unsafe_)
#define simple_sort_sequence_ _n(simple_sort_sequence_)
#define bubble_sort_sequence_ _n(bubble_sort_sequence_)
#define quick_sort_sequence_ _n(quick_sort_sequence_)
#define merge_sort_sequence_ _n(merge_sort_sequence_)

/* unsafe */
int simplesort_cons_unsafe_(Execute ptr, addr *ret, addr cons,
		int (*call_)(Execute ptr, addr left, addr right, int *ret));
int simplesort_info_cons_unsafe_(Execute ptr, addr *ret, addr cons, addr info,
		int (*call_)(Execute ptr, addr info, addr left, addr right, int *ret));

/* sort */
int simple_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
int bubble_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
int quick_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
int merge_sort_sequence_(Execute ptr, addr pos, addr call, addr key);

#endif

