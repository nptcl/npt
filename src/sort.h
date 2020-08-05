#ifndef __SORT_HEADER__
#define __SORT_HEADER__

#include "execute.h"
#include "typedef.h"

/*
 *  unsafe
 */
_g int simplesort_cons_unsafe_(addr *ret,
		addr cons, int (*call_)(addr left, addr right, int *ret));
_g int simplesort_info_cons_unsafe_(addr *ret, addr cons, addr info,
		int (*call_)(addr info, addr left, addr right, int *ret));

/*
 *  sort
 */
_g int simple_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
_g int bubble_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
_g int quick_sort_sequence_(Execute ptr, addr pos, addr call, addr key);
_g int merge_sort_sequence_(Execute ptr, addr pos, addr call, addr key);

#endif

