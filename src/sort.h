#ifndef __SORT_HEADER__
#define __SORT_HEADER__

#include "execute.h"
#include "typedef.h"

/*
 *  unsafe
 */
_g void simplesort_cons_unsafe(addr *ret, addr cons, int (*call)(addr left, addr right));
_g void simplesort_info_cons_unsafe(addr *ret, addr cons, addr info,
		int (*call)(addr info, addr left, addr right));

/*
 *  sort
 */
_g int simple_sort_sequence(Execute ptr, addr pos, addr call, addr key);
_g int bubble_sort_sequence(Execute ptr, addr pos, addr call, addr key);
_g int quick_sort_sequence(Execute ptr, addr pos, addr call, addr key);
_g int merge_sort_sequence(Execute ptr, addr pos, addr call, addr key);

#endif

