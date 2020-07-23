#ifndef __PATHNAME_TRANSLATE_HEADER__
#define __PATHNAME_TRANSLATE_HEADER__

#include "typedef.h"

/* found=0, notfound=1 */
_g void table_logical_pathname(addr *ret);
_g int gethost_logical_pathname_(addr key, addr *ret);
_g int sethost_logical_pathname_(addr key, addr value);

_g int translate_pathname_alloc_(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp);
_g int translate_pathname_heap_(Execute ptr, addr *ret, addr pos, addr from, addr to);

_g void build_pathname_translate(void);

#endif

