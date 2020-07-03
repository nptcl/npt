#ifndef __PATHNAME_TRANSLATE_HEADER__
#define __PATHNAME_TRANSLATE_HEADER__

#include "typedef.h"

/* found=0, notfound=1 */
_g void table_logical_pathname(addr *ret);
_g int gethost_logical_pathname(addr key, addr *ret);
_g void sethost_logical_pathname(addr key, addr value);

_g void translate_pathname_alloc(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp);
_g void translate_pathname_heap(Execute ptr, addr *ret, addr pos, addr from, addr to);

_g void build_pathname_translate(void);

#endif

