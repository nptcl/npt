#ifndef __PATHNAME_TRANSLATE_HEADER__
#define __PATHNAME_TRANSLATE_HEADER__

#include "typedef.h"

#define table_logical_pathname _n(table_logical_pathname)
#define gethost_logical_pathname_ _n(gethost_logical_pathname_)
#define sethost_logical_pathname_ _n(sethost_logical_pathname_)
#define translate_pathname_alloc_ _n(translate_pathname_alloc_)
#define translate_pathname_heap_ _n(translate_pathname_heap_)
#define build_pathname_translate _n(build_pathname_translate)

/* found=0, notfound=1 */
void table_logical_pathname(addr *ret);
int gethost_logical_pathname_(addr key, addr *ret);
int sethost_logical_pathname_(addr key, addr value);

int translate_pathname_alloc_(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp);
int translate_pathname_heap_(Execute ptr, addr *ret, addr pos, addr from, addr to);

void build_pathname_translate(void);

#endif

