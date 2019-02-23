#ifndef __READLITE_HEADER__
#define __READLITE_HEADER__

#include "typedef.h"

void readlite_package_alloc(LocalRoot local, addr *ret,
		const char *package, const char *str);
void readlite_package_local(LocalRoot local, addr *ret,
		const char *package, const char *str);
void readlite_package_heap(addr *ret, const char *package, const char *str);

void readlite_alloc(LocalRoot local, addr *ret, const char *str);
void readlite_local(LocalRoot local, addr *ret, const char *str);
void readlite_heap(addr *ret, const char *str);

#endif

