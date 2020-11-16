#ifndef __COPY_HEADER__
#define __COPY_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

#define copyhard_object _n(copyhard_object)
#define copylocal_object _n(copylocal_object)
#define copylocal_list_stdarg _n(copylocal_list_stdarg)
#define copyheap _n(copyheap)
#define copyheapr _n(copyheapr)
#define init_copy _n(init_copy)

void copyhard_object(LocalRoot local, addr *ret, addr pos);
int copylocal_object(LocalRoot local, addr *ret, addr pos);
void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args);
int copyheap(addr *ret, addr pos);
addr copyheapr(addr pos);

void init_copy(void);

#endif

