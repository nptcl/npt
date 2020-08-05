#ifndef __COPY_HEADER__
#define __COPY_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

_g void copyhard_object(LocalRoot local, addr *ret, addr pos);
_g int copylocal_object(LocalRoot local, addr *ret, addr pos);
_g void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args);
_g int copyheap(addr *ret, addr pos);
_g addr copyheapr(addr pos);

_g void init_copy(void);

#endif

