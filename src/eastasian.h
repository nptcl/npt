#ifndef __EASTASIAN_HEADER__
#define __EASTASIAN_HEADER__

#include "typedef.h"

/* length */
_g int eastasian_length_(addr pos, size_t *ret, int *rerrp);
/* syscall */
_g int eastasian_set_syscall_(addr pos, addr value, addr errorp, addr *ret);
_g int eastasian_get_syscall_(addr pos, addr *retsize, addr *retsymbol);
_g int eastasian_width_syscall_(addr pos, addr *ret, addr *retbool);

#endif

