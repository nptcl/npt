#ifndef __EASTASIAN_HEADER__
#define __EASTASIAN_HEADER__

#include "typedef.h"

/* length */
_g int eastasian_length(addr pos, size_t *ret);
/* syscall */
_g int eastasian_set_syscall_(addr pos, addr value, addr errorp, addr *ret);
_g void eastasian_get_syscall(addr pos, addr *retsize, addr *retsymbol);
_g void eastasian_width_syscall(addr pos, addr *ret, addr *retbool);

#endif

