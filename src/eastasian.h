#ifndef __EASTASIAN_HEADER__
#define __EASTASIAN_HEADER__

#include "typedef.h"

#define eastasian_length_ _n(eastasian_length_)
#define eastasian_set_syscall_ _n(eastasian_set_syscall_)
#define eastasian_get_syscall_ _n(eastasian_get_syscall_)
#define eastasian_width_syscall_ _n(eastasian_width_syscall_)

_g int eastasian_length_(addr pos, size_t *ret, int *rerrp);
_g int eastasian_set_syscall_(addr pos, addr value, addr errorp, addr *ret);
_g int eastasian_get_syscall_(addr pos, addr *retsize, addr *retsymbol);
_g int eastasian_width_syscall_(addr pos, addr *ret, addr *retbool);

#endif

