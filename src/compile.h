#ifndef __COMPILE_HEADER__
#define __COMPILE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int compile_file_common(Execute ptr, addr file, addr rest,
		addr *ret1, addr *ret2, addr *ret3);
_g void compile_file_pathname_common(Execute ptr, addr file, addr rest, addr *ret);
_g void with_compilation_unit_common(addr form, addr *ret);
_g void syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call);

_g void init_compile(void);

#endif

