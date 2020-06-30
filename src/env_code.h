#ifndef __ENV_CODE_HEADER__
#define __ENV_CODE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int disassemble_common(Execute ptr, addr pos);
_g void trace_common(addr form, addr env, addr *ret);
_g void untrace_common(addr form, addr env, addr *ret);
_g void trace_add_common(Execute ptr, addr list, addr *ret);
_g void trace_del_common(Execute ptr, addr list, addr *ret);

_g void init_environment_code(void);
_g void build_environment_code(void);

#endif

