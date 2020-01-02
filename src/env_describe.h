#ifndef __ENV_DESCRIBE_HEADER__
#define __ENV_DESCRIBE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

_g int describe_common(Execute ptr, addr object, addr stream);
_g void init_environment_describe(void);
_g void build_environment_describe(Execute ptr);

#endif

