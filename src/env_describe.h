#ifndef __ENV_DESCRIBE_HEADER__
#define __ENV_DESCRIBE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define describe_common _n(describe_common)
#define inspect_common _n(inspect_common)
#define init_environment_describe _n(init_environment_describe)
#define build_environment_describe _n(build_environment_describe)

_g int describe_common(Execute ptr, addr object, addr stream);
_g int inspect_common(Execute ptr, addr object);
_g void init_environment_describe(void);
_g void build_environment_describe(Execute ptr);

#endif

