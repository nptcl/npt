#ifndef __ENV_DESCRIBE_HEADER__
#define __ENV_DESCRIBE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define describe_common_ _n(describe_common_)
#define inspect_common_ _n(inspect_common_)
#define init_environment_describe _n(init_environment_describe)
#define build_environment_describe _n(build_environment_describe)

int describe_common_(Execute ptr, addr object, addr stream);
int inspect_common_(Execute ptr, addr object);
void init_environment_describe(void);
void build_environment_describe(Execute ptr);

#endif

