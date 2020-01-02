#include "environment.h"
#include "env_describe.h"

_g void init_environment(void)
{
	init_environment_describe();
}

_g void build_environment(Execute ptr)
{
	build_environment_describe(ptr);
}

