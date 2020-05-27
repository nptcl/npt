#include "code_init.h"
#include "code_make.h"
#include "optimize_common.h"
#include "typedef.h"

_g void init_code(void)
{
	init_code_make();
	init_code_init();
	init_optimize_common();
}

_g void build_code(void)
{
	build_code_init();
	build_optimize_common();
}

