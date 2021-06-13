#include "code.h"
#include "code_init.h"
#include "optimize_common.h"
#include "typedef.h"

void init_code(void)
{
	init_code_init();
	init_optimize_common();
}

void build_code(void)
{
	build_code_init();
	build_optimize_common();
}

