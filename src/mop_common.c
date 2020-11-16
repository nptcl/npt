#include "execute.h"
#include "mop_common.h"
#include "typedef.h"

void init_metaobject_protocol(void)
{
	init_mop_class();
	init_mop_reader();
	init_mop_generic();
	init_mop_protocols();
}

void build_metaobject_protocol(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	Error(build_mop_class_(ptr));
	Error(build_mop_reader_(ptr));
	Error(build_mop_generic_(ptr));
	Error(build_mop_protocols_(ptr));
}

