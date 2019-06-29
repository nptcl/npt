#include "thread.h"
#include "execute.h"

_g int create_thread(execfunction proc, Execute arg)
{
	fprintf(stderr, "create_thread error\n");
	return 1;
}

_g int join_thread(threadhandle *handle)
{
	return 0;
}

