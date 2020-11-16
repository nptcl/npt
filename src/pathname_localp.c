#include "pathname_localp.h"
#include "typedef.h"

void push_localp(LocalpRoot local, LocalStack *ret)
{
	if (local->localp)
		*ret = NULL;
	else
		push_local(local->local, ret);
}

void rollback_localp(LocalpRoot local, LocalStack stack)
{
	if (! local->localp)
		rollback_local(local->local, stack);
}

LocalRoot localp_alloc(LocalpRoot local)
{
	return local->localp? local->local: NULL;
}

