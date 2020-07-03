#include "pathname_localp.h"
#include "typedef.h"

_g void push_localp(LocalpRoot local, LocalStack *ret)
{
	if (local->localp)
		*ret = NULL;
	else
		push_local(local->local, ret);
}

_g void rollback_localp(LocalpRoot local, LocalStack stack)
{
	if (! local->localp)
		rollback_local(local->local, stack);
}

_g LocalRoot localp_alloc(LocalpRoot local)
{
	return local->localp? local->local: NULL;
}

