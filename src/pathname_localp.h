#ifndef __PATHNAME_LOCALP_HEADER__
#define __PATHNAME_LOCALP_HEADER__

#include "local.h"
#include "typedef.h"

#define push_localp _n(push_localp)
#define rollback_localp _n(rollback_localp)
#define localp_alloc _n(localp_alloc)

struct localp_struct {
	unsigned localp : 1;
	LocalRoot local;
};

typedef struct localp_struct *LocalpRoot;

void push_localp(LocalpRoot local, LocalStack *ret);
void rollback_localp(LocalpRoot local, LocalStack stack);
LocalRoot localp_alloc(LocalpRoot local);

#endif

