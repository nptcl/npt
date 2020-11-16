#ifndef __CHARACTER_QUEUE_HEADER__
#define __CHARACTER_QUEUE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "memory.h"

#define charqueue_heapr _n(charqueue_heapr)
#define charqueue_localr _n(charqueue_localr)
#define charqueue_allocr _n(charqueue_allocr)
#define charqueue_heap _n(charqueue_heap)
#define charqueue_local _n(charqueue_local)
#define charqueue_alloc _n(charqueue_alloc)
#define getsize_charqueue _n(getsize_charqueue)
#define getchar_charqueue _n(getchar_charqueue)
#define push_charqueue_alloc_ _n(push_charqueue_alloc_)
#define push_charqueue_local_ _n(push_charqueue_local_)
#define push_charqueue_heap_ _n(push_charqueue_heap_)
#define make_charqueue_alloc _n(make_charqueue_alloc)
#define make_charqueue_local _n(make_charqueue_local)
#define make_charqueue_heap _n(make_charqueue_heap)
#define clear_charqueue _n(clear_charqueue)
#define free_charqueue _n(free_charqueue)
#define position_charqueue _n(position_charqueue)
#define pushstring_charqueue_alloc_ _n(pushstring_charqueue_alloc_)
#define pushstring_charqueue_local_ _n(pushstring_charqueue_local_)
#define pushstring_charqueue_heap_ _n(pushstring_charqueue_heap_)
#define pushchar_charqueue_alloc_ _n(pushchar_charqueue_alloc_)
#define pushchar_charqueue_local_ _n(pushchar_charqueue_local_)
#define pushchar_charqueue_heap_ _n(pushchar_charqueue_heap_)

struct charqueue_struct {
	size_t size, max;
};

#define GetCharBitNext(x,y)		GetArrayAB((x), 0, (y))
#define SetCharBitNext(x,y)		SetArrayAB((x), 0, (y))
#define PtrCharBitBody(x)		PtrBodyABa((x), 1)
#define PtrCharBitChar(x)		((unicode *)(PtrCharBitBody(x) + IdxSize))
#define GetCharBitSize(x,y)		(*(y) = *(size_t *)PtrCharBitBody(x))
#define SetCharBitSize(x,y)		(*(size_t *)PtrCharBitBody(x) = (y))
#define GetCharBitChar(x,i,y)	(*(y) = PtrCharBitChar(x)[i])
#define SetCharBitChar(x,i,y)	(PtrCharBitChar(x)[i] = (y))

#define StructCharQueue(x)		((struct charqueue_struct *)PtrBodySSa((x), 2))
#define GetCharQueueRoot(x,y)	GetArraySS((x), 0, (y))
#define GetCharQueueTail(x,y)	GetArraySS((x), 1, (y))
#define SetCharQueueRoot(x,y)	SetArraySS((x), 0, (y))
#define SetCharQueueTail(x,y)	SetArraySS((x), 1, (y))
#define GetCharQueueSize(x,y)	(*(y) = StructCharQueue(x)->size)
#define GetCharQueueMax(x,y)	(*(y) = StructCharQueue(x)->max)
#define SetCharQueueSize(x,y)	(StructCharQueue(x)->size = (y))
#define SetCharQueueMax(x,y)	(StructCharQueue(x)->max = (y))
#define IncCharQueueSize(x)		(StructCharQueue(x)->size++)

addr charqueue_heapr(size_t size);
addr charqueue_localr(LocalRoot local, size_t size);
addr charqueue_allocr(LocalRoot local, size_t size);
void charqueue_heap(addr *ret, size_t size);
void charqueue_local(LocalRoot local, addr *ret, size_t size);
void charqueue_alloc(LocalRoot local, addr *ret, size_t size);

void getsize_charqueue(addr pos, size_t *ret);
void getchar_charqueue(addr pos, size_t index, unicode *ret);

int push_charqueue_alloc_(LocalRoot local, addr pos, unicode c);
int push_charqueue_local_(LocalRoot local, addr pos, unicode c);
int push_charqueue_heap_(addr pos, unicode c);

void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret);
void make_charqueue_local(LocalRoot local, addr pos, addr *ret);
void make_charqueue_heap(addr pos, addr *ret);

void clear_charqueue(addr pos);
void free_charqueue(addr pos);
int position_charqueue(addr pos, size_t size);

int pushstring_charqueue_alloc_(LocalRoot local, addr pos, addr push);
int pushstring_charqueue_local_(LocalRoot local, addr pos, addr push);
int pushstring_charqueue_heap_(addr pos, addr push);
int pushchar_charqueue_alloc_(LocalRoot local, addr pos, const char *str);
int pushchar_charqueue_local_(LocalRoot local, addr pos, const char *str);
int pushchar_charqueue_heap_(addr pos, const char *str);

#endif

