#ifndef __CHARQUEUE_HEADER__
#define __CHARQUEUE_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "memory.h"

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

_g addr charqueue_heapr(size_t size);
_g addr charqueue_localr(LocalRoot local, size_t size);
_g addr charqueue_allocr(LocalRoot local, size_t size);
_g void charqueue_heap(addr *ret, size_t size);
_g void charqueue_local(LocalRoot local, addr *ret, size_t size);
_g void charqueue_alloc(LocalRoot local, addr *ret, size_t size);

_g void getsize_charqueue(addr pos, size_t *ret);
_g void getchar_charqueue(addr pos, size_t index, unicode *ret);

_g void push_charqueue_heap(addr pos, unicode c);
_g void push_charqueue_local(LocalRoot local, addr pos, unicode c);
_g void push_charqueue_alloc(LocalRoot local, addr pos, unicode c);

_g addr make_charqueue_heapr(addr pos);
_g addr make_charqueue_localr(LocalRoot local, addr pos);
_g addr make_charqueue_allocr(LocalRoot local, addr pos);
_g void make_charqueue_heap(addr pos, addr *ret);
_g void make_charqueue_local(LocalRoot local, addr pos, addr *ret);
_g void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret);

_g void clear_charqueue(addr pos);
_g void free_charqueue(addr pos);
_g int position_charqueue(addr pos, size_t size);

_g void pushstring_charqueue_heap(addr pos, addr push);
_g void pushstring_charqueue_local(LocalRoot local, addr pos, addr push);
_g void pushstring_charqueue_alloc(LocalRoot local, addr pos, addr push);
_g void pushchar_charqueue_heap(addr pos, const char *str);
_g void pushchar_charqueue_local(LocalRoot local, addr pos, const char *str);
_g void pushchar_charqueue_alloc(LocalRoot local, addr pos, const char *str);

#endif

