#ifndef __CHARQUEUE_HEADER__
#define __CHARQUEUE_HEADER__

#include "execute.h"
#include "lisp.h"
#include "local.h"
#include "memory.h"

struct charqueue_struct {
	size_t size, max;
};

#define RefCharBitNext(x)		RefArrayAB((x), 0)
#define GetCharBitNext(x,y)		GetArrayAB((x), 0, (y))
#define SetCharBitNext(x,y)		SetArrayAB((x), 0, (y))
#define PtrCharBitBody(x)		PtrBodyABa((x), 1)
#define PtrCharBitChar(x)		((unicode *)(PtrCharBitBody(x) + IdxSize))
#define RefCharBitSize(x)		(*(size_t *)PtrCharBitBody(x))
#define GetCharBitSize(x,y)		(*(y) = *(size_t *)PtrCharBitBody(x))
#define SetCharBitSize(x,y)		(*(size_t *)PtrCharBitBody(x) = (y))
#define RefCharBitChar(x,i)		(PtrCharBitChar(x)[i])
#define GetCharBitChar(x,i,y)	(*(y) = PtrCharBitChar(x)[i])
#define SetCharBitChar(x,i,y)	(PtrCharBitChar(x)[i] = (y))

#define StructCharQueue(x)		((struct charqueue_struct *)PtrBodySSa((x), 2))
#define RefCharQueueRoot(x)		RefArraySS((x), 0)
#define RefCharQueueTail(x)		RefArraySS((x), 1)
#define GetCharQueueRoot(x,y)	GetArraySS((x), 0, (y))
#define GetCharQueueTail(x,y)	GetArraySS((x), 1, (y))
#define SetCharQueueRoot(x,y)	SetArraySS((x), 0, (y))
#define SetCharQueueTail(x,y)	SetArraySS((x), 1, (y))
#define RefCharQueueSize(x)		(StructCharQueue(x)->size)
#define RefCharQueueMax(x)		(StructCharQueue(x)->max)
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

size_t refsize_charqueue(addr pos);
void getsize_charqueue(addr pos, size_t *ret);
unicode refchar_charqueue(addr pos, size_t index);
void getchar_charqueue(addr pos, size_t index, unicode *ret);

void push_charqueue_heap(addr pos, unicode c);
void push_charqueue_local(LocalRoot local, addr pos, unicode c);
void push_charqueue_alloc(LocalRoot local, addr pos, unicode c);

addr make_charqueue_heapr(addr pos);
addr make_charqueue_localr(LocalRoot local, addr pos);
addr make_charqueue_allocr(LocalRoot local, addr pos);
void make_charqueue_heap(addr pos, addr *ret);
void make_charqueue_local(LocalRoot local, addr pos, addr *ret);
void make_charqueue_alloc(LocalRoot local, addr pos, addr *ret);

void clear_charqueue(addr pos);
void free_charqueue(addr pos);
int position_charqueue(addr pos, size_t size);

void pushstring_charqueue_heap(addr pos, addr push);
void pushstring_charqueue_local(LocalRoot local, addr pos, addr push);
void pushstring_charqueue_alloc(LocalRoot local, addr pos, addr push);
void pushchar_charqueue_heap(addr pos, const char *str);
void pushchar_charqueue_local(LocalRoot local, addr pos, const char *str);
void pushchar_charqueue_alloc(LocalRoot local, addr pos, const char *str);

#endif

