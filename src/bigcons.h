#ifndef __BIGCONS_HEADER__
#define __BIGCONS_HEADER__

#include "bigdata.h"
#include "memory.h"
#include "lisp.h"
#include "typedef.h"

#define PtrBigbuffer(x) PtrBodyABa(x, 1)
#define StructBigbuffer(x) ((struct bigbuffer *)PtrBigbuffer(x))
#define SetNextBigbuffer(x,v) SetArrayAB(x,0,v)
#define GetNextBigbuffer(x,v) GetArrayAB(x,0,v)

#define PtrBigcons(x)		PtrBodySSa(x, 1)
#define StructBigcons(x)	((struct bigcons_struct *)PtrBigcons(x))
#define SetCountBigcons(x,v) (StructBigcons(x)->size = (v))
#define GetCountBigcons(x,v) (*(v) = StructBigcons(x)->size)
#define IncCountBigcons(x,v) (StructBigcons(x)->size += (v))
#define SetRootBigcons(x,v) SetArraySS(x,0,v)
#define GetRootBigcons(x,v) GetArraySS(x,0,v)

struct bigbuffer {
	size_t count;
	bigtype buffer[BIGCONS_SIZE];
};

struct bigcons_struct {
	size_t size;
};

void bigcons_local(LocalRoot local, addr *ret);
void clear_bigcons(addr pos);
void push_bigcons(LocalRoot local, addr pos, unsigned base, unsigned number);
void setchar_bigcons(LocalRoot local, addr pos, unsigned base, const char *value);
void bigcons_char_local(LocalRoot local, addr *ret, unsigned base, const char *value);
int bigcons_empty_p(addr pos);

#endif

