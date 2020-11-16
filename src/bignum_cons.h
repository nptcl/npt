#ifndef __BIGNUM_CONS_HEADER__
#define __BIGNUM_CONS_HEADER__

#include "bignum_data.h"
#include "build.h"
#include "memory.h"
#include "typedef.h"

#define bigcons_local _n(bigcons_local)
#define clear_bigcons _n(clear_bigcons)
#define push_bigcons _n(push_bigcons)
#define setchar_bigcons_ _n(setchar_bigcons_)
#define bigcons_char_local_ _n(bigcons_char_local_)
#define bigcons_char_unsafe _n(bigcons_char_unsafe)
#define bigcons_empty_p _n(bigcons_empty_p)

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
int setchar_bigcons_(LocalRoot local, addr pos, unsigned base, const char *value);
int bigcons_char_local_(LocalRoot local, addr *ret, unsigned base, const char *value);
void bigcons_char_unsafe(LocalRoot local, addr *ret, unsigned base, const char *value);
int bigcons_empty_p(addr pos);

#endif

