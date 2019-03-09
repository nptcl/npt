#ifndef __RANDOM_SEED_HEADER__
#define __RANDOM_SEED_HEADER__

#include "execute.h"
#include "random.h"

struct random_state *struct_random_state(addr pos);

int init_random_state(void);
void free_random_state(void);

void random_state_alloc(LocalRoot local, addr *ret);
void random_state_local(LocalRoot local, addr *ret);
void random_state_heap(addr *ret);

void copy_random_state(addr left, addr right);
void randomly_random_state(addr left);
void constant_random_state(Execute ptr, addr left);
void make_random_state_heap(Execute ptr, addr *ret, addr state);

void make_bignum_random_state_alloc(LocalRoot local, addr pos, addr *ret);
void make_bignum_random_state_local(LocalRoot local, addr pos, addr *ret);
void make_bignum_random_state_heap(addr pos, addr *ret);

int equal_random_state_addr(addr left, addr right);

#endif

