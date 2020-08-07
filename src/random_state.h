#ifndef __RANDOM_STATE_HEADER__
#define __RANDOM_STATE_HEADER__

#include "execute.h"
#include "random.h"

_g struct random_state *struct_random_state(addr pos);

_g int init_random_state(void);
_g void free_random_state(void);

_g void random_state_alloc(LocalRoot local, addr *ret);
_g void random_state_local(LocalRoot local, addr *ret);
_g void random_state_heap(addr *ret);

_g void copy_random_state(addr left, addr right);
_g void randomly_random_state(addr left);
_g int constant_random_state_(Execute ptr, addr left);
_g int make_random_state_heap_(Execute ptr, addr *ret, addr state);

_g void make_bignum_random_state_alloc(LocalRoot local, addr pos, addr *ret);
_g void make_bignum_random_state_local(LocalRoot local, addr pos, addr *ret);
_g void make_bignum_random_state_heap(addr pos, addr *ret);

_g int equal_random_state_addr(addr left, addr right);

#endif

