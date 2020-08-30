#ifndef __RANDOM_STATE_HEADER__
#define __RANDOM_STATE_HEADER__

#include "execute.h"
#include "random.h"

#define struct_random_state _n(struct_random_state)
#define init_random_state _n(init_random_state)
#define free_random_state _n(free_random_state)
#define random_state_alloc _n(random_state_alloc)
#define random_state_local _n(random_state_local)
#define random_state_heap _n(random_state_heap)
#define copy_random_state _n(copy_random_state)
#define randomly_random_state _n(randomly_random_state)
#define constant_random_state_ _n(constant_random_state_)
#define make_random_state_heap_ _n(make_random_state_heap_)
#define make_bignum_random_state_alloc _n(make_bignum_random_state_alloc)
#define make_bignum_random_state_local _n(make_bignum_random_state_local)
#define make_bignum_random_state_heap _n(make_bignum_random_state_heap)
#define equal_random_state_addr _n(equal_random_state_addr)

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

