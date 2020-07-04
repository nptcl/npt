#ifndef __BIGNUM_MULTI_HEADER__
#define __BIGNUM_MULTI_HEADER__

#include "local.h"
#include "typedef.h"

_g void multi_ff_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ff_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_ff_real_common(addr left, addr right, addr *ret);
_g void multi_bf_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bf_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bf_real_common(LocalRoot local, addr left, addr right, addr *ret);
#define multi_fb_bignum_local(a,b,c,d) multi_bf_bignum_local((a),(c),(b),(d))
#define multi_fb_real_local(a,b,c,d) multi_bf_real_local((a),(c),(b),(d))
#define multi_fb_real_common(a,b,c,d) multi_bf_real_common((a),(c),(b),(d))
_g void multi_bb_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bb_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bb_real_common(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bb_nosign_bignum_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bb_nosign_real_local(LocalRoot local, addr left, addr right, addr *ret);
_g void multi_bb_nosign_real_common(LocalRoot local, addr left, addr right, addr *ret);

#endif

