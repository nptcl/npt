#ifndef __ARRAY_SEQUENCE_HEADER__
#define __ARRAY_SEQUENCE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g void array_bitcalc(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call);
_g void array_bitnot(addr *ret, addr pos, addr opt);
_g void array_fill(addr pos, addr item, addr start, addr end);
_g void array_subseq(addr *ret, addr pos, addr start, addr end);
_g void array_reverse(addr *ret, addr pos);
_g void array_nreverse(addr *ret, addr pos);

#endif

