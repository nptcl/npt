#ifndef __ARRAY_COMMON_HEADER__
#define __ARRAY_COMMON_HEADER__

#include "array.h"
#include "local.h"
#include "typedef.h"

void array_bitcalc(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call);
void array_bitnot(addr *ret, addr pos, addr opt);
void array_fill(addr pos, addr item, addr start, addr end);
void array_subseq(addr *ret, addr pos, addr start, addr end);
void array_reverse(LocalRoot local, addr *ret, addr pos);
void array_nreverse(addr *ret, addr pos);

#endif

