#ifndef __ARRAY_SEQUENCE_HEADER__
#define __ARRAY_SEQUENCE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

_g int array_bitcalc_(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call);
_g int array_bitnot_(addr *ret, addr pos, addr opt);
_g int array_fill_(addr pos, addr item, addr start, addr end);
_g int array_subseq_(addr *ret, addr pos, addr start, addr end);
_g int array_reverse_(addr *ret, addr pos);
_g int array_nreverse_(addr *ret, addr pos);

#endif

