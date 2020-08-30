#ifndef __ARRAY_SEQUENCE_HEADER__
#define __ARRAY_SEQUENCE_HEADER__

#include "array.h"
#include "define.h"
#include "local.h"
#include "typedef.h"

#define array_bitcalc_ _n(array_bitcalc_)
#define array_bitnot_ _n(array_bitnot_)
#define array_fill_ _n(array_fill_)
#define array_subseq_ _n(array_subseq_)
#define array_reverse_ _n(array_reverse_)
#define array_nreverse_ _n(array_nreverse_)

_g int array_bitcalc_(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call);
_g int array_bitnot_(addr *ret, addr pos, addr opt);
_g int array_fill_(addr pos, addr item, addr start, addr end);
_g int array_subseq_(addr *ret, addr pos, addr start, addr end);
_g int array_reverse_(addr *ret, addr pos);
_g int array_nreverse_(addr *ret, addr pos);

#endif

