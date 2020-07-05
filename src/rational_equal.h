#ifndef __RATIONAL_EQUAL_HEADER__
#define __RATIONAL_EQUAL_HEADER__

#include "local.h"
#include "typedef.h"

_g int plusp_rational(addr pos);
_g int minusp_rational(addr pos);
_g int zerop_rational(addr pos);
_g int equal_rational(addr left, addr right);
#define not_equal_rational(a,b) (! equal_rational((a), (b)))
_g int compare_rational(LocalRoot local, addr left, addr right);
#define less_rational(m,a,b) (compare_rational((m),(a), (b)) < 0)
#define less_equal_rational(m,a,b) (compare_rational((m),(a), (b)) <= 0)
#define greater_rational(m,a,b) (compare_rational((m),(a), (b)) > 0)
#define greater_equal_rational(m,a,b) (compare_rational((m),(a), (b)) >= 0)
_g int less_rational_clang(LocalRoot local, addr left, addr right);
_g int less_equal_rational_clang(LocalRoot local, addr left, addr right);

#endif

