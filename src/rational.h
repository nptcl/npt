#ifndef __RATIO_HEADER__
#define __RATIO_HEADER__

#include "local.h"
#include "number.h"
#include "typedef.h"

int ratiop(addr pos);
int rationalp(addr pos);

int rational_result_alloc(LocalRoot local, addr pos, addr *ret);
void rational_throw_alloc(LocalRoot local, addr pos, addr *ret);
void rational_throw_local(LocalRoot local, addr pos, addr *ret);
void rational_throw_heap(addr pos, addr *ret);

int plusp_rational(addr pos);
int minusp_rational(addr pos);
int zerop_rational(addr pos);
int equal_rational(addr left, addr right);
#define not_equal_rational(a,b) (! equal_rational((a), (b)))
int compare_rational(LocalRoot local, addr left, addr right);
#define less_rational(m,a,b) (compare_rational((m),(a), (b)) < 0)
#define less_equal_rational(m,a,b) (compare_rational((m),(a), (b)) <= 0)
#define greater_rational(m,a,b) (compare_rational((m),(a), (b)) > 0)
#define greater_equal_rational(m,a,b) (compare_rational((m),(a), (b)) >= 0)

int less_rational_clang(LocalRoot local, addr left, addr right);
int less_equal_rational_clang(LocalRoot local, addr left, addr right);

void plus_rational(LocalRoot local, addr left, addr right, addr *ret);
void minus_rational(LocalRoot local, addr left, addr right, addr *ret);
void multi_rational(LocalRoot local, addr left, addr right, addr *ret);
void division_rational(LocalRoot local, addr left, addr right, addr *ret);

#endif

