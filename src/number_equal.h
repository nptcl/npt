#ifndef __NUMBER_EQUAL_HEADER__
#define __NUMBER_EQUAL_HEADER__

#include "local.h"
#include "real_equal.h"
#include "typedef.h"

#define plusp_number plusp_real
#define minusp_number minusp_real
_g int zerop_number(addr left);
_g int equal_number(LocalRoot local, addr left, addr right);
#define not_equal_number(m,a,b) (! equal_number((m),(a),(b)))
#define compare_number compare_real
#define less_number less_real
#define greater_number greater_real
#define less_equal_number less_equal_real
#define greater_equal_number greater_equal_real

#endif

