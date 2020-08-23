#ifndef __NUMBER_EQUAL_HEADER__
#define __NUMBER_EQUAL_HEADER__

#include "local.h"
#include "real_equal.h"
#include "typedef.h"

#define plusp_number_ plusp_real_
#define minusp_number_ minusp_real_
_g int zerop_numberp(addr left, int *ret);
_g int zerop_number_(addr left, int *ret);
_g int equal_number_(LocalRoot local, addr left, addr right, int *ret);
_g int not_equal_number_(LocalRoot local, addr left, addr right, int *ret);

#define compare_number_ compare_real_
#define less_number_ less_real_
#define greater_number_ greater_real_
#define less_equal_number_ less_equal_real_
#define greater_equal_number_ greater_equal_real_

#endif

