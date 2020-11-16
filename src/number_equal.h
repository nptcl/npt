#ifndef __NUMBER_EQUAL_HEADER__
#define __NUMBER_EQUAL_HEADER__

#include "local.h"
#include "real_equal.h"
#include "typedef.h"

#define zerop_numberp _n(zerop_numberp)
#define zerop_number_ _n(zerop_number_)
#define equal_number_ _n(equal_number_)
#define not_equal_number_ _n(not_equal_number_)

#define plusp_number_ plusp_real_
#define minusp_number_ minusp_real_
int zerop_numberp(addr left, int *ret);
int zerop_number_(addr left, int *ret);
int equal_number_(LocalRoot local, addr left, addr right, int *ret);
int not_equal_number_(LocalRoot local, addr left, addr right, int *ret);

#define compare_number_ compare_real_
#define less_number_ less_real_
#define greater_number_ greater_real_
#define less_equal_number_ less_equal_real_
#define greater_equal_number_ greater_equal_real_

#endif

