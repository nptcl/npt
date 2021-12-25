#ifndef __CALL_NUMBERS_HEADER__
#define __CALL_NUMBERS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define number_equal_common_ _n(number_equal_common_)
#define number_not_equal_common_ _n(number_not_equal_common_)
#define number_less_common_ _n(number_less_common_)
#define number_greater_common_ _n(number_greater_common_)
#define number_less_equal_common_ _n(number_less_equal_common_)
#define number_greater_equal_common_ _n(number_greater_equal_common_)
#define max_common_ _n(max_common_)
#define min_common_ _n(min_common_)
#define plus_common_ _n(plus_common_)
#define minus_common_ _n(minus_common_)
#define asterisk_common_ _n(asterisk_common_)
#define slash_common_ _n(slash_common_)
#define incf_common_ _n(incf_common_)
#define decf_common_ _n(decf_common_)
#define random_common_ _n(random_common_)
#define conjugate_common_ _n(conjugate_common_)
#define realpart_common_ _n(realpart_common_)
#define imagpart_common_ _n(imagpart_common_)
#define parse_integer_common_ _n(parse_integer_common_)

int number_equal_common_(LocalRoot local, addr left, addr rest, int *ret);
int number_not_equal_common_(LocalRoot local, addr left, addr rest, int *ret);
int number_less_common_(LocalRoot local, addr left, addr rest, int *ret);
int number_greater_common_(LocalRoot local, addr left, addr rest, int *ret);
int number_less_equal_common_(LocalRoot local, addr left, addr rest, int *ret);
int number_greater_equal_common_(LocalRoot local, addr left, addr rest, int *ret);
int max_common_(LocalRoot local, addr left, addr rest, addr *ret);
int min_common_(LocalRoot local, addr left, addr rest, addr *ret);
int plus_common_(LocalRoot local, addr rest, addr *ret);
int minus_common_(LocalRoot local, addr left, addr rest, addr *ret);
int asterisk_common_(LocalRoot local, addr rest, addr *ret);
int slash_common_(LocalRoot local, addr left, addr rest, addr *ret);
int incf_common_(Execute ptr, addr form, addr env, addr *ret);
int decf_common_(Execute ptr, addr form, addr env, addr *ret);
int random_common_(Execute ptr, addr limit, addr state, addr *ret);
int conjugate_common_(addr var, addr *ret);
int realpart_common_(addr var, addr *ret);
int imagpart_common_(addr var, addr *ret);
int parse_integer_common_(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2);

#endif

