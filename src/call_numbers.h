#ifndef __CALL_NUMBERS_HEADER__
#define __CALL_NUMBERS_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define number_equal_common _n(number_equal_common)
#define number_not_equal_common _n(number_not_equal_common)
#define number_less_common _n(number_less_common)
#define number_greater_common _n(number_greater_common)
#define number_less_equal_common _n(number_less_equal_common)
#define number_greater_equal_common _n(number_greater_equal_common)
#define max_common _n(max_common)
#define min_common _n(min_common)
#define plus_common _n(plus_common)
#define minus_common _n(minus_common)
#define asterisk_common _n(asterisk_common)
#define slash_common _n(slash_common)
#define incf_common _n(incf_common)
#define decf_common _n(decf_common)
#define random_common _n(random_common)
#define conjugate_common _n(conjugate_common)
#define realpart_common_ _n(realpart_common_)
#define imagpart_common_ _n(imagpart_common_)
#define parse_integer_common _n(parse_integer_common)

int number_equal_common(LocalRoot local, addr left, addr rest, int *ret);
int number_not_equal_common(LocalRoot local, addr left, addr rest, int *ret);
int number_less_common(LocalRoot local, addr left, addr rest, int *ret);
int number_greater_common(LocalRoot local, addr left, addr rest, int *ret);
int number_less_equal_common(LocalRoot local, addr left, addr rest, int *ret);
int number_greater_equal_common(LocalRoot local, addr left, addr rest, int *ret);
int max_common(LocalRoot local, addr left, addr rest, addr *ret);
int min_common(LocalRoot local, addr left, addr rest, addr *ret);
int plus_common(LocalRoot local, addr rest, addr *ret);
int minus_common(LocalRoot local, addr left, addr rest, addr *ret);
int asterisk_common(LocalRoot local, addr rest, addr *ret);
int slash_common(LocalRoot local, addr left, addr rest, addr *ret);
int incf_common(Execute ptr, addr form, addr env, addr *ret);
int decf_common(Execute ptr, addr form, addr env, addr *ret);
int random_common(Execute ptr, addr limit, addr state, addr *ret);
int conjugate_common(addr var, addr *ret);
int realpart_common_(addr var, addr *ret);
int imagpart_common_(addr var, addr *ret);
int parse_integer_common(LocalRoot local,
		addr var, addr rest, addr *ret1, addr *ret2);

#endif

