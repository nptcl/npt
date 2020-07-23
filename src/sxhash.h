#ifndef __SXHASH_HEADER__
#define __SXHASH_HEADER__

#include "typedef.h"

_g void init_sxhash(void);
_g int sxhash_equalp_depth_(addr pos, int depth, fixnum *ret);
_g int sxhash_equalp_(addr pos, fixnum *ret);
_g int sxhash_equal_depth_(addr pos, int depth, fixnum *ret);
_g int sxhash_equal_(addr pos, fixnum *ret);
_g int sxhash_eq_(addr pos, fixnum *ret);

_g int sxhash_char_equalp_(const char *pos, fixnum *ret);
_g int sxhash_char_equal_(const char *pos, fixnum *ret);
_g int sxhash_unicode_equalp_(unicode pos, fixnum *ret);
_g int sxhash_unicode_equal_(unicode pos, fixnum *ret);
_g int sxhash_character2_equalp_(unicode a, unicode b, fixnum *ret);
_g int sxhash_character2_equal_(unicode a, unicode b, fixnum *ret);

#endif

