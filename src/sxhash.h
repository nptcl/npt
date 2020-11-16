#ifndef __SXHASH_HEADER__
#define __SXHASH_HEADER__

#include "typedef.h"

#define init_sxhash _n(init_sxhash)
#define sxhash_equalp_depth_ _n(sxhash_equalp_depth_)
#define sxhash_equalp_ _n(sxhash_equalp_)
#define sxhash_equal_depth_ _n(sxhash_equal_depth_)
#define sxhash_equal_ _n(sxhash_equal_)
#define sxhash_eq_ _n(sxhash_eq_)
#define sxhash_char_equalp_ _n(sxhash_char_equalp_)
#define sxhash_char_equal_ _n(sxhash_char_equal_)
#define sxhash_unicode_equalp_ _n(sxhash_unicode_equalp_)
#define sxhash_unicode_equal_ _n(sxhash_unicode_equal_)
#define sxhash_character2_equalp_ _n(sxhash_character2_equalp_)
#define sxhash_character2_equal_ _n(sxhash_character2_equal_)

void init_sxhash(void);
int sxhash_equalp_depth_(addr pos, int depth, fixnum *ret);
int sxhash_equalp_(addr pos, fixnum *ret);
int sxhash_equal_depth_(addr pos, int depth, fixnum *ret);
int sxhash_equal_(addr pos, fixnum *ret);
int sxhash_eq_(addr pos, fixnum *ret);

int sxhash_char_equalp_(const char *pos, fixnum *ret);
int sxhash_char_equal_(const char *pos, fixnum *ret);
int sxhash_unicode_equalp_(unicode pos, fixnum *ret);
int sxhash_unicode_equal_(unicode pos, fixnum *ret);
int sxhash_character2_equalp_(unicode a, unicode b, fixnum *ret);
int sxhash_character2_equal_(unicode a, unicode b, fixnum *ret);

#endif

