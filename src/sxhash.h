#ifndef __SXHASH_HEADER__
#define __SXHASH_HEADER__

#include "typedef.h"

_g void init_sxhash(void);
_g fixnum sxhash_equalp_depth(addr pos, int depth);
_g fixnum sxhash_equalp(addr pos);
_g fixnum sxhash_equal_depth(addr pos, int depth);
_g fixnum sxhash_equal(addr pos);
_g fixnum sxhash_pointer(addr pos);

_g fixnum sxhash_binary_equalp(const void *pos, size_t size);
_g fixnum sxhash_binary_equal(const void *pos, size_t size);
_g fixnum sxhash_char_equalp(const char *pos);
_g fixnum sxhash_char_equal(const char *pos);
_g fixnum sxhash_unicode_equalp(unicode pos);
_g fixnum sxhash_unicode_equal(unicode pos);
_g fixnum sxhash_character2_equalp(unicode a, unicode b);
_g fixnum sxhash_character2_equal(unicode a, unicode b);

#endif

