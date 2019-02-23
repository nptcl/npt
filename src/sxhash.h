#ifndef __SXHASH_HEADER__
#define __SXHASH_HEADER__

#include "typedef.h"

void init_sxhash(void);
fixnum sxhash_equalp_depth(addr pos, int depth);
fixnum sxhash_equalp(addr pos);
fixnum sxhash_equal_depth(addr pos, int depth);
fixnum sxhash_equal(addr pos);
fixnum sxhash_pointer(addr pos);

fixnum sxhash_binary_equalp(const void *pos, size_t size);
fixnum sxhash_binary_equal(const void *pos, size_t size);
fixnum sxhash_char_equalp(const char *pos);
fixnum sxhash_char_equal(const char *pos);
fixnum sxhash_unicode_equalp(unicode pos);
fixnum sxhash_unicode_equal(unicode pos);
fixnum sxhash_character2_equalp(unicode a, unicode b);
fixnum sxhash_character2_equal(unicode a, unicode b);

#endif

