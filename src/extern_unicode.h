#ifndef __LISP_EXTERN_UNICODE_HEADER__
#define __LISP_EXTERN_UNICODE_HEADER__

#include "extern_typedef.h"
#include "typedef_basic.h"

/* eastasian */
int lisp_eastasian_set(enum LispEastAsianType type, unsigned width);
int lisp_eastasian_get(enum LispEastAsianType type, unsigned *ret);
enum LispEastAsianType lisp_eastasian_type_unicode(unicode c);
enum LispEastAsianType lisp_eastasian_type_character(addr value);

unsigned lisp_eastasian_unicode(unicode c);
int lisp_eastasian_character_(addr value, unsigned *ret);
int lisp_eastasian_string_(addr value, size_t *ret);
int lisp_eastasian_width_(addr value, size_t *ret);

int lisp_unicode_count(void);

int lisp_utf8_encode(unicode c, void *ptr, size_t *ret);
int lisp_utf16_range(unicode c);
int lisp_utf16_high(unicode c);
int lisp_utf16_low(unicode c);
unicode lisp_utf16_merge(byte16 first, byte16 second);

#endif

