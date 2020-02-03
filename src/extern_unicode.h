#ifndef __LISP_EXTERN_UNICODE_HEADER__
#define __LISP_EXTERN_UNICODE_HEADER__

#include "typedef_basic.h"

enum LispEastAsianType {
	LispEastAsianType_error,
	LispEastAsianType_N,
	LispEastAsianType_A,
	LispEastAsianType_H,
	LispEastAsianType_W,
	LispEastAsianType_F,
	LispEastAsianType_NA
};

/* eastasian */
void lisp_eastasian_set(enum LispEastAsianType type, unsigned width);
unsigned lisp_eastasian_get(enum LispEastAsianType type);
enum LispEastAsianType lisp_eastasian_type_unicode(unicode c);
enum LispEastAsianType lisp_eastasian_type_character(addr value);

unsigned lisp_eastasian_unicode(unicode c);
unsigned lisp_eastasian_character(addr value);
int lisp_eastasian_string(addr value, size_t *ret);
int lisp_eastasian_width(addr value, size_t *ret);

/* UTF-8 */
int lisp_utf8_encode(unicode c, void *ptr, size_t *ret);

#endif

