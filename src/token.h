#ifndef __TOKEN_HEADER__
#define __TOKEN_HEADER__

#include "lisp.h"

int getchar_digit(unsigned v, int upperp, unicode *ret);
int getvalue_digit(unsigned base, unicode c, unsigned *ret);
unicode checkchar_digit(unsigned v, int upperp);
unsigned checkvalue_digit(unsigned base, unicode c);

void maketoken_integer(LocalRoot local, addr queue, unsigned base, addr *ret);
void maketoken_float(Execute ptr, addr queue, addr *ret);
void maketoken_ratio(LocalRoot local, addr queue, unsigned base, addr *ret);

#endif

