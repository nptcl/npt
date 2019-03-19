#ifndef __COPY_HEADER__
#define __COPY_HEADER__

#include "typedef.h"
#include "local.h"

void copyhard_cons(LocalRoot local, addr *ret, addr pos);
void copyhard_vectorA2(LocalRoot local, addr *ret, addr left);
void copyhard_vectorA4(LocalRoot local, addr *ret, addr left);
#ifdef LISP_ARCH_64BIT
void copyhard_vectorA8(LocalRoot local, addr *ret, addr left);
#endif
void copyhard_vector(LocalRoot local, addr *ret, addr pos);
void copyhard_character(LocalRoot local, addr *ret, addr pos);
void copyhard_string(LocalRoot local, addr *ret, addr pos);
void copyhard_fixnum(LocalRoot local, addr *ret, addr pos);
void copyhard_bignum(LocalRoot local, addr *ret, addr pos);
void copyhard_ratio(LocalRoot local, addr *ret, addr pos);
void copyhard_float(LocalRoot local, addr *ret, addr pos);
void copyhard_double(LocalRoot local, addr *ret, addr pos);
void copyhard_long_double(LocalRoot local, addr *ret, addr pos);
void copyhard_complex(LocalRoot local, addr *ret, addr pos);
void copyhard_callname(LocalRoot local, addr *ret, addr pos);
void copyhard_random_state(LocalRoot local, addr *ret, addr pos);
void copyhard_pathname(LocalRoot local, addr *ret, addr pos);
void copyhard_object(LocalRoot local, addr *ret, addr pos);

int copylocalp(LocalRoot local, addr pos);
int copylocal_check(LocalRoot local, addr pos);
int copylocal_object(LocalRoot local, addr *ret, addr pos);
void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args);
int copyheap(addr *ret, addr pos);

void init_copy(void);

#endif

