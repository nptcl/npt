#ifndef __EXTERN_SEQUENCE_HEADER__
#define __EXTERN_SEQUENCE_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef_basic.h"

_g void lisp0_list_va_alloc(LocalRoot local, addr *ret, va_list args);
_g void lisp0_lista_va_alloc(LocalRoot local, addr *ret, va_list args);

#endif

