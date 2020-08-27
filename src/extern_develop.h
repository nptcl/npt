#ifndef __EXTERN_DEVELOP_HEADER__
#define __EXTERN_DEVELOP_HEADER__

#include <stdarg.h>
#include "constant.h"
#include "local.h"
#include "typedef_basic.h"

/* error */
void lisp_abort_type(addr value, constindex index);
#define Lisp_abort_type(x,y) lisp_abort_type((x), CONSTANT_COMMON_##y)

/* list */
_g void lisp0_list_va_alloc(LocalRoot local, addr *ret, va_list args);
_g void lisp0_lista_va_alloc(LocalRoot local, addr *ret, va_list args);

#endif

