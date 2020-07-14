#ifndef __LISP_EXTERN_ERROR_HEADER__
#define __LISP_EXTERN_ERROR_HEADER__

#include "constant.h"
#include "typedef.h"

void lisp_abort(const char *fmt, ...);
void lisp_abort_type(addr value, constindex index);
#define Lisp_abort_type(x,y) lisp_abort_type((x), CONSTANT_COMMON_##y)

#endif

