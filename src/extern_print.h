#ifndef __LISP_EXTERN_PRINT_HEADER__
#define __LISP_EXTERN_PRINT_HEADER__

#include <stddef.h>
#include "typedef_basic.h"
#include "typedef_stream.h"

/* format */
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);

/* stdout */
int lisp_stdout8_(const void *str, ...);
int lisp_stdout16_(const void *str, ...);
int lisp_stdout32_(const void *str, ...);

/* stderr */
int lisp_stderr8_(const void *str, ...);
int lisp_stderr16_(const void *str, ...);
int lisp_stderr32_(const void *str, ...);

/* stringf */
int lisp0_stringf8_(addr *ret, const void *str, ...);
int lisp0_stringf16_(addr *ret, const void *str, ...);
int lisp0_stringf32_(addr *ret, const void *str, ...);
int lisp_stringf8_(addr x, const void *str, ...);
int lisp_stringf16_(addr x, const void *str, ...);
int lisp_stringf32_(addr x, const void *str, ...);

#endif

