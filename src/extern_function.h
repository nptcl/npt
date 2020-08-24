#ifndef __LISP_EXTERN_FUNCTION_HEADER__
#define __LISP_EXTERN_FUNCTION_HEADER__

#include "typedef_basic.h"

/* function */
int lisp0_function_(addr *ret, addr value);
int lisp0_function8_(addr *ret, const void *str);
int lisp0_function16_(addr *ret, const void *str);
int lisp0_function32_(addr *ret, const void *str);
int lisp_function_(addr x, addr value);
int lisp_function8_(addr x, const void *str);
int lisp_function16_(addr x, const void *str);
int lisp_function32_(const addr x, void *str);

/* funcall */
int lisp0_funcall_(addr *ret, addr call, ...);
int lisp0_funcall8_(addr *ret, const void *str, ...);
int lisp0_funcall16_(addr *ret, const void *str, ...);
int lisp0_funcall32_(addr *ret, const void *str, ...);
int lisp_funcall_(addr x, addr call, ...);
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);

/* apply */
int lisp0_apply_(addr *ret, addr call, ...);
int lisp0_apply8_(addr *ret, const void *str, ...);
int lisp0_apply16_(addr *ret, const void *str, ...);
int lisp0_apply32_(addr *ret, const void *str, ...);
int lisp_apply_(addr x, addr call, ...);
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);

#endif

