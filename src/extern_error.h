#ifndef __LISP_EXTERN_ERROR_HEADER__
#define __LISP_EXTERN_ERROR_HEADER__

#include "typedef.h"

/* abort */
void lisp_abort(void);
void lisp_abortf(const char *fmt, ...);
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);
lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call);
lisp_abort_calltype lisp_set_abort_setjmp_handler(void);

/* signal */
int lisp_signal_(addr condition);
int lisp_error_(addr condition);

/* error */
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);

/* warn */
int lisp_warn8_(const void *str, ...);
int lisp_warn16_(const void *str, ...);
int lisp_warn32_(const void *str, ...);

#endif

