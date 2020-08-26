#ifndef __LISP_EXTERN_CONTROL_HEADER__
#define __LISP_EXTERN_CONTROL_HEADER__

#include "typedef_basic.h"

/* control */
void lisp_push_control(addr *ret);
int lisp_pop_control_(addr control);

/* special */
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);
int lisp0_get_special_(addr *ret, addr symbol);
int lisp0_get_special8_(addr *ret, const void *name);
int lisp0_get_special16_(addr *ret, const void *name);
int lisp0_get_special32_(addr *ret, const void *name);
int lisp_get_special_(addr x, addr symbol);
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);
int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);

/* format */
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);

/* unwind-protect */
int lisp_unwind_protect(addr code, addr clean);
void lisp_set_unwind_protect(addr clean);

/* error */
void lisp_abort_execute(void);
void lisp_abort(const char *fmt, ...);
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);

/* catch / throw */
int lisp_catch_(addr symbol, addr code, addr *ret);
int lisp_throw_(addr symbol);

#endif

