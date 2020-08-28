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

/* unwind-protect */
void lisp_unwind_protect(addr clean);

/* catch / throw */
void lisp_catch(addr symbol);
int lisp_throw_(addr symbol);

/* handler */
int lisp_handler_bind_(addr name, addr call);
int lisp_handler_case_(addr name, addr call);
void lisp_handler_reverse(void);

/* restart */
void lisp0_restart_make(addr *ret, addr name, addr call, int casep);
void lisp_restart_make(addr x, addr name, addr call, int casep);
void lisp_restart_interactive(addr restart, addr call);
void lisp_restart_report(addr restart, addr call);
void lisp_restart_test(addr restart, addr call);
void lisp_restart_push(addr restart);
void lisp_restart_reverse(void);

#endif

