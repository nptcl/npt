#ifndef __LISP_EXTERN_CONTROL_HEADER__
#define __LISP_EXTERN_CONTROL_HEADER__

#include "typedef_basic.h"

/* control */
void lisp_push_control(addr *ret);
int lisp_free_control_(addr control);
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);
void lisp_nth_values_control(size_t index, addr *ret);
void lisp_values_control(addr *ret);
void lisp_result_control(addr *ret);
void lisp_result2_control(addr *ret1, addr *ret2);
int lisp_eval8_(addr *ret, const void *str);
int lisp_eval16_(addr *ret, const void *str);
int lisp_eval_loop_(void);

/* format */
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);

/* syscall */
typedef int (*lisp_calltype_syscall)(addr args);
void lisp_syscall_rest(int index, lisp_calltype_syscall);
void lisp_syscall_dynamic(int index, lisp_calltype_syscall);
void lisp_syscall_function(int index, addr name, addr *ret);
void lisp_syscall_setvalue(addr pos, addr value);
void lisp_syscall_getvalue(addr *ret);

/* unwind-protect */
int lisp_unwind_protect(addr code, addr clean);
void lisp_set_unwind_protect(addr clean);

/* error */
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);

/* catch / throw */
int lisp_catch_(addr symbol, addr code, addr *ret);
int lisp_throw_(addr symbol);

#endif

