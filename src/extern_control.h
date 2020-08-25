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

/* eval */
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);

void lisp0_nth_values_control(addr *ret, size_t index);
void lisp0_values_control(addr *ret);
void lisp0_result_control(addr *ret);
void lisp0_result2_control(addr *ret1, addr *ret2);
void lisp_nth_values_control(addr x, size_t index);
void lisp_values_control(addr x);
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);

int lisp0_eval8_(addr *ret, const void *str);
int lisp0_eval16_(addr *ret, const void *str);
int lisp0_eval32_(addr *ret, const void *str);
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);

void lisp_eval_clean(void);
int lisp_eval_loop_(void);

/* format */
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);

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
void lisp_abort_execute(void);
void lisp_abort(const char *fmt, ...);
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);

/* catch / throw */
int lisp_catch_(addr symbol, addr code, addr *ret);
int lisp_throw_(addr symbol);

#endif

