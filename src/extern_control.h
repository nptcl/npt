#ifndef __LISP_EXTERN_CONTROL_HEADER__
#define __LISP_EXTERN_CONTROL_HEADER__

#include "typedef_basic.h"

/* control */
addr lisp_push_control(void);
int lisp_free_control(addr control);
int lisp_eval_object_control(addr eval);
int lisp_eval_string_control(addr eval);
addr lisp_nth_control(size_t index);
addr lisp_values_control(void);
addr lisp_result_control(void);
void lisp_result2_control(addr *ret1, addr *ret2);
int lisp_eval8(addr *ret, const void *str);
int lisp_eval16(addr *ret, const void *str);

/* format */
int lisp_format8(addr stream, const void *str, ...);
int lisp_format16(addr stream, const void *str, ...);

/* syscall */
typedef void (*lisp_calltype_syscall)(addr args);
void lisp_syscall_rest(int index, lisp_calltype_syscall);
void lisp_syscall_dynamic(int index, lisp_calltype_syscall);
addr lisp_syscall_function(int index, addr name);
void lisp_syscall_setvalue(addr pos, addr value);
addr lisp_syscall_getvalue(void);

/* unwind-protect */
int lisp_unwind_protect(addr code, addr clean);

#endif

