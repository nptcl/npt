#ifndef __LISP_EXTERN_FUNCTION_HEADER__
#define __LISP_EXTERN_FUNCTION_HEADER__

#include "typedef_basic.h"

/* function */
void lisp0_get_function(addr *ret, addr value);
void lisp0_get_setf(addr *ret, addr value);
void lisp_get_function(addr x, addr value);
void lisp_get_setf(addr x, addr value);

int lisp0_get_function_(addr *ret, addr value);
int lisp0_get_function8_(addr *ret, const void *str);
int lisp0_get_function16_(addr *ret, const void *str);
int lisp0_get_function32_(addr *ret, const void *str);
int lisp_get_function_(addr x, addr value);
int lisp_get_function8_(addr x, const void *str);
int lisp_get_function16_(addr x, const void *str);
int lisp_get_function32_(addr x, const void *str);

int lisp0_get_setf_(addr *ret, addr value);
int lisp0_get_setf8_(addr *ret, const void *str);
int lisp0_get_setf16_(addr *ret, const void *str);
int lisp0_get_setf32_(addr *ret, const void *str);
int lisp_get_setf_(addr x, addr value);
int lisp_get_setf8_(addr x, const void *str);
int lisp_get_setf16_(addr x, const void *str);
int lisp_get_setf32_(addr x, const void *str);

/* eval */
int lisp0_eval_(addr *ret, addr value);
int lisp0_eval8_(addr *ret, const void *str);
int lisp0_eval16_(addr *ret, const void *str);
int lisp0_eval32_(addr *ret, const void *str);
int lisp_eval_(addr x, addr value);
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);

/* call */
int lisp0_call_(addr *ret, addr call, addr args);
int lisp_call_(addr x, addr call, addr args);

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

/* lowlevel */
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);
int lisp_call_control_(addr call, addr args);
int lisp_funcall_control_(addr call, ...);
int lisp_apply_control_(addr call, ...);
void lisp_clean_control(void);
int lisp_eval_loop_(void);

/* values */
void lisp0_nth_values_control(addr *ret, size_t index);
void lisp0_values_control(addr *ret);
void lisp0_result_control(addr *ret);
void lisp0_result2_control(addr *ret1, addr *ret2);
void lisp_nth_values_control(addr x, size_t index);
void lisp_values_control(addr x);
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);
void lisp_set_result_control(addr value);
void lisp_set_values_control(addr first, ...);
void lisp_set_values_nil_control(void);
void lisp_set_values_list_control(addr list);

#endif

