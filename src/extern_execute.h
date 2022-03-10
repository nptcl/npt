#ifndef __LISP_EXTERN_EXECUTE_HEADER__
#define __LISP_EXTERN_EXECUTE_HEADER__

#include "extern_typedef.h"
#include "typedef_basic.h"

/* eval */
int lisp0_eval_(addr *ret, addr pos);
int lisp0_eval8_(addr *ret, const void *str);
int lisp0_eval16_(addr *ret, const void *str);
int lisp0_eval32_(addr *ret, const void *str);
int lisp_eval_(addr x, addr pos);
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

/* values */
void lisp0_result_control(addr *ret);
void lisp0_result2_control(addr *ret1, addr *ret2);
void lisp0_values_control(addr *ret);
void lisp0_nth_value_control(addr *ret, size_t index);
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);
void lisp_values_control(addr x);
void lisp_nth_value_control(addr x, size_t index);
void lisp_set_result_control(addr value);
void lisp_set_values_control(addr first, ...);
void lisp_set_values_nil_control(void);
void lisp_set_values_list_control(addr list);

/* escape */
int lisp_equal_control(addr control);
int lisp_break_control(void);
int lisp_escape_control(void);
void lisp_reset_control(void);
enum lisp_escape lisp_escape_type_control(void);
void lisp_save_control(addr *ret);
void lisp_rollback_control(addr value);

/* system */
int lisp_eval_loop_(void);

#endif

