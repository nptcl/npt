#ifndef __LISP_EXTERN_FUNCTION_HEADER__
#define __LISP_EXTERN_FUNCTION_HEADER__

#include "typedef_basic.h"

/* function */
void lisp0_get_function(addr *ret, addr symbol);
void lisp0_get_setf(addr *ret, addr symbol);
void lisp_get_function(addr x, addr symbol);
void lisp_get_setf(addr x, addr symbol);

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

/* compiled */
typedef int (*lisp_calltype_dynamic)(addr args);
typedef int (*lisp_calltype_rest)(addr args);
typedef int (*lisp_calltype_empty)(void);
typedef int (*lisp_calltype_var1)(addr var);
typedef int (*lisp_calltype_var2)(addr var1, addr var2);
typedef int (*lisp_calltype_var3)(addr var1, addr var2, addr var3);

void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);

int lisp0_compiled_function_(addr *ret, int index, addr symbol);
int lisp0_compiled_function8_(addr *ret, int index, const void *str);
int lisp0_compiled_function16_(addr *ret, int index, const void *str);
int lisp0_compiled_function32_(addr *ret, int index, const void *str);
int lisp_compiled_function_(addr x, int index, addr symbol);
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);
int lisp_compiled_defun_(int index, addr symbol);
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);
int lisp_compiled_defun_setf_(int index, addr symbol);
int lisp_compiled_defun_setf8_(int index, const void *str);
int lisp_compiled_defun_setf16_(int index, const void *str);
int lisp_compiled_defun_setf32_(int index, const void *str);

void lisp_compiled_setvalue(addr pos, addr value);
void lisp_compiled_getvalue(addr *ret);

#endif

