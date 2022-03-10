#ifndef __LISP_EXTERN_FUNCTION_HEADER__
#define __LISP_EXTERN_FUNCTION_HEADER__

#include "extern_typedef.h"
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
void lisp_compiled_macro(int index, lisp_calltype_macro call);
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);
void lisp_compiled_any(int index, lisp_calltype_any call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);
void lisp_compiled_var4(int index, lisp_calltype_var4 call);
void lisp_compiled_var5(int index, lisp_calltype_var5 call);
void lisp_compiled_var6(int index, lisp_calltype_var6 call);
void lisp_compiled_opt1(int index, lisp_calltype_opt1 call);
void lisp_compiled_opt2(int index, lisp_calltype_opt2 call);
void lisp_compiled_opt3(int index, lisp_calltype_opt3 call);
void lisp_compiled_var1opt1(int index, lisp_calltype_var1opt1 call);
void lisp_compiled_var1opt2(int index, lisp_calltype_var1opt2 call);
void lisp_compiled_var1opt3(int index, lisp_calltype_var1opt3 call);
void lisp_compiled_var2opt1(int index, lisp_calltype_var2opt1 call);
void lisp_compiled_var2opt2(int index, lisp_calltype_var2opt2 call);
void lisp_compiled_var2opt3(int index, lisp_calltype_var2opt3 call);
void lisp_compiled_var3opt1(int index, lisp_calltype_var3opt1 call);
void lisp_compiled_var3opt2(int index, lisp_calltype_var3opt2 call);
void lisp_compiled_var3opt3(int index, lisp_calltype_var3opt3 call);
void lisp_compiled_var1rest(int index, lisp_calltype_var1rest call);
void lisp_compiled_var2rest(int index, lisp_calltype_var2rest call);
void lisp_compiled_var3rest(int index, lisp_calltype_var3rest call);
void lisp_compiled_var1dynamic(int index, lisp_calltype_var1dynamic call);
void lisp_compiled_var2dynamic(int index, lisp_calltype_var2dynamic call);
void lisp_compiled_var3dynamic(int index, lisp_calltype_var3dynamic call);

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

