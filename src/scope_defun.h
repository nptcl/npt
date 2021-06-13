#ifndef __SCOPE_DEFUN_HEADER__
#define __SCOPE_DEFUN_HEADER__

#include "execute.h"
#include "scope_typedef.h"
#include "typedef.h"

#define scope_defun_call_ _n(scope_defun_call_)
#define scope_deftype_call_ _n(scope_deftype_call_)
#define scope_define_compiler_macro_call_ _n(scope_define_compiler_macro_call_)
#define scope_bind_call_ _n(scope_bind_call_)
#define scope_flet_call_ _n(scope_flet_call_)
#define scope_labels_call_ _n(scope_labels_call_)

int scope_defun_call_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_deftype_call_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_define_compiler_macro_call_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_bind_call_(Execute ptr, addr *ret, addr expr, addr args);
int scope_flet_call_(Execute ptr, struct let_struct *str);
int scope_labels_call_(Execute ptr, struct let_struct *str);

#endif

