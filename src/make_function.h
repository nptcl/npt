#ifndef __MAKE_FUNCTION_HEADER__
#define __MAKE_FUNCTION_HEADER__

#include "make_typedef.h"
#include "typedef.h"

#define code_allcons_ _n(code_allcons_)
#define code_allcons_set_ _n(code_allcons_set_)
#define code_allcons_rem_ _n(code_allcons_rem_)

#define code_make_function_ _n(code_make_function_)
#define code_make_lambda_ _n(code_make_lambda_)
#define code_make_defun_ _n(code_make_defun_)
#define code_make_macro_lambda_ _n(code_make_macro_lambda_)
#define code_make_defmacro_ _n(code_make_defmacro_)
#define code_make_deftype_ _n(code_make_deftype_)
#define code_make_define_compiler_macro_ _n(code_make_define_compiler_macro_)
#define code_make_destructuring_bind_ _n(code_make_destructuring_bind_)
#define code_make_flet_ _n(code_make_flet_)
#define code_make_labels_ _n(code_make_labels_)

int code_allcons_(CodeMake ptr, addr cons, addr escape);
int code_allcons_set_(CodeMake ptr, addr cons, addr escape);
int code_allcons_rem_(CodeMake ptr, addr cons, addr escape);

int code_make_function_(CodeMake ptr, addr scope);
int code_make_lambda_(CodeMake ptr, addr scope);
int code_make_defun_(CodeMake ptr, addr scope);
int code_make_macro_lambda_(CodeMake ptr, addr scope);
int code_make_defmacro_(CodeMake ptr, addr scope);
int code_make_deftype_(CodeMake ptr, addr scope);
int code_make_define_compiler_macro_(CodeMake ptr, addr scope);
int code_make_destructuring_bind_(CodeMake ptr, addr scope);
int code_make_flet_(CodeMake ptr, addr scope);
int code_make_labels_(CodeMake ptr, addr scope);

#endif

