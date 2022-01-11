#ifndef __OPTIMIZE_DEFINE_HEADER__
#define __OPTIMIZE_DEFINE_HEADER__

#include "optimize.h"
#include "typedef.h"

#define checkparse_lambda_ _n(checkparse_lambda_)
#define checkparse_defun_ _n(checkparse_defun_)
#define checkparse_defmacro_ _n(checkparse_defmacro_)
#define checkparse_deftype_ _n(checkparse_deftype_)
#define checkparse_define_compiler_macro_ _n(checkparse_define_compiler_macro_)
#define checkparse_flet_ _n(checkparse_flet_)

#define optparse_lambda_ _n(optparse_lambda_)
#define optparse_defun_ _n(optparse_defun_)
#define optparse_defmacro_ _n(optparse_defmacro_)
#define optparse_deftype_ _n(optparse_deftype_)
#define optparse_define_compiler_macro_ _n(optparse_define_compiler_macro_)
#define optparse_flet_ _n(optparse_flet_)

int checkparse_lambda_(OptimizeInfo *str, int *ret);
int checkparse_defun_(OptimizeInfo *str, int *ret);
int checkparse_defmacro_(OptimizeInfo *str, int *ret);
int checkparse_deftype_(OptimizeInfo *str, int *ret);
int checkparse_define_compiler_macro_(OptimizeInfo *str, int *ret);
int checkparse_flet_(OptimizeInfo *str, int *ret);

int optparse_lambda_(OptimizeInfo *str, int *ret);
int optparse_defun_(OptimizeInfo *str, int *ret);
int optparse_defmacro_(OptimizeInfo *str, int *ret);
int optparse_deftype_(OptimizeInfo *str, int *ret);
int optparse_define_compiler_macro_(OptimizeInfo *str, int *ret);
int optparse_flet_(OptimizeInfo *str, int *ret);

#endif

