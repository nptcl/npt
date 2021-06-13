#ifndef __MAKE_VALUE_HEADER__
#define __MAKE_VALUE_HEADER__

#include "make_typedef.h"
#include "typedef.h"

#define code_make_nil_ _n(code_make_nil_)
#define code_make_t_ _n(code_make_t_)
#define code_make_value_ _n(code_make_value_)
#define code_make_symbol_ _n(code_make_symbol_)
#define code_make_declaim_ _n(code_make_declaim_)
#define code_make_lexical_ _n(code_make_lexical_)
#define code_make_progn_ _n(code_make_progn_)
#define code_make_let_ _n(code_make_let_)
#define code_make_leta_ _n(code_make_leta_)
#define code_make_setq_ _n(code_make_setq_)
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
#define code_make_values_ _n(code_make_values_)
#define code_make_the_ _n(code_make_the_)
#define code_make_eval_when_ _n(code_make_eval_when_)
#define code_make_locally_ _n(code_make_locally_)
#define code_make_if_ _n(code_make_if_)
#define code_make_unwind_protect_ _n(code_make_unwind_protect_)
#define code_make_tagbody_ _n(code_make_tagbody_)
#define code_make_go_ _n(code_make_go_)
#define code_make_block_ _n(code_make_block_)
#define code_make_return_from_ _n(code_make_return_from_)
#define code_make_catch_ _n(code_make_catch_)
#define code_make_throw_ _n(code_make_throw_)
#define code_make_multiple_value_bind_ _n(code_make_multiple_value_bind_)
#define code_make_multiple_value_call_ _n(code_make_multiple_value_call_)
#define code_make_multiple_value_prog1_ _n(code_make_multiple_value_prog1_)
#define code_make_nth_value_ _n(code_make_nth_value_)
#define code_make_progv_ _n(code_make_progv_)
#define code_make_load_time_value_ _n(code_make_load_time_value_)
#define code_make_step_ _n(code_make_step_)

int code_make_nil_(CodeMake ptr, addr ignore);
int code_make_t_(CodeMake ptr, addr ignore);
int code_make_value_(CodeMake ptr, addr scope);
int code_make_symbol_(CodeMake ptr, addr scope);
int code_make_declaim_(CodeMake ptr, addr scope);
int code_make_lexical_(CodeMake ptr, addr scope);
int code_make_progn_(CodeMake ptr, addr scope);
int code_make_let_(CodeMake ptr, addr scope);
int code_make_leta_(CodeMake ptr, addr scope);
int code_make_setq_(CodeMake ptr, addr scope);
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
int code_make_values_(CodeMake ptr, addr scope);
int code_make_the_(CodeMake ptr, addr scope);
int code_make_eval_when_(CodeMake ptr, addr scope);
int code_make_locally_(CodeMake ptr, addr scope);
int code_make_if_(CodeMake ptr, addr scope);
int code_make_unwind_protect_(CodeMake ptr, addr scope);
int code_make_tagbody_(CodeMake ptr, addr scope);
int code_make_go_(CodeMake ptr, addr scope);
int code_make_block_(CodeMake ptr, addr scope);
int code_make_return_from_(CodeMake ptr, addr scope);
int code_make_catch_(CodeMake ptr, addr scope);
int code_make_throw_(CodeMake ptr, addr scope);
int code_make_multiple_value_bind_(CodeMake ptr, addr scope);
int code_make_multiple_value_call_(CodeMake ptr, addr scope);
int code_make_multiple_value_prog1_(CodeMake ptr, addr scope);
int code_make_nth_value_(CodeMake ptr, addr scope);
int code_make_progv_(CodeMake ptr, addr scope);
int code_make_load_time_value_(CodeMake ptr, addr scope);
int code_make_step_(CodeMake ptr, addr scope);

#endif

