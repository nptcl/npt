#ifndef __MAKE_VALUE_HEADER__
#define __MAKE_VALUE_HEADER__

#include "make_typedef.h"
#include "typedef.h"

#define code_make_debug_ _n(code_make_debug_)
#define code_make_nil_ _n(code_make_nil_)
#define code_make_t_ _n(code_make_t_)
#define code_make_value_ _n(code_make_value_)
#define code_make_value2_ _n(code_make_value2_)
#define code_make_symbol_ _n(code_make_symbol_)
#define code_make_declaim_ _n(code_make_declaim_)
#define code_make_lexical_ _n(code_make_lexical_)
#define code_make_progn_ _n(code_make_progn_)
#define code_make_let_ _n(code_make_let_)
#define code_make_leta_ _n(code_make_leta_)
#define code_make_setq_ _n(code_make_setq_)
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

#define code_make_free_ _n(code_make_free_)
#define code_make_type_value _n(code_make_type_value)

int code_make_debug_(CodeMake ptr, addr scope, int (*call)(CodeMake, addr));
int code_make_nil_(CodeMake ptr, addr ignore);
int code_make_t_(CodeMake ptr, addr ignore);
int code_make_value_(CodeMake ptr, addr scope);
int code_make_value2_(CodeMake ptr, addr scope);
int code_make_symbol_(CodeMake ptr, addr scope);
int code_make_declaim_(CodeMake ptr, addr scope);
int code_make_lexical_(CodeMake ptr, addr scope);
int code_make_progn_(CodeMake ptr, addr scope);
int code_make_let_(CodeMake ptr, addr scope);
int code_make_leta_(CodeMake ptr, addr scope);
int code_make_setq_(CodeMake ptr, addr scope);
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

int code_make_free_(CodeMake ptr, addr list, addr escape);
void code_make_type_value(CodeMake ptr, addr pos, addr escape);

#endif

