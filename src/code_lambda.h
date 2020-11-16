#ifndef __CODE_LAMBDA_HEADER__
#define __CODE_LAMBDA_HEADER__

#include "execute.h"
#include "typedef.h"

#define pop_code _n(pop_code)
#define pop_unbound_code _n(pop_unbound_code)
#define getf_code _n(getf_code)
#define rest_code _n(rest_code)
#define allow_other_keys_code _n(allow_other_keys_code)
#define rest_null_code _n(rest_null_code)
#define whole_code _n(whole_code)
#define lambda_code _n(lambda_code)
#define lambda_name_code _n(lambda_name_code)
#define lambda_type_code _n(lambda_type_code)
#define lambda_doc_code _n(lambda_doc_code)
#define lambda_form_code _n(lambda_form_code)
#define lambda_defun_code _n(lambda_defun_code)
#define lambda_closure_code _n(lambda_closure_code)
#define lambda_lexical_code _n(lambda_lexical_code)
#define lambda_cache_code _n(lambda_cache_code)
#define lambda_cache_set_code _n(lambda_cache_set_code)
#define macro_code _n(macro_code)
#define macro_special_code _n(macro_special_code)
#define macro_env_code _n(macro_env_code)
#define macro_whole_code _n(macro_whole_code)
#define bind1_type_code _n(bind1_type_code)
#define bind1_special_code _n(bind1_special_code)
#define bind1_lexical_code _n(bind1_lexical_code)
#define bind2_type_code _n(bind2_type_code)
#define bind2_special_code _n(bind2_special_code)
#define bind2_lexical_code _n(bind2_lexical_code)

int pop_code(Execute ptr, CodeValue x);
int pop_unbound_code(Execute ptr, CodeValue x);
int getf_code(Execute ptr, CodeValue x);
int rest_code(Execute ptr, CodeValue x);
int allow_other_keys_code(Execute ptr, CodeValue x);
int rest_null_code(Execute ptr, CodeValue x);
int whole_code(Execute ptr, CodeValue x);
int lambda_code(Execute ptr, CodeValue x);
int lambda_name_code(Execute ptr, CodeValue x);
int lambda_type_code(Execute ptr, CodeValue x);
int lambda_doc_code(Execute ptr, CodeValue x);
int lambda_form_code(Execute ptr, CodeValue x);
int lambda_defun_code(Execute ptr, CodeValue x);
int lambda_closure_code(Execute ptr, CodeValue x);
int lambda_lexical_code(Execute ptr, CodeValue x);
int lambda_cache_code(Execute ptr, CodeValue x);
int lambda_cache_set_code(Execute ptr, CodeValue x);
int macro_code(Execute ptr, CodeValue x);
int macro_special_code(Execute ptr, CodeValue x);
int macro_env_code(Execute ptr, CodeValue x);
int macro_whole_code(Execute ptr, CodeValue x);
int bind1_type_code(Execute ptr, CodeValue x);
int bind1_special_code(Execute ptr, CodeValue x);
int bind1_lexical_code(Execute ptr, CodeValue x);
int bind2_type_code(Execute ptr, CodeValue x);
int bind2_special_code(Execute ptr, CodeValue x);
int bind2_lexical_code(Execute ptr, CodeValue x);

#endif

