#ifndef __CODE_LAMBDA_HEADER__
#define __CODE_LAMBDA_HEADER__

#include "execute.h"
#include "typedef.h"

_g int pop_code(Execute ptr, CodeValue x);
_g int pop_unbound_code(Execute ptr, CodeValue x);
_g int getf_code(Execute ptr, CodeValue x);
_g int rest_code(Execute ptr, CodeValue x);
_g int allow_other_keys_code(Execute ptr, CodeValue x);
_g int rest_null_code(Execute ptr, CodeValue x);
_g int whole_code(Execute ptr, CodeValue x);
_g int lambda_code(Execute ptr, CodeValue x);
_g int lambda_name_code(Execute ptr, CodeValue x);
_g int lambda_type_code(Execute ptr, CodeValue x);
_g int lambda_doc_code(Execute ptr, CodeValue x);
_g int lambda_form_code(Execute ptr, CodeValue x);
_g int lambda_defun_code(Execute ptr, CodeValue x);
_g int lambda_closure_code(Execute ptr, CodeValue x);
_g int lambda_lexical_code(Execute ptr, CodeValue x);
_g int macro_code(Execute ptr, CodeValue x);
_g int macro_special_code(Execute ptr, CodeValue x);
_g int macro_env_code(Execute ptr, CodeValue x);
_g int macro_whole_code(Execute ptr, CodeValue x);
_g int bind1_type_code(Execute ptr, CodeValue x);
_g int bind1_special_code(Execute ptr, CodeValue x);
_g int bind1_lexical_code(Execute ptr, CodeValue x);
_g int bind2_type_code(Execute ptr, CodeValue x);
_g int bind2_special_code(Execute ptr, CodeValue x);
_g int bind2_lexical_code(Execute ptr, CodeValue x);

#endif

