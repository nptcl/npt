#ifndef __CODE_LAMBDA_HEADER__
#define __CODE_LAMBDA_HEADER__

#include "execute.h"
#include "typedef.h"

_g int let_free_code(Execute ptr, addr list);

_g int lambda_set_code(Execute ptr, CodeValue x);
_g int lambda_push_code(Execute ptr, CodeValue x);
_g int lambda_execute_code(Execute ptr, CodeValue x);
_g int macro_set_code(Execute ptr, CodeValue x);
_g int macro_push_code(Execute ptr, CodeValue x);
_g int macro_execute_code(Execute ptr, CodeValue x);
_g int bind_set_code(Execute ptr, CodeValue x);
_g int bind_push_code(Execute ptr, CodeValue x);
_g int flet_set_code(Execute ptr, CodeValue x);
_g int flet_push_code(Execute ptr, CodeValue x);
_g int labels_set_code(Execute ptr, CodeValue x);
_g int labels_push_code(Execute ptr, CodeValue x);
_g int locally_declare_code(Execute ptr, CodeValue x);
_g int bind_values_set_code(Execute ptr, CodeValue x);
_g int bind_values_push_code(Execute ptr, CodeValue x);

#endif

