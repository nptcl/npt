#ifndef __CALL_DATA_HEADER__
#define __CALL_DATA_HEADER__

#include "execute.h"
#include "typedef.h"

_g int apply_common(Execute ptr, addr call, addr arg, addr args);
_g int defun_common(Execute ptr, addr right, addr env, addr *ret);
_g int fdefinition_common(Execute ptr, addr name, addr *ret);
_g void setf_fdefinition_common(addr value, addr name);
_g int fboundp_common(addr name);
_g void fmakunbound_common(addr name);
_g void function_lambda_expression_common(addr var, addr *ret1, addr *ret2, addr *ret3);
_g void lambda_list_keywords_common(addr *ret);
_g void defconstant_common(addr form, addr env, addr *ret);
_g void defparameter_common(addr form, addr env, addr *ret);
_g void defvar_common(addr form, addr env, addr *ret);
_g int destructuring_bind_common(Execute ptr, addr form, addr env, addr *ret);
_g void psetq_common(Execute ptr, addr form, addr env, addr *ret);
_g void psetf_common(Execute ptr, addr form, addr env, addr *ret);
_g void return_common(addr form, addr env, addr *ret);
_g void complement_common(addr var, addr *ret);
_g void constantly_common(addr var, addr *ret);
_g int every_common(Execute ptr, addr call, addr rest, addr *ret);
_g int notevery_common(Execute ptr, addr call, addr rest, addr *ret);
_g int some_common(Execute ptr, addr call, addr rest, addr *ret);
_g int notany_common(Execute ptr, addr call, addr rest, addr *ret);
_g void and_common(addr form, addr env, addr *ret);
_g void cond_common(addr form, addr env, addr *ret);
_g void or_common(Execute ptr, addr form, addr env, addr *ret);
_g void when_common(addr form, addr env, addr *ret);
_g void unless_common(addr form, addr env, addr *ret);
_g void case_common(Execute ptr, addr form, addr env, addr *ret);
_g void ecase_common(Execute ptr, addr form, addr env, addr *ret);
_g int ccase_common(Execute ptr, addr form, addr env, addr *ret);
_g void typecase_common(Execute ptr, addr form, addr env, addr *ret);
_g void etypecase_common(Execute ptr, addr form, addr env, addr *ret);
_g int ctypecase_common(Execute ptr, addr form, addr env, addr *ret);
_g int multiple_value_bind_common(Execute ptr, addr form, addr env, addr *ret);
_g void multiple_value_list_common(addr form, addr env, addr *ret);
_g void multiple_value_setq_common(addr form, addr env, addr *ret);
_g void nth_value_common(addr form, addr env, addr *ret);
_g void prog_common(addr form, addr env, addr *ret);
_g void proga_common(addr form, addr env, addr *ret);
_g void prog1_common(Execute ptr, addr form, addr env, addr *ret);
_g void prog2_common(addr form, addr env, addr *ret);
_g void define_modify_macro_common(LocalRoot local, addr form, addr env, addr *ret);
_g void defsetf_common(addr form, addr env, addr *ret);
_g void define_setf_expander_common(addr form, addr env, addr *ret);
_g int setf_common(Execute ptr, addr form, addr env, addr *ret);
_g int shiftf_common(Execute ptr, addr form, addr env, addr *ret);
_g int rotatef_common(Execute ptr, addr form, addr env, addr *ret);

#endif

