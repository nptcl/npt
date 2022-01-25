#ifndef __SETF_HEADER__
#define __SETF_HEADER__

#include "typedef.h"

#define function_setf_the _n(function_setf_the)
#define function_setf_values _n(function_setf_values)
#define function_setf_getf _n(function_setf_getf)
#define function_setf_apply _n(function_setf_apply)
#define get_setf_expansion_ _n(get_setf_expansion_)

/* common-lisp setf-expansion */
int function_setf_the(Execute ptr, addr args, addr env);
int function_setf_values(Execute ptr, addr args, addr env);
int function_setf_getf(Execute ptr, addr args, addr env);
int function_setf_apply(Execute ptr, addr args, addr env);

/* get-setf-expander */
int get_setf_expansion_(Execute ptr, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader);

#endif

