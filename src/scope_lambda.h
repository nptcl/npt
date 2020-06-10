#ifndef __SCOPE_LAMBDA_HEADER__
#define __SCOPE_LAMBDA_HEADER__

#include "execute.h"
#include "parse.h"
#include "scope_let.h"
#include "typedef.h"

struct lambda_struct {
	addr stack, call, table, lexical;
	addr args, decl, doc, cons, clos, free, the;
	addr form, defun, body_the;
	unsigned globalp;
	EvalParse eval;
};

_g void scope_init_lambda(struct lambda_struct *str, EvalParse eval, int globalp);

_g int scope_function_call(Execute ptr, addr *ret, addr eval);
_g void lambda_lexical_heap(addr stack, addr *ret);
_g int scope_lambda_call(Execute ptr, addr *ret, addr eval);
_g int scope_defun_call(Execute ptr, struct lambda_struct *str, addr *ret);
_g int scope_macro_lambda_call(Execute ptr, struct lambda_struct *str, addr *ret);
_g int scope_deftype_call(Execute ptr, struct lambda_struct *str, addr *ret);
_g int scope_define_compiler_macro_call(Execute ptr,
		struct lambda_struct *str, addr *ret);
_g int scope_bind_call(Execute ptr, addr *ret, addr expr, addr args);

_g int scope_flet_call(Execute ptr, struct let_struct *str);
_g int scope_labels_call(Execute ptr, struct let_struct *str);
_g int scope_call_call(Execute ptr, addr first, addr args, addr *ret);

#endif

