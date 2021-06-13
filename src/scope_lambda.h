#ifndef __SCOPE_LAMBDA_HEADER__
#define __SCOPE_LAMBDA_HEADER__

#include "execute.h"
#include "scope_typedef.h"
#include "typedef.h"

#define scope_init_lambda _n(scope_init_lambda)
#define localhold_lambda_struct _n(localhold_lambda_struct)
#define lambda_lexical_heap _n(lambda_lexical_heap)

#define scope_lambda_object_ _n(scope_lambda_object_)
#define scope_lambda_call_ _n(scope_lambda_call_)

#define scope_macro_lambda_execute_ _n(scope_macro_lambda_execute_)
#define scope_macro_lambda_object_ _n(scope_macro_lambda_object_)
#define scope_macro_lambda_call_ _n(scope_macro_lambda_call_)

void scope_init_lambda(struct lambda_struct *str, EvalParse eval, int globalp);
void localhold_lambda_struct(LocalRoot local, struct lambda_struct *str);
void lambda_lexical_heap(addr stack, addr *ret);

int scope_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_lambda_call_(Execute ptr, addr *ret, addr eval);

int scope_macro_lambda_execute_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_macro_lambda_object_(Execute ptr, struct lambda_struct *str, addr *ret);
int scope_macro_lambda_call_(Execute ptr, struct lambda_struct *str, addr *ret);

#endif

