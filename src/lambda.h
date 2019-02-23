#ifndef __LAMBDA_HEADER__
#define __LAMBDA_HEADER__

#include "local.h"
#include "typedef.h"

void lambda_macro(LocalRoot local, addr *ret, addr cons, addr instance);
void lambda_generic_function(LocalRoot local, addr *ret, addr cons);
void lambda_specialized(LocalRoot local, addr *ret, addr cons);
void lambda_ordinary(LocalRoot local, addr *ret, addr cons);
void atleast_argument_count(addr cons, size_t *ret);
void lambda_defsetf(LocalRoot local, addr *ret, addr cons);

void getenvironment_macro_lambda(addr pos, addr *ret);
void allsymbol_macro_lambda_heap(LocalRoot local, addr *ret, addr args);

#endif

