#ifndef __SCOPE_DECLARE_HEADER__
#define __SCOPE_DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

#define specialp_tablevalue_ _n(specialp_tablevalue_)
#define find_tablevalue _n(find_tablevalue)
#define find_tablefunction _n(find_tablefunction)
#define globalp_tablefunction_ _n(globalp_tablefunction_)
#define apply_declare_ _n(apply_declare_)

int specialp_tablevalue_(Execute ptr, addr stack, addr symbol, int *ret);
int find_tablevalue(addr stack, addr symbol, addr *ret);
int find_tablefunction(addr stack, addr call, addr *ret);
int globalp_tablefunction_(Execute ptr, addr stack, addr call, int *ret);
int apply_declare_(Execute ptr, addr stack, addr decl, addr *ret);

#endif

