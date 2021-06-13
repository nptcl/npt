#ifndef __MAKE_HEADER__
#define __MAKE_HEADER__

#include "execute.h"
#include "local.h"
#include "make_typedef.h"
#include "typedef.h"

#define set_code_make_struct _n(set_code_make_struct)
#define code_make_execute_ _n(code_make_execute_)
#define code_make_ _n(code_make_)
#define init_make _n(init_make)

void set_code_make_struct(struct code_make_struct *str, Execute ptr, addr code);
int code_make_execute_(CodeMake ptr, addr scope);
int code_make_(Execute ptr, addr *ret, addr scope);
void init_make(void);

#endif

