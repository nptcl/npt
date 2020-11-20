#ifndef __TERM_ARCH_HEADER__
#define __TERM_ARCH_HEADER__

#include "execute.h"
#include "typedef.h"

#define begin_term _n(begin_term)
#define end_term _n(end_term)

int begin_term(int argv);
int end_term(void);
int prompt_term_(Execute ptr, addr pos);
int readline_term_(Execute ptr, addr *ret);

#endif

