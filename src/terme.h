#ifndef __TERME_HEADER__
#define __TERME_HEADER__

#include "execute.h"
#include "typedef.h"

#define build_terme _n(build_terme)
#define begin_terme _n(begin_terme)
#define end_terme _n(end_terme)
#define prompt_terme_ _n(prompt_terme_)
#define readline_terme_ _n(readline_terme_)

void build_terme(void);
int begin_terme(int argv);
int end_terme(void);
int prompt_terme_(Execute ptr, addr pos);
int readline_terme_(Execute ptr, addr *ret);

#endif
