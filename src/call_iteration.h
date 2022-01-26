#ifndef __CALL_ITERATION_HEADER__
#define __CALL_ITERATION_HEADER__

#include "execute.h"
#include "typedef.h"

#define do_common_ _n(do_common_)
#define doa_common_ _n(doa_common_)
#define dotimes_common_ _n(dotimes_common_)
#define dolist_common_ _n(dolist_common_)

int do_common_(addr form, addr env, addr *ret);
int doa_common_(addr form, addr env, addr *ret);
int dotimes_common_(addr form, addr env, addr *ret);
int dolist_common_(Execute ptr, addr form, addr env, addr *ret);

#endif

