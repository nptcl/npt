#ifndef __REQUIRE_HEADER__
#define __REQUIRE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define provide_common_ _n(provide_common_)
#define modules_find_ _n(modules_find_)
#define modules_delete_ _n(modules_delete_)
#define require_common_ _n(require_common_)
#define require_append_ _n(require_append_)
#define require_delete_ _n(require_delete_)
#define build_require _n(build_require)
#define init_require _n(init_require)

int provide_common_(Execute ptr, addr var);
int modules_find_(Execute ptr, addr var, int *ret);
int modules_delete_(Execute ptr, addr var);
int require_common_(Execute ptr, addr var, addr opt);
int require_append_(Execute ptr, addr var, int forcep, int *ret);
int require_delete_(Execute ptr, addr var, int forcep, int *ret);
void build_require(void);
void init_require(void);

#endif

